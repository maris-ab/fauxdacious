/*
 * output.c
 * Copyright 2009-2024 John Lindgren
 * DSD, DoP and FLOAT64 support added by Maris Abele
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions, and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions, and the following disclaimer in the documentation
 *    provided with the distribution.
 *
 * This software is provided "as is" and without any warranty, express or
 * implied. In no event shall the authors be liable for any damages arising from
 * the use of this software.
 */

#include "output.h"

#include <sys/stat.h>
#include <math.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "audstrings.h"
#include "equalizer.h"
#include "hook.h"
#include "i18n.h"
#include "interface.h"
#include "internal.h"
#include "plugin.h"
#include "plugins.h"
#include "runtime.h"

#include "playlist.h"

/* With Audacious 3.7, there is some support for secondary output plugins.
 * Notes and limitations:
 *  - Only one secondary output can be in use at a time.
 *  - A reduced API is used, consisting of only open_audio(), close_audio(), and
 *    write_audio().
 *  - The primary and secondary outputs are run from the same thread, with
 *    timing controlled by the primary's period_wait().  To avoid dropouts in
 *    the primary output, the secondary's write_audio() must be able to process
 *    audio faster than realtime.
 *  - The secondary's write_audio() is called in a tight loop until it has
 *    caught up to the primary, and should never return a zero byte count. */

static pthread_mutex_t mutex_major = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t mutex_minor = PTHREAD_MUTEX_INITIALIZER;

#define LOCK_MAJOR pthread_mutex_lock (& mutex_major)
#define UNLOCK_MAJOR pthread_mutex_unlock (& mutex_major)
#define LOCK_MINOR pthread_mutex_lock (& mutex_minor)
#define UNLOCK_MINOR pthread_mutex_unlock (& mutex_minor)
#define LOCK_ALL do { LOCK_MAJOR; LOCK_MINOR; } while (0)
#define UNLOCK_ALL do { UNLOCK_MINOR; UNLOCK_MAJOR; } while (0)

/* State variables.  State changes that are allowed between LOCK_MINOR and
 * UNLOCK_MINOR (all others must take place between LOCK_ALL and UNLOCK_ALL):
 * s_paused -> true or false, s_flushed -> true, s_resetting -> true,
 * s_secondary -> true or false */

static bool s_input; /* input plugin connected */
static bool s_output; /* primary output plugin connected */
static bool s_secondary; /* secondary output plugin connected */
static bool s_gain; /* replay gain info set */
static bool s_paused; /* paused */
static bool s_flushed; /* flushed, writes ignored until resume */
static bool s_resetting; /* resetting output system */

/* Condition variable linked to LOCK_MINOR.
 * The input thread will wait if the following is true:
 *   ((! s_output || s_paused || s_resetting) && ! s_flushed)
 * Hence you must signal if you cause the inverse to be true:
 *   ((s_output && ! s_paused && ! s_resetting) || s_flushed) */

static pthread_cond_t cond_minor = PTHREAD_COND_INITIALIZER;

#define SIGNAL_MINOR pthread_cond_broadcast (& cond_minor)
#define WAIT_MINOR pthread_cond_wait (& cond_minor, & mutex_minor)

short pausemuted = 0;   /* JWT:ADDED FOR ALLOWING PAUSE TO CONTINUE READING STREAMS, ONLY PAUSING OUTPUT! */
static bool s_songautoeq; /* JWT:ADDED TO ALLOW SONG/STREAM-SPECIFIC EQUALIZATION. */

static OutputPlugin * cop; /* current (primary) output plugin */
static OutputPlugin * sop; /* secondary output plugin */

static OutputStream record_stream;

static int seek_time;
static String in_filename;
static Tuple in_tuple;
static int in_format, in_channels, in_rate;
static int effect_channels, effect_rate;
static int sec_channels, sec_rate;
static int out_format, out_channels, out_rate;
static int out_bytes_per_sec, out_bytes_held;
static int64_t in_frames, out_bytes_written;
static ReplayGainInfo gain_info;

static Index<audio_sample> floatbuffer; // Float audio
static Index<uint8_t> dsdbuffer; // DSD audio
static Index<char> outbuffer;

static inline int get_format (bool & automatic)
{
    automatic = false;

    switch (aud_get_int (0, "output_bit_depth"))
    {
    case 16:
        if (is_dsd(in_format)) // No DoP for 16 bit
            return FMT_DSD_MSB16_BE;
        return FMT_S16_NE;
    case 24:
        if (is_dsd(in_format) && !aud_get_bool(nullptr, "dsd_dop"))
            return FMT_DSD_MSB24_3BE;
        return FMT_S24_3NE;
    case 32:
        if (is_dsd(in_format) && !aud_get_bool(nullptr, "dsd_dop"))
            return FMT_DSD_MSB32_BE;
        return FMT_S32_NE;
    case 64:
        if (is_dsd(in_format))
            return FMT_DSD_MSB32_BE;
        return FMT_FLOAT64;

        // return FMT_FLOAT for "auto" as well
    case -1:
        automatic = true;
    default:
        if (is_dsd(in_format))
        {
            if (aud_get_bool(nullptr, "dsd_dop")) return FMT_S32_NE;
            return FMT_DSD_MSB32_BE;
        }
        return FMT_FLOAT;
    }
}

/* assumes LOCK_ALL, s_input */
static void setup_effects ()
{
    effect_channels = in_channels;
    effect_rate = in_rate;

    effect_start (effect_channels, effect_rate);
    eq_set_format (effect_channels, effect_rate);
}

/* assumes LOCK_ALL */
static void cleanup_output ()
{
    if (! s_output)
        return;

    if (! s_paused && ! s_flushed && ! s_resetting)
    {
        UNLOCK_MINOR;
        cop->drain ();
        LOCK_MINOR;
    }

    s_output = false;

    floatbuffer.clear ();
    dsdbuffer.clear();
    outbuffer.clear ();

    cop->close_audio ();
    vis_runner_start_stop (false, false);
}

/* assumes LOCK_MINOR */
static void cleanup_secondary ()
{
    if (! s_secondary)
        return;

    s_secondary = false;
    sop->close_audio ();
}

/* assumes LOCK_MINOR, s_output */
static void apply_pause ()
{
    /* JWT:  IF PauseMute SET, PAUSE SHOULD JUST ZERO-OUT OUTPUT (BUT CONTINUE PLAYING)
             THIS IS SO WE CAN "MUTE" COMMERCIALS FROM RADIO WHILST PLAYING
             SOMETHING ELSE!  THIS WAY, WE CAN RESUME AT THE CURRENT POINT vs
             WHERE WE PAUSED!
    */
    if (aud_get_pausemute_mode ())
    {
        pausemuted = s_paused;
        vis_runner_start_stop (true, false);
    }
    else
    {
        cop->pause (s_paused);
        vis_runner_start_stop (true, s_paused);
    }
}

static bool open_audio_with_info (OutputPlugin * op, const char * filename,
 const Tuple & tuple, int format, int rate, int chans, String & error)
{
    op->set_info (filename, tuple);
    return op->open_audio (format, rate, chans, error);
}

/* assumes LOCK_ALL, s_input */
static void setup_output (bool new_input)
{
    if (! cop)
        return;

    bool automatic;
    int format = get_format (automatic);

    if (s_output && effect_channels == out_channels &&
     effect_rate == out_rate && ! (new_input && cop->force_reopen))
    {
        AUDINFO ("Reuse output, %d channels, %d Hz.\n", effect_channels, effect_rate);
        return;
    }

    if (is_dsd(in_format))
    {   effect_rate = in_rate;
        if(aud_get_bool(nullptr, "dsd_dop"))
            effect_rate = effect_rate * FMT_SIZEOF(out_format) / 2;
    }

    AUDINFO ("Setup output, format %d, %d channels, %d Hz.\n", format, effect_channels, effect_rate);

    cleanup_output ();

    String error;
    while (! open_audio_with_info (cop, in_filename, in_tuple, format,
            effect_rate, effect_channels, error))
    {
        if (!is_dsd(in_format) || aud_get_bool(nullptr, "dsd_dop"))
        {   // PCM
            if (automatic && format == FMT_FLOAT)
#ifdef DEF_AUDIO_FLOAT64
                format = FMT_FLOAT64;
            else if (automatic && format == FMT_FLOAT64)
#endif
                format = FMT_S32_NE;
            else if (automatic && format == FMT_S32_NE)
                format = FMT_S24_NE; // some output plugins support only padded 24-bit
            else if (automatic && format == FMT_S24_NE)
                format = FMT_S24_3NE;
            else if (automatic && format == FMT_S24_3NE)
                format = FMT_S16_NE;
            else if (format == FMT_S16_NE)
                {
                    format = FMT_S32_NE; // Default format after fail
                    automatic = false;
                }
            else
            {
                aud_ui_show_error(error ? (const char *)error
                                        : _("Error opening PCM output stream"));
                return;
            }
        }
        else
        {   // DSD
            if (automatic && format == FMT_FLOAT)
                format = FMT_DSD_MSB32_NE;
            else if (automatic && format == FMT_DSD_MSB32_NE)
                format = FMT_DSD_MSB16_BE; // some output plugins support only padded 24-bit
            else if (automatic && format == FMT_DSD_MSB16_BE)
                format = FMT_DSD_MSB24_3BE;
            else if (automatic && format == FMT_DSD_MSB24_3BE)
                {
                    format = FMT_DSD_MSB32_NE; // Default format after fail
                    automatic = false;
                }
            else
            {
                aud_ui_show_error(error ? (const char *)error
                                        : _("Error opening DSD output stream"));
                return;
            }
        }

        AUDINFO ("Falling back to format %d.\n", format);
    }

    s_output = true;

    out_format = format;
    out_channels = effect_channels;
    out_rate = effect_rate;

    out_bytes_per_sec = FMT_SIZEOF (format) * out_channels * out_rate;
    if (is_dsd(in_format) && aud_get_bool(nullptr, "dsd_dop"))
        out_bytes_per_sec = out_bytes_per_sec * 2 / FMT_SIZEOF(out_format);

    out_bytes_held = 0;
    out_bytes_written = 0;

    apply_pause ();

    if (! (s_paused && ! pausemuted) && ! s_flushed && ! s_resetting)
        SIGNAL_MINOR;
}

/* assumes LOCK_MINOR, s_input */
static void setup_secondary (bool new_input)
{
    if (! sop)
        return;

    int rate, channels;
    record_stream = (OutputStream) aud_get_int (0, "record_stream");

    if (record_stream < OutputStream::AfterEffects)
    {
        rate = in_rate;
        channels = in_channels;
    }
    else
    {
        rate = effect_rate;
        channels = effect_channels;
    }

    if (s_secondary && channels == sec_channels && rate == sec_rate &&
     ! (new_input && sop->force_reopen))
        return;

    cleanup_secondary ();

    String error;
    if (! open_audio_with_info (sop, in_filename, in_tuple, FMT_FLOAT, rate, channels, error))
    {
        aud_ui_show_error (error ? (const char *) error : _("Error recording output stream"));
        return;
    }

    s_secondary = true;

    sec_channels = channels;
    sec_rate = rate;
}

/* assumes LOCK_MINOR, s_output */
static void flush_output ()
{
    out_bytes_held = 0;
    out_bytes_written = 0;

    cop->flush ();
    vis_runner_flush ();
}

static void apply_replay_gain (Index<audio_sample> & data)
{
    int extra_gain_factor = aud_get_fudge_gain ();  /* JWT:TO ALLOW SPECIFYING FUDGE-GAIN */
    if (! aud_get_bool (0, "enable_replay_gain"))
    {
        if (extra_gain_factor) {  /* JWT:TO ALLOW SPECIFYING FUDGE-GAIN */
            float factor = powf (10, (double)extra_gain_factor / 20);
            if (factor < 0.99 || factor > 1.01)
                audio_amplify (data.begin (), 1, data.len (), & factor);
        }
        return;
    }

    float factor = powf (10, aud_get_double (0, "replay_gain_preamp") / 20);

    if (s_gain)
    {
        float peak;

        auto mode = (ReplayGainMode) aud_get_int (0, "replay_gain_mode");
        if ((mode == ReplayGainMode::Album) ||
            (mode == ReplayGainMode::Automatic &&
             (! aud_get_bool (0, "shuffle") || aud_get_bool (0, "album_shuffle"))))
        {
            factor *= powf (10, gain_info.album_gain / 20);
            peak = gain_info.album_peak;
        }
        else
        {
            factor *= powf (10, gain_info.track_gain / 20);
            peak = gain_info.track_peak;
        }

        if (aud_get_bool (0, "enable_clipping_prevention") && peak * factor > 1)
            factor = 1 / peak;
    }
    else
        factor *= powf (10, aud_get_double (0, "default_gain") / 20);

    if (extra_gain_factor)   /* JWT:TO ALLOW SPECIFYING FUDGE-GAIN */
        factor *= powf (10, (double)extra_gain_factor / 20);

    if (factor < 0.99 || factor > 1.01)
        audio_amplify (data.begin (), 1, data.len (), & factor);
}

/* assumes LOCK_MINOR, s_secondary */
static void write_secondary (const Index<audio_sample> & data)
{
    auto begin = (const char *) data.begin ();
    auto end = (const char *) data.end ();

    while (begin < end)
        begin += sop->write_audio (begin, end - begin);
}

/* assumes LOCK_ALL, s_output */
static void write_output (Index<audio_sample> & data)
{
    void * out_data;

    if (is_dsd(in_format))
    {
        // DSD audio
        if (!dsdbuffer.len())
            return;
        if (!is_dsd(out_format)) // is_dop
            outbuffer.resize(dsdbuffer.len()*FMT_SIZEOF(out_format)/2);
        else
            outbuffer.resize(dsdbuffer.len());
        dsdaudio_to_out(dsdbuffer.begin(), outbuffer.begin(), out_format, dsdbuffer.len(), out_channels);
        out_data = outbuffer.begin();
        out_bytes_held = outbuffer.len();
    }
    else
    {
        // Float audio
        if (! data.len ())
            return;

        if (s_secondary && record_stream == OutputStream::AfterEffects)
            write_secondary (data);

        int out_time = aud::rescale<int64_t> (out_bytes_written, out_bytes_per_sec, 1000);
        vis_runner_pass_audio (out_time, data, out_channels, out_rate);

        eq_filter (data.begin (), data.len ());

        if (s_secondary && record_stream == OutputStream::AfterEqualizer)
            write_secondary (data);

        if (aud_get_bool (0, "software_volume_control"))
        {
            StereoVolume v = {aud_get_int (0, "sw_volume_left"), aud_get_int (0, "sw_volume_right")};
            audio_amplify (data.begin (), out_channels, data.len () / out_channels, v);
        }

        if (aud_get_bool (0, "soft_clipping"))
            audio_soft_clip (data.begin (), data.len ());
        out_data = data.begin();

        if (out_format != FMT_AUDIO_SAMPLE)
        {
            outbuffer.resize(FMT_SIZEOF(out_format) * data.len());
            audio_to_int(data.begin(), outbuffer.begin(), out_format, data.len());
            out_data = outbuffer.begin();
        }

        out_bytes_held = FMT_SIZEOF(out_format) * data.len();
    }

    while (! (s_paused && ! pausemuted) && ! s_flushed && ! s_resetting)
    {
        /* JWT: ZERO-OUT (SILENCE) STREAM GOING TO OUTPUT WHILST PAUSING */
        if (pausemuted)
            memset (out_data, '\0', out_bytes_held);

        int written = cop->write_audio (out_data, out_bytes_held);

        out_data = (char *) out_data + written;
        out_bytes_held -= written;
        out_bytes_written += written;

        if (! out_bytes_held)
            break;

        UNLOCK_MINOR;
        cop->period_wait ();
        LOCK_MINOR;
    }
}

/* assumes LOCK_ALL, s_input, s_output */
static bool process_audio (const void * data, int size, int stop_time)
{
    int samples = size / FMT_SIZEOF (in_format);
    bool stopped = false;

    if (stop_time != -1)
    {
        int64_t frames_left = aud::rescale<int64_t> (stop_time - seek_time, 1000, in_rate) - in_frames;
        int64_t samples_left = in_channels * aud::max ((int64_t) 0, frames_left);

        if (samples >= samples_left)
        {
            samples = samples_left;
            stopped = true;
        }
    }

    // DSD audio
    if (is_dsd(in_format))
    {
        dsdbuffer.resize(size);
        dsdaudio_from_in(data, in_format, dsdbuffer.begin(), samples, in_channels);

        in_frames += samples / in_channels / 4 * FMT_SIZEOF(in_format);

        write_output (floatbuffer);
        return !stopped;
    }

    in_frames += samples / in_channels;

    // Float audio
    floatbuffer.resize(samples);
    audio_from_int(data, in_format, floatbuffer.begin(), samples);

    if (s_secondary && record_stream == OutputStream::AsDecoded)
        write_secondary (floatbuffer);

    apply_replay_gain (floatbuffer);

    if (s_secondary && record_stream == OutputStream::AfterReplayGain)
        write_secondary (floatbuffer);

    write_output (effect_process (floatbuffer));

    return ! stopped;
}

/* assumes LOCK_ALL, s_output */
static void finish_effects (bool end_of_playlist)
{
    floatbuffer.resize (0);
    dsdbuffer.resize(0);
    write_output (effect_finish (floatbuffer, end_of_playlist));
}

/* JWT:NEXT 2 FUNCTIONS ADDED TO ALLOW SONG/STREAM-SPECIFIC EQUALIZATION. */
/* JWT:FIXME:I WANTED TO USE filename_build() BUT COULD NEVER GET IT TO WORK?! */
static void do_save_eq_file (StringBuf filename)
{
    EqualizerPreset preset;
    aud_eq_update_preset (preset);

    VFSFile file (filename, "w");
    if (file)
        aud_save_preset_file (preset, file);
}

static bool do_load_eq_file (StringBuf filename, bool save_prev_preset_as_default, bool chk_cuefile)
{
    bool loadedit = false;
    struct stat statbuf;
    EqualizerPreset preset;

    StringBuf filename_local = uri_to_filename (filename);
    bool havesongeqfile = ! (stat ((const char *) filename_local, &statbuf));   /* (*->N) SONG HAS NO SONG-SPECIFIC PRESET FILE, SO DO NOTHING. */

    if (! havesongeqfile && chk_cuefile)
    {
        /* JWT:SEE IF WE ARE REALLY A CUESHEET, IF SO, LOOK FOR A PRESET FOR THAT CUESHEET: */
        String cue_suffix = in_tuple.get_str (Tuple::Suffix);
        if (cue_suffix && cue_suffix[0] && strstr_nocase ((const char *) cue_suffix, "cue"))
        {
            StringBuf path = filename_get_parent (filename_local);
            filename = filename_normalize (str_concat ({path, "/", in_tuple.get_str (Tuple::Basename),
                    ".", cue_suffix, ".preset"}));
            const char * filenamechar = (const char *) filename;
            havesongeqfile = ! (stat (filenamechar, &statbuf));
            if (havesongeqfile)
                filename = filename_to_uri (filenamechar);
        }
    }
    if (havesongeqfile)
    {
        if (save_prev_preset_as_default && s_songautoeq && aud_get_bool (nullptr, "_autoeffects_loaded")
                && aud_get_bool(nullptr, "eqpreset_use_effects"))
        {
            /* RESTORE NON-AUTO EFFECTS PRESETS (P(e)=>P(*)) SINCE SONG MAY NOT HAVE EFFECTS SAVED! */
            do_load_eq_file (filename_to_uri (str_concat ({aud_get_path (AudPath::UserDir), "/_nonauto.preset"})),
                    false, false);
            aud_set_bool (nullptr, "_autoeffects_loaded", false);
        }
        VFSFile file (filename, "r");
        if (file)  /* (*->P) SONG HAS A PRESET FILE! */
        {
            /* JWT: (N->P) IF LOADING SONG-SPECIFIC PRESETS AND WE DON'T HAVE SONG-SPECIFIC ONES IN EFFECT NOW, THEN SAVE
                THE CURRENT PRESETS OUT AS THE "DEFAULT" FOR LATER RESTORATION ON NEXT SONG W/O SONG-SPECIFIC PRESETS */
            if (save_prev_preset_as_default && ! s_songautoeq)
            {
                bool saveopt = aud_get_bool (nullptr, "eqpreset_save_effects");  // IGNORE USER-SET SAVE OPT FOR DEFAULT PRESET!:
                aud_set_bool (nullptr, "eqpreset_save_effects", aud_get_bool(nullptr, "eqpreset_use_effects"));
                do_save_eq_file (filename_to_uri (str_concat ({aud_get_path (AudPath::UserDir), "/_nonauto.preset"})));
                aud_set_bool (nullptr, "eqpreset_save_effects", saveopt);
            }
            aud_load_preset_file (preset, file);
            aud_eq_apply_preset (preset);   /* (*->P) APPLY THE SONG-SPECIFIC PRESET FILE AND NOTE THAT WE DID THAT! */
            loadedit = true;
        }
    }

    return loadedit;
}

bool output_open_audio (const String & filename, const Tuple & tuple,
 int format, int rate, int channels, int start_time)
{
    bool prev_songautoeq = aud_get_bool(nullptr, "equalizer_songauto");
    /* prevent division by zero */
    if (rate < 1 || channels < 1 || channels > AUD_MAX_CHANNELS)
        return false;

    s_songautoeq = prev_songautoeq;

    LOCK_ALL;

    if (s_output && s_paused)
    {
        effect_flush (true);
        cleanup_output ();
    }

    s_input = true;
    s_gain = s_paused = s_flushed = false;
    seek_time = start_time;

    in_filename = filename;
    in_tuple = tuple.ref ();
    in_format = format;
    in_channels = channels;
    in_rate = rate;
    in_frames = 0;

    /* JWT:NEXT CONDITION BLOCK ADDED TO ALLOW SONG/STREAM-SPECIFIC EQUALIZATION IF [Autoload].
       4 possible scenarios when opening:  (N=ENTRY HAS NO AUTO-PRESET FILE, P=ENTRY HAS PRESET FILE):
       BASED ON PREV. ENTRY PLAYED (PREV->CURRENT):  (NOTE: WE HANDLE P->N REGARDLESS OF [Autoload] STATE!)
       1) N->N:  Do nothing with equalizer.
       2) N->P:  (IF [Autoload]) Save current EQ settings as "default", then load entry's auto-preset file.
       3) P->P:  (IF [Autoload]) Load entry's auto-preset file.
       4) P->N:  Load the prev. saved "default" preset file.
       IF [Autoload] IS OFF, THEN THE ONLY 2 POSSIBLE STATES ARE N->N AND P->N SINCE NO
       SONG-SPECIFIC PRESET FILE IS CHECKED FOR, MUCH LESS LOADED.
    */
    if (strncmp (filename, "cdda://", 7) && strncmp (filename, "dvd://", 6))
        aud_set_str (nullptr, "playingdiskid", "");  // JWT: WE'RE NOT (NO LONGER) PLAYING A DISK!
    if (aud_get_bool (nullptr, "equalizer_autoload"))
    {
        bool have_valid_filename = false;
        bool found_songpreset = false;
        const char * slash;
        const char * base;
        const char * dross = aud_get_bool (nullptr, "eqpreset_nameonly") ? strstr (filename, "?") : nullptr;
        int ln = -1;
        /* JWT: EXTRACT JUST THE "NAME" PART (URLs MAY END W/"/") TO USE TO NAME THE EQ. FILE: */
        slash = filename ? strrchr (filename, '/') : nullptr;
        if (slash && dross)
        {
            slash = dross;
            while (slash > filename && slash[0] != '/')
            {
                --slash;
            }
            if (slash[0] != '/')
                slash = nullptr;
        }
        base = slash ? slash + 1 : nullptr;
        if (slash && (!base || base[0] == '\0'))  // FILENAME (PBLY. A URL) ENDS IN A "/", SO BACK UP A BIT!
        {
            do
            {
                --slash;
                ++ln;
            } while (slash && slash > filename && slash[0] != '/');
            base = slash ? slash + 1 : nullptr;
            if (ln > 0)
                have_valid_filename = true;
        }
        else if (base && base[0] != '\0' && strncmp (base, "-.", 2))  // NOT AN EMPTY "NAME" OR stdin!
        {
            ln = dross ? dross - base : -1;
            have_valid_filename = true;
        }
        if (have_valid_filename)
        {

            /* JWT:IF WE'RE A "FILE", FIRST CHECK LOCAL DIRECTORY FOR A SONG PRESET FILE: */
            if (! strncmp ((const char *) filename, "file://", 7))
            {
                StringBuf path = filename_get_parent (uri_to_filename (filename));
                found_songpreset = do_load_eq_file (filename_to_uri (str_concat ({path, "/",
                        str_encode_percent (base, ln), ".preset"})), true, true);
            }
            /* JWT:NOT A FILE, OR NOT FOUND, SO NOW CHECK THE GLOBAL CONFIG PATH FOR A SONG PRESET FILE: */
            if (! found_songpreset)
                found_songpreset = do_load_eq_file (filename_to_uri (str_concat ({aud_get_path (AudPath::UserDir), "/",
                        str_encode_percent (base, ln), ".preset"})), true, true);
        }
        if (! found_songpreset)
        {
            /* JWT:NO SONG-PRESET: SEE IF IT IS A "FILE" AND, IF SO, CHECK IT'S DIRECTORY FOR A PRESET:
                OR MAYBE WE'RE PLAYING A DISK, CHECK FOR PRESET BY DISK-ID / DVD TITLE: */
            if (! strncmp (filename, "file://", 7))  // LOOK FOR A DIRECTORY PRESET FILE (LIKE XMMS DOES):
            {
                String eqpreset_dir_default_file = aud_get_str (nullptr, "eqpreset_dir_default_file");
                if (eqpreset_dir_default_file[0])
                {
                    StringBuf path = filename_get_parent (uri_to_filename (filename));
                    found_songpreset = do_load_eq_file (filename_to_uri (str_concat ({path, "/",
                            eqpreset_dir_default_file})), true, false);
                }
            }
            else if (! strncmp (filename, "cdda://", 7) || ! strncmp (filename, "dvd://", 6))
            {
                String playingdiskid = aud_get_str (nullptr, "playingdiskid");
                if (playingdiskid[0])
                    found_songpreset = do_load_eq_file (filename_to_uri (str_concat ({aud_get_path (AudPath::UserDir), "/",
                            playingdiskid, ".preset"})), true, false);
            }
            else if (strncmp (filename, "stdin://", 8))  // WE'RE NOT A FILE, DISK, OR STDIN (ASSUME URL):
            {
                /* LOOK FOR A PRESET FILE MATCHING THE BASE URL, IE. "www.youtube.com": */
                slash = strstr (filename, "//");
                if (slash)
                {
                    slash+=2;
                    const char * endbase = strstr (slash, "/");
                    ln = endbase ? endbase - slash : -1;
                    String urlbase = String (str_copy (slash, ln));
                    auto split = str_list_to_index (slash, "?&#:/");
                    for (auto & str : split)
                    {
                        urlbase = String (str_copy (str));
                        break;
                    }
                    found_songpreset = do_load_eq_file (filename_to_uri (str_concat ({aud_get_path (AudPath::UserDir), "/",
                            urlbase, ".preset"})), true, false);
                }
            }
        }
        s_songautoeq = found_songpreset;
    }
    else
        s_songautoeq = false;

    /* JWT: (P->N) IF PREV. SONG LOADED EQ-AUTO PRESETS, BUT THE CURRENT SONG DID NOT
       THEN RESTORE THE "DEFAULT" (LAST KNOWN NON EQ-AUTO) PRESETS
       THIS IS DONE REGUARDLESS OF [Autoload] STATE SINCE [Autoload] MAY'VE BEEN TURNED
       OFF DURING PREV. SONG AND CURRENT SONG WILL NOT'VE BEEN CHECKED FOR A PRESET FILE. */
    if (prev_songautoeq && ! s_songautoeq)
    {
        do_load_eq_file (filename_to_uri (str_concat ({aud_get_path (AudPath::UserDir), "/_nonauto.preset"})),
                false, false);
        aud_set_bool (nullptr, "_autoeffects_loaded", false);
    }
    setup_effects ();
    setup_output (true);
    if (aud_get_bool (0, "record"))
        setup_secondary (true);

    UNLOCK_ALL;

    aud_set_bool(nullptr, "equalizer_songauto", s_songautoeq);

    return true;
}

void output_set_tuple (const Tuple & tuple)
{
    LOCK_MINOR;

    if (s_input)
        in_tuple = tuple.ref ();

    UNLOCK_MINOR;
}

void output_set_replay_gain (const ReplayGainInfo & info)
{
    LOCK_ALL;

    if (s_input)
    {
        gain_info = info;
        s_gain = true;

        AUDINFO ("Replay Gain info:\n");
        AUDINFO (" album gain: %f dB\n", info.album_gain);
        AUDINFO (" album peak: %f\n", info.album_peak);
        AUDINFO (" track gain: %f dB\n", info.track_gain);
        AUDINFO (" track peak: %f\n", info.track_peak);
    }

    UNLOCK_ALL;
}

/* returns false if stop_time is reached */
bool output_write_audio (const void * data, int size, int stop_time)
{
RETRY:
    LOCK_ALL;
    bool good = false;

    if (s_input && ! s_flushed)
    {
        if (! s_output || (s_paused && ! pausemuted) || s_resetting)
        {
            UNLOCK_MAJOR;
            WAIT_MINOR;
            UNLOCK_MINOR;
            goto RETRY;
        }

        good = process_audio (data, size, stop_time);
    }

    UNLOCK_ALL;
    return good;
}

void output_flush (int time, bool force)
{
    LOCK_MINOR;

    if (s_input && ! s_flushed)
    {
        if (s_output && ! s_resetting)
        {
            // allow effect plugins to prevent the flush, but
            // always flush if paused to prevent locking up
            if (effect_flush (s_paused || force))
            {
                flush_output ();
                s_flushed = true;
                if ( s_paused && ! pausemuted)
                    SIGNAL_MINOR;
            }
        }
        else
        {
            s_flushed = true;
            SIGNAL_MINOR;
        }
    }

    if (s_input)
    {
        seek_time = time;
        in_frames = 0;
    }

    UNLOCK_MINOR;
}

void output_resume ()
{
    LOCK_ALL;

    if (s_input)
        s_flushed = false;

    UNLOCK_ALL;
}

void output_pause (bool pause)
{
    LOCK_MINOR;

    if (s_input && s_paused != pause)
    {
        s_paused = pause;

        if (s_output)
        {
            apply_pause ();
            if (! (s_paused && ! pausemuted) && ! s_flushed && ! s_resetting)
                SIGNAL_MINOR;
        }
    }

    UNLOCK_MINOR;
}

int output_get_time ()
{
    LOCK_MINOR;
    int time = 0, delay = 0;

    if (s_input)
    {
        if (s_output)
        {
            delay = cop->get_delay ();
            delay += aud::rescale<int64_t> (out_bytes_held, out_bytes_per_sec, 1000);
        }

        delay = effect_adjust_delay (delay);
        time = aud::rescale<int64_t> (in_frames, in_rate, 1000);
        time = seek_time + aud::max (time - delay, 0);
    }

    UNLOCK_MINOR;
    return time;
}

int output_get_raw_time ()
{
    LOCK_MINOR;
    int time = 0;

    if (s_output)
    {
        time = aud::rescale<int64_t> (out_bytes_written, out_bytes_per_sec, 1000);
        time = aud::max (time - cop->get_delay (), 0);
    }

    UNLOCK_MINOR;
    return time;
}

void output_close_audio ()
{
    LOCK_ALL;

    if (s_input)
    {
        s_input = false;
        in_filename = String ();
        in_tuple = Tuple ();

        if (s_output && ! (s_paused || s_flushed || s_resetting))
            finish_effects (false); /* first time for end of song */
    }

    UNLOCK_ALL;
}

void output_drain ()
{
    LOCK_ALL;

    if (! s_input)
    {
        if (s_output && ! (s_paused || s_flushed || s_resetting))
            finish_effects (true); /* second time for end of playlist */

        cleanup_output ();
        cleanup_secondary ();
    }

    UNLOCK_ALL;
}

static void output_reset (OutputReset type, OutputPlugin * op)
{
    LOCK_MINOR;

    s_resetting = true;

    if (s_output && ! s_flushed)
        flush_output ();

    UNLOCK_MINOR;
    LOCK_ALL;

    if (type != OutputReset::EffectsOnly)
        cleanup_output ();

    /* this does not reset the secondary plugin */
    if (type == OutputReset::ResetPlugin)
    {
        if (cop)
            cop->cleanup ();

        if (op)
        {
            /* secondary plugin may become primary */
            if (op == sop)
            {
                cleanup_secondary ();
                sop = nullptr;
            }
            else if (! op->init ())
                op = nullptr;
        }

        cop = op;
    }

    if (s_input)
    {
        if (type == OutputReset::EffectsOnly)
            setup_effects ();

        setup_output (false);
        if (aud_get_bool (0, "record"))
            setup_secondary (false);
    }

    s_resetting = false;

    if (s_output && ! s_paused && ! s_flushed)
        SIGNAL_MINOR;

    UNLOCK_ALL;
}

EXPORT void aud_output_reset (OutputReset type)
{
    output_reset (type, cop);
}

EXPORT StereoVolume aud_drct_get_volume ()
{
    StereoVolume volume = {0, 0};
    LOCK_MINOR;

    if (aud_get_bool (0, "software_volume_control"))
        volume = {aud_get_int (0, "sw_volume_left"), aud_get_int (0, "sw_volume_right")};
    else if (cop)
        volume = cop->get_volume ();

    UNLOCK_MINOR;
    return volume;
}

EXPORT void aud_drct_set_volume (StereoVolume volume)
{
    LOCK_MINOR;

    volume.left = aud::clamp (volume.left, 0, 100);
    volume.right = aud::clamp (volume.right, 0, 100);

    if (aud_get_bool (0, "software_volume_control"))
    {
        aud_set_int (0, "sw_volume_left", volume.left);
        aud_set_int (0, "sw_volume_right", volume.right);
    }
    else if (cop)
        cop->set_volume (volume);

    UNLOCK_MINOR;
}

PluginHandle * output_plugin_get_current ()
{
    return cop ? aud_plugin_by_header (cop) : nullptr;
}

PluginHandle * output_plugin_get_secondary ()
{
    return sop ? aud_plugin_by_header (sop) : nullptr;
}

bool output_plugin_set_current (PluginHandle * plugin)
{
    output_reset (OutputReset::ResetPlugin, plugin ?
     (OutputPlugin *) aud_plugin_get_header (plugin) : nullptr);
    return (! plugin || cop);
}

bool output_plugin_set_secondary (PluginHandle * plugin)
{
    LOCK_MINOR;

    cleanup_secondary ();
    if (sop)
        sop->cleanup ();

    sop = plugin ? (OutputPlugin *) aud_plugin_get_header (plugin) : nullptr;
    if (sop && ! sop->init ())
        sop = nullptr;

    if (s_input && aud_get_bool (0, "record"))
        setup_secondary (false);

    UNLOCK_MINOR;
    return (! plugin || sop);
}

static void record_settings_changed (void *, void *)
{
    LOCK_MINOR;

    if (s_input && aud_get_bool (0, "record"))
        setup_secondary (false);
    else
        cleanup_secondary ();


    UNLOCK_MINOR;
}

void output_init ()
{
    hook_associate ("set record", record_settings_changed, nullptr);
    hook_associate ("set record_stream", record_settings_changed, nullptr);
}

void output_cleanup ()
{
    hook_dissociate ("set record", record_settings_changed);
    hook_dissociate ("set record_stream", record_settings_changed);
}
