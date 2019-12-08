/*
 * info-widget.h
 * Copyright 2006-2018 René Bertin, Thomas Lange, John Lindgren,
 *                     William Pitcock, Tomasz Moń, and Eugene Zagidullin
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

#include "info-widget.h"
#include "libfauxdqt.h"
#include "libfauxdqt-internal.h"

#include <QHeaderView>

#include <libfauxdcore/i18n.h>
#include <libfauxdcore/probe.h>
#include <libfauxdcore/runtime.h>
#include <libfauxdcore/tuple.h>
#include <libfauxdcore/drct.h>
#include <libfauxdcore/vfs.h>

namespace audqt {

struct TupleFieldMap {
    const char * name;
    Tuple::Field field;
    bool editable;
};

static const TupleFieldMap tuple_field_map[] = {
    {N_("Metadata"), Tuple::Invalid, false},
    {N_("Artist"), Tuple::Artist, true},
    {N_("Album"), Tuple::Album, true},
    {N_("Title"), Tuple::Title, true},
    {N_("Track Number"), Tuple::Track, true},
    {N_("Genre"), Tuple::Genre, true},
    {N_("Comment"), Tuple::Comment, true},
    {N_("Album Artist"), Tuple::AlbumArtist, true},
    {N_("Composer"), Tuple::Composer, true},
    {N_("Performer"), Tuple::Performer, true},
    {N_("Recording Year"), Tuple::Year, true},
    {N_("Recording Date"), Tuple::Date, true},

    {nullptr, Tuple::Invalid, false},
    {N_("Technical"), Tuple::Invalid, false},
    {N_("Length"), Tuple::Length, false},
    {N_("Codec"), Tuple::Codec, false},
    {N_("Quality"), Tuple::Quality, false},
    {N_("Bitrate"), Tuple::Bitrate, false},
};

class InfoModel : public QAbstractTableModel
{
public:
    InfoModel (QObject * parent = nullptr) :
        QAbstractTableModel (parent) {}

    int rowCount (const QModelIndex & parent = QModelIndex ()) const
        { return aud::n_elems (tuple_field_map); }
    int columnCount (const QModelIndex & parent = QModelIndex ()) const
        { return 2; }

    QVariant data (const QModelIndex & index, int role = Qt::DisplayRole) const;
    bool setData (const QModelIndex & index, const QVariant & value, int role = Qt::EditRole);
    Qt::ItemFlags flags (const QModelIndex & index) const;

    void setTupleData (const Tuple & tuple, String filename, PluginHandle * plugin)
    {
        m_tuple = tuple.ref ();
        m_filename = filename;
        m_plugin = plugin;
        m_dirty = false;
    }

    bool updateFile () const;

private:
    Tuple m_tuple;
    String m_filename;
    PluginHandle * m_plugin = nullptr;
    bool m_dirty = false;
};

EXPORT InfoWidget::InfoWidget (QWidget * parent) :
    QTreeView (parent),
    m_model (new InfoModel (this))
{
    setModel (m_model);
    header ()->hide ();
    setIndentation (0);
    resizeColumnToContents (0);
    setContextMenuPolicy (Qt::CustomContextMenu);

    connect (this, & QWidget::customContextMenuRequested, [this] (const QPoint & pos)
    {
        auto index = indexAt (pos);
        if (index.column () != 1)
            return;
        auto text = m_model->data (index, Qt::DisplayRole).toString ();
        if (! text.isEmpty ())
            show_copy_context_menu (this, mapToGlobal (pos), text);
    });
}

EXPORT InfoWidget::~InfoWidget ()
{
}

EXPORT void InfoWidget::fillInfo (int playlist, int entry, const char * filename, const Tuple & tuple,
 PluginHandle * decoder, bool updating_enabled)
{
    m_model->setTupleData (tuple, String (filename), decoder);
    reset ();
    setEditTriggers (updating_enabled ? QAbstractItemView::SelectedClicked : QAbstractItemView::NoEditTriggers);
}

EXPORT bool InfoWidget::updateFile ()
{
    return m_model->updateFile ();
}

bool InfoModel::updateFile () const
{
    bool success = false;

    if (! m_dirty)
        return true;

    //x return aud_file_write_tuple (m_filename, m_plugin, m_tuple);
    if (aud_drct_get_record_enabled ())
    {
        String recording_file = aud_get_str ("filewriter", "_record_fid");
        if (recording_file && recording_file[0])
        {
            AUDDBG ("-infowin_update_tuple: RECORDING ON - SAVE TO (%s)!\n", (const char *) recording_file);
            String error;
            VFSFile file (recording_file, "r");
            PluginHandle * out_plugin = aud_file_find_decoder (recording_file, true, file, & error);

            success = aud_file_write_tuple (recording_file, out_plugin, m_tuple);
        }
    }
    if (! success)
        success = aud_file_write_tuple (m_filename, m_plugin, m_tuple);

    return success;
}

bool InfoModel::setData (const QModelIndex & index, const QVariant & value, int role)
{
    if (role != Qt::EditRole)
        return false;

    Tuple::Field field_id = tuple_field_map [index.row ()].field;
    if (field_id == Tuple::Invalid)
        return false;

    m_dirty = true;

    auto t = Tuple::field_get_type (field_id);
    auto str = value.toString ();

    if (str.isEmpty ())
        m_tuple.unset (field_id);
    else if (t == Tuple::String)
        m_tuple.set_str (field_id, str.toUtf8 ());
    else /* t == Tuple::Int */
        m_tuple.set_int (field_id, str.toInt ());

    emit dataChanged (index, index, {role});
    return true;
}

QVariant InfoModel::data (const QModelIndex & index, int role) const
{
    Tuple::Field field_id = tuple_field_map [index.row ()].field;

    if (role == Qt::DisplayRole || role == Qt::EditRole)
    {
        if (index.column () == 0)
            return translate_str (tuple_field_map [index.row ()].name);
        else if (index.column () == 1)
        {
            if (field_id == Tuple::Invalid)
                return QVariant ();

            switch (m_tuple.get_value_type (field_id))
            {
            case Tuple::String:
                return QString (m_tuple.get_str (field_id));
            case Tuple::Int:
                /* convert to string so Qt allows clearing the field */
                return QString::number (m_tuple.get_int (field_id));
            default:
                return QVariant ();
            }
        }
    }
    else if (role == Qt::FontRole)
    {
        if (field_id == Tuple::Invalid)
        {
            QFont f;
            f.setBold (true);
            return f;
        }
        return QVariant ();
    }

    return QVariant ();
}

Qt::ItemFlags InfoModel::flags (const QModelIndex & index) const
{
    if (index.column () == 1)
    {
        auto & t = tuple_field_map [index.row ()];

        if (t.field == Tuple::Invalid)
            return Qt::ItemNeverHasChildren;
        else if (t.editable)
            return Qt::ItemIsSelectable | Qt::ItemIsEditable | Qt::ItemIsEnabled;

        return Qt::ItemIsEnabled;
    }

    return Qt::ItemNeverHasChildren;
}

} // namespace audqt
