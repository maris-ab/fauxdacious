/*
 * colorbutton.h
 * Copyright 2019 William Pitcock
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

#include "colorbutton.h"
#include "libfauxdqt.h"

#include <QColorDialog>
#include <QPainter>

namespace audqt {

ColorButton::ColorButton (QWidget * parent) :
    QPushButton (parent)
{
    connect (this, & QPushButton::clicked, [this] ()
    {
        auto dialog = findChild<QColorDialog *> ();
        if (! dialog)
        {
            dialog = new QColorDialog (m_color, this);
            connect (dialog, & QColorDialog::colorSelected, this, & ColorButton::setColor);
        }
        else
            dialog->setCurrentColor (m_color);  //JWT:RESET TO "CURRENT" COLOR!

        window_bring_to_front (dialog);
    });
}

void ColorButton::setColor (const QColor & color)
{
    if (color != m_color)
    {
        m_color = color;
        update ();
        onColorChanged ();
    }
}

void ColorButton::paintEvent (QPaintEvent * event)
{
    QPushButton::paintEvent (event);
    QPainter (this).fillRect (rect () - margins.FourPt, m_color);
}

}
