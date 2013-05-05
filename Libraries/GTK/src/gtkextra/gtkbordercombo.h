/* gtkbordercombo - border combo widget for gtk+
 * Copyright 1999-2001 Adrian E. Feiguin <feiguin@ifir.edu.ar>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */


#ifndef __GTK_BORDER_COMBO_H__
#define __GTK_BORDER_COMBO_H__

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "gtkcombobutton.h"

#define GTK_BORDER_COMBO(obj)			GTK_CHECK_CAST (obj, gtk_border_combo_get_type (), GtkBorderCombo)
#define GTK_BORDER_COMBO_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, gtk_border_combo_get_type (), GtkBorderComboClass)
#define GTK_IS_BORDER_COMBO(obj)       GTK_CHECK_TYPE (obj, gtk_border_combo_get_type ())

typedef struct _GtkBorderCombo		GtkBorderCombo;
typedef struct _GtkBorderComboClass	GtkBorderComboClass;

/* you should access only the entry and list fields directly */
struct _GtkBorderCombo {
	GtkComboButton border_combo;

        gint nrows;
        gint ncols;
        gint row;
        gint column;

        GtkWidget ***button;
	GtkWidget *table;

};

struct _GtkBorderComboClass {
	GtkComboButtonClass parent_class;

        void (*changed) (GtkBorderCombo *border_combo, gint selection);
};

GtkType      gtk_border_combo_get_type              (void);

GtkWidget *gtk_border_combo_new                   (void);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GTK_BORDER_COMBO_H__ */


