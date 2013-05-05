/* gtkcombo_button - combo_button widget for gtk+
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


#ifndef __GTK_COMBO_BUTTON_H__
#define __GTK_COMBO_BUTTON_H__

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#define GTK_COMBO_BUTTON(obj)			GTK_CHECK_CAST (obj, gtk_combo_button_get_type (), GtkComboButton)
#define GTK_COMBO_BUTTON_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, gtk_combo_button_get_type (), GtkComboButtonClass)
#define GTK_IS_COMBO_BUTTON(obj)       GTK_CHECK_TYPE (obj, gtk_combo_button_get_type ())

typedef struct _GtkComboButton		GtkComboButton;
typedef struct _GtkComboButtonClass	GtkComboButtonClass;

/* you should access only the entry and list fields directly */
struct _GtkComboButton {
	GtkHBox hbox;

	GtkWidget *button;
        GtkWidget *arrow;
	GtkWidget *popup;
	GtkWidget *popwin;
	GtkWidget *frame;
};

struct _GtkComboButtonClass {
	GtkHBoxClass parent_class;
};

GtkType      gtk_combo_button_get_type              (void);

GtkWidget *gtk_combo_button_new                   (void);

void	   gtk_combo_button_hide_popdown_window   (GtkComboButton *combo_button);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GTK_COMBO_BUTTON_H__ */


