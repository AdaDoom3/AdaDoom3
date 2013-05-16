/* gtkfontcombo - font_combo widget for gtk+
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


#ifndef __GTK_FONT_COMBO_H__
#define __GTK_FONT_COMBO_H__


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


#include "gtkpsfont.h"


#define GTK_FONT_COMBO(obj)			GTK_CHECK_CAST (obj, gtk_font_combo_get_type (), GtkFontCombo)
#define GTK_FONT_COMBO_CLASS(klass)	GTK_CHECK_CLASS_CAST (klass, gtk_font_combo_get_type (), GtkFontComboClass)
#define GTK_IS_FONT_COMBO(obj)       GTK_CHECK_TYPE (obj, gtk_font_combo_get_type ())

typedef struct _GtkFontCombo		GtkFontCombo;
typedef struct _GtkFontComboClass	GtkFontComboClass;

/* you should access only the entry and list fields directly */
struct _GtkFontCombo {
	GtkToolbar toolbar;

	GtkWidget *name_combo;
	GtkWidget *size_combo;
        GtkWidget *bold_button;
	GtkWidget *italic_button;
};

struct _GtkFontComboClass {
	GtkToolbarClass parent_class;

        void (* changed)      (GtkFontCombo *font_combo);
};

GtkType      gtk_font_combo_get_type              (void);

GtkWidget *gtk_font_combo_new                   (void);

void	   gtk_font_combo_select		(GtkFontCombo *font_combo,
						 const gchar *family,
			                         gboolean bold,
                        			 gboolean italic,
						 gint height);
void	   gtk_font_combo_select_nth		(GtkFontCombo *font_combo,
						 gint n,
			                         gboolean bold,
                        			 gboolean italic,
						 gint height);
gint	   gtk_font_combo_get_font_height	(GtkFontCombo *font_combo);
GtkPSFont  *gtk_font_combo_get_psfont		(GtkFontCombo *font_combo);
PangoFontDescription  *gtk_font_combo_get_font_description (GtkFontCombo *font_combo);
GdkFont    *gtk_font_combo_get_gdkfont		(GtkFontCombo *font_combo);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* __GTK_FONT_COMBO_H__ */


