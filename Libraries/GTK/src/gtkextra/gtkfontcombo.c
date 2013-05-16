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

#include <string.h>
#include <stdlib.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include <pango/pango.h>
#include "gtkfontcombo.h"
#include "gtkextra-marshal.h"

/* Signals */
enum {
      CHANGED,
      LAST_SIGNAL 
};

/* XPM */
static char * bold_xpm[] = {
"16 16 2 1",
" 	c None",
".	c #000000000000",
"                ",
"  .........     ",
"   ...   ...    ",
"   ...    ...   ",
"   ...    ...   ",
"   ...    ...   ",
"   ...   ...    ",
"   ........     ",
"   ...    ...   ",
"   ...     ...  ",
"   ...     ...  ",
"   ...     ...  ",
"   ...     ...  ",
"   ...    ...   ",
"  .........     ",
"                "};


/* XPM */
static char * italic_xpm[] = {
"16 16 2 1",
" 	c None",
".	c #000000000000",
"                ",
"        .....   ",
"         ...    ",
"         ...    ",
"        ...     ",
"        ...     ",
"       ...      ",
"       ...      ",
"      ...       ",
"      ...       ",
"     ...        ",
"     ...        ",
"    ...         ",
"    ...         ",
"   .....        ",
"                "};

#define NUM_SIZES 20

static gchar *default_sizes[] = {"8","9","10","12","13","14","16","18",
                                 "20","22","24","26","28","32","36","40",
                                 "48","56","64","72"};

static void         gtk_font_combo_class_init      (GtkFontComboClass *klass);
static void         gtk_font_combo_init            (GtkFontCombo      *font_combo);
static void         gtk_font_combo_destroy         (GtkObject     *font_combo);
static void         new_font			   (GtkWidget *widget, 
                                                    gpointer data);

static GtkToolbarClass *parent_class = NULL;
static guint font_combo_signals[LAST_SIGNAL] = {0};

static void
gtk_font_combo_class_init (GtkFontComboClass * klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;

  parent_class = gtk_type_class (gtk_toolbar_get_type ());
  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;

  object_class->destroy = gtk_font_combo_destroy;
  
  font_combo_signals[CHANGED] =
    gtk_signal_new ("changed",
                    GTK_RUN_LAST,
                    GTK_CLASS_TYPE(object_class),
                    GTK_SIGNAL_OFFSET (GtkFontComboClass, changed),
                    gtkextra_VOID__VOID,
                    GTK_TYPE_NONE, 0);

}

static void
gtk_font_combo_destroy (GtkObject * font_combo)
{
  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (*GTK_OBJECT_CLASS (parent_class)->destroy) (font_combo);

  gtk_psfont_unref();
}


static void
gtk_font_combo_init (GtkFontCombo * font_combo)
{
  GtkWidget *widget;
  GtkToolbar *toolbar;
  GdkColormap *colormap;
  GdkPixmap *pixmap;
  GtkWidget *tpixmap;
  GdkBitmap *mask;
  GtkRequisition req;
  GList *family = NULL;
  GList *size = NULL;
  gint numf, i;

  gtk_psfont_init();

  widget=GTK_WIDGET(font_combo);

  toolbar = GTK_TOOLBAR(font_combo);
  gtk_container_set_border_width(GTK_CONTAINER(toolbar), 0);

  colormap = gdk_colormap_get_system();

  font_combo->name_combo = gtk_combo_new ();
  gtk_entry_set_editable(GTK_ENTRY(GTK_COMBO(font_combo->name_combo)->entry), FALSE);
  font_combo->size_combo = gtk_combo_new ();
  gtk_entry_set_editable(GTK_ENTRY(GTK_COMBO(font_combo->size_combo)->entry), FALSE);
  font_combo->bold_button = gtk_toggle_button_new ();
  gtk_widget_set_usize(font_combo->bold_button, 24, 24);
  font_combo->italic_button = gtk_toggle_button_new ();
  gtk_widget_set_usize(font_combo->italic_button, 24, 24);

  pixmap = gdk_pixmap_colormap_create_from_xpm_d(NULL, colormap, &mask, NULL,
                                                 bold_xpm);
  tpixmap = gtk_pixmap_new(pixmap, mask);
  gtk_container_add(GTK_CONTAINER(font_combo->bold_button), tpixmap);
  gtk_widget_show(tpixmap);

  pixmap = gdk_pixmap_colormap_create_from_xpm_d(NULL, colormap, &mask, NULL,
                                                 italic_xpm);
  tpixmap = gtk_pixmap_new(pixmap, mask);
  gtk_container_add(GTK_CONTAINER(font_combo->italic_button), tpixmap);
  gtk_widget_show(tpixmap);

  gtk_toolbar_append_widget(toolbar, font_combo->name_combo, NULL, NULL);

  gtk_widget_size_request(font_combo->size_combo, &req);
  req.width = 56;
  gtk_widget_set_usize(font_combo->size_combo, req.width, req.height);
  gtk_toolbar_append_widget(toolbar, font_combo->size_combo, NULL, NULL);

/* FIXME */
//  gtk_toolbar_set_space_size(toolbar, 20);

  gtk_toolbar_append_space(toolbar);
  gtk_toolbar_append_widget(toolbar, font_combo->bold_button, "Bold", "Bold");
  gtk_toolbar_append_widget(toolbar, font_combo->italic_button, "Italic", "Italic");

  gtk_widget_show (font_combo->name_combo);
  gtk_widget_show (font_combo->size_combo);
  gtk_widget_show (font_combo->bold_button);
  gtk_widget_show (font_combo->italic_button);

  gtk_psfont_get_families(&family, &numf);
  gtk_combo_set_popdown_strings(GTK_COMBO(font_combo->name_combo), family);

  for(i = 0; i < NUM_SIZES; i++)
     size = g_list_append(size, default_sizes[i]);
  gtk_combo_set_popdown_strings(GTK_COMBO(font_combo->size_combo), size);

  gtk_signal_connect(GTK_OBJECT(GTK_COMBO(GTK_FONT_COMBO(font_combo)->name_combo)->entry),
                     "changed",
                     GTK_SIGNAL_FUNC(new_font), font_combo);

  gtk_signal_connect(GTK_OBJECT(GTK_COMBO(GTK_FONT_COMBO(font_combo)->size_combo)->entry),
                     "changed",
                     GTK_SIGNAL_FUNC(new_font), font_combo);

  gtk_signal_connect(GTK_OBJECT(GTK_FONT_COMBO(font_combo)->italic_button),
                     "clicked",
                     GTK_SIGNAL_FUNC(new_font), font_combo);

  gtk_signal_connect(GTK_OBJECT(GTK_FONT_COMBO(font_combo)->bold_button),
                     "clicked",
                     GTK_SIGNAL_FUNC(new_font), font_combo);

  gtk_list_select_item(GTK_LIST(GTK_COMBO(font_combo->size_combo)->list), 3);
}

GtkType
gtk_font_combo_get_type ()
{
  static GtkType font_combo_type = 0;

  if (!font_combo_type)
    {
      GtkTypeInfo font_combo_info =
      {
	"GtkFontCombo",
	sizeof (GtkFontCombo),
	sizeof (GtkFontComboClass),
	(GtkClassInitFunc) gtk_font_combo_class_init,
	(GtkObjectInitFunc) gtk_font_combo_init,
	NULL,
	NULL,
	(GtkClassInitFunc) NULL,
      };
      font_combo_type = gtk_type_unique (gtk_toolbar_get_type (), &font_combo_info);
    }
  return font_combo_type;
}

GtkWidget *
gtk_font_combo_new ()
{
  GtkFontCombo *font_combo;

  font_combo = gtk_type_new (gtk_font_combo_get_type ());

  return(GTK_WIDGET(font_combo));
}

static void
new_font(GtkWidget *widget, gpointer data)
{
  GtkFontCombo *font_combo;
  const gchar *text;

  font_combo = GTK_FONT_COMBO(data);

  text = gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(font_combo->name_combo)->entry));

  if(!text || strlen(text) == 0) return;

  text = gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(font_combo->size_combo)->entry));

  if(!text || strlen(text) == 0) return;

  gtk_signal_emit(GTK_OBJECT(font_combo), font_combo_signals[CHANGED]);
}

void
gtk_font_combo_select (GtkFontCombo *combo, 
		       const gchar *family,
                       gboolean bold,
		       gboolean italic,
		       gint height)
{
  GtkItem *item;
  GList *children;
  gchar *text;
  gint n = 0;

  children = GTK_LIST(GTK_COMBO(combo->name_combo)->list)->children;

  while(children){
    item = GTK_ITEM(children->data);
    text = GTK_LABEL(GTK_BIN(item)->child)->label;
    if(strcmp(text, family) == 0) break;
    n++;
    children = children->next;
  }

  gtk_font_combo_select_nth(combo, n, bold, italic, height);
}

void
gtk_font_combo_select_nth (GtkFontCombo *combo, 
		           gint n,
                           gboolean bold,
		           gboolean italic,
		           gint height)
{
  gint i;

  gtk_list_select_item(GTK_LIST(GTK_COMBO(combo->name_combo)->list), n);

  for(i = 0; i < NUM_SIZES; i++){
     if(atoi(default_sizes[i]) >= height) break;
  }

  if(i < NUM_SIZES)
    gtk_list_select_item(GTK_LIST(GTK_COMBO(combo->size_combo)->list), i);

  if(GTK_IS_TOGGLE_BUTTON(combo->bold_button))
    gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(combo->bold_button), bold);
  if(GTK_IS_TOGGLE_BUTTON(combo->italic_button))
    gtk_toggle_button_set_state(GTK_TOGGLE_BUTTON(combo->italic_button), italic);
}

gint 
gtk_font_combo_get_font_height (GtkFontCombo *combo)
{
  const gchar *size;

  size = gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(combo->size_combo)->entry));

  return atoi(size);
}

GtkPSFont * 
gtk_font_combo_get_psfont (GtkFontCombo *combo)
{
  const gchar *text;
  gboolean italic = FALSE, bold = FALSE;

  text=gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(combo->name_combo)->entry));

  if(GTK_IS_TOGGLE_BUTTON(GTK_FONT_COMBO(combo)->italic_button))
    italic = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(GTK_FONT_COMBO(combo)->italic_button));
  if(GTK_IS_TOGGLE_BUTTON(GTK_FONT_COMBO(combo)->bold_button))
    bold = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(GTK_FONT_COMBO(combo)->bold_button));

  return (gtk_psfont_get_by_family(text, italic, bold));
}

PangoFontDescription * 
gtk_font_combo_get_font_description (GtkFontCombo *combo)
{
  const gchar *text;
  GtkPSFont *psfont;
  gboolean italic, bold;
  gint height;

  text = gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(combo->name_combo)->entry));

  italic = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(GTK_FONT_COMBO(combo)->italic_button));
  bold = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(GTK_FONT_COMBO(combo)->bold_button));
  height = gtk_font_combo_get_font_height(combo);

  psfont = gtk_psfont_get_by_family(text, italic, bold);
  return (gtk_psfont_get_font_description(psfont, height));
}

GdkFont * 
gtk_font_combo_get_gdkfont (GtkFontCombo *combo)
{
  const gchar *text;
  GtkPSFont *psfont;
  gboolean italic, bold;
  gint height;

  text=gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(combo->name_combo)->entry));

  italic = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(GTK_FONT_COMBO(combo)->italic_button));
  bold = gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(GTK_FONT_COMBO(combo)->bold_button));
  height = gtk_font_combo_get_font_height(combo);

  psfont = gtk_psfont_get_by_family(text, italic, bold);
  return (gtk_psfont_get_gdkfont(psfont, height));
}
