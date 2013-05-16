/* gtkplotdatapixmap - pixmap dataset 
 * Copyright 1999-2001  Adrian E. Feiguin <feiguin@ifir.edu.ar>
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

#ifndef __GTK_PLOT_PIXMAP_H__
#define __GTK_PLOT_PIXMAP_H__

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "gtkplot.h"

#define GTK_PLOT_PIXMAP(obj)        GTK_CHECK_CAST (obj, gtk_plot_pixmap_get_type (), GtkPlotPixmap)
#define GTK_TYPE_PLOT_PIXMAP        (gtk_plot_pixmap_get_type ())
#define GTK_PLOT_PIXMAP_CLASS(klass) GTK_CHECK_CLASS_CAST (klass, gtk_plot_pixmap_get_type(), GtkPlotPixmapClass)
#define GTK_IS_PLOT_PIXMAP(obj)     GTK_CHECK_TYPE (obj, gtk_plot_pixmap_get_type ())

typedef struct _GtkPlotPixmap             GtkPlotPixmap;
typedef struct _GtkPlotPixmapClass        GtkPlotPixmapClass;

struct _GtkPlotPixmap
{
  GtkPlotData data;
 
  GdkPixmap *pixmap;
  GdkBitmap *mask;
};

struct _GtkPlotPixmapClass
{
  GtkPlotDataClass parent_class;
};


GtkType		gtk_plot_pixmap_get_type	(void);
GtkWidget*	gtk_plot_pixmap_new		(GdkPixmap *pixmap,
						 GdkBitmap *mask);

void		gtk_plot_pixmap_construct	(GtkPlotPixmap *data, 
					         GdkPixmap *pixmap,
					         GdkBitmap *mask);

GdkPixmap*	gtk_plot_pixmap_get_pixmap  	(GtkPlotPixmap *data);
GdkBitmap*	gtk_plot_pixmap_get_mask    	(GtkPlotPixmap *data);

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_PLOT_PIXMAP_H__ */
