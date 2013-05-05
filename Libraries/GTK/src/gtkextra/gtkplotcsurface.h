/* gtkplotcsurface - csurface plots widget for gtk+
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

#ifndef __GTK_PLOT_CSURFACE_H__
#define __GTK_PLOT_CSURFACE_H__

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "gtkplot.h"
#include "gtkplotdt.h"
#include "gtkplotsurface.h"
#include "gtkplotpc.h"


#define GTK_PLOT_CSURFACE(obj)        GTK_CHECK_CAST (obj, gtk_plot_csurface_get_type (), GtkPlotCSurface)
#define GTK_TYPE_PLOT_CSURFACE        (gtk_plot_csurface_get_type ())
#define GTK_PLOT_CSURFACE_CLASS(klass) GTK_CHECK_CLASS_CAST (klass, gtk_plot_csurface_get_type, GtkPlotCSurfaceClass)
#define GTK_IS_PLOT_CSURFACE(obj)     GTK_CHECK_TYPE (obj, gtk_plot_csurface_get_type ())

typedef struct _GtkPlotCSurface		GtkPlotCSurface;
typedef struct _GtkPlotCSurfaceClass	GtkPlotCSurfaceClass;

typedef enum
{
  GTK_PLOT_PROJECT_NONE,  /* draw surface with no projections */
  GTK_PLOT_PROJECT_EMPTY, /* draw empty contours -- lines only.
                             Notice that the style of the lines is given by
                             levels/sublevels_line. You should set to 
                             GTK_PLOT_LINE_NONE if you don't want the lines
                             drawn */
  GTK_PLOT_PROJECT_FULL,  /* draw filled contours */
} GtkPlotProjection;      

struct _GtkPlotCSurface
{
  GtkPlotSurface surface;

  gboolean lines_visible;  /* show contour level lines -- for 3d only */
                           /* for 2d it is given by projection and line style */
  GtkPlotProjection projection;  /* project on x-y plane */

  /* all fields below this line are for 2d contours */
  
  GList *levels;           /* polygons corrsponding to each contour level */
  GList *bg_triangles;     /* background triangles, drawn before the contour
                              levels, that are not cut by any contour line */

  GtkPlotLine levels_line;
  GtkPlotLine sublevels_line;
};

struct _GtkPlotCSurfaceClass
{
  GtkPlotSurfaceClass parent_class;
};

/* PlotCSurface */

GtkType		gtk_plot_csurface_get_type	(void);
GtkWidget*	gtk_plot_csurface_new		(void);
GtkWidget*	gtk_plot_csurface_new_function	(GtkPlotFunc3D function);

void		gtk_plot_csurface_construct_function (GtkPlotCSurface *csurface,
					 	      GtkPlotFunc3D function);

void		gtk_plot_csurface_set_lines_visible (GtkPlotCSurface *csurface,
						     gboolean visible);
gboolean	gtk_plot_csurface_get_lines_visible (GtkPlotCSurface *csurface);
void		gtk_plot_csurface_set_projection    (GtkPlotCSurface *csurface,
						     GtkPlotProjection proj);
GtkPlotProjection	gtk_plot_csurface_projection 	(GtkPlotCSurface *csurface);
void            gtk_plot_csurface_set_levels_attributes (GtkPlotCSurface *data,
                                                         GtkPlotLineStyle style,
                                                         gfloat width,
                                                         const GdkColor *color);
void            gtk_plot_csurface_set_sublevels_attributes (GtkPlotCSurface *data,
                                                         GtkPlotLineStyle style,
                                                         gfloat width,
                                                         const GdkColor *color);
void            gtk_plot_csurface_get_levels_attributes (GtkPlotCSurface *data,
                                                         GtkPlotLineStyle *style,
                                                         gfloat *width,
                                                         GdkColor *color);
void            gtk_plot_csurface_get_sublevels_attributes (GtkPlotCSurface *data,
                                                         GtkPlotLineStyle *style,
                                                         gfloat *width,
                                                         GdkColor *color);

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif /* __GTK_PLOT_CSURFACE_H__ */
