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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <gtk/gtk.h>
#include "gtkplot.h"
#include "gtkplot3d.h"
#include "gtkplotdata.h"
#include "gtkplotsurface.h"
#include "gtkplotcsurface.h"
#include "gtkpsfont.h"

#define P_(string) string

typedef struct
{
  GList *polygons;  	     /* list with polygons enclosed by a contour */
  gdouble level;	     /* value of the contour level */
  gboolean sublevel;	     /* is this a level of sublevel? */
} GtkPlotContourLevel;

typedef struct
{
  GtkPlotPoint *contour;  /* array with the contour points */
  gint n1;		  /* number of points of the outer border */
  gdouble level;	  /* value of the contour level */
  gboolean sublevel;	  /* is this a level of sublevel? */
} GtkPlotContourLine;

typedef struct
{
  GtkAllocation *bb;     /* bounding box of the label intersected by the line */
  GtkPlotPoint x1, x2;   /* intersection points */
  gdouble t1, t2;        /* parameters in the line eq r=r0+t*(r-r0) */
  gint type;             /* one or two intersection points */ 
} GtkPlotContourX;

static void gtk_plot_csurface_class_init 	(GtkPlotCSurfaceClass *klass);
static void gtk_plot_csurface_init 		(GtkPlotCSurface *data);
static void gtk_plot_csurface_destroy 		(GtkObject *object);
static void gtk_plot_csurface_get_property         (GObject      *object,
                                                 guint            prop_id,
                                                 GValue          *value,
                                                 GParamSpec      *pspec);
static void gtk_plot_csurface_set_property         (GObject      *object,
                                                 guint            prop_id,
                                                 const GValue          *value,
                                                 GParamSpec      *pspec);

static void gtk_plot_csurface_clone 		(GtkPlotData *data,
						 GtkPlotData *copy);
static void update_data                         (GtkPlotData *data);
static void gtk_plot_csurface_build_polygons 	(GtkPlotSurface *surface);
static void gtk_plot_csurface_build_contours 	(GtkPlotSurface *surface);
static void gtk_plot_csurface_get_legend_size	(GtkPlotData *data, 
						 gint *width, gint *height);
static void gtk_plot_csurface_draw_legend	(GtkPlotData *data, 
						 gint x, gint y);
static void gtk_plot_csurface_draw_private 	(GtkPlotData *data);
static void gtk_plot_csurface_draw_lines	(GtkPlotData *data);
static void gtk_plot_csurface_draw_polygons 	(GtkPlotSurface *surface);
static void gtk_plot_csurface_real_draw_polygons (GtkPlotSurface *surface,
						 GtkPlotProjection p);
static void gtk_plot_csurface_lighting 		(GdkColor *a, 
						 GdkColor *b, 
						 gdouble normal,
						 gdouble ambient);
static void clear_polygons			(GtkPlotCSurface *surface);
extern inline gint roundint			(gdouble x);
static void hsv_to_rgb 				(gdouble  h, 
						 gdouble  s, 
						 gdouble  v,
            					 gdouble *r, 
						 gdouble *g, 
						 gdouble *b);
static void rgb_to_hsv 				(gdouble  r, 
						 gdouble  g, 
						 gdouble  b,
            					 gdouble *h, 
						 gdouble *s, 
						 gdouble *v);

enum {
  ARG_0,
  ARG_LINES_VISIBLE,
  ARG_PROJECTION,
  ARG_LEVELS_STYLE,
  ARG_LEVELS_WIDTH,
  ARG_LEVELS_COLOR,
  ARG_SUBLEVELS_STYLE,
  ARG_SUBLEVELS_WIDTH,
  ARG_SUBLEVELS_COLOR,
};

static GtkPlotSurfaceClass *parent_class = NULL;

GtkType
gtk_plot_csurface_get_type (void)
{
  static GtkType data_type = 0;

  if (!data_type)
    {
      GtkTypeInfo data_info =
      {
	"GtkPlotCSurface",
	sizeof (GtkPlotCSurface),
	sizeof (GtkPlotCSurfaceClass),
	(GtkClassInitFunc) gtk_plot_csurface_class_init,
	(GtkObjectInitFunc) gtk_plot_csurface_init,
	/* reserved 1*/ NULL,
        /* reserved 2 */ NULL,
        (GtkClassInitFunc) NULL,
      };

      data_type = gtk_type_unique (gtk_plot_surface_get_type(), &data_info);
    }
  return data_type;
}

static void
gtk_plot_csurface_class_init (GtkPlotCSurfaceClass *klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;
  GtkPlotDataClass *data_class;
  GtkPlotSurfaceClass *surface_class;
  GObjectClass *gobject_class = G_OBJECT_CLASS(klass);

  parent_class = gtk_type_class (gtk_plot_surface_get_type ());

  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;
  data_class = (GtkPlotDataClass *) klass;
  surface_class = (GtkPlotSurfaceClass *) klass;

  object_class->destroy = gtk_plot_csurface_destroy;

  gobject_class->set_property = gtk_plot_csurface_set_property;
  gobject_class->get_property = gtk_plot_csurface_get_property;

  g_object_class_install_property (gobject_class,
                           ARG_LINES_VISIBLE,
  g_param_spec_int ("lines_visible",
                           P_(""),
                           P_(""),
                           0,G_MAXINT,0,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_PROJECTION,
  g_param_spec_int ("projection",
                           P_(""),
                           P_(""),
                           0,G_MAXINT,0,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_LEVELS_STYLE,
  g_param_spec_int ("levels_style",
                           P_(""),
                           P_(""),
                           0,G_MAXINT,0,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_LEVELS_WIDTH,
  g_param_spec_double ("levels_width",
                           P_(""),
                           P_(""),
                           0,G_MAXDOUBLE,0.0,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_LEVELS_COLOR,
  g_param_spec_pointer ("levels_color",
                           P_(""),
                           P_(""),
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_SUBLEVELS_STYLE,
  g_param_spec_int ("sublevels_style",
                           P_(""),
                           P_(""),
                           0,G_MAXINT,0,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_SUBLEVELS_WIDTH,
  g_param_spec_double ("sublevels_width",
                           P_(""),
                           P_(""),
                           0,G_MAXDOUBLE,0.0,
                           G_PARAM_READABLE|G_PARAM_WRITABLE));
  g_object_class_install_property (gobject_class,
                           ARG_SUBLEVELS_COLOR,
  g_param_spec_pointer ("sublevels_color",
                           P_(""),
                           P_(""),
                           G_PARAM_READABLE|G_PARAM_WRITABLE));

  data_class->clone = gtk_plot_csurface_clone;
  data_class->gradient_changed = update_data;
  data_class->draw_legend = gtk_plot_csurface_draw_legend;
  data_class->get_legend_size = gtk_plot_csurface_get_legend_size;
/*
  data_class->draw_gradient = NULL;
*/
  data_class->draw_data = gtk_plot_csurface_draw_private;

  surface_class->draw_polygons = gtk_plot_csurface_draw_polygons;
  surface_class->build_polygons = gtk_plot_csurface_build_polygons;
}

static void
gtk_plot_csurface_set_property (GObject      *object,
                             guint            prop_id,
                             const GValue          *value,
                             GParamSpec      *pspec)
{
  GtkPlotCSurface *data;

  data = GTK_PLOT_CSURFACE (object);

  switch (prop_id)
    {
      case ARG_LINES_VISIBLE:
        data->lines_visible = g_value_get_boolean(value);
        break;
      case ARG_PROJECTION:
        data->projection = g_value_get_int(value);
        break;
      case ARG_LEVELS_STYLE:
        data->levels_line.line_style = g_value_get_int(value);
        break;
      case ARG_LEVELS_WIDTH:
        data->levels_line.line_width = g_value_get_double(value);
        break;
      case ARG_LEVELS_COLOR:
        data->levels_line.color = *((GdkColor *)g_value_get_pointer(value));
        break;
      case ARG_SUBLEVELS_STYLE:
        data->sublevels_line.line_style = g_value_get_int(value);
        break;
      case ARG_SUBLEVELS_WIDTH:
        data->sublevels_line.line_width = g_value_get_double(value);
        break;
      case ARG_SUBLEVELS_COLOR:
        data->sublevels_line.color = *((GdkColor *)g_value_get_pointer(value));
        break;
    }
}

static void
gtk_plot_csurface_get_property (GObject      *object,
                             guint            prop_id,
                             GValue          *value,
                             GParamSpec      *pspec)
{
  GtkPlotCSurface *data;

  data = GTK_PLOT_CSURFACE (object);

  switch (prop_id)
    {
      case ARG_LINES_VISIBLE:
        g_value_set_boolean(value, data->lines_visible);
        break;
      case ARG_PROJECTION:
        g_value_set_int(value, data->projection);
        break;
      case ARG_LEVELS_STYLE:
        g_value_set_int(value, data->levels_line.line_style);
        break;
      case ARG_LEVELS_WIDTH:
        g_value_set_double(value, data->levels_line.line_width);
        break;
      case ARG_LEVELS_COLOR:
        g_value_set_pointer(value, &data->levels_line.color);
        break;
      case ARG_SUBLEVELS_STYLE:
        g_value_set_int(value, data->sublevels_line.line_style);
        break;
      case ARG_SUBLEVELS_WIDTH:
        g_value_set_double(value, data->sublevels_line.line_width);
        break;
      case ARG_SUBLEVELS_COLOR:
        g_value_set_pointer(value, &data->sublevels_line.color);
        break;
      default:
        G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
        break;
    }
}

static void
update_data (GtkPlotData *data)
{
  GTK_PLOT_SURFACE_CLASS(GTK_OBJECT_GET_CLASS(GTK_OBJECT(data)))->build_polygons(GTK_PLOT_SURFACE(data));
  data->redraw_pending = TRUE;
}

static void
gtk_plot_csurface_init (GtkPlotCSurface *dataset)
{
  GtkWidget *widget;
  GdkColormap *colormap;
  GtkPlotArray *dim;

  GTK_WIDGET_SET_FLAGS(dataset, GTK_NO_WINDOW);

  widget = GTK_WIDGET(dataset);
  colormap = gtk_widget_get_colormap(widget);

  GTK_PLOT_DATA(dataset)->show_gradient = TRUE;
  GTK_PLOT_DATA(dataset)->show_labels = TRUE;
  GTK_PLOT_DATA(dataset)->gradient_custom = TRUE;

  GTK_PLOT_SURFACE(dataset)->show_mesh = TRUE;

  dataset->lines_visible = TRUE;
  dataset->projection = GTK_PLOT_PROJECT_EMPTY;
  dataset->levels = NULL; 
  dataset->bg_triangles = NULL; 

  GTK_PLOT_DATA(dataset)->labels_attr.height = 10;

  dataset->levels_line = GTK_PLOT_SURFACE(dataset)->mesh_line;
  dataset->sublevels_line = GTK_PLOT_SURFACE(dataset)->mesh_line;
  dataset->sublevels_line.line_style = GTK_PLOT_LINE_DASHED;

  dim = gtk_plot_data_find_dimension(GTK_PLOT_DATA(dataset), "y");
  gtk_plot_array_set_independent(dim, TRUE);
  dim = gtk_plot_data_find_dimension(GTK_PLOT_DATA(dataset), "z");
  gtk_plot_array_set_required(dim, TRUE);
}

static void 
gtk_plot_csurface_destroy 		(GtkObject *object)
{
  GtkPlotCSurface *surface;

  surface = GTK_PLOT_CSURFACE(object);

  clear_polygons(surface);

  GTK_OBJECT_CLASS(parent_class)->destroy(object);
}

GtkWidget*
gtk_plot_csurface_new (void)
{
  GtkPlotData *data;

  data = gtk_type_new (gtk_plot_csurface_get_type ());

  return GTK_WIDGET (data);
}

GtkWidget*
gtk_plot_csurface_new_function (GtkPlotFunc3D function)
{
  GtkWidget *data;

  data = gtk_type_new (gtk_plot_csurface_get_type ());

  gtk_plot_csurface_construct_function(GTK_PLOT_CSURFACE(data), function);

  return data;
}

void
gtk_plot_csurface_construct_function (GtkPlotCSurface *data, 
                                      GtkPlotFunc3D function)
{
  GTK_PLOT_DATA(data)->is_function = TRUE;
  GTK_PLOT_DATA(data)->function3d = function;
}

static void
gtk_plot_csurface_clone(GtkPlotData *real_data, GtkPlotData *copy_data)
{
  GTK_PLOT_DATA_CLASS(parent_class)->clone(real_data, copy_data);

  GTK_PLOT_CSURFACE(copy_data)->lines_visible = GTK_PLOT_CSURFACE(real_data)->lines_visible;
  GTK_PLOT_CSURFACE(copy_data)->projection = GTK_PLOT_CSURFACE(real_data)->projection;
  GTK_PLOT_CSURFACE(copy_data)->levels_line = GTK_PLOT_CSURFACE(real_data)->levels_line;
  GTK_PLOT_CSURFACE(copy_data)->sublevels_line = GTK_PLOT_CSURFACE(real_data)->sublevels_line;
}

static void 
gtk_plot_csurface_draw_private 	(GtkPlotData *data)
{
  GtkPlot *plot;
  GtkPlotSurface *surface;
  GtkPlotCSurface *csurface;

  surface = GTK_PLOT_SURFACE(data);
  csurface = GTK_PLOT_CSURFACE(data);
  plot = data->plot;

  GTK_PLOT_DATA(data)->gradient_custom = TRUE;

  GTK_PLOT_DATA_CLASS(parent_class)->draw_data(data); 

  if(!GTK_IS_PLOT3D(data->plot)){
    gtk_plot_set_line_attributes(plot, csurface->levels_line);
    gtk_plot_csurface_draw_lines(data);
  }
}


static void
sort_lines(GtkPlotPoint *contour, gint nin, gint *nout)
{
  gint n, last = 0;

  for(n = 0; n < nin; n++){
    gdouble x1, y1, x2, y2;
    gint i = n*2;
    x1 = contour[i].x; 
    y1 = contour[i].y; 
    x2 = contour[i+1].x; 
    y2 = contour[i+1].y; 
    if(n == 0) {
      last += 2;
    } else if(n == 1) {
      if(x1==contour[1].x && y1==contour[1].y)
        { contour[last].x = x2; contour[last].y = y2; last++; } 
      else if(x2==contour[1].x && y2==contour[1].y)
        { contour[last].x = x1; contour[last].y = y1; last++; } 
      else if(x1==contour[0].x && y1==contour[0].y)
        { 
          contour[last].x = x2; contour[last].y = y2; last++; 
          x2 = contour[0].x; y2 = contour[0].y;
          contour[0].x = contour[1].x; contour[0].y = contour[1].y;
          contour[1].x = x2; contour[1].y = y2;
        }
      else if(x2==contour[0].x && y2==contour[0].y)
        { 
          contour[last].x = x1; contour[last].y = y1; last++; 
          x2 = contour[0].x; y2 = contour[0].y;
          contour[0].x = contour[1].x; contour[0].y = contour[1].y;
          contour[1].x = x2; contour[1].y = y2;
        }
    } else {
      if(x1==contour[last-1].x && y1==contour[last-1].y)
        { contour[last].x = x2; contour[last].y = y2; last++; }
      else if(x2==contour[last-1].x && y2==contour[last-1].y)
        { contour[last].x = x1; contour[last].y = y1; last++; }
    }
  }
  *nout = last;
}

static gboolean 
bb_intersect(GtkAllocation *a, GtkAllocation *b)
{
  gint xmin, xmax, ymin, ymax;
  xmax = MIN(a->x+a->width,b->x+b->width); 
  xmin = MAX(a->x,b->x); 
  if(xmin > xmax) return FALSE;
  ymax = MIN(a->y+a->height,b->y+b->height); 
  ymin = MAX(a->y,b->y); 
  if(ymin > ymax) return FALSE;
  return TRUE;
}

static gint 
line_intersect(GtkAllocation *b, 
               GtkPlotPoint *p1, GtkPlotPoint *p2, 
               GtkPlotPoint *x1, GtkPlotPoint *x2,
               gdouble *t1, gdouble *t2)
{
  gdouble nx = p2->x - p1->x;
  gdouble ny = p2->y - p1->y;
  gdouble t[2], xt;
  gint n = 0;
  gdouble xmin, xmax, ymin, ymax;
  gdouble x = 0, y = 0;
  
  xmin = MIN(p1->x,p2->x);
  xmax = MAX(p1->x,p2->x);
  ymin = MIN(p1->y,p2->y);
  ymax = MAX(p1->y,p2->y);

  if(xmin >= b->x && xmax <= b->x+b->width && ymin >= b->y && ymax <= b->y+b->height) return 3; 

  if(nx != 0.){
    xt = (b->x - p1->x) / nx;
    y = p1->y + xt * ny;
    if(xt <= 1. && xt >= 0. && y >= b->y && y <= b->y+b->height) t[n++] = xt;
    xt = (b->x + b->width - p1->x) / nx;
    y = p1->y + xt * ny;
    if(xt <= 1. && xt >= 0. && y >= b->y && y <= b->y+b->height) t[n++] = xt;
  }
  if(ny != 0.){
    xt = (b->y - p1->y) / ny;
    x = p1->x + xt * nx;
    if(xt <= 1. && xt >= 0. && x >= b->x && x <= b->x+b->width) t[n++] = xt;
    xt = (b->y + b->height - p1->y) / ny;
    x = p1->x + xt * nx;
    if(xt <= 1. && xt >= 0. && x >= b->x && x <= b->x+b->width) t[n++] = xt;
  }
  if(n == 1){
    x1->x = p1->x + t[0]*nx;
    x1->y = p1->y + t[0]*ny;
    *t1 = t[0];
    return 1;
  }
  if(n == 2){
    if(t[0] < t[1]){
      x1->x = p1->x + t[0]*nx;
      x1->y = p1->y + t[0]*ny;
      x2->x = p1->x + t[1]*nx;
      x2->y = p1->y + t[1]*ny;
      *t1 = t[0];
      *t2 = t[1];
      return 2;
    } else {
      x1->x = p1->x + t[1]*nx;
      x1->y = p1->y + t[1]*ny;
      x2->x = p1->x + t[0]*nx;
      x2->y = p1->y + t[0]*ny;
      *t1 = t[1];
      *t2 = t[0];
      return 2;
    }
  }
  return 0;
}

static void
sort_points(GtkPlotContourX *x, gint nxp)
{
  gboolean end = FALSE;

  while(!end){
    gint i;
    end = TRUE;
    for(i = 0; i < nxp-1; i++){
      GtkPlotContourX aux;
      GtkPlotContourX *aux_i = &x[i];
      GtkPlotContourX *aux_j = &x[i + 1];
      if(aux_i->type == 1){
        if(aux_j->type == 1){
          if(aux_i->t1 > aux_j->t1)
            { aux = *aux_j; *aux_j = *aux_i; *aux_i = aux; end = FALSE; } 
        } else {
          if(aux_i->t1 > aux_j->t1 && aux_i->t1 > aux_j->t2)
            { aux = *aux_j; *aux_j = *aux_i; *aux_i = aux; end = FALSE; } 
        }
      } else {
        if(aux_j->type == 1){
          if(aux_i->t1 >= aux_j->t1 && aux_i->t2 >= aux_j->t1)
            { aux = *aux_j; *aux_j = *aux_i; *aux_i = aux; end = FALSE; } 
        } else {
          if(aux_i->t1 > aux_j->t1 && aux_i->t1 > aux_j->t2 && 
             aux_i->t2 > aux_j->t1 && aux_i->t1 > aux_j->t2) 
            { aux = *aux_j; *aux_j = *aux_i; *aux_i = aux; end = FALSE; } 
        }
      }
    }
  }
}

static void 
gtk_plot_csurface_draw_lines(GtkPlotData *data)
{
  GtkPlotCSurface *csurface = GTK_PLOT_CSURFACE(data);
  GtkPlot *plot = GTK_PLOT(data->plot);
  GtkPlotPoint *contour = NULL, *inner_contour = NULL;
  GdkColor color;
  gboolean closed;
  gint n1;
  GList *list, *lines = NULL;
  GtkPlotContourLine *branch = NULL;
  gchar text[100];
  GList *labels = FALSE;
  GdkRectangle clip_area;

  clip_area.x = plot->internal_allocation.x;
  clip_area.y = plot->internal_allocation.y;
  clip_area.width = plot->internal_allocation.width;
  clip_area.height = plot->internal_allocation.height;
  gtk_plot_pc_clip(plot->pc, &clip_area);

  if(csurface->projection == GTK_PLOT_PROJECT_FULL){
    gtk_plot_data_get_gradient_level(data, data->gradient->ticks.min - 1., &color); 
    gtk_plot_pc_set_color(data->plot->pc, &color);
    gtk_plot_pc_draw_rectangle (plot->pc, TRUE,
                                clip_area.x, clip_area.y,
                                clip_area.width, clip_area.height);

    list = csurface->bg_triangles;
    while(list){
      GtkPlotPoint p[3];
      GtkPlotPolygon *poly = (GtkPlotPolygon *)list->data;
  
      gtk_plot_get_pixel(plot, poly->xyz[0].x, poly->xyz[0].y, &p[0].x, &p[0].y);
      gtk_plot_get_pixel(plot, poly->xyz[1].x, poly->xyz[1].y, &p[1].x, &p[1].y);
      gtk_plot_get_pixel(plot, poly->xyz[2].x, poly->xyz[2].y, &p[2].x, &p[2].y);
  
      gtk_plot_data_get_gradient_level(data, poly->level, &color); 
      gtk_plot_pc_set_color(data->plot->pc, &color);
    
      gtk_plot_pc_draw_polygon(plot->pc, TRUE, p, 3); 
  
      list = list->next;
    }
  } 

 
  list = csurface->levels; 
  while(list){
    GtkPlotContourLevel *level = (GtkPlotContourLevel *)list->data;
    GList *polygons = level->polygons;

    while(polygons){
      GList *last = polygons;
      GList *aux = polygons;
      gint nlines = 0, ntotal = 0, n, i;

      while(aux){
        GtkPlotPolygon *p = (GtkPlotPolygon *)aux->data;
        if(p->n == 0) break;
        nlines++;
        ntotal += 2;
        aux = aux->next;
      }
      last = aux->next;

      if(nlines == 0){ polygons = last; continue; }

      contour = g_new0(GtkPlotPoint, ntotal); 
      inner_contour = g_new0(GtkPlotPoint, ntotal); 

      /* Actual contour line */
      for(aux = polygons, n = 0, i = 0; n < nlines; aux = aux->next, n++, i+=2){
        GtkPlotPolygon *p = (GtkPlotPolygon *)aux->data;
        gdouble x1, y1, x2, y2;
        gtk_plot_get_pixel(plot, p->xyz[0].x, p->xyz[0].y, &x1, &y1);
        gtk_plot_get_pixel(plot, p->xyz[1].x, p->xyz[1].y, &x2, &y2);
        contour[i].x = x1; contour[i].y = y1; 
        contour[i+1].x = x2; contour[i+1].y = y2; 
      }
   
      sort_lines(contour, nlines, &n1);
   
      closed = FALSE;
      if(contour[n1-1].x == contour[0].x && contour[n1-1].y == contour[0].y)
        closed = TRUE; 

      branch = g_new0(GtkPlotContourLine, 1);
      branch->contour = contour;
      branch->n1 = n1;
      branch->level = level->level;
      branch->sublevel = level->sublevel;
      lines = g_list_append(lines, branch);
 
      if(csurface->projection == GTK_PLOT_PROJECT_FULL) {
	GtkPlotPoint pp[4];
        gtk_plot_data_get_gradient_level(data, level->level == data->gradient->ticks.min ? level->level - 0.1 : level->level, &color); 
        gtk_plot_pc_set_color(data->plot->pc, &color);
	aux = polygons;
        while(aux){
	  GtkPlotPolygon *p = (GtkPlotPolygon *)aux->data;
	  if(p->n == 0) break;
  	  for(i = 0; i < p->n; i++)
	    gtk_plot_get_pixel(data->plot, p->xyz[i].x, p->xyz[i].y, &pp[i].x, &pp[i].y);
 	  gtk_plot_pc_draw_polygon(plot->pc, TRUE, pp, p->n);
	  aux = aux->next;
	}
      }

      polygons = last;
    }
    list = list->next;
  }

  if(data->show_labels){
    list = lines;
    while(list){
      GtkPlotContourLine *s = (GtkPlotContourLine *)list->data;
      GtkPlotText label;
      gdouble length = 0., pos = 0.;
      gdouble dx = 0., dy = 0.;
      gint x, y, width, height, a, d;
      gint i;
      gdouble prev = 0.;
      GtkAllocation *new_bb;

      if(s->sublevel) { list = list->next; continue; }

      label = data->labels_attr;
      label.justification = GTK_JUSTIFY_CENTER;
      label.angle = 0;
      gtk_plot_axis_parse_label(data->gradient, s->level, data->legends_precision, data->legends_style, text);

      label.text = text; 
      gtk_plot_text_get_size(text, 0, data->labels_attr.font, data->labels_attr.height, &width, &height, &a, &d);

      for(i = 0; i < s->n1-1; i++){
        dx = s->contour[i+1].x - s->contour[i].x;
        dy = s->contour[i+1].y - s->contour[i].y;
        length += sqrt(dx*dx + dy*dy);
      }
      

      for(i = 0; i < (length < 150 ? 1 : s->n1-1); i++){
        dx = s->contour[i+1].x - s->contour[i].x;
        dy = s->contour[i+1].y - s->contour[i].y;
        pos += sqrt(dx*dx + dy*dy);
        if(i == 0 || (pos - prev >= length/2)){
          GtkAllocation bb;
          x = s->contour[i].x + dx/2;
          y = s->contour[i].y + dy/2 + height / 2;
          bb.x = x - width / 2 - label.border_space;
          bb.y = y - height;
          bb.width = width + 2*label.border_space;
          bb.height = height + label.border_space;
          label.angle = 0;

/*
          if(abs(dy) > 2*abs(dx)){
            bb.x = x - height; 
            bb.y = y - width / 2; 
            bb.width = height;
            bb.height = width;
            label.angle = 90;
          }
*/

          gtk_plot_pc_clip(plot->pc, NULL);
          if(bb.x >= plot->internal_allocation.x && bb.x + bb.width <= plot->internal_allocation.x + plot->internal_allocation.width  && bb.y + bb.height < plot->internal_allocation.y + plot->internal_allocation.height && bb.y > plot->internal_allocation.y){
            GList *aux = labels;
            gboolean overlap = FALSE;

            while(aux){
              GtkAllocation *aux_bb = (GtkAllocation *)aux->data;
              if(bb_intersect(aux_bb, &bb)){ overlap = TRUE; break; }
              aux = aux->next;
            }

            if(!overlap){
              prev = pos;
              new_bb = g_new0(GtkAllocation, 1);
              *new_bb = bb;
              labels = g_list_append(labels, new_bb);
              label.x = GTK_WIDGET(plot)->allocation.x + x;
              label.y = GTK_WIDGET(plot)->allocation.y + y;
              label.x /= GTK_WIDGET(plot)->allocation.width;
              label.y /= GTK_WIDGET(plot)->allocation.height;
              gtk_plot_draw_text(plot, label);
              if(i != 0) break;
            }
          }
          gtk_plot_pc_clip(plot->pc, &clip_area);
        }
      }
      list = list->next;
    } 
  }


  if(csurface->projection == GTK_PLOT_PROJECT_FULL || csurface->projection == GTK_PLOT_PROJECT_EMPTY){
    list = lines;
    while(list){
      GtkPlotContourLine *s = (GtkPlotContourLine *)list->data;
      GtkPlotPoint x1, x2;
      GtkPlotPoint *p1 = NULL, *p2 = NULL;
      gint i, n = 0, x = 0;
      GtkAllocation *aux_bb = NULL;
      GtkPlotPoint line[s->n1];
      gboolean prev_x = 0;

      if(s->sublevel){
	if(csurface->sublevels_line.line_style == GTK_PLOT_LINE_NONE)
	  { list = list->next; continue; }
        gtk_plot_set_line_attributes(plot, csurface->sublevels_line);
      }else{
	if(csurface->levels_line.line_style == GTK_PLOT_LINE_NONE)
	  { list = list->next; continue; }
        gtk_plot_set_line_attributes(plot, csurface->levels_line);
      }


      if(csurface->projection == GTK_PLOT_PROJECT_EMPTY){
        if(s->level > data->gradient->ticks.min){
          gtk_plot_data_get_gradient_level(data, s->level, &color); 
          gtk_plot_pc_set_color(data->plot->pc, &color);
        }
      }

      for(i = 0; i < s->n1-1; i++){
        GList *aux;
        GtkPlotContourX xp[20];
        gint nxp = 0;

        p1 = &s->contour[i];
        p2 = &s->contour[i+1];

        aux = labels;
        while(aux){
          gdouble t1, t2;
          aux_bb = (GtkAllocation *)aux->data;
          prev_x = x;
          x = line_intersect(aux_bb,p1,p2,&x1,&x2,&t1,&t2);
          if(x == 3) break;
          if(x != 0 && x != 3){
            xp[nxp].x1 = x1;
            xp[nxp].x2 = x2;
            xp[nxp].t1 = t1;
            xp[nxp].t2 = t2;
            xp[nxp].type = x;
            xp[nxp].bb = aux_bb;
            nxp++;
          }
          aux = aux->next;
        }

        if(x == 3) continue;

        if(nxp > 1) sort_points(xp,nxp);
    
        if(nxp > 0){
          gint j;
          for(j = 0; j < nxp; j++){
            GtkPlotContourX aux = xp[j];
            x = aux.type;

            if(x == 1){ 
              if(p1->x >= aux.bb->x && p1->x <= aux.bb->x+aux.bb->width && p1->y >= aux.bb->y && p1->y <= aux.bb->y+aux.bb->height){
                line[0] = aux.x1;
                n = 1;
              } else {
                if(n == 0) line[n++] = *p1;
                line[n++] = aux.x1;
                gtk_plot_pc_draw_lines(plot->pc, line, n); 
                n = 0;
              }
            } else { /* x == 2 */
              if(n == 0) line[n++] = *p1;
              line[n++] = aux.x1;
              gtk_plot_pc_draw_lines(plot->pc, line, n); 
              line[0] = aux.x2;
              n = 1;
            }
          }
          if(n == 1) line[n++] = *p2;
        } else {
          if(n == 0) line[n++] = *p1;
          line[n++] = *p2;
        }
        if(n > 1) gtk_plot_pc_draw_lines(plot->pc, line, n); 
      }

      list = list->next;
    }

  }

  if(labels){
    for (list = labels; list; list = list->next) g_free(list->data);
    g_list_free(labels);
  }
  if(lines){
    for (list = lines; list; list = list->next){
      GtkPlotContourLine *s = (GtkPlotContourLine *)list->data;
      g_free(s->contour);
      g_free(list->data);
    }
    g_list_free(lines);
  }

  gtk_plot_pc_clip(plot->pc, NULL);
}

static void
gtk_plot_csurface_draw_polygons (GtkPlotSurface *surface)
{
  GtkPlotCSurface *csurface = GTK_PLOT_CSURFACE(surface);
  GtkPlot *plot;

  plot = GTK_PLOT(GTK_PLOT_DATA(surface)->plot);
  if(!GTK_IS_PLOT3D(plot)) return;

  /* draw full projection */
  if(csurface->projection == GTK_PLOT_PROJECT_FULL)
    gtk_plot_csurface_real_draw_polygons(surface, csurface->projection);
  
  /* draw empty projection */
  if(csurface->projection == GTK_PLOT_PROJECT_EMPTY)
    gtk_plot_csurface_real_draw_polygons(surface, csurface->projection);

  /* draw surface */
  gtk_plot_csurface_real_draw_polygons(surface, GTK_PLOT_PROJECT_NONE);
}

static void
gtk_plot_csurface_real_draw_polygons (GtkPlotSurface *surface, GtkPlotProjection p)
{
  GtkPlot *plot;
  GtkPlot3D *plot3d;
  GtkPlotData *data;
  GtkPlotCSurface *csurface;
  GdkDrawable *drawable;
  GtkPlotPoint t[4];
  GtkPlotDTtriangle *triangle;
  GtkPlotVector points[3], side[3], light, normal;
  GdkColor color;
  GdkColor real_color;
  gdouble factor, norm;
  GList *list;

  csurface = GTK_PLOT_CSURFACE(surface);
  data = GTK_PLOT_DATA(surface);
  plot = GTK_PLOT(data->plot);
  plot3d = GTK_PLOT3D(data->plot);
  drawable = plot->drawable;

  list = surface->polygons;
  while(list){
    GtkPlotPolygon *polygon = (GtkPlotPolygon *)list->data;

    if(plot->clip_data){
      gboolean discard = FALSE;
      gint i;
      for(i = 0; i < polygon->n; i++){
        if(polygon->xyz[i].x < plot3d->ax->ticks.min || polygon->xyz[i].x > plot3d->ax->ticks.max ||
           polygon->xyz[i].y < plot3d->ay->ticks.min || polygon->xyz[i].y > plot3d->ay->ticks.max ||
           polygon->xyz[i].z < plot3d->az->ticks.min || polygon->xyz[i].z > plot3d->az->ticks.max)
           { discard = TRUE; break; }
      }
      if(discard)  { list = list->next; continue; }
    }     


    triangle = polygon->t;

    light = surface->light;
    norm = sqrt(light.x*light.x + light.y*light.y + light.z*light.z); 
    light.x /= norm;
    light.y /= norm;
    light.z /= norm;
  
    t[0].x = polygon->p[0].x;
    t[0].y = polygon->p[0].y;
    t[1].x = polygon->p[1].x;
    t[1].y = polygon->p[1].y;
    t[2].x = polygon->p[2].x;
    t[2].y = polygon->p[2].y;
    t[3].x = polygon->p[3].x;
    t[3].y = polygon->p[3].y;
  
    points[0].x = triangle->na->x;
    points[0].y = triangle->na->y;
    points[0].z = triangle->na->z;
    points[1].x = triangle->nb->x;
    points[1].y = triangle->nb->y;
    points[1].z = triangle->nb->z;
    points[2].x = triangle->nc->x;
    points[2].y = triangle->nc->y;
    points[2].z = triangle->nc->z;
  
    side[0].x = -(points[1].x - points[0].x);
    side[0].y = -(points[1].y - points[0].y);
    side[0].z = -(points[1].z - points[0].z);
    side[1].x = -(points[2].x - points[0].x);
    side[1].y = -(points[2].y - points[0].y);
    side[1].z = -(points[2].z - points[0].z);
    
    if(p == GTK_PLOT_PROJECT_NONE){       
      normal.x = side[0].y * side[1].z - side[0].z * side[1].y;
      normal.y = side[0].z * side[1].x - side[0].x * side[1].z;
      normal.z = side[0].x * side[1].y - side[0].y * side[1].x;
  
      norm = sqrt(normal.x*normal.x + normal.y*normal.y + normal.z*normal.z); 
      factor = (normal.x*light.x + normal.y*light.y + normal.z*light.z) / norm;
      gtk_plot_data_get_gradient_level(data, polygon->level, &color); 
      gtk_plot_csurface_lighting(&color, &real_color, factor, 1.); 
    } else {
      if(polygon->level <= data->gradient->ticks.min)
        color = csurface->levels_line.color;
      else
        gtk_plot_data_get_gradient_level(data, polygon->level, &color); 
    }
  
    if(p == GTK_PLOT_PROJECT_FULL){
        GtkPlotPoint xyz[4];
        gdouble z;

        gtk_plot3d_get_pixel(GTK_PLOT3D(plot),
                             polygon->xyz[0].x, polygon->xyz[0].y, GTK_PLOT3D(plot)->origin.z,
                             &xyz[0].x, &xyz[0].y, &z);
        gtk_plot3d_get_pixel(GTK_PLOT3D(plot),
                             polygon->xyz[1].x, polygon->xyz[1].y, GTK_PLOT3D(plot)->origin.z,
                             &xyz[1].x, &xyz[1].y, &z);
        gtk_plot3d_get_pixel(GTK_PLOT3D(plot),
                             polygon->xyz[2].x, polygon->xyz[2].y, GTK_PLOT3D(plot)->origin.z,
                             &xyz[2].x, &xyz[2].y, &z);

        if(polygon->n == 4)
          gtk_plot3d_get_pixel(GTK_PLOT3D(plot),
                               polygon->xyz[3].x, polygon->xyz[3].y, GTK_PLOT3D(plot)->origin.z,
                               &xyz[3].x, &xyz[3].y, &z);

        gtk_plot_pc_set_color(data->plot->pc, &color);
        gtk_plot_pc_draw_polygon(data->plot->pc, TRUE, xyz, polygon->n);

    }
 
    if(p == GTK_PLOT_PROJECT_EMPTY && polygon->cut_level){ 
        GtkPlotPoint xyz[2];
        gdouble z;

        if(p== GTK_PLOT_PROJECT_EMPTY){
          if(polygon->sublevel){
            gtk_plot_set_line_attributes(plot, csurface->sublevels_line);
            if(csurface->sublevels_line.line_style == GTK_PLOT_LINE_NONE)
              gtk_plot_pc_set_color(plot->pc, &color);
          }else{
            gtk_plot_set_line_attributes(plot, csurface->levels_line);
            if(csurface->levels_line.line_style == GTK_PLOT_LINE_NONE)
              gtk_plot_pc_set_color(plot->pc, &color);
          }
        }

        gtk_plot3d_get_pixel(GTK_PLOT3D(plot),
                             polygon->xyz[0].x, polygon->xyz[0].y, GTK_PLOT3D(plot)->origin.z,
                             &xyz[0].x, &xyz[0].y, &z);
        gtk_plot3d_get_pixel(GTK_PLOT3D(plot),
                             polygon->xyz[1].x, polygon->xyz[1].y, GTK_PLOT3D(plot)->origin.z,
                             &xyz[1].x, &xyz[1].y, &z);

        gtk_plot_pc_draw_line(plot->pc, 
      	                      xyz[0].x, xyz[0].y, 
                              xyz[1].x, xyz[1].y);
    }

    if(p == GTK_PLOT_PROJECT_NONE){
      gtk_plot_pc_set_color(data->plot->pc, &real_color);
      gtk_plot_pc_draw_polygon(data->plot->pc, !surface->transparent, t, polygon->n);
      
      if(csurface->lines_visible || surface->show_mesh || surface->show_grid)
        gtk_plot_pc_set_color(data->plot->pc, &surface->grid_foreground);
  
      if(csurface->lines_visible && polygon->cut_level){
        if(polygon->sublevel){
          gtk_plot_set_line_attributes(plot, csurface->sublevels_line);
        }else{
          gtk_plot_set_line_attributes(plot, csurface->levels_line);
        }
        gtk_plot_pc_draw_line(data->plot->pc, 
     	   	              polygon->p[0].x, polygon->p[0].y, 
                              polygon->p[1].x, polygon->p[1].y);
      }
 
      if(surface->show_mesh){
        gtk_plot_set_line_attributes(plot, surface->mesh_line);
        gtk_plot_pc_draw_line(data->plot->pc, 
      	   	              polygon->p[1].x, polygon->p[1].y, 
                              polygon->p[2].x, polygon->p[2].y);
        if(polygon->n == 3)
          gtk_plot_pc_draw_line(data->plot->pc, 
           	                polygon->p[0].x, polygon->p[0].y, 
                                polygon->p[2].x, polygon->p[2].y);
        else{
          gtk_plot_pc_draw_line(data->plot->pc, 
      	     	                polygon->p[0].x, polygon->p[0].y, 
                                polygon->p[3].x, polygon->p[3].y);
          gtk_plot_pc_draw_line(data->plot->pc, 
      	     	                polygon->p[2].x, polygon->p[2].y, 
                                polygon->p[3].x, polygon->p[3].y);
        }
      }
  
      if(!surface->show_mesh && surface->show_grid && surface->dt->quadrilateral){
        gtk_plot_pc_draw_line(data->plot->pc, 
                              polygon->t->nb->px, polygon->t->nb->py, 
                              polygon->t->nc->px, polygon->t->nc->py); 
      }

    }

    list = list->next;
  }

}

static GtkPlotPolygon * 
sides_cut_level(GtkPlotDTtriangle *triangle, GtkPlotVector *points, GtkPlotVector *side, gdouble level)
{
  GtkPlotPolygon *polygon;
  GtkPlotVector c[3];
  gint vertex[3];

  if(points[0].z > level || points[1].z > level || points[2].z > level){
    gint n = 0;
    gint mask = 0;
    gint i;
    for(i = 0; i < 3; i++){
      gdouble a;

      a = -1.0;
      if(side[i].z != 0.0) 
        a = (level - points[i].z) / side[i].z; 

      if(a >= 0. && a <= 1.0){
        mask |= 1 << i;
        c[n].x = points[i].x + side[i].x * a;
        c[n].y = points[i].y + side[i].y * a;
        c[n].z = level;
        n++;
      }

      if(n == 3){ /* intersection in a vertex => repeated point */
        if((c[2].x == c[0].x && c[2].y == c[0].y) || (c[2].x == c[1].x && c[2].y == c[1].y)){ 
          n--;
          mask = 3;
        } else if(c[1].x == c[0].x && c[1].y == c[0].y){
          c[1].x = c[2].x;
          c[1].y = c[2].y;
          n--;
          mask = 5;
        } 
      }
    }

    if(n >= 2){
      switch(mask){
        case 5:
           vertex[0] = 0;
           vertex[1] = 2;
           vertex[2] = 1;
           break;
        case 3:
           vertex[0] = 1;
           vertex[1] = 2;
           vertex[2] = 0;
           break;
        case 6:
           vertex[0] = 2;
           vertex[1] = 0;
           vertex[2] = 1;
           break;
        default:
           if(c[0].z == c[1].z) c[1] = c[2];
           vertex[0] = 0;
           vertex[1] = 2;
           vertex[2] = 1;
           break;
      }
      if(points[vertex[0]].z <= level){
        polygon = g_new0(GtkPlotPolygon, 1);
        polygon->level = level;
        polygon->n = 3;
        polygon->t = triangle;
        polygon->cut_level = TRUE;
        polygon->xyz[0] = c[0];
        polygon->xyz[1] = c[1];
        polygon->xyz[2] = points[vertex[0]];
      }else{
        polygon = g_new0(GtkPlotPolygon, 1);
        polygon->level = level;
        polygon->n = 4;
        polygon->t = triangle;
        polygon->cut_level = TRUE;
        polygon->xyz[0] = c[0];
        polygon->xyz[1] = c[1];
        polygon->xyz[2] = points[vertex[1]];
        polygon->xyz[3] = points[vertex[2]];
      }
      return polygon;
    }
  }
  return NULL;
}  

static GtkPlotPolygon * 
triangle_cuts_level(GtkPlotDTtriangle *triangle, gdouble level)
{
  GtkPlotVector points[3], side[3];

  if(!triangle || !triangle->na || !triangle->nb || !triangle->nc) return NULL;
  points[0].x = triangle->na->x;
  points[0].y = triangle->na->y;
  points[0].z = triangle->na->z;
  points[1].x = triangle->nb->x;
  points[1].y = triangle->nb->y;
  points[1].z = triangle->nb->z;
  points[2].x = triangle->nc->x;
  points[2].y = triangle->nc->y;
  points[2].z = triangle->nc->z;

  side[0].x = (points[1].x - points[0].x);
  side[0].y = (points[1].y - points[0].y);
  side[0].z = (points[1].z - points[0].z);
  side[1].x = (points[2].x - points[1].x);
  side[1].y = (points[2].y - points[1].y);
  side[1].z = (points[2].z - points[1].z);
  side[2].x = (points[0].x - points[2].x);
  side[2].y = (points[0].y - points[2].y);
  side[2].z = (points[0].z - points[2].z);

  return(sides_cut_level(triangle, points, side, level));
}  

static void
gtk_plot_csurface_build_polygons(GtkPlotSurface *surface)
{
  GList *list;
  GtkPlotData *data = GTK_PLOT_DATA(surface);
  gdouble min, max, step;
  gint nlevels;
  gdouble h;
  gint level;
  GtkPlotTick *ticks;
  GtkPlotVector points[3], side[3];

  clear_polygons(GTK_PLOT_CSURFACE(surface));

  if(!GTK_IS_PLOT3D(data->plot)){
    gtk_plot_csurface_build_contours(surface);
    return;
  }

  min = data->gradient->ticks.min;
  max = data->gradient->ticks.max;

  ticks = data->gradient->ticks.values;
  nlevels = data->gradient->ticks.nticks;

  step = (data->gradient->ticks.max - data->gradient->ticks.min);

  list = surface->dt->triangles;
  while(list){
    GtkPlotPolygon *polygon;
    GtkPlotDTtriangle *triangle = (GtkPlotDTtriangle *)list->data;
    if(!triangle || !triangle->na || !triangle->nb || !triangle->nc)
      { list = list->next; continue; }

    points[0].x = triangle->na->x;
    points[0].y = triangle->na->y;
    points[0].z = triangle->na->z;
    points[1].x = triangle->nb->x;
    points[1].y = triangle->nb->y;
    points[1].z = triangle->nb->z;
    points[2].x = triangle->nc->x;
    points[2].y = triangle->nc->y;
    points[2].z = triangle->nc->z;

    side[0].x = (points[1].x - points[0].x);
    side[0].y = (points[1].y - points[0].y);
    side[0].z = (points[1].z - points[0].z);
    side[1].x = (points[2].x - points[1].x);
    side[1].y = (points[2].y - points[1].y);
    side[1].z = (points[2].z - points[1].z);
    side[2].x = (points[0].x - points[2].x);
    side[2].y = (points[0].y - points[2].y);
    side[2].z = (points[0].z - points[2].z);

    for(level = nlevels-1; level >= 0; level--){
        gint n_cuts = 0;

        h = ticks[level].value; 

        if(triangle->na->z <= h) n_cuts++; 
        if(triangle->nb->z <= h) n_cuts++; 
        if(triangle->nc->z <= h) n_cuts++; 
        if(n_cuts != 3){
            polygon = g_new0(GtkPlotPolygon, 1);

            if(level != nlevels-1)
              polygon->level = ticks[level+1].value;
            else
              polygon->level = max+step;
            polygon->n = 3;
            polygon->t = triangle;
            polygon->cut_level = FALSE;
            polygon->xyz[0].x = triangle->na->x;
            polygon->xyz[0].y = triangle->na->y;
            polygon->xyz[0].z = triangle->na->z;
            polygon->xyz[1].x = triangle->nb->x;
            polygon->xyz[1].y = triangle->nb->y;
            polygon->xyz[1].z = triangle->nb->z;
            polygon->xyz[2].x = triangle->nc->x;
            polygon->xyz[2].y = triangle->nc->y;
            polygon->xyz[2].z = triangle->nc->z;
            surface->polygons = g_list_append(surface->polygons, polygon);
            break;
        }
    }

    for(level = nlevels-1; level >= 0; level--){

        h = ticks[level].value; 

        if((polygon = sides_cut_level(triangle, points, side, h))){  
          polygon->sublevel = ticks[level].minor;
          if(level == 0) polygon->level -= step;
          surface->polygons = g_list_append(surface->polygons, polygon);
        }

    }
    list = list->next;
  }

}

static void
gtk_plot_csurface_build_contours(GtkPlotSurface *surface)
{
  GList *list;
  GtkPlotData *data = GTK_PLOT_DATA(surface);
  GtkPlotCSurface *csurface = GTK_PLOT_CSURFACE(surface);
  GtkPlotPolygon *polygon = NULL, *prev = NULL;
  gdouble min, max, step;
  gint nlevels, nsublevels;
  gdouble h = 0;
  gint level;
  gint nlines;
  GtkPlotTick *values;

  min = data->gradient->ticks.min;
  max = data->gradient->ticks.max;

  nlevels = data->gradient->ticks.nticks;
  nsublevels = data->gradient->ticks.nminor;
  step = (data->gradient->ticks.max - data->gradient->ticks.min) ;

  values = data->gradient->ticks.values;
  nlevels = data->gradient->ticks.nticks;

  for(level = nlevels-1; level >= 0; level--){
      GtkPlotContourLevel *_level;

      _level = g_new0(GtkPlotContourLevel,1);

      _level->sublevel = values[level].minor;
      _level->level = values[level].value;
      csurface->levels = g_list_append(csurface->levels, _level);

      nlines = 0;
      list = surface->dt->triangles;
      while(list){
        GtkPlotDTtriangle *triangle = (GtkPlotDTtriangle *)list->data;
        triangle->visited = FALSE;
        list = list->next;
      }
      list = surface->dt->triangles;
      while(list){
        GtkPlotDTtriangle *triangle = (GtkPlotDTtriangle *)list->data;
        GtkPlotDTtriangle *next = triangle;
        GList *first = NULL;
        GList *last = NULL;
        GList *new_list = NULL;
        GList *branch = NULL;
        gboolean end = FALSE;
        nlines = 0;

        if(!triangle->visited){  
          if((polygon = triangle_cuts_level(triangle, values[level].value))){  

            /* New branch */

            triangle->visited = TRUE;
            polygon->level = values[level].value;
            prev = polygon;
            branch = g_list_append(branch, polygon);
            last = first = branch;

            nlines = 2;
            end = FALSE;
            while(!end){
              gint i;
              end = TRUE;
              for(i = 0; i < 3; i++){
                GtkPlotDTtriangle *nn = next->nn[i];
                if(nn && !nn->visited){
                  if((polygon = triangle_cuts_level(nn, values[level].value))){ 
                    nn->visited = TRUE;

                    if(!((prev->xyz[0].x == polygon->xyz[0].x &&
                        prev->xyz[0].y == polygon->xyz[0].y) ||
                       (prev->xyz[1].x == polygon->xyz[0].x &&
                        prev->xyz[1].y == polygon->xyz[0].y) ||
                       (prev->xyz[1].x == polygon->xyz[1].x &&
                        prev->xyz[1].y == polygon->xyz[1].y) ||
                       (prev->xyz[0].x == polygon->xyz[1].x &&
                        prev->xyz[0].y == polygon->xyz[1].y)))
                       { nn->visited = FALSE; continue; }

                    new_list = g_list_alloc();
                    new_list->data = polygon;
                    new_list->prev = last;
                    last->next = new_list;
                    last = new_list;

                    next = nn;
                    end = FALSE;
                    polygon->level = values[level].value;
                    prev = polygon;
                    nlines++;
                    break;
                  }
                }
              }
            }

            /* We add an extra polygon to mark the end of branch */
            polygon = g_new0(GtkPlotPolygon, 1);
            polygon->n = 0;
            polygon->level = 0.0;
            branch = g_list_append(branch, polygon);
            _level->polygons = g_list_concat(_level->polygons, branch);

          }
        }
        list = list->next;
      }
  }

/* Now we determine the background triangles */

  list = surface->dt->triangles;
  while(list){
    GtkPlotDTtriangle *triangle = (GtkPlotDTtriangle *)list->data;
    triangle->visited = FALSE;
    list = list->next;
  }


  for(level = nlevels-1; level >= 0; level--){
      h = values[level].value;
      list = surface->dt->triangles;
      while(list){
        GtkPlotDTtriangle *triangle = (GtkPlotDTtriangle *)list->data;
        gint n_cuts = 0;

        if(!triangle->visited){
          if(triangle->na->z <= h) n_cuts++;
          if(triangle->nb->z <= h) n_cuts++;
          if(triangle->nc->z <= h) n_cuts++;
          if(n_cuts == 1 || n_cuts == 2){
            GtkPlotPolygon *p = g_new0(GtkPlotPolygon, 1);
            p->level = h + step;
            if(level != nlevels-1)
              p->level = values[level+1].value;
            p->xyz[0].x = triangle->na->x;
            p->xyz[0].y = triangle->na->y;
            p->xyz[0].z = triangle->na->z;
            p->xyz[1].x = triangle->nb->x;
            p->xyz[1].y = triangle->nb->y;
            p->xyz[1].z = triangle->nb->z;
            p->xyz[2].x = triangle->nc->x;
            p->xyz[2].y = triangle->nc->y;
            p->xyz[2].z = triangle->nc->z;
            p->n = 3;
            triangle->visited = TRUE;
            csurface->bg_triangles = g_list_append(csurface->bg_triangles, p);
          }
        }
        list = list->next;
      }
  }


  list = surface->dt->triangles;
  while(list){
    GtkPlotDTtriangle *triangle = (GtkPlotDTtriangle *)list->data;
    if(!triangle->visited){
      GtkPlotPolygon *p = g_new0(GtkPlotPolygon, 1);
      p->level = triangle->na->z;
      p->xyz[0].x = triangle->na->x;
      p->xyz[0].y = triangle->na->y;
      p->xyz[0].z = triangle->na->z;
      p->xyz[1].x = triangle->nb->x;
      p->xyz[1].y = triangle->nb->y;
      p->xyz[1].z = triangle->nb->z;
      p->xyz[2].x = triangle->nc->x;
      p->xyz[2].y = triangle->nc->y;
      p->xyz[2].z = triangle->nc->z;
      p->n = 3;
      triangle->visited = TRUE;
      csurface->bg_triangles = g_list_append(csurface->bg_triangles, p);
    }
    list = list->next;
  }

}

static void
clear_polygons(GtkPlotCSurface *csurface)
{
  GtkPlotSurface *surface = GTK_PLOT_SURFACE(csurface);
  if(surface->polygons){
    GList *list;
    for (list = surface->polygons; list; list = list->next)
      if (list->data) g_free(list->data);
    g_list_free(surface->polygons);
    surface->polygons= NULL;
  }

  if(csurface->levels){
    GList *list;
    for (list = csurface->levels; list; list = list->next)
      if (list->data){
        GtkPlotContourLevel *level = (GtkPlotContourLevel *)list->data;
        if(level->polygons){
          GList *aux;
          for (aux = level->polygons; aux; aux = aux->next)
            if (aux->data) g_free(aux->data);
          g_list_free(level->polygons);
        }
        g_free(list->data);
      }
    g_list_free(csurface->levels);
    csurface->levels = NULL;
  }

  if(csurface->bg_triangles){
    GList *list;
    for (list = csurface->bg_triangles; list; list = list->next)
      if (list->data) g_free(list->data);
    g_list_free(csurface->bg_triangles);
    csurface->bg_triangles = NULL;
  }
}

static void
gtk_plot_csurface_get_legend_size(GtkPlotData *data, gint *width, gint *height)
{
  GtkPlotSurface *surface;
  GtkPlotCSurface *csurface;
  GtkPlot *plot = NULL;
  GtkPlotText legend;
  gint lascent = 0, ldescent = 0, lheight = 0, lwidth = 0;
  gdouble m;

  surface = GTK_PLOT_SURFACE(data);
  csurface = GTK_PLOT_CSURFACE(data);

  g_return_if_fail(data->plot != NULL);
  g_return_if_fail(GTK_IS_PLOT(data->plot));

  plot = data->plot;

  m = plot->magnification;
  legend = plot->legends_attr;

  if(data->legend)
    legend.text = data->legend;
  else
    legend.text = "";

  *height = 0;
  *width = roundint(12 * m);

  if(data->show_legend){
    gtk_plot_text_get_size(legend.text, legend.angle, legend.font,
                           roundint(legend.height * m), 
                           &lwidth, &lheight,
                           &lascent, &ldescent);
    *height = MAX(lheight, roundint(data->symbol.size * m));
    *width = lwidth + roundint(12 * m);
  }

}

static void
gtk_plot_csurface_draw_legend(GtkPlotData *data, gint x, gint y)
{
  GtkPlotSurface *surface;
  GtkPlotCSurface *csurface;
  GtkPlot *plot = NULL;
  GtkPlotText legend;
  GdkRectangle area;
  gint lascent = 0, ldescent = 0, lheight = 0, lwidth = 0;
  gdouble m;

  surface = GTK_PLOT_SURFACE(data);
  csurface = GTK_PLOT_CSURFACE(data);

  g_return_if_fail(data->plot != NULL);
  g_return_if_fail(GTK_IS_PLOT(data->plot));

  plot = data->plot;
  area.x = GTK_WIDGET(plot)->allocation.x;
  area.y = GTK_WIDGET(plot)->allocation.y;
  area.width = GTK_WIDGET(plot)->allocation.width;
  area.height = GTK_WIDGET(plot)->allocation.height;

  m = plot->magnification;
  legend = plot->legends_attr;

  if(data->legend)
    legend.text = data->legend;
  else
    legend.text = "";

  gtk_plot_text_get_size(legend.text, legend.angle, legend.font,
                         roundint(legend.height * m), 
                         &lwidth, &lheight,
                         &lascent, &ldescent);

  if(GTK_PLOT_DATA(data)->show_legend){
    legend.x = (gdouble)(area.x + x + roundint(4 * m))
               / (gdouble)area.width;
    legend.y = (gdouble)(area.y + y + lascent) / (gdouble)area.height;

    gtk_plot_draw_text(plot, legend);
    y += 2 * lheight;
  } else
    y += lheight;
  
  GTK_PLOT_DATA(data)->gradient_custom = TRUE;
}

static void
gtk_plot_csurface_lighting (GdkColor *a, GdkColor *b, 
                           gdouble normal, gdouble ambient)
{
  gdouble red, green, blue;
  gdouble h, s, v;

  *b = *a;
  return;

  normal = MIN(fabs(normal), 1.0);

  red = a->red;
  green = a->green;
  blue = a->blue;

  rgb_to_hsv(red, green, blue, &h, &s, &v);

  s *= normal;
  v *= normal;

  s += ambient;
  v += ambient;

  hsv_to_rgb(h, MIN(s, 1.0), MIN(v, 1.0), &red, &green, &blue);

  b->red = red;
  b->green = green;
  b->blue = blue;
}


static void
hsv_to_rgb (gdouble  h, gdouble  s, gdouble  v,
            gdouble *r, gdouble *g, gdouble *b)
{
  gint i;
  gdouble f, w, q, t;

  if (s == 0.0)
    s = 0.000001;

  if (h == -1.0)
    {
      *r = v;
      *g = v;
      *b = v;
    }
  else
    {
      if (h == 360.0) h = 0.0;
      h = h / 60.0;
      i = (gint) h;
      f = h - i;
      w = v * (1.0 - s);
      q = v * (1.0 - (s * f));
      t = v * (1.0 - (s * (1.0 - f)));

      switch (i)
      {
        case 0:
          *r = v;
          *g = t;
          *b = w;
          break;
        case 1:
          *r = q;
          *g = v;
          *b = w;
          break;
        case 2:
          *r = w;
          *g = v;
          *b = t;
          break;
        case 3:
          *r = w;
          *g = q;
          *b = v;
          break;
        case 4:
          *r = t;
          *g = w;
          *b = v;
          break;
        case 5:
          *r = v;
          *g = w;
          *b = q;
          break;
      }
    }

  *r *= 65535.;
  *g *= 65535.;
  *b *= 65535.;
}

static void
rgb_to_hsv (gdouble  r, gdouble  g, gdouble  b,
            gdouble *h, gdouble *s, gdouble *v)
{
  double max, min, delta;

  r /= 65535.;
  g /= 65535.;
  b /= 65535.;

  max = r;
  if (g > max)
    max = g;
  if (b > max)
    max = b;

  min = r;
  if (g < min)
    min = g;
  if (b < min)
    min = b;

  *v = max;
  if (max != 0.0)
    *s = (max - min) / max;
  else
    *s = 0.0;

  if (*s == 0.0)
    *h = -1.0;
  else
    {
      delta = max - min;

      if (r == max)
        *h = (g - b) / delta;
      else if (g == max)
        *h = 2.0 + (b - r) / delta;
      else if (b == max)
        *h = 4.0 + (r - g) / delta;

      *h = *h * 60.0;

      if (*h < 0.0)
        *h = *h + 360;
    }
}



/***********************************

 ***********************************/

void
gtk_plot_csurface_set_lines_visible (GtkPlotCSurface *csurface, gboolean visible)
{
  csurface->lines_visible = visible;
}

gboolean
gtk_plot_csurface_get_lines_visible (GtkPlotCSurface *csurface)
{
  return (csurface->lines_visible);
}

void
gtk_plot_csurface_set_projection    (GtkPlotCSurface *csurface, GtkPlotProjection proj)
{
  csurface->projection = proj;
}

GtkPlotProjection       
gtk_plot_csurface_projection    (GtkPlotCSurface *csurface)
{
  return (csurface->projection);
}

void
gtk_plot_csurface_set_levels_attributes (GtkPlotCSurface *dataset,
                                        GtkPlotLineStyle style,
                                        gfloat width,
                                        const GdkColor *color)
{
  dataset->levels_line.line_style = style;
  dataset->levels_line.line_width = width;
  dataset->levels_line.color = *color;
}

void
gtk_plot_csurface_set_sublevels_attributes (GtkPlotCSurface *dataset,
                                        GtkPlotLineStyle style,
                                        gfloat width,
                                        const GdkColor *color)
{
  dataset->sublevels_line.line_style = style;
  dataset->sublevels_line.line_width = width;
  dataset->sublevels_line.color = *color;
}

void
gtk_plot_csurface_get_levels_attributes (GtkPlotCSurface *dataset,
                                        GtkPlotLineStyle *style,
                                        gfloat *width,
                                        GdkColor *color)
{
  *style = dataset->levels_line.line_style;
  *width = dataset->levels_line.line_width;
  *color = dataset->levels_line.color;
}

void
gtk_plot_csurface_get_sublevels_attributes (GtkPlotCSurface *dataset,
                                           GtkPlotLineStyle *style,
                                           gfloat *width,
                                           GdkColor *color)
{
  *style = dataset->sublevels_line.line_style;
  *width = dataset->sublevels_line.line_width;
  *color = dataset->sublevels_line.color;
}

