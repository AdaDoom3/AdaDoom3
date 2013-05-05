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

#include <string.h>
#include <gtk/gtkhbox.h>
#include <gtk/gtktogglebutton.h>
#include <gtk/gtkarrow.h>
#include <gtk/gtkeventbox.h>
#include <gtk/gtkmain.h>
#include <gtk/gtksignal.h>
#include <gtk/gtkwindow.h>
#include <gdk/gdkkeysyms.h>
#include <gtk/gtkframe.h>
#include "gtkcombobutton.h"

static void         gtk_combo_button_class_init      (GtkComboButtonClass *klass);
static void         gtk_combo_button_init            (GtkComboButton      *combo_button);
static void         gtk_combo_button_destroy         (GtkObject     *combo_button);
static void         gtk_combo_button_get_pos         (GtkComboButton      *combo_button, 
                                               	  gint          *x, 
                                                  gint          *y, 
                                                  gint          *height, 
                                                  gint          *width);
static void         gtk_combo_button_popup_display   (GtkComboButton *combo_button);
static gint	    gtk_combo_button_arrow_press     (GtkWidget * widget, 
					  	  GtkComboButton * combo_button);
static gint         gtk_combo_button_button_press    (GtkWidget     *widget,
				                  GdkEvent      *event,
                                                  gpointer data);
static void         gtk_combo_button_size_allocate   (GtkWidget     *widget,
					          GtkAllocation *allocation);
static void         gtk_combo_button_size_request    (GtkWidget     *widget,
					          GtkRequisition *requisition);


static GtkHBoxClass *parent_class = NULL;

static void
gtk_combo_button_class_init (GtkComboButtonClass * klass)
{
  GtkObjectClass *object_class;
  GtkWidgetClass *widget_class;

  parent_class = gtk_type_class (gtk_hbox_get_type ());
  object_class = (GtkObjectClass *) klass;
  widget_class = (GtkWidgetClass *) klass;

  object_class->destroy = gtk_combo_button_destroy;
  
  widget_class->size_allocate = gtk_combo_button_size_allocate;
  widget_class->size_request = gtk_combo_button_size_request;
}

static void
gtk_combo_button_destroy (GtkObject * combo_button)
{
  if (GTK_COMBO_BUTTON (combo_button)->popwin) {
     gtk_widget_destroy (GTK_COMBO_BUTTON (combo_button)->popwin);
     gtk_widget_unref (GTK_COMBO_BUTTON (combo_button)->popwin);
     GTK_COMBO_BUTTON (combo_button)->popwin = NULL;
  }

  if (GTK_OBJECT_CLASS (parent_class)->destroy)
    (*GTK_OBJECT_CLASS (parent_class)->destroy) (combo_button);
}


static void
gtk_combo_button_get_pos (GtkComboButton * combo_button, gint * x, gint * y, gint * height, gint * width)
{
  GtkBin *popwin;
  GtkWidget *widget;

  gint real_height, real_width;
  GtkRequisition child_requisition;
  gint avail_height, avail_width;
  gint min_height, min_width;
  gint work_width;
  gint work_height;

  widget = GTK_WIDGET(combo_button);
  popwin = GTK_BIN (combo_button->popwin);

  gdk_window_get_origin (combo_button->button->window, x, y);
  *x += combo_button->button->allocation.x;
  *y += combo_button->button->allocation.y;

  real_height = combo_button->button->allocation.height;
  real_width = combo_button->button->allocation.width + combo_button->arrow->allocation.width;

  *y += real_height;
  avail_height = gdk_screen_height () - *y;
  avail_width = gdk_screen_width() - *x;

  gtk_widget_size_request (combo_button->frame, &child_requisition);

  min_height = child_requisition.height;
  min_width = child_requisition.width;

  work_width = work_height = 0;

  if(work_height+child_requisition.height > avail_height)
    if(work_height + min_height > avail_height &&
       *y - real_height > avail_height)
      	      *y -= (work_height + child_requisition.height + real_height);

  if(work_width+child_requisition.width > avail_width)
    if(work_width + min_width > avail_width &&
       *x - real_width > avail_width)
      	      *x = *x + real_width - (work_width + child_requisition.width);

  *width = work_width + child_requisition.width;
  *height = work_height + child_requisition.height;
  
}


static void
gtk_combo_button_popup_display (GtkComboButton * combo_button)
{
  gint height, width, x, y;

  gtk_combo_button_get_pos (combo_button, &x, &y, &height, &width);

  gtk_window_move(GTK_WINDOW(combo_button->popwin), x, y);
  gtk_widget_set_usize (combo_button->popwin, width, height);
  gtk_widget_show (combo_button->popwin);

  gtk_grab_add (combo_button->popwin);
  gdk_pointer_grab (combo_button->popwin->window, TRUE,
		    GDK_BUTTON_PRESS_MASK | 
		    GDK_BUTTON_RELEASE_MASK |
		    GDK_POINTER_MOTION_MASK, 
		    NULL, NULL, GDK_CURRENT_TIME);

}

void
gtk_combo_button_hide_popdown_window(GtkComboButton *combo_button)
{
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(combo_button->arrow), FALSE);

  gtk_grab_remove(combo_button->popwin);
  gdk_pointer_ungrab(GDK_CURRENT_TIME);
  gtk_widget_hide(combo_button->popwin);
}

static gint
gtk_combo_button_arrow_press (GtkWidget * widget, GtkComboButton * combo_button)
{
  GtkToggleButton *button;

  button = GTK_TOGGLE_BUTTON(widget);

  if(!button->active){
     gtk_widget_hide (combo_button->popwin);
     gtk_grab_remove (combo_button->popwin);
     gdk_pointer_ungrab (GDK_CURRENT_TIME);
     return TRUE;
  }

  gtk_combo_button_popup_display(combo_button);
  return TRUE;
}


static void
gtk_combo_button_init (GtkComboButton * combo_button)
{
  GtkWidget *event_box;
  GdkCursor *cursor;
  GtkWidget *widget;
  GtkWidget *arrow;

  widget=GTK_WIDGET(combo_button);

  GTK_BOX(widget)->homogeneous = FALSE;

  combo_button->button = gtk_button_new ();
  combo_button->arrow = gtk_toggle_button_new ();

  arrow = gtk_arrow_new (GTK_ARROW_DOWN, GTK_SHADOW_IN);
  gtk_widget_show (arrow);
  gtk_container_add (GTK_CONTAINER (combo_button->arrow), arrow);

  gtk_box_pack_start (GTK_BOX (combo_button), combo_button->button, TRUE, TRUE, 0);
  gtk_box_pack_end (GTK_BOX (combo_button), combo_button->arrow, FALSE, FALSE, 0);

  gtk_widget_show (combo_button->button);
  gtk_widget_show (combo_button->arrow);

  gtk_signal_connect (GTK_OBJECT (combo_button->arrow), "toggled",
		      (GtkSignalFunc) gtk_combo_button_arrow_press, combo_button);

                       
  combo_button->popwin = gtk_window_new (GTK_WINDOW_POPUP);
  gtk_widget_ref (combo_button->popwin);
  gtk_window_set_resizable (GTK_WINDOW (combo_button->popwin), FALSE);
  gtk_window_set_policy (GTK_WINDOW (combo_button->popwin), 1, 1, 0);
  gtk_widget_set_events (combo_button->popwin, GDK_KEY_PRESS_MASK);
 
  event_box = gtk_event_box_new ();
  gtk_container_add (GTK_CONTAINER (combo_button->popwin), event_box);
  gtk_widget_show (event_box);

  gtk_widget_realize (event_box);
  cursor = gdk_cursor_new (GDK_TOP_LEFT_ARROW);
  gdk_window_set_cursor (event_box->window, cursor);
  gdk_cursor_destroy (cursor);

  combo_button->frame = gtk_frame_new (NULL);
  gtk_container_add (GTK_CONTAINER (event_box), combo_button->frame);
  gtk_frame_set_shadow_type (GTK_FRAME (combo_button->frame), GTK_SHADOW_OUT);
  gtk_widget_show (combo_button->frame);

  gtk_signal_connect (GTK_OBJECT (combo_button->popwin), "button_press_event",
		      GTK_SIGNAL_FUNC (gtk_combo_button_button_press), combo_button);
  

}

GtkType
gtk_combo_button_get_type ()
{
  static GtkType combo_button_type = 0;

  if (!combo_button_type)
    {
      GtkTypeInfo combo_button_info =
      {
	"GtkComboButton",
	sizeof (GtkComboButton),
	sizeof (GtkComboButtonClass),
	(GtkClassInitFunc) gtk_combo_button_class_init,
	(GtkObjectInitFunc) gtk_combo_button_init,
	NULL,
	NULL,
	(GtkClassInitFunc) NULL,
      };
      combo_button_type = gtk_type_unique (gtk_hbox_get_type (), &combo_button_info);
    }
  return combo_button_type;
}

GtkWidget *
gtk_combo_button_new ()
{
  GtkComboButton *combo_button;

  combo_button = gtk_type_new (gtk_combo_button_get_type ());

  return(GTK_WIDGET(combo_button));

}

static gint
gtk_combo_button_button_press (GtkWidget * widget, GdkEvent * event, gpointer data)
{
  GtkWidget *child;

  child = gtk_get_event_widget (event);

  if (child != widget)
    {
      while (child)
	{
	  if (child == widget)
	    return FALSE;
	  child = child->parent;
	}
    }

  gtk_widget_hide (widget);
  gtk_grab_remove (widget);
  gdk_pointer_ungrab (GDK_CURRENT_TIME);
  gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(GTK_COMBO_BUTTON(data)->arrow), FALSE);

  return TRUE;
}

static void
gtk_combo_button_size_request (GtkWidget *widget,
			   GtkRequisition *requisition)
{
  GtkComboButton *combo_button;
  GtkRequisition box_requisition;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_COMBO_BUTTON (widget));
  g_return_if_fail (requisition != NULL);

  GTK_WIDGET_CLASS (parent_class)->size_request (widget, &box_requisition);

  combo_button=GTK_COMBO_BUTTON(widget);
/*
  size = MIN(box_requisition.width, box_requisition.height);
  size = MIN(combo_button->button->requisition.width, box_requisition.height);
  size = MIN(combo_button->button->requisition.width, box_requisition.height);

  widget->requisition.height = size;
  widget->requisition.width = size + combo_button->arrow->requisition.width;
*/
  widget->requisition.height = box_requisition.height;
  widget->requisition.width = box_requisition.width;
}


static void
gtk_combo_button_size_allocate (GtkWidget     *widget,
			 GtkAllocation *allocation)
{
  GtkComboButton *combo_button;
  GtkAllocation button_allocation;

  g_return_if_fail (widget != NULL);
  g_return_if_fail (GTK_IS_COMBO_BUTTON (widget));
  g_return_if_fail (allocation != NULL);

  GTK_WIDGET_CLASS (parent_class)->size_allocate (widget, allocation);

  combo_button = GTK_COMBO_BUTTON (widget);

  button_allocation = combo_button->button->allocation;
/*
  button_allocation.width = MIN(button_allocation.width, 
                                combo_button->button->requisition.width);
  button_allocation.height = MIN(button_allocation.height, 
                               combo_button->button->requisition.height);
  button_allocation.x += (combo_button->button->allocation.width-
                        button_allocation.width) / 2;
  button_allocation.y += (combo_button->button->allocation.height-
                        button_allocation.height) / 2;
*/

  gtk_widget_size_allocate (combo_button->button, &button_allocation);

  button_allocation.x=combo_button->button->allocation.x +
                      combo_button->button->allocation.width;
  button_allocation.width=combo_button->arrow->requisition.width;
  gtk_widget_size_allocate (combo_button->arrow, &button_allocation);

}

