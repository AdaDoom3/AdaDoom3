-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                Copyright (C) 2006-2013, AdaCore                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

--  <description>
--  This package contains low-level subprograms that are used to interact or
--  configure the main loop.
--  This loop is responsible for processing events, monitoring input sources
--  like pipes, sockets,..., and calling callbacks at given time intervals.
--  New event sources can be created.
--
--  To allow multiple independent sets of sources to be handled in different
--  threads, each source is associated with a main context. A main context can
--  only be running in a single thread, but sources can be added to it and
--  removed from it from other threads.
--
--  Each event source is assigned a priority. The default priority,
--  G_PRIORITY_DEFAULT, is 0. Values less than 0 denote higher priorities.
--  Values greater than 0 denote lower priorities. Events from high priority
--  sources are always processed before events from lower priority sources.
--
--  Idle functions can also be added, and assigned a priority. These will be
--  run whenever no events with a higher priority are ready to be processed.
--
--  The GMainLoop data type represents a main event loop. A GMainLoop is
--  created with g_main_loop_new(). After adding the initial event sources,
--  g_main_loop_run() is called. This continuously checks for new events from
--  each of the event sources and dispatches them. Finally, the processing of
--  an event from one of the sources leads to a call to g_main_loop_quit() to
--  exit the main loop, and g_main_loop_run() returns.

--  It is possible to create new instances of GMainLoop recursively. This is
--  often used in GTK+ applications when showing modal dialog boxes. Note that
--  event sources are associated with a particular GMainContext, and will be
--  checked and dispatched for all main loops associated with that
--  GMainContext.
--
--  Creating new sources types
--  ==========================
--
--  One of the unusual features of the GTK+ main loop functionality is that new
--  types of event source can be created and used in addition to the builtin
--  type of event source. A new event source type is used for handling GDK
--  events.
--
--  New source types basically interact with with the main context in two ways.
--  Their prepare function in GSourceFuncs can set a timeout to determine the
--  maximum amount of time that the main loop will sleep before checking the
--  source again. In addition, or as well, the source can add file descriptors
--  to the set that the main context checks using g_source_add_poll().
--
--  Ada
--  ===
--
--  Some of these features duplicate Ada builtin tasking support, but the
--  latter might be more complex to use in the context of a graphical
--  application, since most of the time the windowing system doesn't support
--  multi-threaded applications.
--  </description>
--  <c_version>glib 2.10.2</c_version>
--  <group>Glib, the general-purpose library</group>
--  <testgtk>create_sources.adb</testgtk>

package Glib.Main is

   --------------------
   -- G_Main_Context --
   --------------------

   type G_Main_Context is new Glib.C_Proxy;
   --  This type represents a set of sources to handled in the main loop.
   --  Basically, this represents a main loop. There might be several main
   --  loops running at the same time, although gtk+ itself has only one,
   --  identified as the default main context.

   function Main_Context_New return G_Main_Context;
   --  Create a new context

   procedure Main_Context_Ref   (Context : G_Main_Context);
   procedure Main_Context_Unref (Context : G_Main_Context);
   --  Increase or decreate the reference counting for Context. When this
   --  reaches 0, the memory is freed.

   function Main_Context_Default return G_Main_Context;
   --  Returns the default main context. This is the main context used for main
   --  loop functions when a main loop is not explicitly specified.

   procedure Wakeup (Context : G_Main_Context);
   --  If context is currently waiting in a poll(), interrupt the poll(), and
   --  continue the iteration process.

   function Acquire (Context : G_Main_Context) return Boolean;
   --  Tries to become the owner of the specified context. If some other thread
   --  is the owner of the context, returns FALSE immediately. Ownership is
   --  properly recursive: the owner can require ownership again and will
   --  release ownership when Release() is called as many times as Acquire().
   --  You must be the owner of a context before you can call Prepare(),
   --  Query(), Check(), Dispatch().

   procedure Release (Context : G_Main_Context);
   --  Releases ownership of a context previously acquired by this thread with
   --  Acquire(). If the context was acquired multiple times, the only release
   --  ownership when Release() is called as many times as it was acquired.

   function Is_Owner (Context : G_Main_Context) return Boolean;
   --  Determines whether this thread holds the (recursive) ownership of this
   --  context. This is useful to know before waiting on another thread
   --  that may be blocking to get ownership of context.

   procedure Dispatch (Context : G_Main_Context);
   --  Dispatches all pending sources.

   ---------------
   -- Main loop --
   ---------------

   function Depth return Integer;
   --  The main loop recursion level in the current thread. It returns 0 when
   --  called from the toplevel.

   --------------
   -- G_Source --
   --------------

   type G_Source is new Glib.C_Proxy;
   --  This type represents an event source that can be monitored by the main
   --  loop. There are various internal types of such sources, that can be
   --  configured by setting appropriate callbacks (this is not yet doable in
   --  GtkAda). See Idle_Source_New and Timeout_Source_New.

   type G_Source_Id is new Guint;
   --  The ID of a source within the context to which it is attached.

   No_Source_Id : constant G_Source_Id;

   type G_Source_Func is access function return Boolean;

   type Source_Prepare_Func is access
     function (Source : G_Source; Timeout : access Gint) return Gboolean;
   pragma Convention (C, Source_Prepare_Func);
   --  Called before all the file descriptors are polled. If the source can
   --  determine that it is ready here (without waiting for the results of the
   --  poll() call) it should return TRUE. It can also return a timeout value
   --  which should be the maximum timeout (in milliseconds) which should be
   --  passed to the poll() call. The actual timeout used will be -1 if all
   --  sources returned -1, or it will be the minimum of all the timeout_
   --  values returned which were >= 0.

   type Source_Check_Func is access
     function (Source : G_Source) return Gboolean;
   pragma Convention (C, Source_Check_Func);
   --  Called after all the file descriptors are polled. The source should
   --  return TRUE if it is ready to be dispatched. Note that some time may
   --  have passed since the previous prepare function was called, so the
   --  source should be checked again here.

   type G_Source_Func_User_Data is access
     function (User_Data : System.Address) return Gboolean;
   pragma Convention (C, G_Source_Func_User_Data);
   --  A callback for a G_Source. If it returns False, the source will be
   --  removed and no longer executed. User_Data is the data passed to
   --  Set_Callback.

   type Source_Dispatch_Func is access
     function (Source   : G_Source;
               Callback : G_Source_Func_User_Data;
               Data     : System.Address) return Gboolean;
   pragma Convention (C, Source_Dispatch_Func);
   --  Called to dispatch the event source, after it has returned TRUE in
   --  either its prepare or its check function. The dispatch function is
   --  passed in a callback function and data. The callback function may be
   --  NULL if the source was never connected to a callback using
   --  Set_Callback().
   --  In C, the exact profile of Callback depends on the type of Source. This
   --  is not possible in Ada, which expects a precise profile.

   function Default_Dispatch
     (Source : G_Source; Cb : G_Source_Func_User_Data; Data : System.Address)
      return Gboolean;
   pragma Convention (C, Default_Dispatch);
   --  Default implementation for the dispatch callback for sources. This
   --  simply calls Cb and pass it Data.

   type Source_Finalize_Func is access procedure (Source : G_Source);
   pragma Convention (C, Source_Finalize_Func);
   --  Called when the source is finalized.

   type G_Source_Type is private;
   Null_Source_Type : constant G_Source_Type;
   function G_Source_Type_New
     (Prepare  : Source_Prepare_Func;
      Check    : Source_Check_Func;
      Dispatch : Source_Dispatch_Func := Default_Dispatch'Access;
      Finalize : Source_Finalize_Func := null) return G_Source_Type;
   --  Create a new type of sources.
   --  This function is specific to GtkAda. The returned value is never
   --  freed. Most of the time, you do not need to create a new source type,
   --  or even call Source_New. Most things can be implemented through the
   --  careful use of Idle and Timeout callbacks. However, creating a new
   --  source type allows for cleaner code, by sharing the common part of the
   --  handling.
   --
   --  For idle sources, the prepare and check functions always return TRUE to
   --  indicate that the source is always ready to be processed. The prepare
   --  function also returns a timeout value of 0 to ensure that the poll()
   --  call doesn't block (since that would be time wasted which could have
   --  been spent running the idle function).
   --
   --  For timeout sources, the prepare and check functions both return TRUE if
   --  the timeout interval has expired. The prepare function also returns a
   --  timeout value to ensure that the poll() call doesn't block too long and
   --  miss the next timeout.
   --
   --  For file descriptor sources, the prepare function typically returns
   --  FALSE, since it must wait until poll() has been called before it knows
   --  whether any events need to be processed. It sets the returned timeout to
   --  -1 to indicate that it doesn't mind how long the poll() call blocks. In
   --  the check function, it tests the results of the poll() call to see if
   --  the required condition has been met, and returns TRUE if so.

   function Source_New
     (Source_Type : G_Source_Type; User_Data : System.Address) return G_Source;
   --  Creates a new GSource structure.
   --
   --  The source will not initially be associated with any GMainContext and
   --  must be added to one with Attach() before it will be executed.

   function Get_User_Data (Source : G_Source) return System.Address;
   --  Return the user data passed to Source_New. This only applies to sources
   --  created through that function, and returns undefined results (or even
   --  segfaults) otherwise

   procedure Source_Ref   (Source : G_Source);
   procedure Source_Unref (Source : G_Source);
   --  Increase or decrease the reference counting for Source. When this
   --  reaches 0, the Source is destroyed

   procedure Source_Destroy (Source : G_Source);
   --  Removes the source from its context, and mark it as destroyed (the
   --  memory is not reclaimed while the reference counting doesn't reach 0).
   --  Source cannot be added to another context.

   function Attach
     (Source  : G_Source;
      Context : G_Main_Context := null) return G_Source_Id;
   --  Add Source to Context. The Source will be executed within that context.
   --  If context is null, the source is added to the default context.
   --  Returns the Id of the source within Context.

   function Remove (Id : G_Source_Id) return Boolean;
   procedure Remove (Id : G_Source_Id);
   --  Removes the source with the given id from the default main context.
   --  The id of. Return True if the source was found and removed

   type G_Priority is new Gint;
   Priority_High         : constant G_Priority := -100;
   Priority_Default      : constant G_Priority := 0;
   Priority_High_Idle    : constant G_Priority := 100;
   Priority_Default_Idle : constant G_Priority := 200;
   Priority_Low          : constant G_Priority := 300;
   --  Priority_High and Priority_Low are not used within glib or gtk+. The
   --  priority for all graphical events is Priority_Default. gtk+ uses
   --  Priority_High_Idle+10 for resizing operations, and
   --  Priority_High_Idle+20 for redrawing operations, to ensure that resizing
   --  occurs before redrawing and avoid redrawing twice.

   procedure Set_Priority (Source : G_Source; Priority : G_Priority);
   function  Get_Priority (Source : G_Source) return G_Priority;
   --  Sets the priority of a source. While the main loop is being run, a
   --  source will be dispatched if it is ready to be dispatched and no sources
   --  at a higher (numerically smaller) priority are ready to be dispatched.

   procedure Set_Can_Recurse (Source : G_Source; Can_Recurse : Boolean);
   function  Get_Can_Recurse (Source : G_Source) return Boolean;
   --  Sets whether a source can be called recursively. If can_recurse is TRUE,
   --  then while the source is being dispatched then this source will be
   --  processed normally. Otherwise, all processing of this source is blocked
   --  until the dispatch function returns.

   function Get_Id (Source : G_Source) return G_Source_Id;
   --  Returns the numeric ID for a particular source. The ID of a source is
   --  positive integer which is unique within a particular main loop context.
   --  The reverse mapping from ID to source is done by Find_Source_By_Id

   function Find_Source_By_Id
     (Id : G_Source_Id; Context : G_Main_Context := null) return G_Source;
   --  Find a source given a context and its Id.

   function Get_Context (Source : G_Source) return G_Main_Context;
   --  Gets the context with which the source is associated. Calling this
   --  function on a destroyed source is an error. The returned value is Null
   --  for sources that haven't been attached yet

   ----------------------
   -- Idle and timeout --
   ----------------------

   function Idle_Source_New return G_Source;
   --  Return a newly allocated idle G_Source. Such a source is polled
   --  whenever the main loop is not processing events with a higher priority.
   --  This source must be attached to a main context before it will be
   --  executed.

   function Timeout_Source_New (Interval : Guint) return G_Source;
   --  Return a newly allocated idle G_Source. Such a source is called at
   --  regular intervals. Internval is in milliseconds.

   function Idle_Add (Func : G_Source_Func) return G_Source_Id;
   --  Adds a function to be called whenever there are no higher priority
   --  events pending in the default main loop. This function is given the
   --  priority Priority_Default_Idle. If the function returns False, it is
   --  automatically removed from the list of event sources and will not be
   --  called again.
   --  This function returns the Id of the event source. See Find_Source_By_Id.
   --  This is implemented by using Idle_Source_New internally.

   function Timeout_Add
     (Interval : Guint;
      Func     : G_Source_Func) return G_Source_Id;
   --  Create a new function to be called periodically until it returns False.
   --
   --  Note that timeout functions may be delayed, due to the processing of
   --  other event sources. Thus they should not be relied on for precise
   --  timing. After each call to the timeout function, the time of the next
   --  timeout is recalculated based on the current time and the given interval
   --  (it does not try to 'catch up' time lost in delays).

   generic
      type Data_Type (<>) is private;
   package Generic_Sources is
      type G_Source_Func is access
        function (Data : Data_Type) return Boolean;
      --  If the function returns FALSE it is automatically
      --  removed from the list of event sources and will not be called again.

      type Destroy_Notify is access  procedure (Data : in out Data_Type);
      --  Notify is called just prior to the destruction of Data. It is also
      --  called if the idle or timeout is destroyed through a call to
      --  Remove (Id);

      function Idle_Add
        (Func     : G_Source_Func;
         Data     : Data_Type;
         Priority : G_Priority := Priority_Default_Idle;
         Notify   : Destroy_Notify := null) return G_Source_Id;
      --  Adds a function to be called whenever there are no higher priority
      --  events pending.

      function Timeout_Add
        (Interval : Guint;
         Func     : G_Source_Func;
         Data     : Data_Type;
         Priority : G_Priority := Priority_Default;
         Notify   : Destroy_Notify := null) return G_Source_Id;
      --  Adds a function to be called at regular intervals (in milliseconds).

      procedure Set_Callback
        (Source   : G_Source;
         Func     : G_Source_Func;
         Data     : Data_Type;
         Notify   : Destroy_Notify := null);
      --  Sets the callback function for a source. The callback for a source is
      --  called from the source's dispatch function.
      --
      --  The exact type of func depends on the type of source; ie. you should
      --  not count on func being called with data as its first parameter.
      --
      --  Typically, you won't use this function. Instead use functions
      --  specific to the type of source you are using.

   private
      procedure Free_Data (D : System.Address);
      pragma Convention (C, Free_Data);

      function General_Cb (D : System.Address) return Gint;
      pragma Convention (C, General_Cb);
   end Generic_Sources;

private
   No_Source_Id : constant G_Source_Id := 0;

   type G_Source_Type is new System.Address;
   Null_Source_Type : constant G_Source_Type :=
     G_Source_Type (System.Null_Address);

   pragma Import (C, Main_Context_New,     "g_main_context_new");
   pragma Import (C, Main_Context_Ref,     "g_main_context_ref");
   pragma Import (C, Main_Context_Unref,   "g_main_context_unref");
   pragma Import (C, Main_Context_Default, "g_main_context_default");
   pragma Import (C, Wakeup,               "g_main_context_wakeup");
   pragma Import (C, Release,              "g_main_context_release");
   pragma Import (C, Dispatch,             "g_main_context_dispatch");
   pragma Import (C, Source_Ref,           "g_source_ref");
   pragma Import (C, Source_Unref,         "g_source_unref");
   pragma Import (C, Attach,               "g_source_attach");
   pragma Import (C, Source_Destroy,       "g_source_destroy");
   pragma Import (C, Set_Priority,         "g_source_set_priority");
   pragma Import (C, Get_Priority,         "g_source_get_priority");
   pragma Import (C, Get_Id,               "g_source_get_id");
   pragma Import (C, Get_Context,          "g_source_get_context");
   pragma Import (C, Idle_Source_New,      "g_idle_source_new");
   pragma Import (C, Timeout_Source_New,   "g_timeout_source_new");
   pragma Import (C, Depth,                "g_main_depth");
   pragma Import (C, G_Source_Type_New,    "ada_allocate_g_source_funcs");
   pragma Import (C, Source_New,           "ada_g_source_new");
   pragma Import (C, Get_User_Data,        "ada_g_source_get_user_data");

   --  No binding: g_main_context_find_source_by_user_data
   --  No binding: g_main_context_find_source_by_funcs_user_data
   --  No binding: g_idle_remove_by_data
   --  No binding: g_source_remove_by_funcs_user_data
   --  No binding: g_source_remove_by_user_data
   --  No binding: g_source_get_current_time
   --  No binding: g_source_connect_closure
   --  No binding: g_source_set_callback_indirect
   --  No binding: g_get_current_time

   --  Bounds through ada_g_source_new
   --  No binding: g_source_new

   --  The following functions haven't been examined closely. Most of them
   --  deal with very low-level details, for which Ada generally has better
   --  equivalents anyway.

   --  No binding: g_child_watch_add
   --  No binding: g_child_watch_add_full
   --  No binding: g_child_watch_source_new
   --  No binding: g_main_context_add_poll
   --  No binding: g_main_context_check
   --  No binding: g_main_context_get_poll_func
   --  No binding: g_main_context_iteration
   --  No binding: g_main_context_pending
   --  No binding: g_main_context_prepare
   --  No binding: g_main_context_query
   --  No binding: g_main_context_remove_poll
   --  No binding: g_main_context_set_poll_func
   --  No binding: g_main_context_wait
   --  No binding: g_main_loop_get_context
   --  No binding: g_main_loop_is_running
   --  No binding: g_main_loop_new
   --  No binding: g_main_loop_quit
   --  No binding: g_main_loop_ref
   --  No binding: g_main_loop_run
   --  No binding: g_main_loop_unref
   --  No binding: g_source_add_poll
   --  No binding: g_source_remove_poll

end Glib.Main;
