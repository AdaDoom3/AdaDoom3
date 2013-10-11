--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
with
  Ada.Exceptions,
  Neo,
  Neo.Command,
  --Neo.File.Image,
  --Neo.File.Model,
  Neo.System,
  --Neo.System.Input,
  --Neo.System.Sound,
  --Neo.System.Window,
  Neo.System.Memory,
  --Neo.System.Network,
  Neo.System.Text,
  Neo.System.Text.Console,
  Neo.System.Community,
  Neo.System.Processor;
use
  Ada.Exceptions;
procedure Main
  is
  ---------------
  -- Constants -- The directory constants must be the same as in the project file neo.gpr
  ---------------
    DO_DEBUG           : constant Boolean      := True;
    DO_TEST            : constant Boolean      := True;
    DIRECTORY_GAMES    : constant Neo.String_2 := "../Games";
    DIRECTORY_LOGS     : constant Neo.String_2 := "/Logs";
    DIRECTORY_ASSETS   : constant Neo.String_2 := "/Assets";
    DIRECTORY_SETTINGS : constant Neo.String_2 := "/Settings";
  -----------
  -- Tasks --
  -----------
    task type Task_Loop
      is
        entry Initialize;
        entry Finalize;
      end Task_Loop;
  ---------------
  -- Variables --
  ---------------
    Path_Settings  : Neo.String_2_Unbounded := Neo.NULL_STRING_2_UNBOUNDED;
    Path_Log       : Neo.String_2_Unbounded := Neo.NULL_STRING_2_UNBOUNDED;
    Main_Task_Loop : Task_Loop;
  -----------------
  -- Subprograms --
  -----------------
    procedure Handle_Graphics;
    procedure Handle_Exception(
      Occurrence : in Ada.Exceptions.Exception_Occurrence);
  ---------------
  -- Task_Loop --
  ---------------
    task body Task_Loop
      is
      begin
        ------------------
        accept Initialize;
        ------------------
        if DO_TEST then
          Neo.Command.Test;
          Neo.System.Test;
          Neo.System.Memory.Test;
          Neo.System.Processor.Test;
          Neo.System.Text.Test;
          Neo.System.Text.Console.Test;
        end if;
        ----
        Run:
        ----
          begin
            loop
              select
                ----------------
                accept Finalize;
                ----------------
                exit;
              else null;
                -- Neo.Game.Run;
                -- Timing
                -- Run the render back end, getting the GPU busy with new commands
                -- make sure the game / draw thread has completed
                -- This may block if the game is taking longer than the render back end
                -- Send local usermds to the server.
                -- This happens after the game frame has run so that prediction data is up to date.
                -- Now that we have an updated game frame, we can send out new snapshots to our clients
                -- Render the sound system using the latest commands from the game thread
                -- process the game return for map changes, etc
                -- report timing information
              end select;
            end loop;
          exception
            when Occurrence: others =>
              Handle_Exception(Occurrence);
              --Neo.System.Window.Finalize;
              ----------------
              accept Finalize;
              ----------------
          end Run;
        --Neo.Game.Finalize;
      end Task_Loop;
  ----------------------
  -- Handle_Exception --
  ----------------------
    procedure Handle_Exception(
      Occurrence : in Ada.Exceptions.Exception_Occurrence)
      is
      begin
        Neo.Set_Error(Neo.Get_Error &
          Neo.Localize("Exception:") & Neo.To_String_2(Exception_Name(Occurrence)) & Neo.END_LINE_2 &
          Neo.To_String_2(Exception_Message(Occurrence)) & Neo.END_LINE_2 &(
            if Exception_Name(Occurrence) = "NEO.SYSTEM.CALL_FAILURE" then
              Neo.System.Get_Last_Error & Neo.END_LINE_2
            else
              Neo.NULL_STRING_2));
      end Handle_Exception;
  ---------------------
  -- Handle_Graphics --
  ---------------------
    procedure Handle_Graphics
      is
      begin
        null;
      end Handle_Graphics;
  begin
    ----
    Run:
    ----
      begin
        Neo.Set_Do_Put_Debug(DO_DEBUG);
        Neo.Put_Debug_Line(Neo.Localize("Ready?"));
        --Neo.Game.Initialize;
        Neo.Put_Debug_Line(Neo.Localize("Set"));
        Neo.System.Set_Name("Doom3");
        Neo.System.Set_Icon_Path("../Games/Doom3/Assets/icon.ico");
        Path_Log      := Neo.To_String_2_Unbounded(DIRECTORY_GAMES & "/" & Neo.System.Get_Name & DIRECTORY_LOGS);--     & Neo.System.Community.Get_Name & ".txt");
        Path_Settings := Neo.To_String_2_Unbounded(DIRECTORY_GAMES & "/" & Neo.System.Get_Name & DIRECTORY_SETTINGS);-- & Neo.System.Community.Get_Name & ".csv");
        Neo.Command.Load_Variables(Neo.To_String_2(Path_Settings));
        Neo.Put_Debug_Line(Neo.Localize("Run"));
        Main_Task_Loop.Initialize;
        ---------
        Finalize:
        ---------
          begin
            --Neo.System.Window.Run;
            Main_Task_Loop.Finalize;
          exception
            when Occurrence: others =>
              Handle_Exception(Occurrence);
              Main_Task_Loop.Finalize;
          end Finalize;
        Neo.Command.Save_Variables(Neo.To_String_2(Path_Settings));
        Neo.Put_Debug_Line(Neo.Localize("Goodbye"));
      exception
        when Occurrence: others =>
          Handle_Exception(Occurrence);
      end Run;
    if Neo.Get_Error /= Neo.NULL_STRING_2 then
      Neo.Put_Line(Neo.Get_Error);
      --Neo.Save_Log(Neo.To_String_2(Path_Log));
      if
      not Neo.System.Text.Console.Is_Open and then
      Neo.System.Text.Console.Is_Okay(
        Name    => Neo.System.Get_Name & Neo.Localize(" Error!"),
        Message => Neo.System.Get_Name & Neo.Localize(Neo.Get_Error),
        Buttons => Neo.System.Text.Console.Yes_No_Buttons,
        Icon    => Neo.System.Text.Console.Error_Icon)
      then
        Neo.System.Text.Console.Spawn;
      end if;
    end if;
  end Main;
--        Time_Current   : Duration := 0.0;
--        Time_Last      : Duration := 0.0;
--        Time_Remainder : Duration := 0.0;
--  --      Frames_Total   :
--        Is_Paused      : Boolean := False;
--  --  idFrameData    smpFrameData[NUM_FRAME_DATA];
--  --  idFrameData *  frameData;
--  --  unsigned int  smpFrame;
--        begin
--          ------------------
--          accept Initialize;
--          ------------------
--  --    R_ShutdownFrameData();
--  --    frameData = NULL;
--  --    for ( int i = 0; i < NUM_FRAME_DATA; i++ ) {
--  --      Mem_Free16( smpFrameData[i].frameMemory );
--  --      smpFrameData[i].frameMemory = NULL;
--  --    }
--  --
--  --    for ( int i = 0; i < NUM_FRAME_DATA; i++ ) {
--  --      smpFrameData[i].frameMemory = (byte *) Mem_Alloc16( MAX_FRAME_MEMORY, TAG_RENDER );
--  --    }
--  --
--  --    // must be set before calling R_ToggleSmpFrame()
--  --    frameData = &smpFrameData[ 0 ];
--  --
--  --    // update the highwater mark
--  --    if ( frameData->frameMemoryAllocated.GetValue() > frameData->highWaterAllocated ) {
--  --      frameData->highWaterAllocated = frameData->frameMemoryAllocated.GetValue();
--  --  #if defined( TRACK_FRAME_ALLOCS )
--  --      frameData->highWaterUsed = frameData->frameMemoryUsed.GetValue();
--  --      for ( int i = 0; i < FRAME_ALLOC_MAX; i++ ) {
--  --        frameHighWaterTypeCount[i] = frameAllocTypeCount[i].GetValue();
--  --      }
--  --  #endif
--  --    }
--  --
--  --    // switch to the next frame
--  --    smpFrame++;
--  --    frameData = &smpFrameData[smpFrame % NUM_FRAME_DATA];
--  --
--  --    // reset the memory allocation
--  --    const unsigned int bytesNeededForAlignment = FRAME_ALLOC_ALIGNMENT - ( (unsigned int)frameData->frameMemory & ( FRAME_ALLOC_ALIGNMENT - 1 ) );
--  --    frameData->frameMemoryAllocated.SetValue( bytesNeededForAlignment );
--  --    frameData->frameMemoryUsed.SetValue( 0 );
--  --
--  --  #if defined( TRACK_FRAME_ALLOCS )
--  --    for ( int i = 0; i < FRAME_ALLOC_MAX; i++ ) {
--  --      frameAllocTypeCount[i].SetValue( 0 );
--  --    }
--  --  #endif
--  --
--  --    // clear the command chain and make a RC_NOP command the only thing on the list
--  --    frameData->cmdHead = frameData->cmdTail = (emptyCommand_t *)R_FrameAlloc( sizeof( *frameData->cmdHead ), FRAME_ALLOC_DRAW_COMMAND );
--  --    frameData->cmdHead->commandId = RC_NOP;
--  --    frameData->cmdHead->next = NULL;
--  --  }
--  --  }
--          Neo.Put_Debug_Line(Neo.Localize(START_STAGE_THREE));
--          ----
--          Run:
--          ----
--            begin
--              loop
--                select
--                  ----------------
--                  accept Finalize;
--                  ----------------
--                  exit;
--                else
--                  loop
--                    Time_Current   := Clock;
--                    Time_Last      := Time_Current;
--                    Time_Remainder := Time_Remainder + Time_Scale.Get * ...
--                    if Is_Paused then
--                      Frames_Total   := Frames_Total + 1;
--                      Time_Remainder := 0.0;
--                      exit;
--                    end if;
--                    if Do_Sync_Game_Frame.Get then
--                      Do_Sync_Game_Frame.Set(False);
--                      Frames_Total   := Frames_Total   + 1;
--                      Frames_Current := Frames_Current + 1;
--                      exit;
--                    end if;
--                    loop
--                      Time_Frame     := Duration_Of_Frames(Frames_Total + 1) - Duration_Of_Frames(Frames_Total);
--                      exit when Time_Remainder < Time_Frame;
--                      Time_Remainder := Time_Remainder - Time_Frame;
--                      Frames_Total   := Frames_Total   + 1;
--                      Frames_Current := Frames_Current + 1;
--                    end loop;
--                    exit when Frames_Current > 0;
--                    if not Do_Sleep.Get then
--                      Frames_Current := 1;
--                      Frames_Total   := Frames_Total + Frames_Current;
--                      Time_Remainder := 0;
--                      exit;
--                    end if;
--                  end loop;
--
--                  -- Run the render back end, getting the GPU busy with new commands
--                  -- make sure the game / draw thread has completed
--                  -- This may block if the game is taking longer than the render back end
--                  -- Send local usermds to the server.
--                  -- This happens after the game frame has run so that prediction data is up to date.
--                  -- Now that we have an updated game frame, we can send out new snapshots to our clients
--                  -- Render the sound system using the latest commands from the game thread
--                  -- process the game return for map changes, etc
--                  -- report timing information
--
--  --        if ( !R_IsInitialized() ) {
--  --              return;
--  --        }
--  --
--  --        renderView_t copy = *renderView;
--  --
--  --        // skip front end rendering work, which will result
--  --        // in only gui drawing
--  --        if ( r_skipFrontEnd.GetBool() ) {
--  --              return;
--  --        }
--  --
--  --        SCOPED_PROFILE_EVENT( "RenderWorld::RenderScene" );
--  --
--  --        if ( renderView->fov_x <= 0 || renderView->fov_y <= 0 ) {
--  --              common->Error( "idRenderWorld::RenderScene: bad FOVs: %f, %f", renderView->fov_x, renderView->fov_y );
--  --        }
--  --
--  --        // close any gui drawing
--  --        tr.guiModel->EmitFullScreen();
--  --        tr.guiModel->Clear();
--  --
--  --        int startTime = Sys_Microseconds();
--  --
--  --        // setup view parms for the initial view
--  --        viewDef_t * parms = (viewDef_t *)R_ClearedFrameAlloc( sizeof( *parms ), FRAME_ALLOC_VIEW_DEF );
--  --        parms->renderView = *renderView;
--  --
--  --        if ( tr.takingScreenshot ) {
--  --              parms->renderView.forceUpdate = true;
--  --        }
--  --
--  --        int windowWidth = tr.GetWidth();
--  --        int windowHeight = tr.GetHeight();
--  --        tr.PerformResolutionScaling( windowWidth, windowHeight );
--  --
--  --        // screenFraction is just for quickly testing fill rate limitations
--  --        if ( r_screenFraction.GetInteger() != 100 ) {
--  --              windowWidth = ( windowWidth * r_screenFraction.GetInteger() ) / 100;
--  --              windowHeight = ( windowHeight * r_screenFraction.GetInteger() ) / 100;
--  --        }
--  --        tr.CropRenderSize( windowWidth, windowHeight );
--  --        tr.GetCroppedViewport( &parms->viewport );
--  --
--  --        // the scissor bounds may be shrunk in subviews even if
--  --        // the viewport stays the same
--  --        // this scissor range is local inside the viewport
--  --        parms->scissor.x1 = 0;
--  --        parms->scissor.y1 = 0;
--  --        parms->scissor.x2 = parms->viewport.x2 - parms->viewport.x1;
--  --        parms->scissor.y2 = parms->viewport.y2 - parms->viewport.y1;
--  --
--  --        parms->isSubview = false;
--  --        parms->initialViewAreaOrigin = renderView->vieworg;
--  --        parms->renderWorld = this;
--  --
--  --        // see if the view needs to reverse the culling sense in mirrors
--  --        // or environment cube sides
--  --        idVec3      cross;
--  --        cross = parms->renderView.viewaxis[1].Cross( parms->renderView.viewaxis[2] );
--  --        if ( cross * parms->renderView.viewaxis[0] > 0 ) {
--  --              parms->isMirror = false;
--  --        } else {
--  --              parms->isMirror = true;
--  --        }
--  --
--  --        // save this world for use by some console commands
--  --        tr.primaryWorld = this;
--  --        tr.primaryRenderView = *renderView;
--  --        tr.primaryView = parms;
--  --
--  --        // rendering this view may cause other views to be rendered
--  --        // for mirrors / portals / shadows / environment maps
--  --        // this will also cause any necessary entities and lights to be
--  --        // updated to the demo file
--  --  void R_RenderView( viewDef_t *parms ) {
--  --    // save view in case we are a subview
--  --    viewDef_t * oldView = tr.viewDef;
--  --
--  --    tr.viewDef = parms;
--  --
--  --    // setup the matrix for world space to eye space
--  --    R_SetupViewMatrix( tr.viewDef );
--  --
--  --    // we need to set the projection matrix before doing
--  --    // portal-to-screen scissor calculations
--  --    R_SetupProjectionMatrix( tr.viewDef );
--  --
--  --    // setup render matrices for faster culling
--  --    idRenderMatrix::Transpose( *(idRenderMatrix *)tr.viewDef->projectionMatrix, tr.viewDef->projectionRenderMatrix );
--  --    idRenderMatrix viewRenderMatrix;
--  --    idRenderMatrix::Transpose( *(idRenderMatrix *)tr.viewDef->worldSpace.modelViewMatrix, viewRenderMatrix );
--  --    idRenderMatrix::Multiply( tr.viewDef->projectionRenderMatrix, viewRenderMatrix, tr.viewDef->worldSpace.mvp );
--  --
--  --    // the planes of the view frustum are needed for portal visibility culling
--  --    idRenderMatrix::GetFrustumPlanes( tr.viewDef->frustum, tr.viewDef->worldSpace.mvp, false, true );
--  --
--  --    // the DOOM 3 frustum planes point outside the frustum
--  --    for ( int i = 0; i < 6; i++ ) {
--  --      tr.viewDef->frustum[i] = - tr.viewDef->frustum[i];
--  --    }
--  --    // remove the Z-near to avoid portals from being near clipped
--  --    tr.viewDef->frustum[4][3] -= r_znear.GetFloat();
--  --
--  --    // identify all the visible portal areas, and create view lights and view entities
--  --    // for all the the entityDefs and lightDefs that are in the visible portal areas
--  --    static_cast<idRenderWorldLocal *>(parms->renderWorld)->FindViewLightsAndEntities();
--  --
--  --    // wait for any shadow volume jobs from the previous frame to finish
--  --    tr.frontEndJobList->Wait();
--  --
--  --    // make sure that interactions exist for all light / entity combinations that are visible
--  --    // add any pre-generated light shadows, and calculate the light shader values
--  --    R_AddLights();
--  --
--  --    // adds ambient surfaces and create any necessary interaction surfaces to add to the light lists
--  --    R_AddModels();
--  --
--  --    // build up the GUIs on world surfaces
--  --    R_AddInGameGuis( tr.viewDef->drawSurfs, tr.viewDef->numDrawSurfs );
--  --
--  --    // any viewLight that didn't have visible surfaces can have it's shadows removed
--  --    R_OptimizeViewLightsList();
--  --
--  --    // sort all the ambient surfaces for translucency ordering
--  --    R_SortDrawSurfs( tr.viewDef->drawSurfs, tr.viewDef->numDrawSurfs );
--  --  static void R_SortDrawSurfs( drawSurf_t ** drawSurfs, const int numDrawSurfs ) {
--  --  #if 1
--  --
--  --    uint64 * indices = (uint64 *) _alloca16( numDrawSurfs * sizeof( indices[0] ) );
--  --
--  --    // sort the draw surfs based on:
--  --    // 1. sort value (largest first)
--  --    // 2. depth (smallest first)
--  --    // 3. index (largest first)
--  --    assert( numDrawSurfs <= 0xFFFF );
--  --    for ( int i = 0; i < numDrawSurfs; i++ ) {
--  --      float sort = SS_POST_PROCESS - drawSurfs[i]->sort;
--  --      assert( sort >= 0.0f );
--  --
--  --      uint64 dist = 0;
--  --      if ( drawSurfs[i]->frontEndGeo != NULL ) {
--  --        float min = 0.0f;
--  --        float max = 1.0f;
--  --        idRenderMatrix::DepthBoundsForBounds( min, max, drawSurfs[i]->space->mvp, drawSurfs[i]->frontEndGeo->bounds );
--  --        dist = idMath::Ftoui16( min * 0xFFFF );
--  --      }
--  --
--  --      indices[i] = ( ( numDrawSurfs - i ) & 0xFFFF ) | ( dist << 16 ) | ( (uint64) ( *(uint32 *)&sort ) << 32 );
--  --    }
--  --
--  --    const int64 MAX_LEVELS = 128;
--  --    int64 lo[MAX_LEVELS];
--  --    int64 hi[MAX_LEVELS];
--  --
--  --    // Keep the top of the stack in registers to avoid load-hit-stores.
--  --    register int64 st_lo = 0;
--  --    register int64 st_hi = numDrawSurfs - 1;
--  --    register int64 level = 0;
--  --
--  --    for ( ; ; ) {
--  --      register int64 i = st_lo;
--  --      register int64 j = st_hi;
--  --      if ( j - i >= 4 && level < MAX_LEVELS - 1 ) {
--  --        register uint64 pivot = indices[( i + j ) / 2];
--  --        do {
--  --          while ( indices[i] > pivot ) i++;
--  --          while ( indices[j] < pivot ) j--;
--  --          if ( i > j ) break;
--  --          uint64 h = indices[i]; indices[i] = indices[j]; indices[j] = h;
--  --        } while ( ++i <= --j );
--  --
--  --        // No need for these iterations because we are always sorting unique values.
--  --        //while ( indices[j] == pivot && st_lo < j ) j--;
--  --        //while ( indices[i] == pivot && i < st_hi ) i++;
--  --
--  --        assert( level < MAX_LEVELS - 1 );
--  --        lo[level] = i;
--  --        hi[level] = st_hi;
--  --        st_hi = j;
--  --        level++;
--  --      } else {
--  --        for( ; i < j; j-- ) {
--  --          register int64 m = i;
--  --          for ( int64 k = i + 1; k <= j; k++ ) {
--  --            if ( indices[k] < indices[m] ) {
--  --              m = k;
--  --            }
--  --          }
--  --          uint64 h = indices[m]; indices[m] = indices[j]; indices[j] = h;
--  --        }
--  --        if ( --level < 0 ) {
--  --          break;
--  --        }
--  --        st_lo = lo[level];
--  --        st_hi = hi[level];
--  --      }
--  --    }
--  --
--  --    drawSurf_t ** newDrawSurfs = (drawSurf_t **) indices;
--  --    for ( int i = 0; i < numDrawSurfs; i++ ) {
--  --      newDrawSurfs[i] = drawSurfs[numDrawSurfs - ( indices[i] & 0xFFFF )];
--  --    }
--  --    memcpy( drawSurfs, newDrawSurfs, numDrawSurfs * sizeof( drawSurfs[0] ) );
--  --
--  --
--  --    // generate any subviews (mirrors, cameras, etc) before adding this view
--  --    if ( R_GenerateSubViews( tr.viewDef->drawSurfs, tr.viewDef->numDrawSurfs ) ) {
--  --      // if we are debugging subviews, allow the skipping of the main view draw
--  --      if ( r_subviewOnly.GetBool() ) {
--  --        return;
--  --      }
--  --    }
--  --
--  --    // write everything needed to the demo file
--  --    if ( common->WriteDemo() ) {
--  --      static_cast<idRenderWorldLocal *>(parms->renderWorld)->WriteVisibleDefs( tr.viewDef );
--  --    }
--  --
--  --    // add the rendering commands for this viewDef
--  --    R_AddDrawViewCmd( parms, false );
--  --
--  --    // restore view in case we are a subview
--  --    tr.viewDef = oldView;
--  --        // render any post processing after the view and all its subviews has been draw
--  --        R_RenderPostProcess( parms );
--  --
--  --        // now write delete commands for any modified-but-not-visible entities, and
--  --        // add the renderView command to the demo
--  --        if ( common->WriteDemo() ) {
--  --              WriteRenderView( renderView );
--  --        }
--  --
--  --        tr.UnCrop();
--  --
--  --        int endTime = Sys_Microseconds();
--  --
--  --        tr.pc.frontEndMicroSec += endTime - startTime;
--  --
--  --        // prepare for any 2D drawing after this
--  --        tr.guiModel->Clear();
