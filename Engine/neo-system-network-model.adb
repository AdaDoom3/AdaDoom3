package body Neo.System.Network.Model is
  function Adjust_Is_Client(Prevous, Current : in Boolean) return Boolean is
    begin
      if Previous /= Current then
        if not Current then Client_Task.Finalize; else 
          cl_initialized = qtrue;
          -- init localization subsystem
          L10n_Init();
          Steam_Init();
          VID_Init();
          CL_ClearState();
          -- IPv4
          NET_InitAddress( &address, NA_IP );
          NET_SetAddressPort( &address, cl_port->integer );
          NET_OpenSocket( &cls.socket_udp, SOCKET_UDP, &address, qfalse ) )
          -- IPv6
          NET_InitAddress( &address, NA_IP6 );
          NET_SetAddressPort( &address, cl_port6->integer );
          NET_OpenSocket( &cls.socket_udp6, SOCKET_UDP, &address, qfalse ) )
          -- check for update
          CL_CheckForUpdate();
          CL_InitServerList();
          CL_MM_Init();
          ML_Init();
          CL_Mumble_Init();
          Client_Task.Initialize;
        end if;
      end if;
    end Adjust_Is_Client;
  function Adjust_Is_Server(Prevous, Current : in Boolean) return Boolean is
    begin
      if Previous /= Current then
        if not Current then Server_Task.Finalize;
        else Server_Task.Initialize; end if;
      end if;
    end Adjust_Is_Server;
  procedure Run_Server is
    begin
    end Run_Server;
  procedure Run_Client is
    static int allRealMsec = 0, allGameMsec = 0, extraMsec = 0;
    static float roundingMsec = 0.0f;
    int minMsec;
    float maxFps;
    begin
      if Is_Client.Get then
      end if;
      cls.realtime += realmsec;
      if( cls.demo.playing && cls.demo.play_ignore_next_frametime )
      {
        gamemsec = 0;
        cls.demo.play_ignore_next_frametime = qfalse;
      }
      if( cl_demoavi_fps->modified )
      {
        float newvalue = 1000.0f / (int)( 1000.0f/cl_demoavi_fps->value );
        if( fabs( newvalue - cl_demoavi_fps->value ) > 0.001 )
          Com_Printf( "cl_demoavi_fps value has been adjusted to %.4f\n", newvalue );

        Cvar_SetValue( "cl_demoavi_fps", newvalue );
        cl_demoavi_fps->modified = qfalse;
      }
      -- demoavi
      if( ( cls.demo.avi || cls.demo.pending_avi ) && cls.state == CA_ACTIVE )
      {
        if( cls.demo.pending_avi && !cls.demo.avi )
        {
          cls.demo.pending_avi = qfalse;
          CL_BeginDemoAviDump();
        }
        -- fixed time for next frame
        if( cls.demo.avi_video )
        {
          gamemsec = ( 1000.0 / (double)cl_demoavi_fps->integer ) * Cvar_Value( "timescale" );
          if( gamemsec < 1 )
            gamemsec = 1;
        }
      }
      if( cls.demo.playing && cls.demo.paused )
        gamemsec = 0;
      cls.gametime += gamemsec;
      allRealMsec += realmsec;
      allGameMsec += gamemsec;
      CL_UpdateSnapshot();
      CL_AdjustServerTime( gamemsec );
      CL_NetFrame( realmsec, gamemsec );
      CL_MM_Frame();
      L10n_CheckUserLanguage();
      if( cls.state == CA_CINEMATIC )
      {
        maxFps = SCR_CinematicFramerate() * 2;
        if( maxFps < 24 ) 
          maxFps = 24.0f;
        minMsec = max( ( 1000.0f / maxFps ), 1 );
        roundingMsec += max( ( 1000.0f / maxFps ), 1.0f ) - minMsec;
      }
      else if( cl_maxfps->integer > 0 && !cl_timedemo->integer 
        && !( cls.demo.avi_video && cls.state == CA_ACTIVE ) )
      {
        -- do not allow setting cl_maxfps to very low values to prevent cheating
        if( cl_maxfps->integer < 24 )
          Cvar_ForceSet( "cl_maxfps", "24" );
        maxFps = cl_maxfps->value;
        minMsec = max( ( 1000.0f / maxFps ), 1 );
        roundingMsec += max( ( 1000.0f / maxFps ), 1.0f ) - minMsec;
      }
      else
      {
        maxFps = 10000.0f;
        minMsec = 1;
        roundingMsec = 0;
      }
      if( roundingMsec >= 1.0f )
      {
        minMsec += (int)roundingMsec;
        roundingMsec -= (int)roundingMsec;
      }
      if( allRealMsec + extraMsec < minMsec )
      {
    #ifdef PUTCPU2SLEEP
        -- let CPU sleep while playing fullscreen video or when explicitly told to do so by user
        if( ( cl_sleep->integer || cls.state == CA_CINEMATIC || cls.state == CA_DISCONNECTED ) && minMsec - extraMsec > 1 )
          Sys_Sleep( 1 );
    #endif
        return;
      }
      cls.frametime = (float)allGameMsec * 0.001f;
      cls.realframetime = (float)allRealMsec * 0.001f;
    #if 1
      if( allRealMsec < minMsec ) -- is compensating for a too slow frame
      {
        extraMsec -= ( minMsec - allRealMsec );
        clamp( extraMsec, 0, 100 );
      }
      else -- too slow, or exact frame
      {
        extraMsec = allRealMsec - minMsec;
        clamp( extraMsec, 0, 100 );
      }
    #else
      extraMsec = allRealMsec - minMsec;
      clamp( extraMsec, 0, minMsec );
    #endif
      CL_TimedemoStats();
      -- allow rendering DLL change
      cl.inputRefreshed = qfalse;
      if( cls.state != CA_ACTIVE )
        CL_UpdateCommandInput();
      CL_NewUserCommand( allRealMsec );
      -- update the screen
      if( host_speeds->integer )
        time_before_ref = Sys_Milliseconds();
      SCR_UpdateModels();
      if( host_speeds->integer )
        time_after_ref = Sys_Milliseconds();
      -- 0.50 doesn't refresh input from cgame if prediction is disabled.
      CL_UpdateCommandInput();
      if( CL_WriteAvi() )
      {
        int frame = ++cls.demo.avi_frame;
        if( cls.demo.avi_video )
          re.WriteAviFrame( frame, cl_demoavi_scissor->integer );
      }
      -- advance local effects for next frame
      SCR_RunCinematic();
      SCR_RunConsole( allRealMsec );
      allRealMsec = 0;
      allGameMsec = 0;
      cls.framecount++;
    end Run;
end Neo.System.Network.Model;