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
package Neo.System.Input
  is
  ----------------
  -- Task_Input --
  ----------------
    task body Task_Input
      is
      static int prevTime = 0;
      static uint64 nextCheck[MAX_JOYSTICKS] = { 0 };
      begin
        accept Initialize;
        -- setup the timer that the high frequency thread will wait on
        -- to fire every 4 msec
        timer = CreateWaitableTimer( NULL, FALSE, "JoypadTimer" );
        LARGE_INTEGER dueTime;
        dueTime.QuadPart = -1;
        if ( !SetWaitableTimer( timer, &dueTime, 4, NULL, NULL, FALSE ) ) {
          idLib::FatalError( "SetWaitableTimer for joystick failed" );
        end if;
        -- spawn the high frequency joystick reading thread
        Sys_CreateThread( (xthread_t)JoystickSamplingThread, NULL, THREAD_HIGHEST, "Joystick", CORE_1A );
        loop -- Try to see close to 4000 micro seconds each interation
          select 
            accept Finalize
              do
                exit;
              end Finalize;
          or
            accept Disable
              do
                accept Enable;
              end Disable;
          else
            int now = Sys_Microseconds();
            prevTime = now;
            XINPUT_STATE  joyData[MAX_JOYSTICKS];
            bool      validData[MAX_JOYSTICKS];
            for I in 1..MAXIMUM_NUMBER_OF_GAMEPADS loop
              if now >= nextCheck[i] then
                -- XInputGetState might block... for a _really_ long time..
                validData[i] = XInputGetState( i, &joyData[i] ) == ERROR_SUCCESS;
                -- allow an immediate data poll if the input device is connected else 
                -- wait for some time to see if another device was reconnected.
                -- Checking input state infrequently for newly connected devices prevents 
                -- severe slowdowns on PC, especially on WinXP64.
                if validData[i] then
                  nextCheck[i] = 0;
                else
                  nextCheck[i] = now + waitTime;
                end if;
              end if;
            end loop;
            -- do this short amount of processing inside a critical section
            idScopedCriticalSection cs( win32.g_Joystick.mutexXis );
            for ( int i = 0 ; i < MAX_JOYSTICKS ; i++ ) {
              controllerState_t * cs = &win32.g_Joystick.controllers[i];
              if ( !validData[i] ) {
                cs->valid = false;
                continue;
              }
              cs->valid = true;
              XINPUT_STATE& current = joyData[i];
              cs->current = current;
              -- Switch from using cs->current to current to reduce chance of Load-Hit-Store on consoles
              threadPacket[threadCount&255] = current.dwPacketNumber;
              cs->buttonBits |= current.Gamepad.wButtons;
            }
          end loop;
          -- we want this to be processed at least 250 times a second
          WaitForSingleObject( win32.g_Joystick.timer, INFINITE );
          end select;
        end loop;
      end Task_Input;
  ----------
  -- Test --
  ----------
    procedure Test
      is
      begin
      end Test;
  ----------------
  -- Initialize --
  ----------------
    procedure Initalize
      is
      begin
      end Initialize;
  --------------
  -- Finalize --
  --------------
    procedure Finalize
      is
      begin
      end Finalize;
  -------------
  -- Disable --
  -------------
    procedure Disable
      is
      begin
      end Disable;
  ------------
  -- Enable --
  ------------
    procedure Enable
      is
      begin
      end Enable;
  ---------------------
  -- Get_Peripherals --
  ---------------------
    function Get_Peripherals
      return Array_Record_Peripheral
      is
      begin
      end Get_Peripherals;
  ----------------------
  -- Update_Vibration --
  ----------------------
    procedure Update_Vibration(
      Player  : in Integer_4_Positive;
      Percent : in Float_Percent)
      is
      begin
      end Update_Vibration;
  ----------------
  -- Set_Player --
  ----------------
    procedure Set_Player(
      Identifier : in Integer_4_Positive;
      Player     : in Integer_4_Positive)
      is
      begin
      end Set_Player;
  ----------------------
  -- Set_Handle_Stick --
  ----------------------
    procedure Set_Handle_Stick(
      Handler : in Access_Procedure_Handle_Axis)
      is
      begin
      end Set_Handle_Stick;
  ----------------------
  -- Set_Handle_Mouse --
  ----------------------
    procedure Set_Handle_Mouse(
      Handler : in Access_Procedure_Handle_Movement)
      is
      begin
      end Set_Handle_Mouse;
  ------------------------
  -- Set_Handle_Trigger --
  ------------------------
    procedure Set_Handle_Trigger(
      Handler : in Access_Procedure_Handle_Pedal)
      is
      begin
      end Set_Handle_Trigger;
  --------------------------
  -- Set_Handle_Character --
  --------------------------
    procedure Set_Handle_Character(
      Handler : in Access_Procedure_Handle_Character)
      is
      begin
      end Set_Handle_Character;
  --------------------
  -- Set_Handle_Key --
  --------------------
    procedure Set_Handle_Key(
      Handler : in Access_Procedure_Handle_Key)
      is
      begin
      end Set_Handle_Key;
  ---------------------------
  -- Set_Handle_Peripheral --
  ---------------------------
    procedure Set_Handle_Peripheral(
      Handler : in Access_Procedure_Handle_Peripheral)
      is
      begin
      end Set_Handle_P
  end Neo.System.Input;
