with Neo.System.Community; use Neo.System.Community;
package Neo.Game is
    VARIBLE_PREFIX : constant String_2 := "g_";
    package flashlight_batteryDrainTimeMS( "flashlight_batteryDrainTimeMS", "30000", CVAR_INTEGER, "amount of time (in MS) it takes for full battery to drain (-1 == no battery drain)" );
    package flashlight_batteryChargeTimeMS( "flashlight_batteryChargeTimeMS", "3000", CVAR_INTEGER, "amount of time (in MS) it takes to fully recharge battery" );
    package flashlight_minActivatePercent( "flashlight_minActivatePercent", ".25", CVAR_FLOAT, "( 0.0 - 1.0 ) minimum amount of battery (%) needed to turn on flashlight" );
    package flashlight_batteryFlickerPercent( "flashlight_batteryFlickerPercent", ".1", CVAR_FLOAT, "chance of flickering when battery is low" );
    -- No longer userinfo, but I don't want to rename the cvar
    package ui_showGun( "ui_showGun", "1", CVAR_GAME | CVAR_ARCHIVE | CVAR_BOOL, "show gun" );
    -- Client-authoritative stuff
    package clientAuthoritative_debug( "pm_clientAuthoritative_debug", "0", CVAR_BOOL, "" );
    package controllerShake_damageMaxMag( "pm_controllerShake_damageMaxMag", "60.0f", CVAR_FLOAT, "" );
    package controllerShake_damageMaxDur( "pm_controllerShake_damageMaxDur", "60.0f", CVAR_FLOAT, "" );
    package clientAuthoritative_warnDist( "pm_clientAuthoritative_warnDist", "100.0f", CVAR_FLOAT, "" );
    package clientAuthoritative_minDistZ( "pm_clientAuthoritative_minDistZ", "1.0f", CVAR_FLOAT, "" );
    package clientAuthoritative_minDist( "pm_clientAuthoritative_minDist", "-1.0f", CVAR_FLOAT, "" );
    package clientAuthoritative_Lerp( "pm_clientAuthoritative_Lerp", "0.9f", CVAR_FLOAT, "" );
    package clientAuthoritative_Divergence( "pm_clientAuthoritative_Divergence", "200.0f", CVAR_FLOAT, "" );
    package clientInterpolation_Divergence( "pm_clientInterpolation_Divergence", "5000.0f", CVAR_FLOAT, "" );
    package clientAuthoritative_minSpeedSquared( "pm_clientAuthoritative_minSpeedSquared", "1000.0f", CVAR_FLOAT, "" );
    package Gamename                    is new Variable(VARIBLE_PREFIX & "gamename",                    GAME_VERSION,    CVAR_GAME | CVAR_ROM, "" );
    package Gamedate                    is new Variable(VARIBLE_PREFIX & "gamedate",                    __DATE__,        CVAR_GAME | CVAR_ROM, "" );
    package Map                         is new Variable(VARIBLE_PREFIX & "map",                   "default map choice for profile"  "-1",            CVAR_GAME | CVAR_SERVERINFO | CVAR_ARCHIVE | CVAR_INTEGER, );
    package Mode                        is new Variable(VARIBLE_PREFIX & "mode",                  "default mode choice for profile"  "-1",            CVAR_GAME | CVAR_SERVERINFO | CVAR_ARCHIVE | CVAR_INTEGER, , -1, GAME_COUNT - 1 );
    package FragLimit                   is new Variable(VARIBLE_PREFIX & "fragLimit",             "frag limit"   "10",            CVAR_GAME | CVAR_SERVERINFO | CVAR_ARCHIVE | CVAR_INTEGER, , 1, MP_PLAYER_MAXFRAGS );
    package TimeLimit                   is new Variable(VARIBLE_PREFIX & "timeLimit",             "time limit in minutes"   "10",            CVAR_GAME | CVAR_SERVERINFO | CVAR_ARCHIVE | CVAR_INTEGER, , 0, 60 );
    package TeamDamage                  is new Variable(VARIBLE_PREFIX & "teamDamage",            "enable team damage" "0",            CVAR_GAME | CVAR_SERVERINFO | CVAR_ARCHIVE | CVAR_BOOL,  );
    package Spectators                  is new Variable(VARIBLE_PREFIX & "spectators",            "allow spectators or require all clients to play" "1",            CVAR_GAME | CVAR_SERVERINFO | CVAR_ARCHIVE | CVAR_BOOL,  );
    package FlagDropTimeLimit           is new Variable(VARIBLE_PREFIX & "flagDropTimeLimit",     "seconds before a dropped CTF flag is returned"   "30",            CVAR_GAME | CVAR_SERVERINFO | CVAR_ARCHIVE | CVAR_INTEGER,  );
    package Midnight                    is new Variable(VARIBLE_PREFIX & "midnight",              "Start the game up in midnight CTF (completely dark)" "0",            CVAR_GAME | CVAR_INTEGER | CVAR_SERVERINFO,  );
    package Developer                   is new Variable(VARIBLE_PREFIX & "developer",                "" "0",            CVAR_GAME | CVAR_BOOL, );
    package Cinematic                   is new Variable(VARIBLE_PREFIX & "cinematic",                "skips updating entities that aren't marked 'cinematic' '1' during cinematics" "1",            CVAR_GAME | CVAR_BOOL, );
    package CinematicMaxSkipTime        is new Variable(VARIBLE_PREFIX & "cinematicMaxSkipTime",     "# of seconds to allow game to run when skipping cinematic.  prevents lock-up when cinematic doesn't end." "600",            CVAR_GAME | CVAR_FLOAT, , 0, 3600 );
    package MuzzleFlash                 is new Variable(VARIBLE_PREFIX & "muzzleFlash",           "show muzzle flashes"  "1",            CVAR_GAME | CVAR_ARCHIVE | CVAR_BOOL, );
    package ProjectileLights            is new Variable(VARIBLE_PREFIX & "projectileLights",      "show dynamic lights on projectiles"   "1",            CVAR_GAME | CVAR_ARCHIVE | CVAR_BOOL, );
    package BloodEffects                is new Variable(VARIBLE_PREFIX & "bloodEffects",           "show blood splats, sprays and gibs"   "1",            CVAR_GAME | CVAR_ARCHIVE | CVAR_BOOL,);
    package Monsters                    is new Variable(VARIBLE_PREFIX & "monsters",              "show decals such as bullet holes"   "1",            CVAR_GAME | CVAR_BOOL, "" );
    package Decals                      is new Variable(VARIBLE_PREFIX & "decals",                    "1",            CVAR_GAME | CVAR_ARCHIVE | CVAR_BOOL, );
    package Knockback                   is new Variable(VARIBLE_PREFIX & "knockback",                "1000",            CVAR_GAME | CVAR_INTEGER, "" );
    package Skill                       is new Variable(VARIBLE_PREFIX & "skill",                    "1",            CVAR_GAME | CVAR_INTEGER, "" );
    package Nightmare                   is new Variable(VARIBLE_PREFIX & "nightmare",              "if nightmare mode is allowed"   "0",            CVAR_GAME | CVAR_ARCHIVE | CVAR_BOOL, );
    package RoeNightmare                is new Variable(VARIBLE_PREFIX & "roeNightmare",            "if nightmare mode is allowed for roe" "0",            CVAR_GAME | CVAR_ARCHIVE | CVAR_BOOL, );
    package LeNightmare                 is new Variable(VARIBLE_PREFIX & "leNightmare",           "if nightmare mode is allowed for le"  "0",            CVAR_GAME | CVAR_ARCHIVE | CVAR_BOOL, );
    package Gravity                     is new Variable(VARIBLE_PREFIX & "gravity",        DEFAULT_GRAVITY_STRING, CVAR_GAME | CVAR_FLOAT, "" );
    package SkipFX                      is new Variable(VARIBLE_PREFIX & "skipFX",                    "0",            CVAR_GAME | CVAR_BOOL, "" );
    package Disasm                      is new Variable(VARIBLE_PREFIX & "disasm",                  "disassemble script into base/script/disasm.txt on the local drive when script is compiled"   "0",            CVAR_GAME | CVAR_BOOL, );
    package DebugBounds                 is new Variable(VARIBLE_PREFIX & "debugBounds",            "checks for models with bounds > 2048"  "0",            CVAR_GAME | CVAR_BOOL,  );
    package DebugAnim                   is new Variable(VARIBLE_PREFIX & "debugAnim",                "displays information on which animations are playing on the specified entity number.  set to -1 to disable." "-1",            CVAR_GAME | CVAR_INTEGER, );
    package DebugMove                   is new Variable(VARIBLE_PREFIX & "debugMove",                "0",            CVAR_GAME | CVAR_BOOL, "" );
    package DebugDamage                 is new Variable(VARIBLE_PREFIX & "debugDamage",            "0",            CVAR_GAME | CVAR_BOOL, "" );
    package DebugWeapon                 is new Variable(VARIBLE_PREFIX & "debugWeapon",            "0",            CVAR_GAME | CVAR_BOOL, "" );
    package DebugScript                 is new Variable(VARIBLE_PREFIX & "debugScript",            "0",            CVAR_GAME | CVAR_BOOL, "" );
    package DebugMover                  is new Variable(VARIBLE_PREFIX & "debugMover",                "0",            CVAR_GAME | CVAR_BOOL, "" );
    package DebugTriggers               is new Variable(VARIBLE_PREFIX & "debugTriggers",            "0",            CVAR_GAME | CVAR_BOOL, "" );
    package DebugCinematic              is new Variable(VARIBLE_PREFIX & "debugCinematic",            "0",            CVAR_GAME | CVAR_BOOL, "" );
    package StopTime                    is new Variable(VARIBLE_PREFIX & "stopTime",                "0",            CVAR_GAME | CVAR_BOOL, "" );
    package DamageScale                 is new Variable(VARIBLE_PREFIX & "damageScale",           "scale final damage on player by this factor"  "1",            CVAR_GAME | CVAR_FLOAT | CVAR_ARCHIVE, );
    package ArmorProtection             is new Variable(VARIBLE_PREFIX & "armorProtection",       "armor takes this percentage of damage"  "0.3",            CVAR_GAME | CVAR_FLOAT | CVAR_ARCHIVE, );
    package ArmorProtectionMP           is new Variable(VARIBLE_PREFIX & "armorProtectionMP",     "armor takes this percentage of damage in mp"    "0.6",            CVAR_GAME | CVAR_FLOAT | CVAR_ARCHIVE, );
    package UseDynamicProtection        is new Variable(VARIBLE_PREFIX & "useDynamicProtection",   "scale damage and armor dynamically to keep the player alive more often" "1",            CVAR_GAME | CVAR_BOOL | CVAR_ARCHIVE,  );
    package HealthTakeTime              is new Variable(VARIBLE_PREFIX & "healthTakeTime",        "how often to take health in nightmare mode"     "5",            CVAR_GAME | CVAR_INTEGER | CVAR_ARCHIVE, );
    package HealthTakeAmt               is new Variable(VARIBLE_PREFIX & "healthTakeAmt",          "how much health to take in nightmare mode"   "5",            CVAR_GAME | CVAR_INTEGER | CVAR_ARCHIVE, );
    package HealthTakeLimit             is new Variable(VARIBLE_PREFIX & "healthTakeLimit",       "how low can health get taken in nightmare mode"  "25",            CVAR_GAME | CVAR_INTEGER | CVAR_ARCHIVE, );
    package Show_PVS                    is new Variable(VARIBLE_PREFIX & "showPVS",                "0",            CVAR_GAME | CVAR_INTEGER, "", 0, 2 );
    package Show_Targets                is new Variable(VARIBLE_PREFIX & "showTargets",           "draws entities and thier targets.  hidden entities are drawn grey."  "0",            CVAR_GAME | CVAR_BOOL,  );
    package Show_Triggers               is new Variable(VARIBLE_PREFIX & "showTriggers",          "draws trigger entities (orange) and thier targets (green).  disabled triggers are drawn grey."   "0",            CVAR_GAME | CVAR_BOOL, );
    package Show_Collision_World        is new Variable(VARIBLE_PREFIX & "showCollisionWorld",        "0",            CVAR_GAME | CVAR_BOOL, "" );
    package Show_Collision_Models       is new Variable(VARIBLE_PREFIX & "showCollisionModels",    "0",            CVAR_GAME | CVAR_BOOL, "" );
    package Show_Collision_Traces       is new Variable(VARIBLE_PREFIX & "showCollisionTraces",    "0",            CVAR_GAME | CVAR_BOOL, "" );
    package Max_Show_Distance           is new Variable(VARIBLE_PREFIX & "maxShowDistance",        "128",            CVAR_GAME | CVAR_FLOAT, "" );
    package Show_Entity_Info            is new Variable(VARIBLE_PREFIX & "showEntityInfo",            "0",            CVAR_GAME | CVAR_BOOL, "" );
    package Show_view_pos               is new Variable(VARIBLE_PREFIX & "showviewpos",            "0",            CVAR_GAME | CVAR_BOOL, "" );
    package Show_camera_info            is new Variable(VARIBLE_PREFIX & "showcamerainfo",           "displays the current frame # for the camera when playing cinematics"  "0",            CVAR_GAME | CVAR_ARCHIVE, );
    package Show_Test_Model_Frame       is new Variable(VARIBLE_PREFIX & "showTestModelFrame",      "displays the current animation and frame # for testmodels"   "0",            CVAR_GAME | CVAR_BOOL, );
    package Show_Active_Entities        is new Variable(VARIBLE_PREFIX & "showActiveEntities",      "draws boxes around thinking entities.  dormant entities (outside of pvs) are drawn yellow.  non-dormant are green."  "0",            CVAR_GAME | CVAR_BOOL,  );
    package Show_Enemies                is new Variable(VARIBLE_PREFIX & "showEnemies",           "draws boxes around monsters that have targeted the the player" "0",            CVAR_GAME | CVAR_BOOL,  );
    package Frame_time                  is new Variable(VARIBLE_PREFIX & "frametime",             "displays timing information for each game frame"    "0",            CVAR_GAME | CVAR_BOOL, );
    package Time_entities               is new Variable(VARIBLE_PREFIX & "timeEntities",          "when non-zero, shows entities whose think functions exceeded the # of milliseconds specified"   "0",            CVAR_GAME | CVAR_FLOAT, );
    package Debug_Shockwave             is new Variable(VARIBLE_PREFIX & "debugShockwave",        "Debug the shockwave"    "0",            CVAR_GAME | CVAR_BOOL,  );
    package Enable_Slowmo               is new Variable(VARIBLE_PREFIX & "enableSlowmo",            "0",            CVAR_GAME | CVAR_BOOL,  );
    package Slowmo_Step_Rate            is new Variable(VARIBLE_PREFIX & "slowmoStepRate",        "for testing purposes only"    "0.02",            CVAR_GAME | CVAR_FLOAT, "" );
    package EnableP_ortalSky            is new Variable(VARIBLE_PREFIX & "enablePortalSky",       "enables the portal sky"  "1",            CVAR_GAME | CVAR_BOOL,  );
    package Test_FullscreenFX           is new Variable(VARIBLE_PREFIX & "testFullscreenFX",      "index will activate specific fx, -2 is for all on, -1 is off"  "-1",            CVAR_GAME | CVAR_INTEGER,  );
    package Test_HelltimeFX             is new Variable(VARIBLE_PREFIX & "testHelltimeFX",        "set to 0, 1, 2 to test helltime, -1 is off"    "-1",            CVAR_GAME | CVAR_INTEGER,  );
    package Test_MultiplayerFX          is new Variable(VARIBLE_PREFIX & "testMultiplayerFX",     "set to 0, 1, 2 to test multiplayer, -1 is off"    "-1",            CVAR_GAME | CVAR_INTEGER, );
    package Moveable_DamageScale        is new Variable(VARIBLE_PREFIX & "moveableDamageScale",   "scales damage wrt mass of object in multiplayer"  "0.1",            CVAR_GAME | CVAR_FLOAT,  );
    package test_BloomIntensity         is new Variable(VARIBLE_PREFIX & "testBloomIntensity",        "-0.01",        CVAR_GAME | CVAR_FLOAT, "" );
    package test_BloomNumPasses         is new Variable(VARIBLE_PREFIX & "testBloomNumPasses",        "30",            CVAR_GAME | CVAR_INTEGER, "" );
    package Time                     is new Variable(VARIBLE_PREFIX & "dvTime",                    "1",            CVAR_GAME | CVAR_FLOAT, "" );
    package Amplitude                is new Variable(VARIBLE_PREFIX & "dvAmplitude",            "0.001",        CVAR_GAME | CVAR_FLOAT, "" );
    package Frequency                is new Variable(VARIBLE_PREFIX & "dvFrequency",            "0.5",            CVAR_GAME | CVAR_FLOAT, "" );
    package kick_Time                   is new Variable(VARIBLE_PREFIX & "kickTime",                "1",            CVAR_GAME | CVAR_FLOAT, "" );
    package kick_Amplitude              is new Variable(VARIBLE_PREFIX & "kickAmplitude",            "0.0001",        CVAR_GAME | CVAR_FLOAT, "" );
    package blob_Time                   is new Variable(VARIBLE_PREFIX & "blobTime",                "1",            CVAR_GAME | CVAR_FLOAT, "" );
    package blobSize                    is new Variable(VARIBLE_PREFIX & "blobSize",                "1",            CVAR_GAME | CVAR_FLOAT, "" );
    package test_HealthVision           is new Variable(VARIBLE_PREFIX & "testHealthVision",        "0",            CVAR_GAME | CVAR_FLOAT, "" );
    package editEntityMode              is new Variable(VARIBLE_PREFIX & "editEntityMode",            "0",            CVAR_GAME | CVAR_INTEGER,    "s", 0, 7, idCmdSystem::ArgCompletion_Integer<0,7> );
    package drag_Entity                 is new Variable(VARIBLE_PREFIX & "dragEntity",            "allows dragging physics objects around by placing the crosshair over them and holding the fire button"    "0",            CVAR_GAME | CVAR_BOOL,  );
    package drag_Damping                is new Variable(VARIBLE_PREFIX & "dragDamping",            "0.5",            CVAR_GAME | CVAR_FLOAT, "" );
    package drag_ShowSelection          is new Variable(VARIBLE_PREFIX & "dragShowSelection",        "0",            CVAR_GAME | CVAR_BOOL, "" );
    package drop_ItemRotation           is new Variable(VARIBLE_PREFIX & "dropItemRotation",        "",                CVAR_GAME, "" );
    package flag_AttachJoint            is new Variable(VARIBLE_PREFIX & "flagAttachJoint", "player joint to attach CTF flag to" "Chest", CVAR_GAME | CVAR_CHEAT, );
    package flag_AttachOffsetX          is new Variable(VARIBLE_PREFIX & "flagAttachOffsetX","X offset of CTF flag when carried" "8", CVAR_GAME | CVAR_CHEAT,  );
    package flag_AttachOffsetY          is new Variable(VARIBLE_PREFIX & "flagAttachOffsetY", "Y offset of CTF flag when carried" "4", CVAR_GAME | CVAR_CHEAT,  );
    package flag_AttachOffsetZ          is new Variable(VARIBLE_PREFIX & "flagAttachOffsetZ","Z offset of CTF flag when carried"  "-12", CVAR_GAME | CVAR_CHEAT, );
    package flag_AttachAngleX           is new Variable(VARIBLE_PREFIX & "flagAttachAngleX", "X angle of CTF flag when carried"  "90", CVAR_GAME | CVAR_CHEAT, );
    package flag_AttachAngleY           is new Variable(VARIBLE_PREFIX & "flagAttachAngleY", "25", CVAR_GAME | CVAR_CHEAT, "Y angle of CTF flag when carried" );
    package flag_AttachAngleZ           is new Variable(VARIBLE_PREFIX & "flagAttachAngleZ", "-90", CVAR_GAME | CVAR_CHEAT, "Z angle of CTF flag when carried" );
    package vehicle_Velocity            is new Variable(VARIBLE_PREFIX & "vehicleVelocity",        "1000",            CVAR_GAME | CVAR_FLOAT, "" );
    package vehicle_Force               is new Variable(VARIBLE_PREFIX & "vehicleForce",            "50000",        CVAR_GAME | CVAR_FLOAT, "" );
    package vehicle_SuspensionUp        is new Variable(VARIBLE_PREFIX & "vehicleSuspensionUp",    "32",            CVAR_GAME | CVAR_FLOAT, "" );
    package vehicle_SuspensionDown      is new Variable(VARIBLE_PREFIX & "vehicleSuspensionDown",    "20",            CVAR_GAME | CVAR_FLOAT, "" );
    package Vehicle_Suspension_Compress is new Variable(VARIBLE_PREFIX & "vehicleSuspensionKCompress","200",        CVAR_GAME | CVAR_FLOAT, "" );
    package vehicle_Suspension_Damping  is new Variable(VARIBLE_PREFIX & "vehicleSuspensionDamping","400",            CVAR_GAME | CVAR_FLOAT, "" );
    package vehicle_Tire_Friction       is new Variable(VARIBLE_PREFIX & "vehicleTireFriction",    "0.8",            CVAR_GAME | CVAR_FLOAT, "" );
    package vehicle_Debug               is new Variable(VARIBLE_PREFIX & "vehicleDebug",            "0",            CVAR_GAME | CVAR_BOOL, "" );
    package jump_height                 is new Variable(VARIBLE_PREFIX & "jumpheight",         "approximate hieght the player can jump"    "48",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package step_size                   is new Variable(VARIBLE_PREFIX & "stepsize",          "maximum height the player can step up without jumping"      "16",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT,  );
    package crouch_speed                is new Variable(VARIBLE_PREFIX & "crouchspeed",        "speed the player can move while crouched"    "80",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT,  );
    package walk_speed                  is new Variable(VARIBLE_PREFIX & "walkspeed",          "speed the player can move while walking"      "140",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT,  );
    package run_speed                   is new Variable(VARIBLE_PREFIX & "runspeed",           "speed the player can move while running"      "220",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package no_clip_speed               is new Variable(VARIBLE_PREFIX & "noclipspeed",        "speed the player can move while in noclip"     "200",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package spectate_speed              is new Variable(VARIBLE_PREFIX & "spectatespeed",      "speed the player can move while spectating"       "450",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package spectate_bbox               is new Variable(VARIBLE_PREFIX & "spectatebbox",        "size of the spectator bounding box"    "32",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT,  );
    package use_cylinder                is new Variable(VARIBLE_PREFIX & "usecylinder",         "use a cylinder approximation instead of a bounding box for player collision detection"   "0",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_BOOL,  );
    package min_view_pitch              is new Variable(VARIBLE_PREFIX & "minviewpitch",         "amount player's view can look up (negative values are up)"    "-89",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package max_view_pitch              is new Variable(VARIBLE_PREFIX & "maxviewpitch",         "amount player's view can look down"   "89",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT,  );
    package stamina                     is new Variable(VARIBLE_PREFIX & "stamina",              "length of time player can run"  "24",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT,  );
    package stamina_threshold           is new Variable(VARIBLE_PREFIX & "staminathreshold",      "when stamina drops below this value, player gradually slows to a walk"    "45",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT,);
    package stamina_rate                is new Variable(VARIBLE_PREFIX & "staminarate",          "rate that player regains stamina. divide stamina by this value to determine how long it takes to fully recharge."   "0.75",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package crouch_height               is new Variable(VARIBLE_PREFIX & "crouchheight",         "height of player's bounding box while crouched"    "38",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package crouch_view_height          is new Variable(VARIBLE_PREFIX & "crouchviewheight",     "height of player's view while crouched"    "32",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package normal_height               is new Variable(VARIBLE_PREFIX & "normalheight",          "height of player's bounding box while standing"   "74",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package normal_view_height          is new Variable(VARIBLE_PREFIX & "normalviewheight",      "height of player's view while standing"   "68",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package dead_height                 is new Variable(VARIBLE_PREFIX & "deadheight",            "height of player's bounding box while dead"  "20",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT,);
    package dead_view_height            is new Variable(VARIBLE_PREFIX & "deadviewheight",       "height of player's view while dead"  "10",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package crouch_rate                 is new Variable(VARIBLE_PREFIX & "crouchrate",          "time it takes for player's view to change from standing to crouching"  "0.87",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package bounding_width              is new Variable(VARIBLE_PREFIX & "bboxwidth",             "x/y size of player's bounding box"    "32",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package crouch_bob                  is new Variable(VARIBLE_PREFIX & "crouchbob",            "bob much faster when crouched"     "0.5",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package walk_bob                    is new Variable(VARIBLE_PREFIX & "walkbob",               "bob slowly when walking"   "0.3",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT,);
    package run_bob                     is new Variable(VARIBLE_PREFIX & "runbob",               "bob faster when running"  "0.4",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package run_pitch                   is new Variable(VARIBLE_PREFIX & "runpitch",                "0.002",        CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, "" );
    package run_roll                    is new Variable(VARIBLE_PREFIX & "runroll",                "0.005",        CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, "" );
    package bob_up                      is new Variable(VARIBLE_PREFIX & "bobup",                    "0.005",        CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, "" );
    package bob_pitch                   is new Variable(VARIBLE_PREFIX & "bobpitch",                "0.002",        CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, "" );
    package bob_roll                    is new Variable(VARIBLE_PREFIX & "bobroll",                "0.002",        CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, "" );
    package third_Person_Range          is new Variable(VARIBLE_PREFIX & "thirdPersonRange",      "camera distance from player in 3rd person"   "80",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package third_Person_Height         is new Variable(VARIBLE_PREFIX & "thirdPersonHeight",        "0",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, "height of camera from normal view height in 3rd person" );
    package third_Person_Angle          is new Variable(VARIBLE_PREFIX & "thirdPersonAngle",     "direction of camera from player in 3rd person in degrees (0 = behind player, 180 = in front)"    "0",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_FLOAT, );
    package third_Person_Clip           is new Variable(VARIBLE_PREFIX & "thirdPersonClip",       "clip third person view into world space"   "1",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_BOOL,);
    package third_Person                is new Variable(VARIBLE_PREFIX & "thirdPerson",           "enables third person view" "0",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_BOOL,  );
    package third_Person_Death          is new Variable(VARIBLE_PREFIX & "thirdPersonDeath",      "enables third person view when player dies"   "0",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_BOOL, );
    package model_View                  is new Variable(VARIBLE_PREFIX & "modelView",             "draws camera from POV of player model (1 = always, 2 = when dead)"   "0",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_INTEGER, , 0, 2, idCmdSystem::ArgCompletion_Integer<0,2> );
    package air_Msec                    is new Variable(VARIBLE_PREFIX & "air",                   "how long in milliseconds the player can go without air before he starts taking damage"  "30000",        CVAR_GAME | CVAR_NETWORKSYNC | CVAR_INTEGER, );
    package show_Player_Shadow          is new Variable(VARIBLE_PREFIX & "showPlayerShadow",        "0",            CVAR_GAME | CVAR_ARCHIVE | CVAR_BOOL, "enables shadow of player model" );
    package show_Hud                    is new Variable(VARIBLE_PREFIX & "showHud",                "1",            CVAR_GAME | CVAR_ARCHIVE | CVAR_BOOL, "" );
    package show_Projectile_Pct         is new Variable(VARIBLE_PREFIX & "showProjectilePct",     "enables display of player hit percentage"    "0",            CVAR_GAME | CVAR_ARCHIVE | CVAR_BOOL, );
    package show_Brass                  is new Variable(VARIBLE_PREFIX & "showBrass",               "enables ejected shells from weapon" "1",            CVAR_GAME | CVAR_ARCHIVE | CVAR_BOOL,  );
    package gun_x                       is new Variable(VARIBLE_PREFIX & "gunX",                    "3",            CVAR_GAME | CVAR_ARCHIVE | CVAR_FLOAT, "" );
    package gun_y                       is new Variable(VARIBLE_PREFIX & "gunY",                    "0",            CVAR_GAME | CVAR_ARCHIVE | CVAR_FLOAT, "" );
    package gun_z                       is new Variable(VARIBLE_PREFIX & "gunZ",                    "0",            CVAR_GAME | CVAR_ARCHIVE | CVAR_FLOAT, "" );
    package gunScale                    is new Variable(VARIBLE_PREFIX & "gunScale",                "1",            CVAR_GAME | CVAR_ARCHIVE | CVAR_FLOAT, "" );
    package view_Nodal_X                is new Variable(VARIBLE_PREFIX & "viewNodalX",                "3",            CVAR_GAME | CVAR_FLOAT, "" );
    package view_Nodal_Z                is new Variable(VARIBLE_PREFIX & "viewNodalZ",                "6",            CVAR_GAME | CVAR_FLOAT, "" );
    package fov                         is new Variable(VARIBLE_PREFIX & "fov",                    "80",            CVAR_GAME | CVAR_INTEGER | CVAR_NOCHEAT, "" );
    package skipViewEffects             is new Variable(VARIBLE_PREFIX & "skipViewEffects",        "0",            CVAR_GAME | CVAR_BOOL, "skip damage and other view effects" );
    package mp_Weapon_Angle_Scale       is new Variable(VARIBLE_PREFIX & "mpWeaponAngleScale",        "0",            CVAR_GAME | CVAR_FLOAT, "Control the weapon sway in MP" );
    package test_Particle               is new Variable(VARIBLE_PREFIX & "testParticle",         "test particle visualation, set by the particle editor"   "0",            CVAR_GAME | CVAR_INTEGER, );
    package test_Particle_Name          is new Variable(VARIBLE_PREFIX & "testParticleName",      "name of the particle being tested by the particle editor"   "",                CVAR_GAME, );
    package test_Model_Rotate           is new Variable(VARIBLE_PREFIX & "testModelRotate",        "test model rotation speed"  "0",            CVAR_GAME, );
    package test_Post_Process           is new Variable(VARIBLE_PREFIX & "testPostProcess",       "name of material to draw over screen"  "",                CVAR_GAME, );
    package test_Model_Animate          is new Variable(VARIBLE_PREFIX & "testModelAnimate",      "test model animation"  "0",            CVAR_GAME | CVAR_INTEGER, , 0, 4, idCmdSystem::ArgCompletion_Integer<0,4> );
    package test_Model_Blend            is new Variable(VARIBLE_PREFIX & "testModelBlend",         "number of frames to blend"    "0",            CVAR_GAME | CVAR_INTEGER, );
    package test_Death                  is new Variable(VARIBLE_PREFIX & "testDeath",                "0",            CVAR_GAME | CVAR_BOOL, "" );
    package flush_Save                  is new Variable(VARIBLE_PREFIX & "flushSave",             "1 = don't buffer file writing for save games."    "0",            CVAR_GAME | CVAR_BOOL, )
    package count_Down                  is new Variable(VARIBLE_PREFIX & "countDown",             "pregame countdown in seconds"   "15",            CVAR_GAME | CVAR_INTEGER | CVAR_ARCHIVE, , 4, 3600 );
    package game_Review_Pause           is new Variable(VARIBLE_PREFIX & "gameReviewPause",       "scores review time in seconds (at end game)" "10",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_INTEGER | CVAR_ARCHIVE, , 2, 3600 );
    package CTF_Arrows                  is new Variable(VARIBLE_PREFIX & "CTFArrows",               "draw arrows over teammates in CTF"  "1",            CVAR_GAME | CVAR_NETWORKSYNC | CVAR_BOOL, );
    package net_client_Predict_GUI      is new Variable(VARIBLE_PREFIX & "net_clientPredictGUI",    "test guis in networking without prediction"    "1",            CVAR_GAME | CVAR_BOOL,  );
    package grabber_Hold_econds         is new Variable(VARIBLE_PREFIX & "grabberHoldSeconds",      "number of seconds to hold object"    "3",            CVAR_GAME | CVAR_FLOAT | CVAR_CHEAT,);
    package grabber_Enable_Shake        is new Variable(VARIBLE_PREFIX & "grabberEnableShake",    "enable the grabber shake"     "1",            CVAR_GAME | CVAR_BOOL | CVAR_CHEAT, );
    package grabber_RandomM_otion       is new Variable(VARIBLE_PREFIX & "grabberRandomMotion",  "enable random motion on the grabbed object"   "1",            CVAR_GAME | CVAR_BOOL | CVAR_CHEAT, );
    package grabber_HardS_top           is new Variable(VARIBLE_PREFIX & "grabberHardStop",     "hard stops object if too fast"    "1",            CVAR_GAME | CVAR_BOOL | CVAR_CHEAT, );
    package grabber_Damping             is new Variable(VARIBLE_PREFIX & "grabberDamping",     "damping of grabber"       "0.5",            CVAR_GAME | CVAR_FLOAT | CVAR_CHEAT, );
    package xp_bind_run_once            is new Variable(VARIBLE_PREFIX & "xp_bind_run_once", "Rebind all controls once for D3XP."  "0", CVAR_GAME | CVAR_BOOL | CVAR_ARCHIVE, );
    package That_Was_Close     is new Achivement("That was Close!",     "Kill an enemy with 1 health remaining in DOOM 3, RoE, or Lost Mission"),
    package Boomtastic         is new Achivement("Boomtastic",          "Blow up 50 barrels in the DOOM 3, RoE, or Lost Mission campaigns"),
    package Rage               is new Achivement("RAGE",                "Find the RAGE logo in the Lost Mission"),
    package Bot_Buddy          is new Achivement("Bot Buddy",           "Keep a Sentry Bot alive to its destination in DOOM 3, RoE, or Lost Mission (except Mars City)"),
    package Turncoat           is new Achivement("Turncoat",            "Get 2 demons to fight each other in DOOM 3, RoE, or Lost Mission"),
    package Gimme_Time         is new Achivement("Gimme Time!",         "Defeat the Helltime Hunter in RoE"),
    package Lost_Recruit       is new Achivement("Lost Recruit",        "Complete the Lost Mission campaign on Recruit"),
    package Lost_Marine        is new Achivement("Lost Marine",         "Complete the Lost Mission campaign on Marine"),
    package Lost_Veteran       is new Achivement("Lost Veteran",        "Complete the Lost Mission campaign on Veteran"),
    package Clean_Sheet        is new Achivement("Clean Sheet",         "Complete a DOOM 3 Multiplayer match without dying"),
    package Neophyte           is new Achivement("Neophyte",            "Complete any level in Ultimate DOOM in singleplayer"),
    package Doomed_Recruit     is new Achivement("DOOMed Recruit",      "Complete the DOOM 3 single player campaign on Recruit"),
    package Doomed_Marine      is new Achivement("DOOMed Marine",       "Complete the DOOM 3 single player campaign on Marine"),
    package Doomed_Veteran     is new Achivement("DOOMed Veteran",      "Complete the DOOM 3 single player campaign on Veteran"),
    package Doomed_Nightmare   is new Achivement("DOOMed Nightmare",    "Complete the DOOM 3 single player campaign on Nightmare"),
    package Doomed_Collector   is new Achivement("DOOMed Collector",    "Collect every PDA in DOOM 3" ),
    package I_Like_To_Watch    is new Achivement("I Like to Watch",     "Find all video logs in DOOM 3"),
    package Goody_Finder       is new Achivement("Goody Finder",        "Open all storage lockers in DOOM 3"),
    package Unarmed_Badass     is new Achivement("Unarmed Badass",      "Kill 20 enemies with the fists/melee hands in DOOM 3"),
    package To_Be_Or_Not_To_Be is new Achivement("To Be or Not to Be",  "Kill the scientist trapped next to the Reactor Control Room in DOOM 3"),
    package Double_The_Fun     is new Achivement("Double the Fun!",     "Kill 2 Imps with one shotgun blast in DOOM 3, RoE, or Lost Mission"),
    package Killing_Time       is new Achivement("Killing time",        "Score 25000 on Super Turbo Turkey Puncher 3 in DOOM 3 or RoE"),
    package Ready_For_Action   is new Achivement("Ready for Action!",   "Get the BFG-9000 from Security Chief's office in DOOM 3"),
    package Not_A_Scratch      is new Achivement("Not a Scratch",       "Complete a level without taking any damage in DOOM 3, RoE, or Lost Mission (except Mars City)"),
    package Speed_Run          is new Achivement("Speed Run",           "Complete the DOOM 3 single player campaign in 10 hours or less"),
    package Sticky_Situation   is new Achivement("Sticky Situation",    "Defeat the Vagary boss in DOOM 3"),
    package Cookie_Stealer     is new Achivement("Cookie Stealer",      "Defeat Guardian boss in DOOM 3"),
    package Youre_Not_My_Boss  is new Achivement("You're Not My Boss!", "Defeat Sabaoth boss in DOOM 3"),
    package Big_Boy            is new Achivement("Big Boy",             "Defeat Cyberdemon boss in DOOM 3"),
    package Ripped             is new Achivement("Ripped!",             "Use the chainsaw to kill 20 enemies in DOOM 3"),
    package All_Of_Us          is new Achivement("All of Us",           "Find the id logo secret room in DOOM 3"),
    package You_Laugh_It_Works is new Achivement("You Laugh, It Works", "Find the bloody handiwork of Betruger (in Delta 4 Hallway) in DOOM 3"),
    package Soulfood           is new Achivement("Soulfood",            "Use the Soul Cube to defeat 20 enemies in DOOM 3"),
    package Evil_Recruit       is new Achivement("Evil Recruit",        "Complete the RoE campaign on Recruit"),
    package Evil_Marine        is new Achivement("Evil Marine",         "Complete the RoE campaign on Marine"),
    package Evil_Veteran       is new Achivement("Evil Veteran",        "Complete the RoE campaign on Veteran"),
    package Evil_Nightmare     is new Achivement("Evil Nightmare",      "Complete the RoE campaign on Nightmare"),
    package Evil_Collector     is new Achivement("Evil Collector",      "Collect every PDA in the RoE campaign"),
    package Too_Slow_Fool      is new Achivement("Too Slow, Fool!",     "Kill 5 enemies at once while in Hell Time in RoE"),
    package Gimme_Power        is new Achivement("Gimme Power!",        "Defeat the Berserk Hunter in RoE"),
    package Shocking           is new Achivement("Shocking!",           "Defeat the Invulnerability Hunter in RoE"),
    package Eat_This           is new Achivement("Eat This!",           "Defeat the Maledict boss in RoE"),
    package Play_Catch         is new Achivement("Play Catch",          "Kill 20 enemies with projectiles launched from the Grabber in RoE"),
    package Fists_Of_Fury      is new Achivement("Fists of Fury",       "Use the Artifact with Berserk ability to punch out 20 enemies in RoE"),
    package Lost_Nightmare     is new Achivement("Lost Nightmare",      "Complete the Lost Mission campaign on Nightmare"),
    package Lost_Collector     is new Achivement("Lost Collector",      "Collect every PDA in the Lost Mission campaign"),
    package Telefragged        is new Achivement("Telefragged!",        "Kill an enemy player by jumping into a teleporter after them in DOOM 3 Multiplayer"),
    package Crushed            is new Achivement("Crushed!",            "Catch an enemy player in the Reactor of Frag Chamber in DOOM 3 Multiplayer"),
    package Ninja_Killer       is new Achivement("Ninja Killer",        "Kill 5 enemy players while using Invisibility in DOOM 3 Multiplayer"),
    package Berserked          is new Achivement("Berserked!",          "Use Berserk to kill a player in DOOM 3 Multiplayer");
  end Neo.Game;