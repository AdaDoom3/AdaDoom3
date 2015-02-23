generic
  type Record_Message is record;
  procedure Run_Client;
  procedure Run_Server;
package Neo.System.Network.Model is
  VARIABLE_PREFIX : constant String_2 := "w_";
private
  function Adjust_Is_Server(Prevous, Current : in Boolean) return Boolean;
  function Adjust_Is_Client(Prevous, Current : in Boolean) return Boolean
  package Is_Server is new Variable(VARIABLE_PREFIX & "minimized",  "Query if minimized", Boolean, False, False, False);
  package Is_Client is new Variable(VARIABLE_PREFIX & "minimized",  "Query if minimized", Boolean, False, False, False);
  package Task_Client is new Tasks(Run_Client); 
  package Task_Server is new Tasks(Run_Server); 
  Client_Task : Task_Client.Protected_Task;
  Server_Task : Task_Server.Protected_Task;
end Neo.System.Network.Model;

-- typedef enum { RD_NONE, RD_PACKET } redirect_t;

-- // destination class for SV_multicast
-- typedef enum
-- {
--   MULTICAST_ALL,
--   MULTICAST_PHS,
--   MULTICAST_PVS
-- } multicast_t;

-- #define SV_OUTPUTBUF_LENGTH ( MAX_MSGLEN - 16 )

-- extern char sv_outputbuf[SV_OUTPUTBUF_LENGTH];

-- typedef struct
-- {
--   const socket_t *socket;
--   const netadr_t *address;
-- } flush_params_t;
-- package Neo.System.Network.Model is 
--   type Enumerated_State is (Uninitialized_State, Disconnected_State, Connecting_State, Awaiting_Data_State, Active_State);
--   type Enumerated_Download is (No_Download, Model_Download, Sound_Download, Skin_Download, Single_Download);
--   type Record_Frame is record
--       Running_Time : Duration;
--       Is_Invalid   : Boolean           := False;
--       Frame_Server : Integer_4_Natural := 0;
--       Frame_Delta  : Integer_4_Natural := 0;
--       byte areabits[MAX_MAP_AREAS/8] portalarea visibility bits
--       State        : Record_State;
--       Number_Of_Entities : Integer_4_Natural := 0;
--       Parse_Entities_Index : Integer_4_Natural := 0;
--     end record;
--   type Record_Entity is record
--       Current          : Record_Entity_State
--       Previous         : Record_Entity_State
--       Frame_Server     : Integer_4_Natural
--       Number_Of_Trails : Integer_4_Natural
--       Lerp_origin      : vec3_t
--       Fly_Stop_Time    : Integer_4_Natural
--     end record;
--   type Record_Client_Information is record
--       Name         : String_2_Unbounded;
--       Information  : String_2_Unbounded;
--       Skin         : String_2_Unbounded;
--       Icon         : String_2_Unbounded;
--       Model        : String_2_Unbounded;
--       Weapon_Model : String_2_Unbounded;
--     end record;
--   type Record_Client_State is record
--       Number_Of_Timeouts
--       Time_Demo_Frames
--       Time_Demo_Start
--       Is_Refresh_Prepped
--       Is_Sound_Prepped
--       force_refdef; vid has changed, so we can't use a paused refdef
--       Parse_Entities_Index
--       Command : Record_User_Command
--       Old_Commands : Array_Record_User_Command
--       Predicted_Origins : Array_Short
--       Predicted_Step : Float_4_Real
--       Predicted_Step_Time : Integer_4_Unsigned
--       Predicted_Origin : Vec3_t
--       Predicted_Angles : Vec3_t
--       Prediction_Error : Vec3_t
--       Frame : Record_Frame
--       Number_Of_Supressed_Messages : Integer_4_Natural
--       Frames : Array_Record_Frame
--       View_Angles : vec3_t
--       Render_Time : Duration
--       Lerp_Frac : Float
--       Forward : vec3_t
--       Right : vec3_t
--       Up : vec3_t
--       Layout : String_2(1..1024)
--       Inventory : Array_Integer(MAX_ITEMS)
--       FILE    *cinematic_file;
--       int     cinematictime;    -- cls.realtime for first cinematic frame
--       int     cinematicframe;
--       char    cinematicpalette[768];
--       qboolean  cinematicpalette_active;
--       --
--       -- server state information
--       --
--       qboolean  attractloop;    -- running the attract loop, any key will menu
--       int     servercount;  -- server identification for prespawns
--       char    gamedir[MAX_QPATH];
--       int     playernum;
--       char    configstrings[MAX_CONFIGSTRINGS][MAX_QPATH];
--       --
--       -- locally derived information from server state
--       --
--       struct model_s  *model_draw[MAX_MODELS];
--       struct cmodel_s *model_clip[MAX_MODELS];
--       struct sfx_s  *sound_precache[MAX_SOUNDS];
--       struct image_s  *image_precache[MAX_IMAGES];
--       clientinfo_t  clientinfo[MAX_CLIENTS];
--       clientinfo_t  baseclientinfo;
--     end record;

-- #define MAX_MASTERS 8               // max recipients for heartbeat packets
-- #define HEARTBEAT_SECONDS   300

-- typedef enum
-- {
--   ss_dead,        // no map loaded
--   ss_loading,     // spawning level edicts
--   ss_game         // actively running
-- } server_state_t;

-- // some commands are only valid before the server has finished
-- // initializing (precache commands, static sounds / objects, etc)

-- typedef struct ginfo_s
-- {
--   struct edict_s *edicts;
--   struct client_s *clients;

--   int edict_size;
--   int num_edicts;         // current number, <= max_edicts
--   int max_edicts;
--   int max_clients;    // <= sv_maxclients, <= max_edicts
-- } ginfo_t;

-- #define MAX_FRAME_SOUNDS 256
-- typedef struct
-- {
--   server_state_t state;       // precache commands are only valid during load

--   unsigned nextSnapTime;              // always sv.framenum * svc.snapFrameTime msec
--   unsigned framenum;

--   char mapname[MAX_QPATH];               // map name

--   char configstrings[MAX_CONFIGSTRINGS][MAX_CONFIGSTRING_CHARS];
--   entity_state_t baselines[MAX_EDICTS];
--   int num_mv_clients;     // current number, <= sv_maxmvclients

--   //
--   // global variables shared between game and server
--   //
--   ginfo_t gi;
-- } server_t;

-- struct gclient_s
-- {
--   player_state_t ps;  // communicated by server to clients
--   client_shared_t r;
-- };

-- struct edict_s
-- {
--   entity_state_t s;
--   entity_shared_t r;
-- };

-- #define EDICT_NUM( n ) ( (edict_t *)( (qbyte *)sv.gi.edicts + sv.gi.edict_size*( n ) ) )
-- #define NUM_FOR_EDICT( e ) ( ( (qbyte *)( e )-(qbyte *)sv.gi.edicts ) / sv.gi.edict_size )

-- typedef struct
-- {
--   qboolean allentities;
--   qboolean multipov;
--   qboolean relay;
--   int clientarea;
--   int numareas;
--   int areabytes;
--   qbyte *areabits;          // portalarea visibility bits
--   int numplayers;
--   int ps_size;
--   player_state_t *ps;                 // [numplayers]
--   int num_entities;
--   int first_entity;                   // into the circular sv.client_entities[]
--   unsigned int sentTimeStamp;         // time at what this frame snap was sent to the clients
--   unsigned int UcmdExecuted;
--   game_state_t gameState;
-- } client_snapshot_t;

-- typedef struct
-- {
--   char *name;
--   qbyte *data;            // file being downloaded
--   int size;               // total bytes (can't use EOF because of paks)
--   unsigned int timeout;   // so we can free the file being downloaded
--                           // if client omits sending success or failure message
-- } client_download_t;

-- typedef struct
-- {
--   unsigned int framenum;
--   char command[MAX_STRING_CHARS];
-- } game_command_t;

-- #define LATENCY_COUNTS  16
-- #define RATE_MESSAGES 25  // wsw : jal : was 10: I think it must fit sv_pps, I have to calculate it

-- typedef struct client_s
-- {
--   sv_client_state_t state;

--   char userinfo[MAX_INFO_STRING];             // name, etc

--   qboolean reliable;                  // no need for acks, connection is reliable
--   qboolean mv;                        // send multiview data to the client
--   qboolean individual_socket;         // client has it's own socket that has to be checked separately

--   socket_t socket;

--   char reliableCommands[MAX_RELIABLE_COMMANDS][MAX_STRING_CHARS];
--   unsigned int reliableSequence;      // last added reliable message, not necesarily sent or acknowledged yet
--   unsigned int reliableAcknowledge;   // last acknowledged reliable message
--   unsigned int reliableSent;          // last sent reliable message, not necesarily acknowledged yet

--   game_command_t gameCommands[MAX_RELIABLE_COMMANDS];
--   int gameCommandCurrent;             // position in the gameCommands table

--   unsigned int clientCommandExecuted; // last client-command we received

--   unsigned int UcmdTime;
--   unsigned int UcmdExecuted;          // last client-command we executed
--   unsigned int UcmdReceived;          // last client-command we received
--   usercmd_t ucmds[CMD_BACKUP];        // each message will send several old cmds

--   unsigned int lastPacketSentTime;    // time when we sent the last message to this client
--   unsigned int lastPacketReceivedTime; // time when we received the last message from this client
--   unsigned lastconnect;

--   int lastframe;                  // used for delta compression etc.
--   qboolean nodelta;               // send one non delta compressed frame trough
--   int nodelta_frame;              // when we get confirmation of this frame, the non-delta frame is trough
--   unsigned int lastSentFrameNum;  // for knowing which was last frame we sent

--   int frame_latency[LATENCY_COUNTS];
--   int ping;
-- #ifndef RATEKILLED
--   //int       message_size[RATE_MESSAGES];  // used to rate drop packets
--   int rate;
--   int suppressCount;              // number of messages rate suppressed
-- #endif
--   edict_t *edict;                     // EDICT_NUM(clientnum+1)
--   char name[MAX_INFO_VALUE];          // extracted from userinfo, high bits masked

--   client_snapshot_t snapShots[UPDATE_BACKUP]; // updates can be delta'd from here

--   client_download_t download;

--   int challenge;                  // challenge of this user, randomly generated

--   netchan_t netchan;

--   qboolean tvclient;

--   int mm_session;
--   unsigned int mm_ticket;
--   char mm_login[MAX_INFO_VALUE];
-- } client_t;

-- // a client can leave the server in one of four ways:
-- // dropping properly by quiting or disconnecting
-- // timing out if no valid messages are received for timeout.value seconds
-- // getting kicked off by the server operator
-- // a program error, like an overflowed reliable buffer

-- //=============================================================================

-- // MAX_CHALLENGES is made large to prevent a denial
-- // of service attack that could cycle all of them
-- // out before legitimate users connected
-- #define MAX_CHALLENGES  1024

-- // MAX_SNAP_ENTITIES is the guess of what we consider maximum amount of entities
-- // to be sent to a client into a snap. It's used for finding size of the backup storage
-- #define MAX_SNAP_ENTITIES 64

-- typedef struct
-- {
--   netadr_t adr;
--   int challenge;
--   int time;
-- } challenge_t;

-- // for server side demo recording
-- typedef struct
-- {
--   int file;
--   char *filename;
--   char *tempname;
--   time_t localtime;
--   unsigned int basetime, duration;
--   client_t client;                // special client for writing the messages
--   char meta_data[SNAP_MAX_DEMO_META_DATA_SIZE];
--   size_t meta_data_realsize;
-- } server_static_demo_t;

-- typedef server_static_demo_t demorec_t;

-- #ifdef TCP_SUPPORT
-- #define MAX_INCOMING_CONNECTIONS 256
-- typedef struct
-- {
--   qboolean active;
--   unsigned int time;      // for timeout
--   socket_t socket;
--   netadr_t address;
-- } incoming_t;
-- #endif

-- #define MAX_MOTD_LEN 1024

-- typedef struct client_entities_s
-- {
--   unsigned num_entities;        // maxclients->integer*UPDATE_BACKUP*MAX_PACKET_ENTITIES
--   unsigned next_entities;       // next client_entity to use
--   entity_state_t *entities;     // [num_entities]
-- } client_entities_t;

-- typedef struct fatvis_s
-- {
--   vec_t *skyorg;
--   qbyte pvs[MAX_MAP_LEAFS/8];
--   qbyte phs[MAX_MAP_LEAFS/8];
-- } fatvis_t;

-- typedef struct
-- {
--   qboolean initialized;               // sv_init has completed
--   unsigned int realtime;                  // real world time - always increasing, no clamping, etc
--   unsigned int gametime;                  // game world time - always increasing, no clamping, etc

--   socket_t socket_udp;
--   socket_t socket_udp6;
--   socket_t socket_loopback;
-- #ifdef TCP_SUPPORT
--   socket_t socket_tcp;
-- #endif

--   char mapcmd[MAX_TOKEN_CHARS];       // ie: *intro.cin+base

--   int spawncount;                     // incremented each server start
--                                       // used to check late spawns

--   client_t *clients;                  // [sv_maxclients->integer];
--   client_entities_t client_entities;

--   challenge_t challenges[MAX_CHALLENGES]; // to prevent invalid IPs from connecting
-- #ifdef TCP_SUPPORT
--   incoming_t incoming[MAX_INCOMING_CONNECTIONS]; // holds socket while tcp client is connecting
-- #endif

--   server_static_demo_t demo;

--   purelist_t *purelist;       // pure file support

--   cmodel_state_t *cms;                // passed to CM-functions

--   fatvis_t fatvis;

--   char *motd;
-- } server_static_t;

-- typedef struct
-- {
--   int last_heartbeat;
--   int last_mmheartbeat;
--   unsigned int last_activity;
--   unsigned int snapFrameTime;   // msecs between server packets
--   unsigned int gameFrameTime;   // msecs between game code executions
--   qboolean autostarted;
-- } server_constant_t;

-- // shared message buffer to be used for occasional messages
-- extern msg_t tmpMessage;
-- extern qbyte tmpMessageData[MAX_MSGLEN];

-- extern mempool_t *sv_mempool;

-- extern server_constant_t svc;              // constant server info (trully persistant since sv_init)
-- extern server_static_t svs;                // persistant server info
-- extern server_t sv;                 // local server

-- extern cvar_t *sv_ip;
-- extern cvar_t *sv_port;
-- extern cvar_t *sv_ip6;
-- extern cvar_t *sv_port6;
-- extern cvar_t *sv_tcp;

-- extern cvar_t *sv_skilllevel;
-- extern cvar_t *sv_maxclients;
-- extern cvar_t *sv_maxmvclients;

-- extern cvar_t *sv_enforcetime;
-- extern cvar_t *sv_showRcon;
-- extern cvar_t *sv_showChallenge;
-- extern cvar_t *sv_showInfoQueries;
-- extern cvar_t *sv_highchars;

-- //wsw : jal
-- extern cvar_t *sv_maxrate;
-- extern cvar_t *sv_compresspackets;
-- extern cvar_t *sv_public;         // should heartbeats be sent

-- // wsw : debug netcode
-- extern cvar_t *sv_debug_serverCmd;

-- extern cvar_t *sv_uploads;
-- extern cvar_t *sv_uploads_from_server;
-- extern cvar_t *sv_uploads_baseurl;
-- extern cvar_t *sv_uploads_demos_baseurl;

-- extern cvar_t *sv_pure;
-- extern cvar_t *sv_pure_forcemodulepk3;

-- // MOTD: 0=disable MOTD
-- //       1=Enable MOTD
-- extern cvar_t *sv_MOTD;
-- // File to read MOTD from
-- extern cvar_t *sv_MOTDFile;
-- // String to display
-- extern cvar_t *sv_MOTDString;
-- extern cvar_t *sv_lastAutoUpdate;
-- extern cvar_t *sv_defaultmap;

-- extern cvar_t *sv_demodir;

-- extern cvar_t *sv_mm_authkey;
-- extern cvar_t *sv_mm_loginonly;
-- extern cvar_t *sv_mm_debug_reportbots;

--   package cl_stereo_separation;
--   package cl_stereo;
--   package cl_gun;
--   package cl_add_blend;
--   package cl_add_lights;
--   package cl_add_particles;
--   package cl_add_entities;
--   package cl_predict;
--   package cl_footsteps;
--   package cl_noskins;
--   package cl_autoskins;
--   package cl_upspeed;
--   package cl_forwardspeed;
--   package cl_sidespeed;
--   package cl_yawspeed;
--   package cl_pitchspeed;
--   package cl_run;
--   package cl_anglespeedkey;
--   package cl_shownet;
--   package cl_showmiss;
--   package cl_showclamp;
--   package lookspring;
--   package lookstrafe;
--   package sensitivity;
--   package m_pitch;
--   package m_yaw;
--   package m_forward;
--   package m_side;
--   package freelook;
--   package cl_lightlevel; -- FIXME HACK
--   package cl_paused;
--   package cl_timedemo;
--   package cl_vwep;
-- private
--   #define	CMD_BACKUP		64	-- allow a lot of command backups for very fast systems
--   #define MAX_PARSE_ENTITIES  1024
--   extern  netadr_t  net_from;
--   extern  sizebuf_t net_message;
--   extern client_static_t	cls;
--   extern	client_state_t	cl;
--   extern char cl_weaponmodels[MAX_CLIENTWEAPONMODELS][MAX_QPATH];
--   extern int num_cl_weaponmodels;
--   connstate_t	state;
--   keydest_t	key_dest;
--   int			framecount;
--   int			realtime;			-- always increasing, no clamping, etc
--   float		frametime;			-- seconds since last frame
--   -- screen rendering information
--   float		disable_screen;		-- showing loading plaque between levels
--   -- or changing rendering dlls
--   -- if time gets > 30 seconds ahead, break it
--   int			disable_servercount;	-- when we receive a frame and cl.servercount
--   -- > cls.disable_servercount, clear disable_screen
--   -- connection information
--   char		servername[MAX_OSPATH];	-- name of server from original connect
--   float		connect_time;		-- for connection retransmits
--   int			quakePort;			-- a 16 bit value that allows quake servers to work around address translating routers
--   netchan_t	netchan;
--   int			serverProtocol;		-- in case we are doing some kind of version hack
--   int			challenge;			-- from the server to use for connecting
--   FILE		*download;			-- file transfer from server
--   char		downloadtempname[MAX_OSPATH];
--   char		downloadname[MAX_OSPATH];
--   int			downloadnumber;
--   dltype_t	downloadtype;
--   int			downloadpercent;
--   -- demo recording info must be here, so it isn't cleared on level change
--   qboolean	demorecording;
--   qboolean	demowaiting;	-- don't record until a non-delta message is received
--   FILE		*demofile;
--   extern	centity_t	cl_entities[MAX_EDICTS];
--   extern	cdlight_t	cl_dlights[MAX_DLIGHTS];
--   -- the cl_parse_entities must be large enough to hold UPDATE_BACKUP frames of
--   -- entities, so that when a delta compressed message arives from the server
--   -- it can be un-deltad from the original 
--   extern	entity_state_t	cl_parse_entities[MAX_PARSE_ENTITIES];



-- void DrawString (int x, int y, char *s);
-- void DrawAltString (int x, int y, char *s);	-- toggle high bit
-- qboolean	CL_CheckOrDownloadFile (char *filename);

-- void CL_AddNetgraph (void);


-- #define MAX_SUSTAINS		32
-- void CL_ParticleSteamEffect2(cl_sustain_t *self);

-- void CL_TeleporterParticles (entity_state_t *ent);
-- void CL_ParticleEffect (vec3_t org, vec3_t dir, int color, int count);
-- void CL_ParticleEffect2 (vec3_t org, vec3_t dir, int color, int count);

-- -- RAFAEL
-- void CL_ParticleEffect3 (vec3_t org, vec3_t dir, int color, int count);


-- --=================================================

-- -- ========
-- -- PGM

-- #define	PARTICLE_GRAVITY	40
-- #define BLASTER_PARTICLE_COLOR		0xe0
-- -- PMM
-- #define INSTANT_PARTICLE	-10000.0
-- -- PGM
-- -- ========

-- void CL_ClearEffects (void);
-- void CL_ClearTEnts (void);
-- void CL_BlasterTrail (vec3_t start, vec3_t end);
-- void CL_QuadTrail (vec3_t start, vec3_t end);
-- void CL_RailTrail (vec3_t start, vec3_t end);
-- void CL_BubbleTrail (vec3_t start, vec3_t end);
-- void CL_FlagTrail (vec3_t start, vec3_t end, float color);

-- -- RAFAEL
-- void CL_IonripperTrail (vec3_t start, vec3_t end);


-- int CL_ParseEntityBits (unsigned *bits);
-- void CL_ParseDelta (entity_state_t *from, entity_state_t *to, int number, int bits);
-- void CL_ParseFrame (void);

-- void CL_ParseTEnt (void);
-- void CL_ParseConfigString (void);
-- void CL_ParseMuzzleFlash (void);
-- void CL_ParseMuzzleFlash2 (void);
-- void SmokeAndFlash(vec3_t origin);

-- void CL_SetLightstyle (int i);

-- void CL_RunParticles (void);
-- void CL_RunDLights (void);
-- void CL_RunLightStyles (void);

-- void CL_AddEntities (void);
-- void CL_AddDLights (void);
-- void CL_AddTEnts (void);
-- void CL_AddLightStyles (void);

-- --=================================================

-- void CL_PrepRefresh (void);
-- void CL_RegisterSounds (void);

-- void CL_Quit_f (void);

-- void IN_Accumulate (void);

-- void CL_ParseLayout (void);


-- --
-- -- cl_main
-- --
-- extern	refexport_t	re;		-- interface to refresh .dll

-- void CL_Init (void);

-- void CL_FixUpGender(void);
-- void CL_Disconnect (void);
-- void CL_GetChallengePacket (void);
-- void CL_PingServers_f (void);
-- void CL_Snd_Restart_f (void);
-- void CL_RequestNextDownload (void);

-- --
-- -- cl_input
-- --

-- extern	kbutton_t	in_mlook, in_klook;
-- extern 	kbutton_t 	in_strafe;
-- extern 	kbutton_t 	in_speed;

-- void CL_InitInput (void);
-- void CL_SendCmd (void);
-- void CL_SendMove (usercmd_t *cmd);

-- void CL_ClearState (void);

-- void CL_ReadPackets (void);

-- int  CL_ReadFromServer (void);
-- void CL_WriteToServer (usercmd_t *cmd);
-- void CL_BaseMove (usercmd_t *cmd);

-- void IN_CenterView (void);

-- float CL_KeyState (kbutton_t *key);
-- char *Key_KeynumToString (int keynum);

-- --
-- -- cl_demo.c
-- --
-- void CL_WriteDemoMessage (void);
-- void CL_Stop_f (void);
-- void CL_Record_f (void);

-- --
-- -- cl_parse.c
-- --
-- extern	char *svc_strings[256];

-- void CL_ParseServerMessage (void);
-- void CL_LoadClientinfo (clientinfo_t *ci, char *s);
-- void SHOWNET(char *s);
-- void CL_ParseClientinfo (int player);
-- void CL_Download_f (void);

-- --
-- -- cl_view.c
-- --
-- extern	int			gun_frame;
-- extern	struct model_s	*gun_model;

-- void V_Init (void);
-- void V_RenderView( float stereo_separation );
-- void V_AddEntity (entity_t *ent);
-- void V_AddParticle (vec3_t org, int color, float alpha);
-- void V_AddLight (vec3_t org, float intensity, float r, float g, float b);
-- void V_AddLightStyle (int style, float r, float g, float b);

-- --
-- -- cl_tent.c
-- --
-- void CL_RegisterTEntSounds (void);
-- void CL_RegisterTEntModels (void);
-- void CL_SmokeAndFlash(vec3_t origin);


-- --
-- -- cl_pred.c
-- --
-- void CL_InitPrediction (void);
-- void CL_PredictMove (void);
-- void CL_CheckPredictionError (void);

-- --
-- -- cl_fx.c
-- --
-- cdlight_t *CL_AllocDlight (int key);
-- void CL_BigTeleportParticles (vec3_t org);
-- void CL_RocketTrail (vec3_t start, vec3_t end, centity_t *old);
-- void CL_DiminishingTrail (vec3_t start, vec3_t end, centity_t *old, int flags);
-- void CL_FlyEffect (centity_t *ent, vec3_t origin);
-- void CL_BfgParticles (entity_t *ent);
-- void CL_AddParticles (void);
-- void CL_EntityEvent (entity_state_t *ent);
-- -- RAFAEL
-- void CL_TrapParticles (entity_t *ent);

-- --
-- -- menus
-- --
-- void M_Init (void);
-- void M_Keydown (int key);
-- void M_Draw (void);
-- void M_Menu_Main_f (void);
-- void M_ForceMenuOff (void);
-- void M_AddToServerList (netadr_t adr, char *info);

-- --
-- -- cl_inv.c
-- --
-- void CL_ParseInventory (void);
-- void CL_KeyInventory (int key);
-- void CL_DrawInventory (void);

-- --
-- -- cl_pred.c
-- --
-- void CL_PredictMovement (void);

-- #if id386
-- void x86_TimerStart( void );
-- void x86_TimerStop( void );
-- void x86_TimerInit( unsigned long smallest, unsigned longest );
-- unsigned long *x86_TimerGetHistogram( void );
-- #endif
-- package Neo.World.Client.Server is
--   type Enumerated_State(Dead_State, Loading_State, Game_State, Cinematic_State, Demo_State, Picture_State);
--   type Record_Data is record
--       State : Enumerated_State := Enumerated_State'first;
--     Do_Attract : Boolean := False;
--     Do_Initialize : Boolean := False;
--     Frame_Count : Natural := 0;
--     Name : String_2_Unbounded := NULL_STRING_2_UNBOUNDED;

--     end Record_Data;
--   type Record_Client is record
--       State : Enumerated_Client_State;
      


-- //
-- // sv_main.c
-- //
-- int SV_ModelIndex( const char *name );
-- int SV_SoundIndex( const char *name );
-- int SV_ImageIndex( const char *name );
-- int SV_SkinIndex( const char *name );

-- void SV_WriteClientdataToMessage( client_t *client, msg_t *msg );

-- void SV_AutoUpdateFromWeb( qboolean checkOnly );
-- void SV_InitOperatorCommands( void );
-- void SV_ShutdownOperatorCommands( void );

-- void SV_SendServerinfo( client_t *client );
-- void SV_UserinfoChanged( client_t *cl );

-- void SV_MasterHeartbeat( void );

-- void SVC_MasterInfoResponse( const socket_t *socket, const netadr_t *address );
-- int SVC_FakeConnect( char *fakeUserinfo, char *fakeSocketType, char *fakeIP );

-- void SV_UpdateActivity( void );

-- //
-- // sv_oob.c
-- //
-- void SV_ConnectionlessPacket( const socket_t *socket, const netadr_t *address, msg_t *msg );
-- void SV_InitMaster( void );

-- //
-- // sv_init.c
-- //
-- void SV_InitGame( void );
-- void SV_Map( const char *level, qboolean devmap );
-- void SV_SetServerConfigStrings( void );

-- void SV_AddPureFile( const char *filename );
-- void SV_PureList_f( void );

-- //
-- // sv_phys.c
-- //
-- void SV_PrepWorldFrame( void );

-- //
-- // sv_send.c
-- //
-- qboolean SV_Netchan_Transmit( netchan_t *netchan, msg_t *msg );
-- void SV_AddServerCommand( client_t *client, const char *cmd );
-- void SV_SendServerCommand( client_t *cl, const char *format, ... );
-- void SV_AddGameCommand( client_t *client, const char *cmd );
-- void SV_AddReliableCommandsToMessage( client_t *client, msg_t *msg );
-- qboolean SV_SendClientsFragments( void );
-- void SV_InitClientMessage( client_t *client, msg_t *msg, qbyte *data, size_t size );
-- qboolean SV_SendMessageToClient( client_t *client, msg_t *msg );
-- void SV_ResetClientFrameCounters( void );


-- void SV_FlushRedirect( int sv_redirected, char *outputbuf, const void *extra );
-- void SV_SendClientMessages( void );

-- void SV_Multicast( vec3_t origin, multicast_t to );
-- void SV_BroadcastCommand( const char *format, ... );

-- //
-- // sv_client.c
-- //
-- void SV_ParseClientMessage( client_t *client, msg_t *msg );
-- qboolean SV_ClientConnect( const socket_t *socket, const netadr_t *address, client_t *client, char *userinfo,
--                            int game_port, int challenge, qboolean fakeClient, qboolean tvClient,
--                            unsigned int ticket_id, int session_id );
-- void SV_DropClient( client_t *drop, int type, const char *format, ... );
-- void SV_ExecuteClientThinks( int clientNum );
-- void SV_ClientResetCommandBuffers( client_t *client );

-- //
-- // sv_mv.c
-- //
-- qboolean SV_Multiview_CreateStartMessages( qboolean ( *callback )( msg_t *msg, void *extra ), void *extra );


-- //
-- // sv_ccmds.c
-- //
-- void SV_Status_f( void );

-- //
-- // sv_ents.c
-- //
-- void SV_WriteFrameSnapToClient( client_t *client, msg_t *msg );
-- void SV_BuildClientFrameSnap( client_t *client );


-- void SV_Error( char *error, ... );

-- //
-- // sv_game.c
-- //
-- extern game_export_t *ge;

-- void SV_InitGameProgs( void );
-- void SV_ShutdownGameProgs( void );
-- void SV_InitEdict( edict_t *e );


-- //============================================================

-- //
-- // sv_demos.c
-- //
-- void SV_Demo_WriteSnap( void );
-- void SV_Demo_Start_f( void );
-- void SV_Demo_Stop_f( void );
-- void SV_Demo_Cancel_f( void );
-- void SV_Demo_Purge_f( void );

-- void SV_DemoList_f( client_t *client );
-- void SV_DemoGet_f( client_t *client );

-- #define SV_SetDemoMetaKeyValue(k,v) svs.demo.meta_data_realsize = SNAP_SetDemoMetaKeyValue(svs.demo.meta_data, sizeof(svs.demo.meta_data), svs.demo.meta_data_realsize, k, v)

-- qboolean SV_IsDemoDownloadRequest( const char *request );

-- //
-- // sv_motd.c
-- //
-- void SV_MOTD_Update( void );
-- void SV_MOTD_Get_f( client_t *client );

-- //
-- // sv_mm.c
-- //
-- #ifdef TCP_SUPPORT
-- void SV_MM_SetConnection( incoming_t *connection );
-- #endif

-- void SV_MM_Init( void );
-- void SV_MM_Shutdown( qboolean logout );
-- void SV_MM_Frame( void );
-- qboolean SV_MM_Initialized( void );

-- int SV_MM_ClientConnect( const netadr_t *address, char *userinfo, unsigned int ticket, int session );
-- void SV_MM_ClientDisconnect( client_t *client );

-- int SV_MM_GenerateLocalSession( void );

-- // match report
-- #include "../matchmaker/mm_common.h"
-- void SV_MM_SendQuery( stat_query_t *query );
-- void SV_MM_GameState( qboolean state );
-- void SV_MM_GetMatchUUID( void (*callback_fn)( const char *uuid ) );
