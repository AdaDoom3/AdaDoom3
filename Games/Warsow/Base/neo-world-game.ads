package Neo.World.Game is
  type Enumerated_Attenuation is (No_Attenuation, Distant_Attenuation, Normal_Attenuation, Idle_Attenuation, Static_Attenuation, Moving_Attenuation);
  type Enumerated_Mode        is (No_Move, Player_Move, God_Move, Push_Move, Fly_Move, Toss_Move, Projectile_Move, Bounce_Move, Slide_Move);
  type Enumerated_Stage       is (No_Stage, Warmup_Stage, Countdown_Stage, Play_Stage, Review_Stage, Exit_Stage);
  type Enumerated_Trail       is (GIB_Trail, Blaster_Trail, Electro_Trail, Rocket_Trail, Plasma_Trail);
  type Enumerated_AI          is (Stand_AI, Target_AI, Aimless_AI, Listen_AI, Wait_AI, Hide_AI);
  type Enumerated_Orientation is (Left_Orientation, Center_Orientation, Right_Orientation);
  type Enumerated_Character   is (Lower_Character, Upper_Character, Head_Character);
  type Enumerated_Material    is (Water_Material, Slime_Material, Lava_Material);
  type Enumerated_Damage      is (Splash_Damage, Stun_Damage, Knockback_Damage);
  type Enumerated_Gender      is (Male_Gender, Female_Gender, Neutral_Gender);
  type Enumerated_Spawn       is (Instant_Spawn, Wave_Spawn);
  type Enumerated_Object is(
    Player_Object,    Corpse_Object, Beam_Object,  Portal_Object,   Trigger_Object,
    Weapon_Object,    Ammo_Object,   Armor_Object, Power_Up_Object, Health_Object,
    Expansion_Object, Barrel_Object, Flash_Object, Hand_Object);
  type Enumerated_State is(
    Fly_State,        Swim_State,       Immune_Laser_State, In_Water_State,
    God_State,        Not_Target_State, Immune_Slime_State, Partial_Ground_State,
    Water_Jump_State, Team_Slave_State, Immune_Lava,        No_Knockback_State);
  type Enumerated_Weapon is(
    Blade_Weapon,      Machine_Gun_Weapon,     Riot_Gun_Weapon,  Grenade_Weapon, Rocket_Gun_Weapon,
    Plasma_Gun_Weapon, Electrobolt_Gun_Weapon, Insta_Gun_Weapon, Laser_Gun_Weapon);
  type Enumerated_Weapon_Animation is(
    Ready_Animation, Activating_Animation, Drop_Animation,   Power_Animation,
    Fire_Animation,  Cool_Down_Animation,  Reload_Animation, No_Ammo_Animation);
  type Enumerated_Character is(
    );
  type Enumerated_Character_Animation is(
    Death_1_Animation,          Dead_1_Animation,         Death_2_Animation,             Dead_2_Animation,              Death_3_Animation,           Dead_3_Animation,
    Walk_Forward_Animation,     Walk_Backward_Animation,  Walk_Left_Animation,           Walk_Right_Animation,          Stand_Idle_Animation,        Land_Animation,
    Run_Forward_Animation,      Run_Backward_Animation,   Run_Left_Animation,            Run_Right_Animation,           Jump_1_Animation,            Jump_2_Animation,
    Jump_Neutral,               Crouch_Idle_Animation,    Crouch_Walk_Animation,         Swim_Forward_Animation,        Swim_Neutral_Animation,      Walljump_Forward_Animation,
    Walljump_Left_Animation,    Walljump_Right_Animation, Walljump_Backward_Animation,   Dash_Forward_Animation,        Dash_Left_Animation,         Dash_Right_Animation,
    Dash_Backward_Animation,    Hold_Blade_Animation,     Hold_Pistol_Animation,         Hold_Light_Weapon_Animation,   Hold_Heavy_Weapon_Animation, Hold_Aim_Weapon_Animation,
    Attack_Blade_Animation,     Attack_Pistol_Animation,  Attack_Light_Weapon_Animation, Attack_Heavy_Weapon_Animation, Hold_Aim_Weapon_Animation,   Switch_Out_Weapon_Animation,
    Switch_In_Weapon_Animation, Drop_Hold_Animation,      Drop_Animation,                Swim_Animation,                Pain_1_Animation,            Pain_2_Animation,
    Pain_3_Animation);
  type Enumerated_Talk is(
    Need_Health_Talk, Need_Weapon_Talk, Need_Armor_Talk, Affirmative_Talk, Negative_Talk, Yes_Talk, No_Talk, Defense_Talk, Offensive_Talk, Oops_Talk, Sorry_Talk, Thanks_Talk,
    No_Problem_Talk, Yeah_Talk, Good_Game_Talk, Defend_Talk, Attack_Talk, Need_Backup_Talk, Boo_Talk, Need_Offense_Talk, Need_Defense_Talk, Need_Help_Talk, Roger_Talk, Armorless_Talk,
    Secured_Talk, Shutup_Talk, Boomstick_Talk, Powerup_Talk, Quad_Talk, Ok_Talk);
    void SP_func_plat( edict_t *ent );
void SP_func_rotating( edict_t *ent );
void SP_func_button( edict_t *ent );
void SP_func_door( edict_t *ent );
void SP_func_door_rotating( edict_t *ent );
void SP_func_door_secret( edict_t *self );
void SP_func_water( edict_t *self );
void SP_func_train( edict_t *ent );
void SP_func_conveyor( edict_t *self );
void SP_func_wall( edict_t *self );
void SP_func_object( edict_t *self );
void SP_func_explosive( edict_t *self );
void SP_func_killbox( edict_t *ent );
void SP_func_static( edict_t *ent );
void SP_func_bobbing( edict_t *ent );
void SP_func_pendulum( edict_t *ent );

    // S_DEFAULT_ATTENUATION_MODEL			"1"
#define	ATTN_NONE				0	// full volume the entire level
#define	ATTN_DISTANT			2	// distant sound (most likely explosions)
#define	ATTN_NORM				5	// players, weapons, etc
#define	ATTN_IDLE				8	// stuff around you
#define	ATTN_STATIC				10	// diminish very rapidly with distance
#define	ATTN_FOOTSTEPS			21	// must be very close to hear it

  type Record_Data is record
      Start_Time            : Duration;
      Run_Time              : Duration;
      Do_Clock_Override     : Boolean := False;
      Do_Pause              : Boolean := False;
      Do_Wait               : Boolean := False;
      Do_Instantly_GIB      : Boolean := False;
      Do_Extend             : Boolean := False;
      Do_Fall_Damage        : Boolean := False;
      Do_Not_Allow_Shooting : Boolean := False;
      Do_Race               : Boolean := False;
      Do_Countdown          : Boolean := False;
      Do_Self_Damage        : Boolean := False;
      Do_Use_Infinite_Ammo  : Boolean := False;
      Do_Force_Models       : Boolean := False;
      Do_Use_Minimap        : Boolean := False;
      Do_Use_Team_Minimap   : Boolean := False;
    end record;
  #define GS_MODULE_GAME	    1
#define GS_MODULE_CGAME	    2

#define GAMELONG_MATCHSTART 0
#define GAMELONG_MATCHDURATION 1
#define GAMELONG_CLOCKOVERRIDE 2

#define GAMESTAT_FLAGS 0
#define GAMESTAT_MATCHSTATE 1
#define GAMESTAT_MAXPLAYERSINTEAM 2


	ET_GENERIC,
	ET_PLAYER,
	ET_CORPSE,
	ET_BEAM,
	ET_PORTALSURFACE,
	ET_PUSH_TRIGGER,

	ET_GIB,         // leave a trail
	ET_BLASTER,     // redlight + trail
	ET_ELECTRO_WEAK,
	ET_ROCKET,      // redlight + trail
	ET_GRENADE,
	ET_PLASMA,

	ET_SPRITE,

	ET_ITEM,        // for simple items
	ET_LASERBEAM,   // for continuous beams
	ET_CURVELASERBEAM, // for curved beams
	ET_FLAG_BASE,

	ET_MINIMAP_ICON,
	ET_DECAL,
	ET_ITEM_TIMER,	// for specs only
	ET_PARTICLES,
	ET_SPAWN_INDICATOR,

	// eventual entities: types below this will get event treatment
	ET_EVENT = EVENT_ENTITIES_START,
	ET_SOUNDEVENT,
	ET_TOTAL_TYPES, // current count
	MAX_ENTITY_TYPES = 128


#define GS_MODULE_GAME	    1
#define GS_MODULE_CGAME	    2

#define GAMELONG_MATCHSTART 0
#define GAMELONG_MATCHDURATION 1
#define GAMELONG_CLOCKOVERRIDE 2

#define GAMESTAT_FLAGS 0
#define GAMESTAT_MATCHSTATE 1
#define GAMESTAT_MAXPLAYERSINTEAM 2

enum
{
	CHAN_AUTO = S_CHANNEL_AUTO,
	CHAN_PAIN,
	CHAN_VOICE,
	CHAN_ITEM,
	CHAN_BODY,
	CHAN_MUZZLEFLASH,
	CHAN_ANNOUNCER,

	CHAN_TOTAL,

	CHAN_FIXED = 128
};

#define ISWALKABLEPLANE( x ) ( ((cplane_t *)x)->normal[2] >= 0.7 )

#define MAX_SLIDEMOVE_CLIP_PLANES   16
#define SLIDEMOVE_PLANEINTERACT_EPSILON	0.05
#define SLIDEMOVEFLAG_PLANE_TOUCHED 16
#define SLIDEMOVEFLAG_WALL_BLOCKED  8
#define SLIDEMOVEFLAG_TRAPPED	    4
#define SLIDEMOVEFLAG_BLOCKED	    2   // it was blocked at some point, doesn't mean it didn't slide along the blocking object
#define SLIDEMOVEFLAG_MOVED	    1
ITEMS_TOTAL
#define MAX_ITEM_MODELS 2
// gsitem_t->flags
#define	ITFLAG_PICKABLE		1
#define	ITFLAG_USABLE		2
#define ITFLAG_DROPABLE		4
#define ITFLAG_STAY_COOP	8

typedef enum
{
	IT_WEAPON = 1
	, IT_AMMO = 2
	, IT_ARMOR = 4
	, IT_POWERUP = 8
	, IT_HEALTH = 64
} itemtype_t;
enum
{
	TEAM_SPECTATOR,
	TEAM_PLAYERS,
	TEAM_ALPHA,
	TEAM_BETA,

	GS_MAX_TEAMS
};
enum
{
	BASE_CHANNEL,
	EVENT_CHANNEL,

	PLAYERANIM_CHANNELS
};

typedef enum
{
	MATCHMESSAGE_NONE
	, MATCHMESSAGE_CHALLENGERS_QUEUE
	, MATCHMESSAGE_ENTER_CHALLENGERS_QUEUE
	, MATCHMESSAGE_SPECTATOR_MODES
	, MATCHMESSAGE_GET_READY
	, MATCHMESSAGE_WAITING_FOR_PLAYERS
} matchmessage_t;
#define	PMFEAT_CROUCH			( 1<<0 )
#define	PMFEAT_WALK				( 1<<1 )
#define	PMFEAT_JUMP				( 1<<2 )
#define	PMFEAT_DASH				( 1<<3 )
#define	PMFEAT_WALLJUMP			( 1<<4 )
#define	PMFEAT_FWDBUNNY			( 1<<5 )
#define	PMFEAT_AIRCONTROL		( 1<<6 )
#define	PMFEAT_ZOOM				( 1<<7 )
#define	PMFEAT_GHOSTMOVE		( 1<<8 )
#define	PMFEAT_CONTINOUSJUMP	( 1<<9 )
#define	PMFEAT_ITEMPICK			( 1<<10 )
#define	PMFEAT_GUNBLADEAUTOATTACK ( 1<<11 )
#define	PMFEAT_WEAPONSWITCH ( 1<<12 )
#define PMFEAT_ALL			( 0xFFFF )
#define PMFEAT_DEFAULT		( PMFEAT_ALL & ~PMFEAT_GHOSTMOVE )

enum
{
	STAT_LAYOUTS = 0
	, STAT_HEALTH
	, STAT_ARMOR
	, STAT_WEAPON
	, STAT_WEAPON_TIME
	, STAT_PENDING_WEAPON

	, STAT_PICKUP_ITEM

	, STAT_SCORE
	, STAT_TEAM
	, STAT_REALTEAM
	, STAT_NEXT_RESPAWN

	, STAT_POINTED_PLAYER
	, STAT_POINTED_TEAMPLAYER

	, STAT_TEAM_ALPHA_SCORE
	, STAT_TEAM_BETA_SCORE

	, STAT_LAST_KILLER

	// the stats below this point are set by the gametype scripts
	, GS_GAMETYPE_STATS_START = 32

	, STAT_PROGRESS_SELF = GS_GAMETYPE_STATS_START
	, STAT_PROGRESS_OTHER
	, STAT_PROGRESS_ALPHA
	, STAT_PROGRESS_BETA

	, STAT_IMAGE_SELF
	, STAT_IMAGE_OTHER
	, STAT_IMAGE_ALPHA
	, STAT_IMAGE_BETA

	, STAT_TIME_SELF
	, STAT_TIME_BEST
	, STAT_TIME_RECORD
	, STAT_TIME_ALPHA
	, STAT_TIME_BETA

	, STAT_MESSAGE_SELF
	, STAT_MESSAGE_OTHER
	, STAT_MESSAGE_ALPHA
	, STAT_MESSAGE_BETA

	, GS_GAMETYPE_STATS_END = PS_MAX_STATS

	, MAX_STATS = PS_MAX_STATS //64
};

static const char *gs_keyicon_names[] = {
#endif
	"forward",
	"backward",
	"left",
	"right",
	"fire",
	"jump",
	"crouch",
	"special"
};

#define	STAT_LAYOUT_SPECDEAD		0x00000001
#define	STAT_LAYOUT_UNUSED1			0x00000002
#define	STAT_LAYOUT_SCOREBOARD		0x00000004
#define	STAT_LAYOUT_TEAMTAB			0x00000008
#define	STAT_LAYOUT_CHALLENGER		0x00000010 // player is in challengers queue (used for ingame menu)
#define	STAT_LAYOUT_READY			0x00000020 // player is ready (used for ingame menu)
//#define	STAT_LAYOUT_UNUSED			0x00000040
#define STAT_LAYOUT_SPECTEAMONLY	0x00000080
#define STAT_NOTSET					-9999 // used for stats that don't have meaningful value atm.
#define MOD_UNKNOWN	    0


enum
{
	PAIN_20,
	PAIN_30,
	PAIN_60,
	PAIN_100,
	PAIN_WARSHELL,

	PAIN_TOTAL
};
enum
{
	FIRE_MODE_WEAK,
	FIRE_MODE_STRONG,

	FIRE_MODES_COUNT
};

typedef enum
{
	EV_NONE,

	// predictable events

	EV_WEAPONACTIVATE,
	EV_FIREWEAPON,
	EV_ELECTROTRAIL,
	EV_INSTATRAIL,
	EV_FIRE_RIOTGUN,
	EV_FIRE_BULLET,
	EV_SMOOTHREFIREWEAPON,
	EV_NOAMMOCLICK,

	EV_DASH,

	EV_WALLJUMP,
	EV_WALLJUMP_FAILED,
	EV_DOUBLEJUMP,
	EV_JUMP,
	EV_JUMP_PAD,
	EV_FALL,

	// non predictable events

	EV_WEAPONDROP = PREDICTABLE_EVENTS_MAX,

	EV_ITEM_RESPAWN,
	EV_PAIN,
	EV_DIE,
	EV_GIB,

	EV_PLAYER_RESPAWN,
	EV_PLAYER_TELEPORT_IN,
	EV_PLAYER_TELEPORT_OUT,

	EV_GESTURE,
	EV_DROP,
	EV_SPOG,

	EV_BLOOD,

	EV_BLADE_IMPACT,
	EV_GUNBLADEBLAST_IMPACT,
	EV_GRENADE_BOUNCE,
	EV_GRENADE_EXPLOSION,
	EV_ROCKET_EXPLOSION,
	EV_PLASMA_EXPLOSION,
	EV_BOLT_EXPLOSION,
	EV_INSTA_EXPLOSION,

	// 3 spots reserved for new weapons sfx, so
	// the events below don't change their numbers easily
	EV_FREE2,
	EV_FREE3,
	EV_FREE4,

	EV_EXPLOSION1,
	EV_EXPLOSION2,

	EV_BLASTER,
	EV_SPARKS,
	EV_BULLET_SPARKS,

	EV_SEXEDSOUND,

	EV_VSAY,

	EV_LASER_SPARKS,

	EV_FIRE_SHOTGUN,
	EV_PNODE,
	EV_GREEN_LASER,

	// func movers
	EV_PLAT_HIT_TOP,
	EV_PLAT_HIT_BOTTOM,
	EV_PLAT_START_MOVING,

	EV_DOOR_HIT_TOP,
	EV_DOOR_HIT_BOTTOM,
	EV_DOOR_START_MOVING,

	EV_BUTTON_FIRE,

	EV_TRAIN_STOP,
	EV_TRAIN_START,

	MAX_EVENTS = 128
} entity_event_t;

typedef enum
{
	PSEV_NONE = 0,
	PSEV_HIT,
	PSEV_PICKUP,
	PSEV_DAMAGE_20,
	PSEV_DAMAGE_40,
	PSEV_DAMAGE_60,
	PSEV_DAMAGE_80,
	PSEV_INDEXEDSOUND,
	PSEV_ANNOUNCER,
	PSEV_ANNOUNCER_QUEUED,

	PSEV_MAX_EVENTS = 0xFF
} playerstate_event_t;
enum
{
	ET_GENERIC,
	ET_PLAYER,
	ET_CORPSE,
	ET_BEAM,
	ET_PORTALSURFACE,
	ET_PUSH_TRIGGER,

	ET_GIB,         // leave a trail
	ET_BLASTER,     // redlight + trail
	ET_ELECTRO_WEAK,
	ET_ROCKET,      // redlight + trail
	ET_GRENADE,
	ET_PLASMA,

	ET_SPRITE,

	ET_ITEM,        // for simple items
	ET_LASERBEAM,   // for continuous beams
	ET_CURVELASERBEAM, // for curved beams
	ET_FLAG_BASE,

	ET_MINIMAP_ICON,
	ET_DECAL,
	ET_ITEM_TIMER,	// for specs only
	ET_PARTICLES,
	ET_SPAWN_INDICATOR,

	// eventual entities: types below this will get event treatment
	ET_EVENT = EVENT_ENTITIES_START,
	ET_SOUNDEVENT,
	ET_TOTAL_TYPES, // current count
	MAX_ENTITY_TYPES = 128
};
#define	EF_ROTATE_AND_BOB			1			// rotate and bob (bonus items)
#define EF_SHELL					2
#define EF_STRONG_WEAPON			4
#define	EF_QUAD						8
#define EF_CARRIER					16
#define EF_BUSYICON					32
#define EF_FLAG_TRAIL				64			// player is carrying the enemy flag
#define EF_TAKEDAMAGE				128
#define EF_TEAMCOLOR_TRANSITION		256
#define EF_EXPIRING_QUAD			512
#define EF_EXPIRING_SHELL			1024
#define EF_GODMODE					2048
#define EF_REGEN					4096
#define EF_EXPIRING_REGEN			8192
#define EF_NOPORTALENTS				EF_CARRIER
#define EF_PLAYER_STUNNED			EF_ROTATE_AND_BOB
#define EF_PLAYER_HIDENAME			EF_TEAMCOLOR_TRANSITION
#define EF_AMMOBOX					( 1<<16 )
#define EF_RACEGHOST				( 1<<17 )
#define EF_OUTLINE					( 1<<18 )
typedef struct
{
	int module;
	int maxclients;
	char gametypeName[MAX_CONFIGSTRING_CHARS];
	game_state_t gameState;
} gs_state_t;

typedef struct
{
	vec3_t velocity;
	vec3_t origin;
	vec3_t mins, maxs;
	float remainingTime;

	vec3_t gravityDir;
	float slideBounce;
	int groundEntity;

	int passent, contentmask;

	int numClipPlanes;
	vec3_t clipPlaneNormals[MAX_SLIDEMOVE_CLIP_PLANES];

	int numtouch;
	int touchents[MAXTOUCH];
} move_t;
typedef struct gitem_s
{
	//header
	char * const classname;        // spawning name
	int tag;
	itemtype_t type;
	int flags;              // actions the item does in the game
	//media
	const char * const world_model[MAX_ITEM_MODELS];
	const char * const icon;
	const char * const simpleitem;       // Kurim : we use different images for representing simpleitems
	const char * const pickup_sound;
	int effects;
	char *name;      // for printing on pickup
	char *shortname;       // for printing on messages
	char *color;            // for printing on messages
	int quantity;           // how much it gives at picking
	int inventory_max;		// how much quantity of this the inventory can carry
	// special
	int ammo_tag;           // uses this ammo, for weapons
	int weakammo_tag;
	void *info;             // miscelanea info goes pointed in here
	// space separated string of stuff to precache that's not mentioned above
	const char * const precache_models;
	const char * const precache_sounds;
	const char * const precache_images;
	int asRefCount, asFactored;
} gsitem_t;
typedef struct
{
	int newanim[PMODEL_PARTS];

} gs_animationbuffer_t;

typedef struct  
{
	int anim;
	int frame;
	unsigned int startTimestamp;
	float lerpFrac;
}gs_animstate_t;

typedef struct
{
	// animations in the mixer
	gs_animstate_t curAnims[PMODEL_PARTS][PLAYERANIM_CHANNELS];
	gs_animationbuffer_t buffer[PLAYERANIM_CHANNELS];

	// results
	int frame[PMODEL_PARTS];
	int oldframe[PMODEL_PARTS];
	float lerpFrac[PMODEL_PARTS];
} gs_pmodel_animationstate_t;

typedef struct
{
	int firstframe[PMODEL_TOTAL_ANIMATIONS];
	int lastframe[PMODEL_TOTAL_ANIMATIONS];
	int loopingframes[PMODEL_TOTAL_ANIMATIONS];
	float frametime[PMODEL_TOTAL_ANIMATIONS];
} gs_pmodel_animationset_t;
typedef struct firedef_s
{

} firedef_t;

typedef struct
{
	char *name;
	int weapon_id;
	//ammo def
	int fire_mode;
	int ammo_id;
	int usage_count;
	int projectile_count;

	// timings
	unsigned int weaponup_time;
	unsigned int weapondown_time;
	unsigned int reload_time;
	unsigned int cooldown_time;
	unsigned int timeout;
	qboolean smooth_refire;

	// damages
	float damage;
	float selfdamage;
	int knockback;
	int stun;
	int splash_radius;
	int mindamage;
	int minknockback;

	// projectile def
	int speed;
	int spread;

	// ammo amounts
	int weapon_pickup;
	int ammo_pickup;
	int ammo_max;
	firedef_t firedef_weak;

} gs_weapon_definition_t;

typedef struct  
{
	vec3_t origins[LASERGUN_WEAK_TRAIL_BACKUP];
	unsigned int timeStamps[LASERGUN_WEAK_TRAIL_BACKUP];
	qboolean teleported[LASERGUN_WEAK_TRAIL_BACKUP];
	int head;
}gs_laserbeamtrail_t;
  type Record_AI is record
      player_state_t ps;       
      client_shared_t	r;
      // Review notes
      - Revise the calls to G_ClearPlayerStateEvents, they may be useless now
      - self->ai.pers.netname for what? there is self->r.client->netname already
      - CTF prints personal bonuses in global console. I don't think this is worth it
      */
      client_respawnreset_t resp;
      client_levelreset_t level;
      client_teamreset_t teamstate;
      //short ucmd_angles[3]; // last ucmd angles
      // persistent info along all the time the client is connected
      char userinfo[MAX_INFO_STRING];
      char netname[MAX_NAME_BYTES];	// maximum name length is characters without counting color tokens is controlled by MAX_NAME_CHARS constant
      char clanname[MAX_CLANNAME_BYTES];
      char ip[MAX_INFO_VALUE];
      char socket[MAX_INFO_VALUE];
      int mm_session;		// 0 - invalid session, < 0 - local session, > 0 authenticated account
      clientRating_t *ratings;	// list of ratings for gametypes
      qboolean connecting;
      qboolean multiview, tv;
      byte_vec4_t color;
      int team;
      int hand;
      int handicap;
      int fov;
      int movestyle;
      int movestyle_latched;
      int zoomfov;
      qboolean isoperator;
      unsigned int queueTimeStamp;
      int muted;     // & 1 = chat disabled, & 2 = vsay disabled
      usercmd_t ucmd;
      int timeDelta;              // time offset to adjust for shots collision (antilag)
      int timeDeltas[G_MAX_TIME_DELTAS];
      int timeDeltasHead;
      pmove_state_t old_pmove;    // for detecting out-of-pmove changes
      int asRefCount, asFactored;
    end record;
  type Record_Entity is record
      Is_Single     : Boolean            := False;
      Is_Team       : Boolean            := False;
      Is_Duel       : Boolean            := False;
      Is_Flag       : Boolean            := False;
      Field_Of_View : Float_4_Real       := 0.0;
      Roll          : Float_4_Real       := 0.0;
      Radius        : Float_4_Real       := 0.0;
      Phase         : Float_4_Real       := 0.0;
      Scale         : Float_4_Real       := 0.0;
      Lip           : Integer_4_Natural  := 0;
      Size          : Integer_4_Natural  := 0;
      Distance      : Integer_4_Natural  := 0;
      Height        : Integer_4_Natural  := 0;
      Weight        : Integer_4_Natural  := 0;
      Noise         : String_2_Unbounded := STRING_2_UNBOUNDED;
      Noise_Start   : String_2_Unbounded := STRING_2_UNBOUNDED;
      Noise_Stop    : String_2_Unbounded := STRING_2_UNBOUNDED;
      Item          : String_2_Unbounded := STRING_2_UNBOUNDED;
      Gravity       : String_2_Unbounded := STRING_2_UNBOUNDED;
      Debris        : String_2_Unbounded := STRING_2_UNBOUNDED;
      Kind          : String_2_Unbounded := STRING_2_UNBOUNDED; 
      Shader        : String_2_Unbounded := STRING_2_UNBOUNDED;
    end record;
  type Record_Weapon(Kind : Enumerated_Weapon := Enumerated_Weapon'first) is record
      int radius;
      float minDamage;
      float maxDamage;
      float minKnockback;
      float maxKnockback;
      int stun;
    end record;
  type Record_Camera is record
      qboolean active;
      int target;
      int mode;                   //3rd or 1st person
      int range;
      qboolean teamonly;
      unsigned int timeout;           //delay after loosing target
      int followmode;
    end record;
  type Record_Location is record
      // fixed data
      vec3_t start_origin;
      vec3_t start_angles;
      vec3_t end_origin;
      vec3_t end_angles;
      int sound_start;
      int sound_middle;
      int sound_end;
      vec3_t movedir;  // direction defined in the bsp
      float speed;
      float distance;    // used by binary movers
      float wait;
      float phase;
      // state data
      int state;
      vec3_t dir;             // used by func_bobbing and func_pendulum
      float current_speed;    // used by func_rotating
      void ( *endfunc )( edict_t * );
      void ( *blocked )( edict_t *self, edict_t *other );
      vec3_t dest;
      vec3_t destangles;
    end record;
  type Record_Award is record
      int ebhit_count;
      int directrocket_count;
      int directgrenade_count;
      int multifrag_timer;
      int multifrag_count;
      int frag_count;
      int accuracy_award;
      int directrocket_award;
      int directgrenade_award;
      int multifrag_award;
      int spree_award;
      int gl_midair_award;
      int rl_midair_award;
      int uh_control_award;
      int mh_control_award;
      int ra_control_award;
      qbyte combo[MAX_CLIENTS]; // combo management for award
      edict_t *lasthit;
      unsigned int lasthit_time;
    end record;
void SP_light( edict_t *self );
void SP_light_mine( edict_t *ent );
void SP_info_null( edict_t *self );
void SP_info_notnull( edict_t *self );
void SP_info_camp( edict_t *self );
void SP_path_corner( edict_t *self );

void SP_misc_teleporter_dest( edict_t *self );
void SP_misc_model( edict_t *ent );
void SP_misc_portal_surface( edict_t *ent );
void SP_misc_portal_camera( edict_t *ent );
void SP_skyportal( edict_t *ent );
void SP_misc_particles( edict_t *ent );
  type Record_Level is record
      void *initFunc;
      void *preThinkFunc;
      void *postThinkFunc;
      void *exitFunc;
      unsigned int framenum;
      unsigned int time; // time in milliseconds
      unsigned int spawnedTimeStamp; // time when map was restarted
      char level_name[MAX_CONFIGSTRING_CHARS];    // the descriptive name (Outer Base, etc)
      char mapname[MAX_CONFIGSTRING_CHARS];       // the server name (q3dm0, etc)
      char nextmap[MAX_CONFIGSTRING_CHARS];       // go here when match is finished
      char forcemap[MAX_CONFIGSTRING_CHARS];      // go here
      char autorecord_name[128];
      // backup entities string
      char *mapString;
      size_t mapStrlen;
      // string used for parsing entities
      char *map_parsed_ents;      // string used for storing parsed key values
      size_t map_parsed_len;
      qboolean canSpawnEntities; // security check to prevent entities being spawned before map entities
      // intermission state
      qboolean exitNow;
      qboolean hardReset;
      // angelScript engine handle
      int asEngineHandle;
      qboolean asGlobalsInitialized;
      // gametype definition and execution
      gametype_descriptor_t gametype;
      // map scripts
      qboolean teamlock;
      qboolean ready[MAX_CLIENTS];
      qboolean forceStart;    // force starting the game, when warmup timelimit is up
      qboolean forceExit;     // just exit, ignore extended time checks
      edict_t	*current_entity;    // entity running from G_RunFrame
      edict_t	*spawning_entity;    // entity being spawned from G_InitLevel
      int body_que;               // dead bodies
      int numCheckpoints;
      int numLocations;
      timeout_t timeout;
    end record;
  type Record_Trigger is record
      void SP_trigger_teleport( edict_t *ent );
      void SP_info_teleport_destination( edict_t *ent );
      void SP_trigger_always( edict_t *ent );
      void SP_trigger_once( edict_t *ent );
      void SP_trigger_multiple( edict_t *ent );
      void SP_trigger_relay( edict_t *ent );
      void SP_trigger_push( edict_t *ent );
      void SP_trigger_hurt( edict_t *ent );
      void SP_trigger_key( edict_t *ent );
      void SP_trigger_counter( edict_t *ent );
      void SP_trigger_elevator( edict_t *ent );
      void SP_trigger_gravity( edict_t *ent );
    end record;

#define MAX_CLIENT_EVENTS   16
#define MAX_CLIENT_EVENTS_MASK ( MAX_CLIENT_EVENTS - 1 )
#define G_MAX_TIME_DELTAS   8
#define G_MAX_TIME_DELTAS_MASK ( G_MAX_TIME_DELTAS - 1 )
  type Record_Player is record
      int buttons;
      qbyte plrkeys; // used for displaying key icons
      int damageTaken;
      vec3_t damageTakenDir;
      client_snapreset_t snap;
      chasecam_t chase;
      award_info_t awardInfo;
      unsigned int timeStamp; // last time it was reset
      // player_state_t event
      int events[MAX_CLIENT_EVENTS];
      unsigned int eventsCurrent;
      unsigned int eventsHead;
      gs_laserbeamtrail_t trail;
      qboolean takeStun;
      float armor;
      float instashieldCharge;
      unsigned int gunbladeChargeTimeStamp;
      unsigned int next_drown_time;
      int drowningDamage;
      int old_waterlevel;
      int old_watertype;
      unsigned int pickup_msg_time;
      unsigned int timeStamp;			// last time it was reset
      unsigned int respawnCount;
      matchmessage_t matchmessage;
      unsigned int last_vsay;         // time when last vsay was said
      unsigned int last_activity;
      score_stats_t stats;
      qboolean showscores;
      unsigned int scoreboard_time;	// when scoreboard was last sent
      qboolean showPLinks;			// bot debug
      // flood protection
      unsigned int flood_locktill;	// locked from talking
      unsigned int flood_when[MAX_FLOOD_MESSAGES];		// when messages were said
      int flood_whenhead;				// head pointer for when said
      // team only
      unsigned int flood_team_when[MAX_FLOOD_MESSAGES];	// when messages were said
      int flood_team_whenhead;		// head pointer for when said
      unsigned int timeStamp; // last time it was reset
      qboolean is_coach;
      unsigned int readyUpWarningNext; // (timer) warn people to ready up
      int readyUpWarningCount;
      // for position command
      qboolean position_saved;
      vec3_t position_origin;
      vec3_t position_angles;
      unsigned int position_lastcmd;
      gsitem_t *last_drop_item;
      vec3_t last_drop_location;
      edict_t	*last_pickup;
      edict_t *last_killer;
      char netname[MAX_NAME_CHARS];
      int team;
      int mm_session;
      score_stats_t stats;
      unsigned int timePlayed;
      qboolean final;			// is true, player was there in the end
      struct gclient_quit_s *next;
      qboolean kill;
      qboolean teamkill;
      float damage_taken;
      float damage_saved;         // saved by the armor.
      vec3_t damage_dir;
      vec3_t damage_at;
      float damage_given;             // for hitsounds
      float damageteam_given;
      float damage_fall;
    end record;
  type Record_Particle is record
      int speed;
      int shaderIndex;
      int spread;
      int size;
      int time;
      qboolean spherical;
      qboolean bounce;
      qboolean gravity;
      qboolean expandEffect;
      qboolean shrinkEffect;
      int frequency;
    end record;
  type Record_Entity is record
      State :
      Shared ??
      Previous_States :
      Name : String_2_Unbounded;
      Team
      Path
      Target : 
      Velocity
      Angular_Velocity
      Angle
      Speed
      Acceleration
      Deceleration
      Time_Delta
      Projectiles
      Particles
      
      
struct edict_s
{
	entity_state_t s;
	entity_shared_t	r;
	entity_state_t olds; // state in the last sent frame snap
	int movetype;
	int flags;
	int numEvents;
	unsigned int freetime;          // time when the object was freed
	qboolean eventPriority[2];
	float angle;                // set in qe3, -1 = up, -2 = down
	float speed;
	float accel, decel;         // usef for func_rotating
	float attenuation;
	float wait;
	float delay;                // before firing targets
	float health;
	float yaw_speed;
	float gravity;              // per entity gravity multiplier (1.0 is normal) // use for lowgrav artifact, flares
	//
	// only used locally in game, not by server
	//
	edict_t	*chain;
	edict_t	*enemy;
	edict_t	*oldenemy;
	edict_t	*activator;
	edict_t	*groundentity;
	edict_t	*teamchain;
	edict_t	*teammaster;
	const char *spawnString;			// keep track of string definition of this entity
	char *model;
	char *model2;
	char *classname;
	char *target;
	char *targetname;
	char *killtarget;
	char *team;
	char *pathtarget;
	char *message;
	char *map;                  // target_changelevel
	char *sounds;                   //make this a spawntemp var?
	int spawnflags;
	unsigned int nextThink;
	void ( *think )( edict_t *self );
	void ( *touch )( edict_t *self, edict_t *other, cplane_t *plane, int surfFlags );
	void ( *use )( edict_t *self, edict_t *other, edict_t *activator );
	void ( *pain )( edict_t *self, edict_t *other, float kick, int damage );
	void ( *die )( edict_t *self, edict_t *inflictor, edict_t *attacker, int damage, const vec3_t point );
	void ( *stop )( edict_t *self );
	edict_t	*target_ent;
	vec3_t velocity;
	vec3_t avelocity;
	unsigned int timeStamp;
	unsigned int deathTimeStamp;
	int timeDelta;              // SVF_PROJECTILE only. Used for 4D collision detection
	projectileinfo_t projectileInfo;	// specific for projectiles
	particles_edict_t particlesInfo;		// specific for ET_PARTICLES
	int dmg;
	int mass;
	unsigned int air_finished;
	edict_t	*goalentity;
	edict_t	*movetarget;
	unsigned int pain_debounce_time;
	int max_health;
	int gib_health;
	int deadflag;
	int viewheight;             // height above origin where eyesight is determined
	int takedamage;
	int count;
	unsigned int timeout; // for SW and fat PG
	int groundentity_linkcount;
	int noise_index;
	// timing variables
	int watertype;
	int waterlevel;
	int style;                  // also used as areaportal number
	float light;
	vec3_t color;
	gsitem_t	*item;              // for bonus items
	int invpak[AMMO_TOTAL];         // small inventory-like for dropped backpacks. Handles weapons and ammos of both types
	// common data blocks
	moveinfo_t moveinfo;        // func movers movement
	ai_handle_t ai;     //MbotGame
	snap_edict_t snap; // information that is cleared each frame snap
	//JALFIXME
	qboolean is_swim;
	qboolean is_step;
	qboolean is_ladder;
	qboolean was_swim;
	qboolean was_step;
	edict_t	*trigger_entity;
	unsigned int trigger_timeout;
	qboolean linked;
	int asRefCount, asFactored;
	qboolean scriptSpawned;
	const char *scriptModule;
	void *asSpawnFunc, *asThinkFunc, *asUseFunc, *asTouchFunc, *asPainFunc, *asDieFunc, *asStopFunc;
};
  package password is new Variable(VARIABLE_PREFIX & "");
  package operator_password is new Variable(VARIABLE_PREFIX & "");
  package select_empty is new Variable(VARIABLE_PREFIX & "");
  package dedicated is new Variable(VARIABLE_PREFIX & "");
  package developer is new Variable(VARIABLE_PREFIX & "");
  package filterban is new Variable(VARIABLE_PREFIX & "");
  package gravity is new Variable(VARIABLE_PREFIX & "");
  package maxvelocity is new Variable(VARIABLE_PREFIX & "");
  package cheats is new Variable(VARIABLE_PREFIX & "");
  package mapHeader is new Variable(VARIABLE_PREFIX & "");
  package mapVersion is new Variable(VARIABLE_PREFIX & "");
  package floodprotection_messages is new Variable(VARIABLE_PREFIX & "");
  package floodprotection_team is new Variable(VARIABLE_PREFIX & "");
  package floodprotection_seconds is new Variable(VARIABLE_PREFIX & "");
  package floodprotection_penalty is new Variable(VARIABLE_PREFIX & "");
  package inactivity_maxtime is new Variable(VARIABLE_PREFIX & "");
  package maplist is new Variable(VARIABLE_PREFIX & "");
  package maprotation is new Variable(VARIABLE_PREFIX & "");
  package enforce_map_pool is new Variable(VARIABLE_PREFIX & "");
  package map_pool is new Variable(VARIABLE_PREFIX & "");
  package scorelimit is new Variable(VARIABLE_PREFIX & "");
  package timelimit is new Variable(VARIABLE_PREFIX & "");
  package projectile_touch_owner is new Variable(VARIABLE_PREFIX & "");
  package projectile_prestep is new Variable(VARIABLE_PREFIX & "");
  package numbots is new Variable(VARIABLE_PREFIX & "");
  package maxtimeouts is new Variable(VARIABLE_PREFIX & "");
  package self_knockback is new Variable(VARIABLE_PREFIX & "");
  package knockback_scale is new Variable(VARIABLE_PREFIX & "");
  package allow_stun is new Variable(VARIABLE_PREFIX & "");
  package armor_degradation is new Variable(VARIABLE_PREFIX & "");
  package armor_protection is new Variable(VARIABLE_PREFIX & "");
  package allow_falldamage is new Variable(VARIABLE_PREFIX & "");
  package allow_selfdamage is new Variable(VARIABLE_PREFIX & "");
  package allow_teamdamage is new Variable(VARIABLE_PREFIX & "");
  package allow_bunny is new Variable(VARIABLE_PREFIX & "");
  package ammo_respawn is new Variable(VARIABLE_PREFIX & "");
  package weapon_respawn is new Variable(VARIABLE_PREFIX & "");
  package health_respawn is new Variable(VARIABLE_PREFIX & "");
  package armor_respawn is new Variable(VARIABLE_PREFIX & "");
  package respawn_delay_min is new Variable(VARIABLE_PREFIX & "");
  package respawn_delay_max is new Variable(VARIABLE_PREFIX & "");
  package deadbody_followkiller is new Variable(VARIABLE_PREFIX & "");
  package deadbody_autogib_delay is new Variable(VARIABLE_PREFIX & "");
  package challengers_queue is new Variable(VARIABLE_PREFIX & "");
  package antilatimenudge is new Variable(VARIABLE_PREFIX & "");
  package antilamaxtimedelta is new Variable(VARIABLE_PREFIX & "");
  package teams_maxplayers is new Variable(VARIABLE_PREFIX & "");
  package teams_allow_uneven is new Variable(VARIABLE_PREFIX & "");
  package autorecord is new Variable(VARIABLE_PREFIX & "");
  package autorecord_maxdemos is new Variable(VARIABLE_PREFIX & "");
  package allow_spectator_voting is new Variable(VARIABLE_PREFIX & "");
  package instagib is new Variable(VARIABLE_PREFIX & "");
  package instajump is new Variable(VARIABLE_PREFIX & "");
  package instashield is new Variable(VARIABLE_PREFIX & "");
  package asGC_stats is new Variable(VARIABLE_PREFIX & "");
  package asGC_interval is new Variable(VARIABLE_PREFIX & "");
  package skillRating is new Variable(VARIABLE_PREFIX & "");
private
	edict_t	*edicts;        // [maxentities]
	gclient_t *clients;     // [maxclients]
	gclient_quit_t *quits;	// [dynamic] <-- MM
	clientRating_t *ratings;	// list of ratings for current game and gametype <-- MM
	linear_allocator_t *raceruns;	// raceRun_t <-- MM
end Neo.World.Game;
  

#define	FOFS( x ) (size_t)&( ( (edict_t *)0 )->x )
#define	STOFS( x ) (size_t)&( ( (spawn_temp_t *)0 )->x )
#define	LLOFS( x ) (size_t)&( ( (level_locals_t *)0 )->x )
#define	CLOFS( x ) (size_t)&( ( (gclient_t *)0 )->x )


#define G_IsQ1Map() ( *cm_mapHeader->string == '\0' ? qtrue : qfalse )
#define G_IsQ2Map() ( !strcmp( cm_mapHeader->string, "IBSP" ) && cm_mapVersion->integer < 46 )
#define G_IsQ3Map() ( !strcmp( cm_mapHeader->string, "IBSP" ) && cm_mapVersion->integer >= 46 )

  type Record_Teams is record
    end record;
  type Record_Match is record
    end record;
  type Record_Spawn is record
    end record;
    
edict_t **G_Teams_ChallengersQueue( void );
void G_Teams_Join_Cmd( edict_t *ent );
qboolean G_Teams_JoinTeam( edict_t *ent, int team );
void G_Teams_UnInvitePlayer( int team, edict_t *ent );
void G_Teams_RemoveInvites( void );
qboolean G_Teams_TeamIsLocked( int team );
qboolean G_Teams_LockTeam( int team );
qboolean G_Teams_UnLockTeam( int team );
void G_Teams_Invite_f( edict_t *ent );
void G_Teams_UpdateMembersList( void );
qboolean G_Teams_JoinAnyTeam( edict_t *ent, qboolean silent );
void G_Teams_SetTeam( edict_t *ent, int team );

void Cmd_Say_f( edict_t *ent, qboolean arg0, qboolean checkflood );
void G_Say_Team( edict_t *who, char *msg, qboolean checkflood );

void G_Match_Ready( edict_t *ent );
void G_Match_NotReady( edict_t *ent );
void G_Match_ToggleReady( edict_t *ent );
void G_Match_CheckReadys( void );
void G_EndMatch( void );

void G_Teams_JoinChallengersQueue( edict_t *ent );
void G_Teams_LeaveChallengersQueue( edict_t *ent );
void G_InitChallengersQueue( void );

void G_MoveClientToPostMatchScoreBoards( edict_t *ent, edict_t *spawnpoint );
void G_Gametype_Init( void );
void G_Gametype_GenerateAllowedGametypesList( void );
qboolean G_Gametype_IsVotable( const char *name );
void G_Gametype_ScoreEvent( gclient_t *client, const char *score_event, const char *args );
void G_RunGametype( void );


void G_SpawnQueue_Init( void );
void G_SpawnQueue_SetTeamSpawnsystem( int team, int spawnsystem, int wave_time, int wave_maxcount, qboolean spectate_team );
int G_SpawnQueue_NextRespawnTime( int team );
void G_SpawnQueue_ResetTeamQueue( int team );
int G_SpawnQueue_GetSystem( int team );
void G_SpawnQueue_ReleaseTeamQueue( int team );
void G_SpawnQueue_AddClient( edict_t *ent );
void G_SpawnQueue_RemoveClient( edict_t *ent );
void G_SpawnQueue_Think( void );

edict_t *SelectDeathmatchSpawnPoint( edict_t *ent );
void SelectSpawnPoint( edict_t *ent, edict_t **spawnpoint, vec3_t origin, vec3_t angles );
edict_t *G_SelectIntermissionSpawnPoint( void );
float PlayersRangeFromSpot( edict_t *spot, int ignore_team );
void SP_info_player_start( edict_t *ent );
void SP_info_player_deathmatch( edict_t *ent );
void SP_info_player_intermission( edict_t *ent );

//
// g_func.c
//
void G_AssignMoverSounds( edict_t *ent, char *start, char *move, char *stop );
qboolean G_EntIsADoor( edict_t *ent );

void SP_func_plat( edict_t *ent );
void SP_func_rotating( edict_t *ent );
void SP_func_button( edict_t *ent );
void SP_func_door( edict_t *ent );
void SP_func_door_rotating( edict_t *ent );
void SP_func_door_secret( edict_t *self );
void SP_func_water( edict_t *self );
void SP_func_train( edict_t *ent );
void SP_func_conveyor( edict_t *self );
void SP_func_wall( edict_t *self );
void SP_func_object( edict_t *self );
void SP_func_explosive( edict_t *self );
void SP_func_killbox( edict_t *ent );
void SP_func_static( edict_t *ent );
void SP_func_bobbing( edict_t *ent );
void SP_func_pendulum( edict_t *ent );

//
// g_ascript.c
//
#define ANGEL_SCRIPT_EXTENSION				".as"

qboolean GT_asLoadScript( const char *gametypeName );
void GT_asShutdownScript( void );
void GT_asCallSpawn( void );
void GT_asCallMatchStateStarted( void );
qboolean GT_asCallMatchStateFinished( int incomingMatchState );
void GT_asCallThinkRules( void );
void GT_asCallPlayerKilled( edict_t *targ, edict_t *inflictor, edict_t *attacker, int damage, vec3_t point, int mod );
void GT_asCallPlayerRespawn( edict_t *ent, int old_team, int new_team );
void GT_asCallScoreEvent( gclient_t *client, const char *score_event, const char *args );
char *GT_asCallScoreboardMessage( unsigned int maxlen );
edict_t *GT_asCallSelectSpawnPoint( edict_t *ent );
qboolean GT_asCallGameCommand( gclient_t *client, const char *cmd, const char *args, int argc );
qboolean GT_asCallBotStatus( edict_t *ent );
void GT_asCallShutdown( void );

void G_asCallMapEntityThink( edict_t *ent );
void G_asCallMapEntityTouch( edict_t *ent, edict_t *other, cplane_t *plane, int surfFlags );
void G_asCallMapEntityUse( edict_t *ent, edict_t *other, edict_t *activator );
void G_asCallMapEntityPain( edict_t *ent, edict_t *other, float kick, float damage );
void G_asCallMapEntityDie( edict_t *ent, edict_t *inflicter, edict_t *attacker, int damage, const vec3_t point );
void G_asCallMapEntityStop( edict_t *ent );
void G_asResetEntityBehaviors( edict_t *ent );
void G_asClearEntityBehaviors( edict_t *ent );
void G_asReleaseEntityBehaviors( edict_t *ent );

qboolean G_asLoadMapScript( const char *mapname );
void G_asShutdownMapScript( void );
void G_asCallMapInit( void );
void G_asCallMapPreThink( void );
void G_asCallMapPostThink( void );
void G_asCallMapExit( void );

qboolean G_asCallMapEntitySpawnScript( const char *classname, edict_t *ent );

void G_asInitGameModuleEngine( void );
void G_asShutdownGameModuleEngine( void );
void G_asGarbageCollect( qboolean force );
void G_asDumpAPI_f( void );

#define world	( (edict_t *)game.edicts )

// item spawnflags
#define ITEM_TRIGGER_SPAWN	0x00000001
#define ITEM_NO_TOUCH		0x00000002
// 6 bits reserved for editor flags
// 8 bits used as power cube id bits for coop games
#define DROPPED_ITEM		0x00010000
#define	DROPPED_PLAYER_ITEM	0x00020000
#define ITEM_TARGETS_USED	0x00040000


extern const field_t fields[];


//
// g_cmds.c
//
void G_BOTvsay_f( edict_t *ent, char *msg, qboolean team );

//
// g_items.c
//
void DoRespawn( edict_t *ent );
void PrecacheItem( gsitem_t *it );
void G_PrecacheItems( void );
edict_t *Drop_Item( edict_t *ent, gsitem_t *item );
void SetRespawn( edict_t *ent, int delay );
void G_Items_RespawnByType( unsigned int typeMask, int item_tag, float delay );
void G_FireWeapon( edict_t *ent, int parm );
void SpawnItem( edict_t *ent, gsitem_t *item );
void G_Items_FinishSpawningItems( void );
void MegaHealth_think( edict_t *self );
int PowerArmorType( edict_t *ent );
gsitem_t *GetItemByTag( int tag );
qboolean Add_Ammo( gclient_t *client, gsitem_t *item, int count, qboolean add_it );
void Touch_ItemSound( edict_t *other, gsitem_t *item );
void Touch_Item( edict_t *ent, edict_t *other, cplane_t *plane, int surfFlags );
qboolean G_PickupItem( struct edict_s *ent, struct edict_s *other );
void G_UseItem( struct edict_s *ent, struct gitem_s *item );
void G_DropItem( struct edict_s *ent, struct gitem_s *item );
qboolean Add_Armor( edict_t *ent, edict_t *other, qboolean pick_it );

//
// g_utils.c
//
#define G_LEVELPOOL_BASE_SIZE	5 * 1024 * 1024

qboolean KillBox( edict_t *ent );
float LookAtKillerYAW( edict_t *self, edict_t *inflictor, edict_t *attacker );
edict_t *G_Find( edict_t *from, size_t fieldofs, char *match );
edict_t *findradius( edict_t *from, edict_t *to, vec3_t org, float rad );
edict_t *G_FindBoxInRadius( edict_t *from, edict_t *to, vec3_t org, float rad );
edict_t *G_PickTarget( char *targetname );
void G_UseTargets( edict_t *ent, edict_t *activator );
void G_SetMovedir( vec3_t angles, vec3_t movedir );
void G_InitMover( edict_t *ent );
void G_DropSpawnpointToFloor( edict_t *ent );

void G_InitState( edict_t *e );
edict_t *G_Spawn( void );
void G_FreeState( edict_t *e );

void G_LevelInitPool( size_t size );
void G_LevelFreePool( void );
void *_G_LevelMalloc( size_t size, const char *filename, int fileline );
void _G_LevelFree( void *data, const char *filename, int fileline );
char *_G_LevelCopyString( const char *in, const char *filename, int fileline );
void G_LevelGarbageCollect( void );

void G_StringPoolInit( void );
char *_G_RegisterLevelString( const char *string, const char *filename, int fileline );
#define G_RegisterLevelString( in ) _G_RegisterLevelString( in, __FILE__, __LINE__ )

char *G_ListNameForPosition( const char *namesList, int position, const char separator );
char *G_AllocCreateNamesList( const char *path, const char *extension, const char separator );

char *_G_CopyString( const char *in, const char *filename, int fileline );
#define G_CopyString( in ) _G_CopyString( in, __FILE__, __LINE__ )

void G_ProjectSource( vec3_t point, vec3_t distance, vec3_t forward, vec3_t right, vec3_t result );

void G_AddEvent( edict_t *ent, int event, int parm, qboolean highPriority );
edict_t *G_SpawnEvent( int event, int parm, vec3_t origin );
void G_TurnEntityIntoEvent( edict_t *ent, int event, int parm );

void G_CallThink( edict_t *ent );
void G_CallTouch( edict_t *self, edict_t *other, cplane_t *plane, int surfFlags );
void G_CallUse( edict_t *self, edict_t *other, edict_t *activator );
void G_CallStop( edict_t *self );
void G_CallPain( edict_t *ent, edict_t *attacker, float kick, float damage );
void G_CallDie( edict_t *ent, edict_t *inflictor, edict_t *attacker, int damage, const vec3_t point );

int G_PlayerGender( edict_t *player );

void G_PrintMsg( edict_t *ent, const char *format, ... );
void G_PrintChasersf( edict_t *self, const char *format, ... );
void G_ChatMsg( edict_t *ent, edict_t *who, qboolean teamonly, const char *format, ... );
void G_CenterPrintMsg( edict_t *ent, const char *format, ... );
void G_UpdatePlayerMatchMsg( edict_t *ent );
void G_UpdatePlayersMatchMsgs( void );
void G_Obituary( edict_t *victim, edict_t *attacker, int mod );

void G_Sound( edict_t *owner, int channel, int soundindex, float attenuation );
void G_PositionedSound( vec3_t origin, int channel, int soundindex, float attenuation );
void G_GlobalSound( int channel, int soundindex );
void G_LocalSound( edict_t *owner, int channel, int soundindex );

float vectoyaw( vec3_t vec );

void G_PureSound( const char *sound );
void G_PureModel( const char *model );

extern game_locals_t game;
#define ENTNUM( x ) ( ( x ) != NULL ? ( x ) - game.edicts : -1 )

#define PLAYERNUM( x ) ( ( x ) - game.edicts - 1 )
#define PLAYERENT( x ) ( game.edicts + ( x ) + 1 )
#define G_ISGHOSTING( x ) ( ( ( x )->s.modelindex == 0 ) && ( ( x )->r.solid == SOLID_NOT ) )
#define ISBRUSHMODEL( x ) ( ( ( x > 0 ) && ( (int)x < trap_CM_NumInlineModels() ) ) ? qtrue : qfalse )

void G_TeleportEffect( edict_t *ent, qboolean in );
void G_RespawnEffect( edict_t *ent );
qboolean G_Visible( edict_t *self, edict_t *other );
qboolean G_InFront( edict_t *self, edict_t *other );
qboolean G_EntNotBlocked( edict_t *viewer, edict_t *targ );
void G_DropToFloor( edict_t *ent );
int G_SolidMaskForEnt( edict_t *ent );
void G_CheckGround( edict_t *ent );
void G_CategorizePosition( edict_t *ent );
qboolean G_CheckBottom( edict_t *ent );
void G_ReleaseClientPSEvent( gclient_t *client );
void G_AddPlayerStateEvent( gclient_t *client, int event, int parm );
void G_ClearPlayerStateEvents( gclient_t *client );

// announcer events
void G_AnnouncerSound( edict_t *targ, int soundindex, int team, qboolean queued, edict_t *ignore );
edict_t *G_PlayerForText( const char *text );

void G_LoadFiredefsFromDisk( void );
void G_PrecacheWeapondef( int weapon, firedef_t *firedef );

void G_MapLocations_Init( void );
int G_RegisterMapLocationName( const char *name );
int G_MapLocationTAGForName( const char *name );
int G_MapLocationTAGForOrigin( const vec3_t origin );
void G_MapLocationNameForTAG( int tag, char *buf, size_t buflen );

void G_SetBoundsForSpanEntity( edict_t *ent, vec_t size );

//
// g_callvotes.c
//
void G_CallVotes_Init( void );

//
// g_trigger.c
//

//
// g_clip.c
//

int	G_PointContents( vec3_t p );
void	G_Trace( trace_t *tr, vec3_t start, vec3_t mins, vec3_t maxs, vec3_t end, edict_t *passedict, int contentmask );
int G_PointContents4D( vec3_t p, int timeDelta );
void G_Trace4D( trace_t *tr, vec3_t start, vec3_t mins, vec3_t maxs, vec3_t end, edict_t *passedict, int contentmask, int timeDelta );
void GClip_BackUpCollisionFrame( void );
edict_t *GClip_FindBoxInRadius4D( edict_t *from, vec3_t org, float rad, int timeDelta );
void G_SplashFrac4D( int entNum, vec3_t hitpoint, float maxradius, vec3_t pushdir, float *kickFrac, float *dmgFrac, int timeDelta );
void	GClip_ClearWorld( void );
void	GClip_SetBrushModel( edict_t *ent, char *name );
void	GClip_SetAreaPortalState( edict_t *ent, qboolean open );
void	GClip_LinkEntity( edict_t *ent );
void	GClip_UnlinkEntity( edict_t *ent );
void	GClip_TouchTriggers( edict_t *ent );
void G_PMoveTouchTriggers( pmove_t *pm );



//
// g_combat.c
//
void G_Killed( edict_t *targ, edict_t *inflictor, edict_t *attacker, int damage, const vec3_t point, int mod );
int G_ModToAmmo( int mod );
qboolean CheckTeamDamage( edict_t *targ, edict_t *attacker );
void G_SplashFrac( const vec3_t origin, const vec3_t mins, const vec3_t maxs, const vec3_t point, float maxradius, vec3_t pushdir, float *kickFrac, float *dmgFrac );
void G_Damage( edict_t *targ, edict_t *inflictor, edict_t *attacker, const vec3_t pushdir, const vec3_t dmgdir, const vec3_t point, float damage, float knockback, float stun, int dflags, int mod );
void G_RadiusDamage( edict_t *inflictor, edict_t *attacker, cplane_t *plane, edict_t *ignore, int mod );
		-40


//
// g_misc.c
//
void ThrowClientHead( edict_t *self, int damage );
void ThrowSmallPileOfGibs( edict_t *self, int damage );

void BecomeExplosion1( edict_t *self );



//
// g_weapon.c
//
void ThrowDebris( edict_t *self, int modelindex, float speed, vec3_t origin );
qboolean fire_hit( edict_t *self, vec3_t aim, int damage, int kick );
void G_HideLaser( edict_t *ent );

//
// g_chasecam	//newgametypes
//
void G_SpectatorMode( edict_t *ent );
void G_ChasePlayer( edict_t *ent, const char *name, qboolean teamonly, int followmode );
void G_ChaseStep( edict_t *ent, int step );
void Cmd_SwitchChaseCamMode_f( edict_t *ent );
void Cmd_ChaseCam_f( edict_t *ent );
void Cmd_Spec_f( edict_t *ent );
void G_EndServerFrames_UpdateChaseCam( void );

//
// g_client.c
//
 View
 Target
 Command
 HUD
 Object
//
// g_player.c
//
void player_pain( edict_t *self, edict_t *other, float kick, int damage );
void player_die( edict_t *self, edict_t *inflictor, edict_t *attacker, int damage, const vec3_t point );
void player_think( edict_t *self );

//
// g_target.c
//
void target_laser_start( edict_t *self );

void SP_target_temp_entity( edict_t *ent );
void SP_target_speaker( edict_t *ent );
void SP_target_explosion( edict_t *ent );
void SP_target_spawner( edict_t *ent );
void SP_target_crosslevel_trigger( edict_t *ent );
void SP_target_crosslevel_target( edict_t *ent );
void SP_target_laser( edict_t *self );
void SP_target_lightramp( edict_t *self );
void SP_target_earthquake( edict_t *ent );
void SP_target_string( edict_t *ent );
void SP_target_location( edict_t *self );
void SP_target_position( edict_t *self );
void SP_target_print( edict_t *self );
void SP_target_give( edict_t *self );
void SP_target_changelevel( edict_t *ent );

//
// g_svcmds.c
//
void SV_ResetPacketFiltersTimeouts( void );
qboolean SV_FilterPacket( char *from );
void G_AddServerCommands( void );
void G_RemoveCommands( void );
void SV_ReadIPList( void );
void SV_WriteIPList( void );

//
// p_view.c
//
void G_ClientEndSnapFrame( edict_t *ent );
void G_ClientAddDamageIndicatorImpact( gclient_t *client, int damage, const vec3_t dir );
void G_ClientDamageFeedback( edict_t *ent );

//
// p_hud.c
//

//scoreboards string
extern char scoreboardString[MAX_STRING_CHARS];
extern const unsigned int scoreboardInterval;

void MoveClientToIntermission( edict_t *client );
void G_SetClientStats( edict_t *ent );
void G_Snap_UpdateWeaponListMessages( void );
void G_ScoreboardMessage_AddSpectators( void );
void G_UpdateScoreBoardMessages( void );

//
// g_phys.c
//

//
// g_main.c
//

// memory management
#define G_Malloc( size ) trap_MemAlloc( size, __FILE__, __LINE__ )
#define G_Free( mem ) trap_MemFree( mem, __FILE__, __LINE__ )

#define	G_LevelMalloc( size ) _G_LevelMalloc( ( size ), __FILE__, __LINE__ )
#define	G_LevelFree( data ) _G_LevelFree( ( data ), __FILE__, __LINE__ )
#define	G_LevelCopyString( in ) _G_LevelCopyString( ( in ), __FILE__, __LINE__ )

int	G_API( void );
void	G_Error( const char *format, ... );
void	G_Printf( const char *format, ... );
void	G_Init( unsigned int seed, unsigned int framemsec, int protocol );
void	G_Shutdown( void );
void	G_ExitLevel( void );
void G_RestartLevel( void );
game_state_t *G_GetGameState( void );
void	G_Timeout_Reset( void );

qboolean    G_AllowDownload( edict_t *ent, const char *requestname, const char *uploadname );

//
// g_frame.c
//
void G_CheckCvars( void );
void G_RunFrame( unsigned int msec, unsigned int serverTime );
void G_SnapClients( void );
void G_ClearSnap( void );
void G_SnapFrame( void );

  type Record_ is record
//
// g_spawn.c
//;
  type Record_ is record
//
// g_awards.c
//
