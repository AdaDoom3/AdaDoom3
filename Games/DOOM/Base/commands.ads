--                                                                                                                                      --
-- Neo is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the --
-- Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                    --
--                                                                                                                                      --
-- Neo is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.                            --
--                                                                                                                                      --
-- You should have received a copy of the GNU General Public License along with Neo. If not, see gnu.org/licenses                       --
--                                                                                                                                      --

package body Mani is
mani_adverts		on -- advertising insertion on
off -- advertising on disabling
mani_time_between_adverts
120
Time interval of commercials in seconds
mani_adverts_chat_area		on -- show ads in the chat, off -- do not show
mani_adverts_top_left		on -- display advertising in the upper left corner, off -- do not show
mani_advert_col
0

0

255
Red / green / blue color in advertising
(255 -- max) per. colour
mani_advert_dead_only		on -- Only dead players see the adverts,
off -- All players see the adverts
mani_adverts_bottom_area		Displays the adverts as a text box in the lower center of CS: S.
Modules: 02 Stats
mani_stats		on -- statistics enabled, off -- disabled
mani_stats_mode		on -- calculate statistics at the end of each round,
off -- statistics will not be calculated
mani_stats_drop_player_days
5
Player is eliminated from the ranking
if he was not on the server for X days
mani_stats_calculate		0 -- Stats to Kills
1 -- Stats to Kill / Death Ratio
2 -- Kills - Deaths
3 -- Stats to Points (victim_points / killer_points) * multiplier
mani_stats_kills_required
0
Required kills to participate in the ranking
mani_stats_kills_before_points_removed
0
Number of kills until the rank is reset
mani_stats_top_display_time
10
Time in seconds that the Top10 remain displayed (5 - 30)
mani_stats_show_rank_to_all		on -- Rank is displayed to all players,
off -- Only player who entered it
mani_stats_alternative_rank_message
Chat message issued by the Rank command
mani_stats_write_text_file		a -- create mani_rank.txt
to export rankings eg to a website
mani_stats_calculate_frequency
0
Pause in minutes between the calculation of stats,
0 -- only for mapchange
mani_stats_write_frequency_to_disk
0
Pause in minutes between saving the stats,
0 -- only at Mapchange
mani_stats_by_steam_id		on -- determine stats via SteamID,
off -- SteamIDs will not be used (LAN)
mani_stats_include_bot_kills		on -- put bot kills in the stats,
off -- do not put kills on the stats
mani_stats_decay_start
2
Number of days after the points start to decay
mani_stats_decay_period
7
No information
mani_stats_decay_restore_points_on_
connect		No information
mani_stats_points_add_only		on -- Punks never expire (eg as in BF2)
mani_stats_ignore_ranks_after_x_days
21
X - days after the last connect
from which one becomes invisible in the ranking.
mani_stats_points_multiplier
5.0
Multiplier for points allocation (default -- 5.0)
mani_stats_points_death_multiplier
1.0
No information
mani_stats_css_weapon_ak47
1.0
Kill points multiplier for the "ak47" in CS: S
mani_stats_css_weapon_m4a1
1.0
Kill points multiplier for the "m4a1" in CS: S
mani_stats_css_weapon_mp5navy
1.2
Kill points multiplier for the "mp5navy" in CS: S
mani_stats_css_weapon_awp
1.0
Kill points multiplier for the "awp" in CS: S
mani_stats_css_weapon_usp
1.4
Kill points multiplier for the "usp" in CS: S
mani_stats_css_weapon_deagle
1.2
Kill points multiplier for the "deagle" in CS: S
mani_stats_css_weapon_aug
1.0
Kill points multiplier for the "aug" in CS: S
mani_stats_css_weapon_hegrenade
1.8
Kill points Multiplier for the "hegrenade" in CS: S
mani_stats_css_weapon_xm1014
1.1
Kill points multiplier for the "xm1014" in CS: S
mani_stats_css_weapon_knife
2.0
Kill Points Multiplier for the "knife" in CS: S
mani_stats_css_weapon_g3sg1
0.8
Kill points multiplier for the "g3sg1" in CS: S
mani_stats_css_weapon_sg550
0.8
Kill points multiplier for the "sg550" in CS: S
mani_stats_css_weapon_galil
1.1
Kill Points multiplier for the "galil" in CS: S
mani_stats_css_weapon_m3
1.2
Kill points multiplier for the "m3" in CS: S
mani_stats_css_weapon_scout
1.1
Kill points multiplier for the "scout" in CS: S
mani_stats_css_weapon_sg552
1.0
Kill points multiplier for the "sg552" in CS: S
mani_stats_css_weapon_famas
1.0
Kill points multiplier for the "famas" in CS: S
mani_stats_css_weapon_glock
1.4
Kill points Multiplier for the "glock" in CS: S
mani_stats_css_weapon_tmp
1.5
Kill points multiplier for the "tmp" in CS: S
mani_stats_css_weapon_ump45
1.2
Kill points multiplier for the "ump45" in CS: S
mani_stats_css_weapon_p90
1.2
Kill points multiplier for the "p90" in CS: S
mani_stats_css_weapon_m249
1.2
Kill points multiplier for the "m249" in CS: S
mani_stats_css_weapon_elite
1.4
Kill points Multiplier for the "elite" in CS: S
mani_stats_css_weapon_mac10
1.5
Kill points multiplier for the "mac10" in CS: S
mani_stats_css_weapon_fiveseven
1.5
Kill points multiplier for the "fiveseven" in CS: S
mani_stats_css_weapon_p228
1.5
Kill points multiplier for the "p228" in CS: S
mani_stats_css_weapon_flashbang
5.0
Kill Points Multiplier for the "flashbang" in CS: S
mani_stats_css_weapon_smokegrenade
5.0
Kill points multiplier for the "smokegrenade" in CS: S
mani_stats_css_bomb_planted_bonus
10
Points you get for putting the bomb in CS: S
mani_stats_css_bomb_defused_bonus
10
Points you get for disarming the bomb in CS: S
mani_stats_css_hostage_rescued_bonus
5
Points you get for saving a hostage in CS: S
mani_stats_css_hostage_killed_bonus
-15
Points you get for killing a hostage in CS: S
mani_stats_css_vip_escape_bonus
4
Points you get for saving a VIP in CS: S
mani_stats_css_vip_killed_bonus
10
Points you get for killing a VIP in CS: S
mani_stats_css_ct_eliminated_team_bonus
2
Points you get as T when all CT's are eliminated
mani_stats_css_t_eliminated_team_bonus
2
Points you get as CT when all T's are eliminated
mani_stats_css_ct_vip_escaped_team_
bonus
10
Points that you get as CT when all VIP's are rescued
mani_stats_css_t_vip_assassinated_team_
bonus
6
Points you get as T when all VIP's are eliminated
mani_stats_css_t_target_bombed_team_
bonus
5
Points you get as T when the bomb explodes
mani_stats_css_ct_all_hostages_rescued_
team_bonus
10
Points you get as CT when all hostages are rescued
mani_stats_css_ct_bomb_defused_team_
bonus
5
Points that you get as CT when the bomb is defused
mani_stats_css_ct_hostage_killed_team_
bonus
1
Points you get as CT when a T kills a hostage
mani_stats_css_ct_hostage_rescued_team_
bonus
1
Points you get as CT when a hostage is rescued
mani_stats_css_t_bomb_planted_team_
bonus
2
Points you get as T when the bomb is put
mani_stats_dods_weapon_amerknife
3.0
Kill Points Multiplier for the "amerknife" in DOD: S
mani_stats_dods_weapon_spade
3.0
Kill points multiplier for the "spade" in DOD: S
mani_stats_dods_weapon_colt
1.6
Kill points multiplier for the "colt" in DOD: S
mani_stats_dods_weapon_p38
1.5
Kill points multiplier for the "p38" in DOD: S
mani_stats_dods_weapon_c96
1.5
Kill points multiplier for the "c96" in DOD: S
mani_stats_dods_weapon_garand
1.3
Kill Points Multiplier for the "garande" in DOD: S
mani_stats_dods_weapon_m1carbine
1.2
Kill Points Multiplier for the "m1carbine" in DOD: S
mani_stats_dods_weapon_k98
1.3
Kill points multiplier for the "k98" in DOD: S
mani_stats_dods_weapon_spring
1.5
Kill points multiplier for the "spring" in DOD: S
mani_stats_dods_weapon_k98_scoped
1.5
Kill points multiplier for the "k98_scoped" in DOD: S
mani_stats_dods_weapon_thompson
1.25
Kill Points Multiplier for the "thompson" in DOD: S
mani_stats_dods_weapon_mp40
1.25
Kill points multiplier for the "mp40" in DOD: S
mani_stats_dods_weapon_mp44
1.35
Kill points multiplier for the "mp44" in DOD: S
mani_stats_dods_weapon_bar
1.2
Kill points multiplier for the "bar" in DOD: S
mani_stats_dods_weapon_30cal
1.25
Kill Points Multiplier for the "30cal" in DOD: S
mani_stats_dods_weapon_mg42
1.2
Kill points multiplier for the "mg42" in DOD: S
mani_stats_dods_weapon_bazooka
2.25
Kill Points multiplier for the "bazooka" in DOD: S
mani_stats_dods_weapon_pschreck
2.25
Kill Points multiplier for the "pschreck" in DOD: S
mani_stats_dods_weapon_frag_us
1.0
Kill points multiplier for the "frag_us" in DOD: S
mani_stats_dods_weapon_frag_ger
1.0
Kill points multiplier for the "frag_ger" in DOD: S
mani_stats_dods_weapon_smoke_us
5.0
Kill Points Multiplier for the "smoke_us" in DOD: S
mani_stats_dods_weapon_smoke_ger
5.0
Kill points multiplier for the "smoke_ger" in DOD: S
mani_stats_dods_weapon_riflegren_us
1.3
Kill Points Multiplier for the "riflegren_us" in DOD: S
mani_stats_dods_weapon_riflegren_ger
1.3
Kill Points Multiplier for the "riflegren_ger" in DOD: S
mani_stats_dods_weapon_punch
3.0
Kill points multiplier for the "punch" in DOD: S
mani_stats_dods_capture_point
4
Points you get for conquering a flag in DOD: S
mani_stats_dods_block_capture
4
Points you get for defending a flag in DOD: S
mani_stats_dods_round_win_bonus
4
Points you get for winning a round in DOD: S
Modules: 03 Victim Stats
mani_show_victim_stats		on -- show damage statistics on death
mani_show_victim_stats_inflicted_only		a -- damage added to itself was not displayed
mani_player_settings_damage		Default setting for players on the server:
0 -- display off,
1 -- partial
display on 2 -- full display with hit groups (arm, head, etc.)
3 -- partial display in the GUI window
Modules: 04 Most Destructive
mani_stats_most_destructive		on -- Allows players to be displayed at the end of a round who
caused the most damage in the round
mani_stats_most_destructive_mode		on -- display after kills, off -- display after damage
mani_player_settings_destructive		on -- The default setting in the Settings menu
for the "mani_stats_most_destructive" display is on
Modules: 05 Team Kill / Wound Protection
mani_tk_protection		on -- enable TeamKill protection,
off -- disable TeamKill protection
mani_tk_forgive		TeamKill Punishment Menu
mani_tk_spawn_time
5
Time the TeamKill protection lasts after the Freezetime
mani_tk_allow_bots_to_punish		On -- Bots are allowed to use TeamKill Punish on players
mani_tk_allow_bots_to_add_violations		on -- the player's TeamKill counter is increased
when he kills a team bot
mani_tk_offences_for_ban
7
TeamKills until a player is banned
mani_tk_ban_time
5
Time in minutes that a TeamKill Ban lasts. 0 -- forever
mani_tk_slap_on_team_wound		on -- player receives slap on TeamWound
mani_tk_slap_on_team_wound_damage
0
Damage value at Slap by TeamWound
mani_tk_show_opposite_team_wound		Shows all players on the server TA's, off -- normal css style
mani_tk_add_violation_without_forgive		on -- TeamKill counter is raised
even if TeamKill has been awarded
mani_tk_allow_forgive_option		on -- Activate the Forgive option in the TeamKill Punish Menu
mani_tk_allow_blind_option		on -- Players are allowed to blind TeamKiller
mani_tk_blind_amount
253
Indicates how much the vision is limited
in blindness (0-255, higher -- stronger)
mani_tk_allow_slap_option		on -- Activate the Slap option in the TeamKill Punish Menu
mani_tk_allow_cash_option		on -- Activate the Cash option in the TeamKill Punish Menu
mani_tk_slap_to_damage
10
Damage value that a TeamKiller receives after Slap
mani_tk_cash_percent
30
Percentage obtained from the money of a team killer
mani_tk_allow_freeze_option		on -- Activate the Freeze option in the TeamKill Punish Menu
mani_tk_allow_drugged_option		on -- Activate the drug option in the TeamKill Punish Menu
mani_tk_allow_burn_option		on -- Activate the Burn option in the TeamKill Punish Menu
mani_tk_burn_time
100
Time in seconds a player burns after a TeamKill
mani_tk_allow_slay_option		on -- Activate the Slay option in the TeamKill Punish Menu
mani_tk_team_wound_reflect		On -- Team Wound Reflect turned on
mani_tk_team_wound_reflect_threshold
5
Number of team wounds per map a player is penalized for
mani_tk_team_wound_reflect_ratio
1.0
Multiplier for Team Wound Reflect
1 -- player gets damage he caused
2 -- player gets double damage
X -- player gets damage done X times
mani_tk_team_wound_reflect_ratio_
increase
0.1
Value by which mani_tk_team_wound_reflect_ratio is increased
with each team wound .
mani_tk_allow_time_bomb_option		on -- Enables the Time bomb option in the TeamKill menu
mani_tk_time_bomb_seconds
10
Sets the seconds until the time bomb explodes
mani_tk_time_bomb_blast_radius
1000
Sets the explosion radius of the time bomb
mani_tk_time_bomb_show_beams		on -- indicates rays, off -- indicates no rays
mani_tk_time_bomb_blast_mode		0 -- only hunts the team killer
1 -- hunts up all team members in the explosion radius
2 -- hunts everyone up in the explosion radius
mani_tk_time_bomb_beep_radius
0
Radius of the Beep circle of the time bomb
0 -- radius corresponds to the explosion radius, default value is 256
mani_tk_allow_fire_bomb_option		on -- Enables the option Napalm bomb in the TeamKill menu
mani_tk_fire_bomb_seconds
10
Sets the seconds until the napalm bomb explodes
mani_tk_fire_bomb_blast_radius
1000
Sets the explosion radius of the napalm bomb
mani_tk_fire_bomb_show_beams		on -- indicates rays, off -- indicates no rays
mani_tk_fire_bomb_blast_mode		0 -- ignites only the team killer
1 -- ignites all team members in the blast radius
2 -- ignites all in the blast radius
mani_tk_fire_bomb_burn_time
100
Sets the duration of burning in seconds
mani_tk_fire_bomb_beep_radius
0
Radius of the beep circle of the napalm bomb
0 -- radius corresponds to the explosion radius, default value is 256
mani_tk_allow_freeze_bomb_option		on -- Activates the option Frostbomb in the TeamKill menu
mani_tk_freeze_bomb_seconds
10
Sets the seconds until the explosion of the freeze bomb
mani_tk_freeze_bomb_blast_radius
1000
Sets the explosion radius of the frost bomb
mani_tk_freeze_bomb_show_beams		on -- indicates rays, off -- indicates no rays
mani_tk_freeze_bomb_blast_mode		0 -- only freezes the team killer
1 -- freezes all team members in the explosion radius
2 -- sets all in the explosion radius
mani_tk_freeze_bomb_beep_radius
0
Radius of the beep circle of the freeze bomb,
0 -- radius corresponds to the explosion radius, default value is 256
mani_tk_allow_beacon_option		Determines whether the napalm bomb may be used in the TK Menu
mani_tk_beacon_radius
384
Radius of the beacon circle
Modules: 06 Reserve Slot
mani_reserve_slots		on -- reserve slots on, off -- do not reserve slots
mani_reserve_slots_number_of_slots
1
Number of reserve slots
mani_reserve_slots_kick_message
You were disconnected for using a reserve slot
Message for kick because of reserve slots
mani_reserve_slots_redirect_message
This server is full, you are being redirected to another one of our servers
Message issued by the redirect through reserve slot
mani_reserve_slots_redirect
IP of the server for redirect.
Leave blank to turn Redirect off
mani_reserve_slots_allow_slot_fill		on -- It can be connected to a reserve slot
off -- Player is kicked on Connect on Reserver Slot
mani_reserve_slots_kick_method		Kick method if admin on reserve slot connected
= kicks in players with the highest ping
= kicks the player who came on the server the last time
Spectators are kicked first
mani_reserve_slots_ip_keep_history
14
No information
mani_reserve_slots_enforce_password		No information
mani_reserve_slots_include_admin		a -- Admins that are in the adminlist.txt
have the right to connect to reserve slot
Modules: 07 High Ping Kick
mani_high_ping_kick		on -- Enables high ping kick, off -- no high ping kick
mani_high_ping_kick_ping_limit
400
Players are kicked off a ping of X ms
mani_high_ping_kick_samples_required
60
Distance of the ping review. (1 sample -- 1.5 seconds)
mani_high_ping_kick_message
Your ping is too high
Message on kick because of too high ping
Modules: 08 Admin Action Messages
mani_adminslap_anonymous		off -- player see the name of the admin of the Slapt, on
= only admins see the name
mani_adminblind_anonymous		off -- player see name of admin over others Blind, on
= only admins see the name
mani_adminfreeze_anonymous		off -- player see name of admins the other freezes, on
= admins only see the name
mani_adminteleport_anonymous		off -- player see name of the admin of the teleported, on
= only admins see the name
mani_admindrug_anonymous		off -- player see name of admin the other one drug't,
one -- only admins see the name
mani_adminmap_anonymous		off -- player see name of the admin who changes a map, on
= only admins see the name
mani_adminswap_anonymous		off -- player see name of admin the player shifts, on
= only admins see the name
mani_adminvote_anonymous		off -- player see name of the admins initiating a vote, on
= admins only see the name
mani_adminsay_anonymous		off -- player see admin's name at Adminsay, on
= admins only see the name
mani_adminkick_anonymous		off -- player see name of the admin of the kicker, on
= only admins see the name
mani_adminslay_anonymous		off -- player see name of admin at Slay, on
= only admins see the name
mani_adminban_anonymous		off -- player see name of admin at Ban,
a -- Only admins see the name
mani_adminburn_anonymous		off -- player see the name of the admin when burning, on
= only admins see the name.
mani_adminnoclip_anonymous		off -- see player name of admin at NoClip, on
= admins only see the name
mani_adminmute_anonymous		off -- player see admin name on mute, on
= admins only see the name
mani_admincash_anonymous		off -- player see name of admin in case of money allocation, on
= only admins see the name
mani_adminsetskin_anonymous		off -- player see name of the admin at Skin Put, on
= Only admins see the name
mani_admindropc4_anonymous		off -- player see name of admin at C4 Drop,
a -- Only admins see the name
mani_admintimebomb_anonymous		off -- player see name of admin at time bomb, on
= only admins see the name
mani_adminfirebomb_anonymous		off -- player see name of admin at napalm bomb, on
= admins only see the name
mani_adminfreezebomb_anonymous		off -- player see name of the admin at Frostbomb, on
= only admins see the name
mani_adminhealth_anonymous		off -- see player name of the other HP's admins there, on
= only admins see the name
mani_adminbeacon_anonymous		off -- players see the name of the admin at the beacon command, on
= admins only see the name
mani_admingravity_anonymous		off -- players see the name of the admin at the Gravity command, on
= only admins see the name
Modules: 09 Chat Flooding Control
mani_chat_flood_time
0
Chat Spam Protection. 0 -- no protection, x -- x seconds
mani_chat_flood_message
STOP SPAMMING THE SERVER !!
Message to the spammer
Modules: 10 Basic Auto Balance Teams
mani_autobalance_teams		on -- Auto Team Balance at the end of a round
(Alternative to CS: Source Balancer)
mani_autobalance_mode		0 -- player is changed whether dead or alive
1 -- dead player first
2 -- dead player only
Modules: 11 Current Time Display
mani_military_time		one -- 24 hours, off -- 12 hours time
mani_thetime_timezone
GMT
Time zone of the server displayed next to the clock.
Leave empty to show no time zone.
mani_adjust_time
0
Correct Time of Mani Admin Plugin (+/- X Minutes)
Modules: 12 Voting Functionality
mani_voting		a -- allows the voting process, off -- forbids it
mani_vote_dont_show_last_maps
3
Determines the number of recent maps
that are not displayed in random Vote lists
mani_vote_extend_time
20
Time in minutes that
adds an Extended Vote to the remaining time
mani_vote_allow_extend		on -- time can be extended by Extended Vote
mani_vote_allowed_voting_time
45
Duration of a vote in seconds
mani_vote_allow_end_of_map_vote		on -- A random voting is started at the end of a map
mani_vote_max_extends
2
Number of extensions per map. 0 -- infinite
mani_vote_extend_rounds
10
Number of laps to be extended if mp_winlimit is not 0
mani_vote_mapcycle_mode_for_
random_map_vote		Determines which maplist generates the random vote:
0 -- mapcycle.txt
1 -- votemapslist.txt
2 -- maplist.txt
mani_vote_mapcycle_mode_for_
admin_map_vote		determines from which Maplist the admin can make a vote:
0 -- mapcycle.txt
1 -- votemapslist.txt
2 -- maplist.txt
mani_vote_time_before_end_of_
map_vote
3
Time in minutes before the end of the map
before starting a Random Map Vote
mani_vote_max_maps_for_end_of_
map_vote
6
Sets the number of available maps
mani_vote_end_of_map_swap_team		Allows a Team Change on Map Extension Votes (CSS Only)
mani_vote_end_of_map_percent_required
60
Percentage of end-of-map voting required
mani_vote_rcon_percent_required
60
Percentage of Rcon voting required
mani_vote_question_percent_required
60
Percentage of a question vote needed
mani_vote_map_percent_required
60
Percentage of map voting required
mani_vote_random_map_percent_required
60
Percentage of Random Map Voting needed
mani_vote_show_vote_mode		Vote Display:
0 -- no display
1 -- only name
2 -- only voices
3 -- names and voices
mani_vote_dont_show_if_alive		eus -- live players will see the vote menu
= live players must enter for the menu "vote"
mani_vote_allow_user_vote_map		on -- Players are allowed to start Map Vote off
= Players are not allowed to start a Map Vote
mani_vote_allow_user_vote_map_extend		on -- player votes may extend map time
= players may not extend map time
mani_vote_allow_user_vote_kick		on -- Players are allowed to start Kick Vote off
= Players are not allowed to start Kick Vote
mani_vote_allow_user_vote_ban		on -- Players are allowed to start Ban Vote off
= Players are not allowed to start Ban Vote
mani_vote_extend_percent_required
60
Percentage of an extend map vote required
mani_vote_user_vote_map_percentage
60
Percentage of a map change vote required
mani_vote_user_vote_map_time_before_
vote
60
Time after the map start in which a vote is allowed
mani_vote_user_vote_map_minimum_
votes
4
Number of votes required for a map change
mani_vote_user_vote_kick_mode		If Votekick on, then:
off -- only if there is no admin on the server, on
= no matter if an admin is on the server
mani_vote_user_vote_kick_percentage
60
Percentage of kick-voting required
mani_vote_user_vote_kick_time_before_
vote
60
Time after the map start in which a votekick is allowed
mani_vote_user_vote_kick_minimum_
votes
4
Number of votes the min. needed for a kick
mani_vote_user_vote_ban_mode		If Voteban on, then:
off -- only if there is no admin on the server, on
= no matter if an admin is on the server
mani_vote_user_vote_ban_percentage
60
Percentage of ban vote needed
mani_vote_user_vote_ban_time_before
_vote
60
Time after the map start in which a Voteban is allowed
mani_vote_user_vote_ban_minimum_votes
4
Number of votes the min. needed for a ban
mani_vote_user_vote_ban_time
30
Duration of the ban in minutes. 0 -- forever
mani_vote_user_vote_ban_type		Ban Mode:
0 -- over ID
1 -- over IP
2 -- over ID and IP
mani_vote_allow_rock_the_vote		a -- allow "rock the vote", off -- do not allow
mani_vote_rock_the_vote_percent_
required
60
Percentage of a "Rock the Vote" vote needed
mani_vote_time_before_rock_the_vote
120
Time after the map start after the "rock the vote" is allowed
mani_vote_rock_the_vote_number_of_
nominations
4
Number of nominations included in the vote
mani_vote_rock_the_vote_number_of_
maps
8
Number of random maps,
which are integrated into the vote from the votemaplist.txt
mani_vote_rock_the_vote_threshold_
percent
60
Percentage of players who need to enter "rockthevote"
before "rock the vote" starts
mani_vote_rock_the_vote_threshold_
minimum
4
Number of players who need to enter at least "rockthevote"
mani_player_settings_vote_progress		Vote display default (Settings menu): on
= Show , off -- Hide
Modules: 13 word filters
mani_filter_words_mode		output of word filter:
0 -- off, 1 -- warning to player,
2 -- show filtered text (****)
mani_filter_words_warning
SWEARING IS NOT ALLOWED ON THIS SERVER !!!
Text with word filter warning
Modules: 14 Sounds Control
mani_sounds_per_round
0
Sounds a player can play per round
mani_sounds_filter_if_dead		on -- Living players will not hear sounds from dead players
mani_sounds_auto_download		Download server sounds automatically.
on -- download, off -- do not download
mani_player_settings_sounds		Default settings for players on the server on
= sounds on, off -- sounds off
mani_play_sound_type		on -- Only 1 sound at a time can be played
off -- Sounds can overlap
Modules: 15 Plugin Logging
mani_log_mode		Logging Mode:
0 -- Default Valve Log Directory
1 -- Logs in mani_log_directory with default Valve Filenames
2 -- One big file in mani_log_directory
3 -- For each Admin a STEAM_x_x_xxxxxxx.log is written
mani_log_directory
mani_logs
Directory in which the logs are stored
Modules: 16 Death Beams
mani_show_death_beams		off -- death rays are not displayed globally on
= show death rays only if the setting is correct
mani_player_settings_death_beam		Death rays default: on
= show, off -- hide
Modules: 17 Anti IP Ghosting
mani_blind_ghosters		off -- Do not blind blind players with the same IP, on
= Blind players
mani_vote_allow_user_vote_ban_ghost		on -- players with the same IP can use voteban multiple times,
off -- players with the same IP can not.
mani_vote_allow_user_vote_kick_ghost		on -- players with the same IP can use votekick multiple times,
off -- players with the same IP can not
Modules: 18 Decal Map Adverts
mani_map_adverts		on
= turn on map advertising, off -- turn off map advertising
mani_map_adverts_in_war		on -- enable map advertising also in war mode,
off -- do not allow map advertising in war mode.
Modules: 19 Anti-Name Hacking
mani_player_name_change_threshold
15
Number of Allowed Name Changes
mani_player_name_change_reset		on -- counter is reset after each map,
off -- Name Change counter is reset after each round.
mani_player_name_change_punishment		Penalty for too many Name Changes:
0 -- kick,
1 -- Ban over ID,
2 -- Ban over IP,
3 -- Ban over ID and IP
mani_player_name_change_ban_time
0
Duration of the ban in minutes. 0 -- forever
Modules: 20 extra spawnpoints
mani_spawnpoints_mode		on -- used extra spawnpoints from "spawnpoints.txt",
off -- did not use extra spawnpoints
Modules: 21 Spray Tag Tracking
mani_spray_tag		on -- spray day monitoring on,
off -- no spray day monitoring
mani_spray_tag_spray_duration
120
Time interval in seconds in which the monitoring takes place
mani_spray_tag_spray_distance_limit
500
No information
mani_spray_tag_spray_highlight		Effect at Logo identification:
0 -- no
1 -- beam (standard glow for DoD)
2 -- glow
mani_spray_tag_ban_time
60
Duration of the banana in minutes. because of too many sprays, 0 -- forever
mani_spray_tag_warning_message
Please stop using your spray
Bewarnnachricht because of too many sprays
mani_spray_tag_kick_message
You have been kicked for using an offensive spray
Message for kick because of too many sprays
mani_spray_tag_ban_message
You have been banned for 60 minutes through using an offensive spray
Message for time-limited ban because of too many sprays
mani_spray_tag_perm_ban_message
You have been permanently banned for using an offensive spray
Message for Permanent Ban because of too many sprays
mani_spray_tag_block_mode		on -- Blocks all spray actions ("mani_spray_tag" must be 1),
off -- Allows spray actions on the server
mani_spray_tag_block_message
Sprays are blocked on this server !!
Warning message when spray actions are blocked
mani_spray_tag_slap_damage
0
Damage value that you get in a blocked spray action
Modules: 22 warmup timer
mani_warmup_timer
0
Whyup time in seconds to Maprestart
0 -- No Whyup
mani_warmup_timer_show_countdown		on -- indicates the remaining time in a downdown,
off -- no downdown
mani_warmup_timer_knives_only		a -- Only knives in the warmup round are allowed,
off -- all non-restricted weapons are allowed.
mani_warmup_timer_knives_respawn		No information
mani_warmup_timer_ignore_tk
on -- teamkills are ignored during warmup, off -- teamkills are not ignored
mani_warmup_timer_knives_only_ignore_
fyi_aim_maps		on -- "mani_warmup_timer_knives_only" is set to "off" for
fy_ and aim_ maps
mani_warmup_timer_unlimited_grenades		a -- players have infinite HE grenades in the warmup round
mani_warmup_timer_spawn_item_1
item_assaultsuit
1. weapon automatically gets the player in the warm-up round.
Several details must be separated with a ":".
eg "weapon_xm1014: weapon_usp: weapon_ump45" etc
mani_warmup_timer_spawn_item_2
2. Weapon automatically gets the player in the warm-up round.
Several details must be separated with a ":".
eg "weapon_tmp: weapon_scout: weapon_p90" etc
mani_warmup_timer_spawn_item_3
3. Weapon automatically gets the player in the warm-up round.
Several details must be separated with a ":".
eg "weapon_aug: weapon_p228: weapon_mp5navy" etc
mani_warmup_timer_spawn_item_4
4. weapon automatically gets the player in the warm-up round.
Several details must be separated with a ":".
eg "weapon_mac10: weapon_m4a1: weapon_m3" etc
mani_warmup_timer_spawn_item_5
5. Weapon the player gets automatically in the warm-up round.
Several details must be separated with a ":".
eg "weapon_m249: weapon_glock: weapon_galil" etc
mani_warmup_timer_disable_ff		on -- Disables Frendlyfire in the Wurmup Round
mani_warmup_infinite_ammo		On -- Infinite ammo in the Wurmup Round
Modules: 23 Menu Options
mani_use_amx_style_menu		on -- AMX style admin menu,
off -- escape style admin menu
mani_sort_menus		on -- Alphabetic menu sorting,
off -- No menu sorting
Modules: 24 External Logging
mani_external_stats_log		Allows the external logging of stats,
(eg needed for extra options on HlstatsX)
mani_external_stats_log_allow_war_logs		on -- It will also be logged externally in war mode,
off -- it will not be logged externally in war mode
mani_external_stats_css_include_bots		on -- include bots in in the external log,
off -- ignore Bot's external logs
Modules: 25 Save Scores
mani_save_scores		No information
mani_save_scores_tracking_time
5
No information
mani_save_scores_css_cash		No information
Modules: 26 Auto Join Restriction
mani_team_join_force_auto		on -- Join Team only chooses the car selection
= player can choose the team
mani_team_join_keep_same_team		on -- When a player reconnects,
he automatically returns to his team
Modules: 27 Steam ID Pending Kicker
mani_steam_id_pending_timeout
0
Time to kick with fluctuating Steam ID
mani_steam_id_pending_show_admin		Message at kick because of fluctuating Steam ID to the admin
Modules: 28 AFK Managers
mani_afk_kicker		on -- AFK Manager On, Off -- AFK Manager off
mani_afk_kicker_mode		off -- Kicks Spectators first off the server
mani_afk_kicker_alive_rounds
0
Number of laps until an AFK player is
kicked off the server or to the Spectators, 0 -- off
mani_afk_kicker_spectator_rounds
0
Number of laps until a Spectator is kicked off the server,
0 -- off
mani_afk_kicker_alive_timer
0
Time in seconds until an AFK player is
kicked from the server or to the Spectators, 0 -- off
mani_afk_kicker_spectator_timer
0
Time in seconds until a Spectator is kicked off the server,
0 -- off
mani_afk_kicker_immunity_to_spec_only		No information
Modules: 29 betting
mani_css_betting		on -- Activates the betting option
mani_css_betting_dead_only		on -- You can only make bets if you are dead
out -- You can also bet if you are not dead
mani_css_betting_pay_losing_bets
0
Betting ratio X vs 1
mani_css_betting_announce_one_v_one		on -- message popup to set bets
off -- no popup.
Modules: 30 Bounty
mani_css_bounty		on -- Bounty Modules Active
mani_css_bounty_kill_streak
5
How many kills in a row must a player have
before you can put a bounty on him
mani_css_bounty_start_cash
1000
Bounty money at the beginning
mani_css_bounty_survive_round_cash
500
Bounty sum that will be added after each round
mani_css_bounty_kill_cash
250
Bounty added to
when the hunt kills
mani_css_bounty_ct
255

255

255

255
Red / green / blue / gamma color of the hunted CT gets
(255 -- max) per. Color / gamma
mani_css_bounty_t
255

255

255

255
Red / green / blue / gamma the hunted T gets
(255 -- max) per. Color / gamma
Modules: 31 Objectives for CSS
mani_css_objectives		on -- Kill all players who
do not reach their mission goal before the end of the round
Modules: 32 AutoMap
mani_automap		Map change at a certain number of players
mani_automap_map_list
List of maps to be loaded.
The maps must be separated with a ":".
eg "de_dust2: de_aztec: cs_office"
mani_automap_player_threshold
0
The number of players or less that
trigger the AutoMap feature.
mani_automap_include_bots		on -- bots are taken into account,
off -- bots are not taken into account
mani_automap_timer
300
Time in seconds after the AutoMap function triggers
when the player threshold has been reached
mani_automap_set_nextmap		No information
Modules: 33 Command Flooding
mani_command_flood_time		Time Threshold for Command Spamming
On -- Is a good setting
Off -- No Threshold
mani_command_flood_total
14
Number of commands that can be executed
mani_command_flood_punish		Penalty for too much command spamming:
0 -- Off, 1 -- Kick, 2 -- Ban
mani_command_flood_violation_count
5
Number of times the Flood Limit can be reached
before an action is executed
mani_command_flood_punish_ban_time
60
Ban time in minutes (0 -- permanent)
Modules: 34 Miscellaneous
mani_mapcycle_mode		0 -- Default Mapcycle
1 -- If you do not want to start over again
when playing a map that is not in Mapcycle
2 -- Random Mapcycle
3 -- Jump to the next unplayed map until all are played
mani_unlimited_grenades		off -- default settings, on
= Players in CSS get Infinity He shells
mani_war_mode_force_overview_zero		on -- Imposing "overview_mode 0" in war mode dead players
(important for not being able to locate players in Wars).
mani_use_ma_in_say_command		a -- Admin Say in the say radio. must be preceded by "ma",
off -- "ma_" can be omitted
mani_dead_alltalk		a -- Allow the dead to speak with the living,
aus -- forbid the dead to speak with the living
mani_mute_con_command_spam		on -- automatically removes spam alarms,
off -- normal operation (no effect)
mani_adminsay_top_left		on -- enables adminsay on top left,
off -- disables adminsay on top left
mani_adminsay_chat_area		show a -- "admin-say" in the chat,
off -- do not show admin-say
mani_adminsay_bottom_area		adminsay will be displayed at the bottom of the screen
mani_allow_chat_to_admin		a -- users are allowed to chat with admins via "ma_chat",
off -- users are not allowed to chat with admins via "ma_chat"
mani_ff_player_only		an -- ff command is only shown to the player,
off -- all players see the issue
mani_nextmap_player_only		a -- nextmap command is only shown to the player,
off -- all players see the output
mani_timeleft_player_only		a -- timeleft command is only displayed to the player,
off -- all players see the issue
mani_thetime_player_only		a -- thetime command will only be shown to the player,
off -- all players will see the issue
mani_admin_burn_time
20
Time in seconds a player burns after a TeamKill
if he was lit by an admin
mani_hostage_follow_warning		on -- show if a hostage stops running afterwards,
off -- no display
mani_say_command_prefix
@
Prefix for admin chat command
mani_all_see_ma_rates		off -- Only admins have ma_rates access on
= All players have ma_rates access
mani_swap_team_score		on -- If at an end of map vote the team exchanges sides,
then the points are exchanged with.
off -- stay points
(CSS Only)
mani_old_style_menu_behaviour		on -- menu disappears after selection of player punishment,
off -- menu remains open
mani_menu_force_text_input_via_esc		on -- Enables the input box when the AMX style is used.
mani_admin_temp_ban_time_limit
360
Max. Time in Min. A user with the admin-Flag "b"
can ban another player!
mani_anti_rejoin		Kill players killed during a CSS round
and back on the server to
continue playing in the same round.
mani_weapon_restrict_refund_on_spawn		Gives the player the money
when he buys a locked weapon.
mani_weapon_restrict_prevent_pickup
1
How many locked weapons may a team pick up?
mani_sb_observe_mode		It automatically executes the "sb_status" command
when watching a player.
(Need only if Steam Bans is running.)
mani_exec_default_file1
1. Configfile running on my mapstart.
mani_exec_default_file2
2. Configfile which is executed on my Mapstart.
mani_exec_default_file3
3. Configfile running on my mapstart.
mani_exec_default_file4
4. Configfile running on my mapstart.
mani_exec_default_file5
5. Configfile running on my mapstart.
