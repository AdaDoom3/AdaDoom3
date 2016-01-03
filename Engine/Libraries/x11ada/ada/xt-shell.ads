-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-shell.ads,v $ 
-- $Revision: 1.3 $ $Date: 95/12/05 08:53:53 $ $Author: mg $ 

-- --------------------------------------------------------------------------
-- THIS FILE AND ANY ASSOCIATED DOCUMENTATION IS PROVIDED "AS IS" WITHOUT 
-- WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED 
-- TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A 
-- PARTICULAR PURPOSE.  The user assumes the entire risk as to the accuracy 
-- and the use of this file.  
--  
-- Ada version Copyright (c) Intermetrics, Inc. 1994 
-- Royalty-free, unlimited, worldwide, non-exclusive use, modification, 
-- reproduction and further distribution of this Ada file is permitted. 
--
-- C version contains additional copyrights, 
-- see the accompanying file Shell.h.
-- --------------------------------------------------------------------------

with Interfaces.C;
with X.Strings;
with Xt.Intrinsic;

package Xt.Shell is

    XtShellStrings : Interfaces.C.Char_Array(0..1000);
    pragma Import(C, XtShellStrings, "XtShellStrings");

    subtype CCP is X.Strings.const_charp;

    XtNiconName : constant CCP := XtShellStrings(0)'access;
    XtCIconName : constant CCP := XtShellStrings(9)'access;
    XtNiconPixmap : constant CCP := XtShellStrings(18)'access;
    XtCIconPixmap : constant CCP := XtShellStrings(29)'access;
    XtNiconWindow : constant CCP := XtShellStrings(40)'access;
    XtCIconWindow : constant CCP := XtShellStrings(51)'access;
    XtNiconMask : constant CCP := XtShellStrings(62)'access;
    XtCIconMask : constant CCP := XtShellStrings(71)'access;
    XtNwindowGroup : constant CCP := XtShellStrings(80)'access;
    XtCWindowGroup : constant CCP := XtShellStrings(92)'access;
    XtNvisual : constant CCP := XtShellStrings(104)'access;
    XtCVisual : constant CCP := XtShellStrings(111)'access;
    XtNtitleEncoding : constant CCP := XtShellStrings(118)'access;
    XtCTitleEncoding : constant CCP := XtShellStrings(132)'access;
    XtNsaveUnder : constant CCP := XtShellStrings(146)'access;
    XtCSaveUnder : constant CCP := XtShellStrings(156)'access;
    XtNtransient : constant CCP := XtShellStrings(166)'access;
    XtCTransient : constant CCP := XtShellStrings(176)'access;
    XtNoverrideRedirect : constant CCP := XtShellStrings(186)'access;
    XtCOverrideRedirect : constant CCP := XtShellStrings(203)'access;
    XtNtransientFor : constant CCP := XtShellStrings(220)'access;
    XtCTransientFor : constant CCP := XtShellStrings(233)'access;
    XtNiconNameEncoding : constant CCP := XtShellStrings(246)'access;
    XtCIconNameEncoding : constant CCP := XtShellStrings(263)'access;
    XtNallowShellResize : constant CCP := XtShellStrings(280)'access;
    XtCAllowShellResize : constant CCP := XtShellStrings(297)'access;
    XtNcreatePopupChildProc : constant CCP := XtShellStrings(314)'access;
    XtCCreatePopupChildProc : constant CCP := XtShellStrings(335)'access;
    XtNtitle : constant CCP := XtShellStrings(356)'access;
    XtCTitle : constant CCP := XtShellStrings(362)'access;
    XtRAtom : constant CCP := XtShellStrings(368)'access;
    XtNargc : constant CCP := XtShellStrings(373)'access;
    XtCArgc : constant CCP := XtShellStrings(378)'access;
    XtNargv : constant CCP := XtShellStrings(383)'access;
    XtCArgv : constant CCP := XtShellStrings(388)'access;
    XtNiconX : constant CCP := XtShellStrings(393)'access;
    XtCIconX : constant CCP := XtShellStrings(399)'access;
    XtNiconY : constant CCP := XtShellStrings(405)'access;
    XtCIconY : constant CCP := XtShellStrings(411)'access;
    XtNinput : constant CCP := XtShellStrings(417)'access;
    XtCInput : constant CCP := XtShellStrings(423)'access;
    XtNiconic : constant CCP := XtShellStrings(429)'access;
    XtCIconic : constant CCP := XtShellStrings(436)'access;
    XtNinitialState : constant CCP := XtShellStrings(443)'access;
    XtCInitialState : constant CCP := XtShellStrings(456)'access;
    XtNgeometry : constant CCP := XtShellStrings(469)'access;
    XtCGeometry : constant CCP := XtShellStrings(478)'access;
    XtNbaseWidth : constant CCP := XtShellStrings(487)'access;
    XtCBaseWidth : constant CCP := XtShellStrings(497)'access;
    XtNbaseHeight : constant CCP := XtShellStrings(507)'access;
    XtCBaseHeight : constant CCP := XtShellStrings(518)'access;
    XtNwinGravity : constant CCP := XtShellStrings(529)'access;
    XtCWinGravity : constant CCP := XtShellStrings(540)'access;
    XtNminWidth : constant CCP := XtShellStrings(551)'access;
    XtCMinWidth : constant CCP := XtShellStrings(560)'access;
    XtNminHeight : constant CCP := XtShellStrings(569)'access;
    XtCMinHeight : constant CCP := XtShellStrings(579)'access;
    XtNmaxWidth : constant CCP := XtShellStrings(589)'access;
    XtCMaxWidth : constant CCP := XtShellStrings(598)'access;
    XtNmaxHeight : constant CCP := XtShellStrings(607)'access;
    XtCMaxHeight : constant CCP := XtShellStrings(617)'access;
    XtNwidthInc : constant CCP := XtShellStrings(627)'access;
    XtCWidthInc : constant CCP := XtShellStrings(636)'access;
    XtNheightInc : constant CCP := XtShellStrings(645)'access;
    XtCHeightInc : constant CCP := XtShellStrings(655)'access;
    XtNminAspectY : constant CCP := XtShellStrings(665)'access;
    XtCMinAspectY : constant CCP := XtShellStrings(676)'access;
    XtNmaxAspectY : constant CCP := XtShellStrings(687)'access;
    XtCMaxAspectY : constant CCP := XtShellStrings(698)'access;
    XtNminAspectX : constant CCP := XtShellStrings(709)'access;
    XtCMinAspectX : constant CCP := XtShellStrings(720)'access;
    XtNmaxAspectX : constant CCP := XtShellStrings(731)'access;
    XtCMaxAspectX : constant CCP := XtShellStrings(742)'access;
    XtNwmTimeout : constant CCP := XtShellStrings(753)'access;
    XtCWmTimeout : constant CCP := XtShellStrings(763)'access;
    XtNwaitForWm : constant CCP := XtShellStrings(773)'access;
    XtCWaitForWm : constant CCP := XtShellStrings(783)'access;

    type ShellClassRec is private;                          -- Shell.h:367
    type OverrideShellClassRec is private;                  -- Shell.h:368
    type WMShellClassRec is private;                        -- Shell.h:369
    type TransientShellClassRec is private;                 -- Shell.h:370
    type TopLevelShellClassRec is private;                  -- Shell.h:371
    type ApplicationShellClassRec is private;               -- Shell.h:372

    type ShellWidgetClass is access all ShellClassRec;      -- Shell.h:367
    type OverrideShellWidgetClass is access all OverrideShellClassRec;
                                                            -- Shell.h:368
    type WMShellWidgetClass is access all WMShellClassRec;  -- Shell.h:369
    type TransientShellWidgetClass is access all TransientShellClassRec;
                                                            -- Shell.h:370
    type TopLevelShellWidgetClass is access all TopLevelShellClassRec;
                                                            -- Shell.h:371
    type ApplicationShellWidgetClass is access all ApplicationShellClassRec;
                                                            -- Shell.h:372

    shellWidgetClass_obj           : Xt.Intrinsic.WidgetClass;
                                                            -- Shell.h:375
    overrideShellWidgetClass_obj   : Xt.Intrinsic.WidgetClass;
                                                            -- Shell.h:376
    wmShellWidgetClass_obj         : Xt.Intrinsic.WidgetClass;
                                                            -- Shell.h:377
    transientShellWidgetClass_obj  : Xt.Intrinsic.WidgetClass;
                                                            -- Shell.h:378
    topLevelShellWidgetClass_obj   : Xt.Intrinsic.WidgetClass;
                                                            -- Shell.h:379
    applicationShellWidgetClass_obj: Xt.Intrinsic.WidgetClass;
                                                            -- Shell.h:380

private

    type ApplicationShellClassRec is null record;           -- Shell.h:372
    type TopLevelShellClassRec is null record;              -- Shell.h:371
    type TransientShellClassRec is null record;             -- Shell.h:370
    type WMShellClassRec is null record;                    -- Shell.h:369
    type OverrideShellClassRec is null record;              -- Shell.h:368
    type ShellClassRec is null record;                      -- Shell.h:367
    pragma Import(C, shellWidgetClass_obj, "shellWidgetClass");
                                                            -- Shell.h:375
    pragma Import(C, overrideShellWidgetClass_obj, "overrideShellWidgetClass");
                                                            -- Shell.h:376
    pragma Import(C, wmShellWidgetClass_obj, "wmShellWidgetClass");
                                                            -- Shell.h:377
    pragma Import(C, transientShellWidgetClass_obj, 
                     "transientShellWidgetClass");          -- Shell.h:378
    pragma Import(C, topLevelShellWidgetClass_obj, "topLevelShellWidgetClass");
                                                            -- Shell.h:379
    pragma Import(C, applicationShellWidgetClass_obj, 
                     "applicationShellWidgetClass");        -- Shell.h:380

end Xt.Shell;
