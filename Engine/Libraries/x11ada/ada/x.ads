-- $Source: /home/harp/1/proto/monoBANK/xbind/x.ads,v $ 
-- $Revision: 1.12 $ $Date: 96/02/29 14:56:46 $ $Author: mg $ 

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
-- see the accompanying file X.h.
-- --------------------------------------------------------------------------

with Interfaces.C;

package X is

    X_PROTOCOL                 : constant := 11;            -- X.h:36
    X_PROTOCOL_REVISION        : constant := 0;             -- X.h:37
    None                       : constant := 0;             -- X.h:66
    ParentRelative             : constant := 1;             -- X.h:68
    CopyFromParent             : constant := 0;             -- X.h:71
    PointerWindow              : constant := 0;             -- X.h:76
    InputFocus                 : constant := 1;             -- X.h:77
    PointerRoot                : constant := 1;             -- X.h:79
    AnyPropertyType            : constant := 0;             -- X.h:81
    AnyKey                     : constant := 0;             -- X.h:83
    AnyButton                  : constant := 0;             -- X.h:85
    AllTemporary               : constant := 0;             -- X.h:87
    CurrentTime                : constant := 0;             -- X.h:89
    NoSymbol                   : constant := 0;             -- X.h:91
    NoEventMask                : constant := 0;             -- X.h:100
    KeyPressMask               : constant := 1;             -- X.h:101
    KeyReleaseMask             : constant := 2;             -- X.h:102
    ButtonPressMask            : constant := 4;             -- X.h:103
    ButtonReleaseMask          : constant := 8;             -- X.h:104
    EnterWindowMask            : constant := 16;            -- X.h:105
    LeaveWindowMask            : constant := 32;            -- X.h:106
    PointerMotionMask          : constant := 64;            -- X.h:107
    PointerMotionHintMask      : constant := 128;           -- X.h:108
    Button1MotionMask          : constant := 256;           -- X.h:109
    Button2MotionMask          : constant := 512;           -- X.h:110
    Button3MotionMask          : constant := 1024;          -- X.h:111
    Button4MotionMask          : constant := 2048;          -- X.h:112
    Button5MotionMask          : constant := 4096;          -- X.h:113
    ButtonMotionMask           : constant := 8192;          -- X.h:114
    KeymapStateMask            : constant := 16384;         -- X.h:115
    ExposureMask               : constant := 32768;         -- X.h:116
    VisibilityChangeMask       : constant := 65536;         -- X.h:117
    StructureNotifyMask        : constant := 131072;        -- X.h:118
    ResizeRedirectMask         : constant := 262144;        -- X.h:119
    SubstructureNotifyMask     : constant := 524288;        -- X.h:120
    SubstructureRedirectMask   : constant := 1048576;       -- X.h:121
    FocusChangeMask            : constant := 2097152;       -- X.h:122
    PropertyChangeMask         : constant := 4194304;       -- X.h:123
    ColormapChangeMask         : constant := 8388608;       -- X.h:124
    OwnerGrabButtonMask        : constant := 16777216;      -- X.h:125
    KeyPress                   : constant := 2;             -- X.h:131
    KeyRelease                 : constant := 3;             -- X.h:132
    ButtonPress                : constant := 4;             -- X.h:133
    ButtonRelease              : constant := 5;             -- X.h:134
    MotionNotify               : constant := 6;             -- X.h:135
    EnterNotify                : constant := 7;             -- X.h:136
    LeaveNotify                : constant := 8;             -- X.h:137
    FocusIn                    : constant := 9;             -- X.h:138
    FocusOut                   : constant := 10;            -- X.h:139
    KeymapNotify               : constant := 11;            -- X.h:140
    Expose                     : constant := 12;            -- X.h:141
    GraphicsExpose             : constant := 13;            -- X.h:142
    NoExpose                   : constant := 14;            -- X.h:143
    VisibilityNotify           : constant := 15;            -- X.h:144
    CreateNotify               : constant := 16;            -- X.h:145
    DestroyNotify              : constant := 17;            -- X.h:146
    UnmapNotify                : constant := 18;            -- X.h:147
    MapNotify                  : constant := 19;            -- X.h:148
    MapRequest                 : constant := 20;            -- X.h:149
    ReparentNotify             : constant := 21;            -- X.h:150
    ConfigureNotify            : constant := 22;            -- X.h:151
    ConfigureRequest           : constant := 23;            -- X.h:152
    GravityNotify              : constant := 24;            -- X.h:153
    ResizeRequest              : constant := 25;            -- X.h:154
    CirculateNotify            : constant := 26;            -- X.h:155
    CirculateRequest           : constant := 27;            -- X.h:156
    PropertyNotify             : constant := 28;            -- X.h:157
    SelectionClear             : constant := 29;            -- X.h:158
    SelectionRequest           : constant := 30;            -- X.h:159
    SelectionNotify            : constant := 31;            -- X.h:160
    ColormapNotify             : constant := 32;            -- X.h:161
    ClientMessage              : constant := 33;            -- X.h:162
    MappingNotify              : constant := 34;            -- X.h:163
    LASTEvent                  : constant := 35;            -- X.h:164
    ShiftMask                  : constant := 1;             -- X.h:170
    LockMask                   : constant := 2;             -- X.h:171
    ControlMask                : constant := 4;             -- X.h:172
    Mod1Mask                   : constant := 8;             -- X.h:173
    Mod2Mask                   : constant := 16;            -- X.h:174
    Mod3Mask                   : constant := 32;            -- X.h:175
    Mod4Mask                   : constant := 64;            -- X.h:176
    Mod5Mask                   : constant := 128;           -- X.h:177
    ShiftMapIndex              : constant := 0;             -- X.h:182
    LockMapIndex               : constant := 1;             -- X.h:183
    ControlMapIndex            : constant := 2;             -- X.h:184
    Mod1MapIndex               : constant := 3;             -- X.h:185
    Mod2MapIndex               : constant := 4;             -- X.h:186
    Mod3MapIndex               : constant := 5;             -- X.h:187
    Mod4MapIndex               : constant := 6;             -- X.h:188
    Mod5MapIndex               : constant := 7;             -- X.h:189
    Button1Mask                : constant := 256;           -- X.h:195
    Button2Mask                : constant := 512;           -- X.h:196
    Button3Mask                : constant := 1024;          -- X.h:197
    Button4Mask                : constant := 2048;          -- X.h:198
    Button5Mask                : constant := 4096;          -- X.h:199
    AnyModifier                : constant := 32768;         -- X.h:201
    Button1                    : constant := 1;             -- X.h:208
    Button2                    : constant := 2;             -- X.h:209
    Button3                    : constant := 3;             -- X.h:210
    Button4                    : constant := 4;             -- X.h:211
    Button5                    : constant := 5;             -- X.h:212
    NotifyNormal               : constant := 0;             -- X.h:216
    NotifyGrab                 : constant := 1;             -- X.h:217
    NotifyUngrab               : constant := 2;             -- X.h:218
    NotifyWhileGrabbed         : constant := 3;             -- X.h:219
    NotifyHint                 : constant := 1;             -- X.h:221
    NotifyAncestor             : constant := 0;             -- X.h:225
    NotifyVirtual              : constant := 1;             -- X.h:226
    NotifyInferior             : constant := 2;             -- X.h:227
    NotifyNonlinear            : constant := 3;             -- X.h:228
    NotifyNonlinearVirtual     : constant := 4;             -- X.h:229
    NotifyPointer              : constant := 5;             -- X.h:230
    NotifyPointerRoot          : constant := 6;             -- X.h:231
    NotifyDetailNone           : constant := 7;             -- X.h:232
    VisibilityUnobscured       : constant := 0;             -- X.h:236
    VisibilityPartiallyObscured: constant := 1;             -- X.h:237
    VisibilityFullyObscured    : constant := 2;             -- X.h:238
    PlaceOnTop                 : constant := 0;             -- X.h:242
    PlaceOnBottom              : constant := 1;             -- X.h:243
    FamilyInternet             : constant := 0;             -- X.h:247
    FamilyDECnet               : constant := 1;             -- X.h:248
    FamilyChaos                : constant := 2;             -- X.h:249
    PropertyNewValue           : constant := 0;             -- X.h:253
    PropertyDelete             : constant := 1;             -- X.h:254
    ColormapUninstalled        : constant := 0;             -- X.h:258
    ColormapInstalled          : constant := 1;             -- X.h:259
    GrabModeSync               : constant := 0;             -- X.h:263
    GrabModeAsync              : constant := 1;             -- X.h:264
    GrabSuccess                : constant := 0;             -- X.h:268
    AlreadyGrabbed             : constant := 1;             -- X.h:269
    GrabInvalidTime            : constant := 2;             -- X.h:270
    GrabNotViewable            : constant := 3;             -- X.h:271
    GrabFrozen                 : constant := 4;             -- X.h:272
    AsyncPointer               : constant := 0;             -- X.h:276
    SyncPointer                : constant := 1;             -- X.h:277
    ReplayPointer              : constant := 2;             -- X.h:278
    AsyncKeyboard              : constant := 3;             -- X.h:279
    SyncKeyboard               : constant := 4;             -- X.h:280
    ReplayKeyboard             : constant := 5;             -- X.h:281
    AsyncBoth                  : constant := 6;             -- X.h:282
    SyncBoth                   : constant := 7;             -- X.h:283
    RevertToNone               : constant Interfaces.C.Int := 0;
                                                            -- X.h:287
    RevertToPointerRoot        : constant Interfaces.C.Int := 1;
                                                            -- X.h:288
    RevertToParent             : constant := 2;             -- X.h:289
    Success                    : constant := 0;             -- X.h:295
    BadRequest                 : constant := 1;             -- X.h:296
    BadValue                   : constant := 2;             -- X.h:297
    BadWindow                  : constant := 3;             -- X.h:298
    BadPixmap                  : constant := 4;             -- X.h:299
    BadAtom                    : constant := 5;             -- X.h:300
    BadCursor                  : constant := 6;             -- X.h:301
    BadFont                    : constant := 7;             -- X.h:302
    BadMatch                   : constant := 8;             -- X.h:303
    BadDrawable                : constant := 9;             -- X.h:304
    BadAccess                  : constant := 10;            -- X.h:305
    BadAlloc                   : constant := 11;            -- X.h:314
    BadColor                   : constant := 12;            -- X.h:315
    BadGC                      : constant := 13;            -- X.h:316
    BadIDChoice                : constant := 14;            -- X.h:317
    BadName                    : constant := 15;            -- X.h:318
    BadLength                  : constant := 16;            -- X.h:319
    BadImplementation          : constant := 17;            -- X.h:320
    FirstExtensionError        : constant := 128;           -- X.h:322
    LastExtensionError         : constant := 255;           -- X.h:323
    InputOutput                : constant := 1;             -- X.h:332
    InputOnly                  : constant := 2;             -- X.h:333
    CWBackPixmap               : constant := 1;             -- X.h:337
    CWBackPixel                : constant := 2;             -- X.h:338
    CWBorderPixmap             : constant := 4;             -- X.h:339
    CWBorderPixel              : constant := 8;             -- X.h:340
    CWBitGravity               : constant := 16;            -- X.h:341
    CWWinGravity               : constant := 32;            -- X.h:342
    CWBackingStore             : constant := 64;            -- X.h:343
    CWBackingPlanes            : constant := 128;           -- X.h:344
    CWBackingPixel             : constant := 256;           -- X.h:345
    CWOverrideRedirect         : constant := 512;           -- X.h:346
    CWSaveUnder                : constant := 1024;          -- X.h:347
    CWEventMask                : constant := 2048;          -- X.h:348
    CWDontPropagate            : constant := 4096;          -- X.h:349
    CWColormap                 : constant := 8192;          -- X.h:350
    CWCursor                   : constant := 16384;         -- X.h:351
    CWX                        : constant := 1;             -- X.h:355
    CWY                        : constant := 2;             -- X.h:356
    CWWidth                    : constant := 4;             -- X.h:357
    CWHeight                   : constant := 8;             -- X.h:358
    CWBorderWidth              : constant := 16;            -- X.h:359
    CWSibling                  : constant := 32;            -- X.h:360
    CWStackMode                : constant := 64;            -- X.h:361
    ForgetGravity              : constant := 0;             -- X.h:366
    NorthWestGravity           : constant := 1;             -- X.h:367
    NorthGravity               : constant := 2;             -- X.h:368
    NorthEastGravity           : constant := 3;             -- X.h:369
    WestGravity                : constant := 4;             -- X.h:370
    CenterGravity              : constant := 5;             -- X.h:371
    EastGravity                : constant := 6;             -- X.h:372
    SouthWestGravity           : constant := 7;             -- X.h:373
    SouthGravity               : constant := 8;             -- X.h:374
    SouthEastGravity           : constant := 9;             -- X.h:375
    StaticGravity              : constant := 10;            -- X.h:376
    UnmapGravity               : constant := 0;             -- X.h:380
    NotUseful                  : constant := 0;             -- X.h:384
    WhenMapped                 : constant := 1;             -- X.h:385
    Always                     : constant := 2;             -- X.h:386
    IsUnmapped                 : constant := 0;             -- X.h:390
    IsUnviewable               : constant := 1;             -- X.h:391
    IsViewable                 : constant := 2;             -- X.h:392
    SetModeInsert              : constant := 0;             -- X.h:396
    SetModeDelete              : constant := 1;             -- X.h:397
    DestroyAll                 : constant := 0;             -- X.h:401
    RetainPermanent            : constant := 1;             -- X.h:402
    RetainTemporary            : constant := 2;             -- X.h:403
    Above                      : constant := 0;             -- X.h:407
    Below                      : constant := 1;             -- X.h:408
    TopIf                      : constant := 2;             -- X.h:409
    BottomIf                   : constant := 3;             -- X.h:410
    Opposite                   : constant := 4;             -- X.h:411
    RaiseLowest                : constant := 0;             -- X.h:415
    LowerHighest               : constant := 1;             -- X.h:416
    PropModeReplace            : constant := 0;             -- X.h:420
    PropModePrepend            : constant := 1;             -- X.h:421
    PropModeAppend             : constant := 2;             -- X.h:422
    GXclear                    : constant := 16#0#;         -- X.h:430
    GXand                      : constant := 16#1#;         -- X.h:431
    GXandReverse               : constant := 16#2#;         -- X.h:432
    GXcopy                     : constant := 16#3#;         -- X.h:433
    GXandInverted              : constant := 16#4#;         -- X.h:434
    GXnoop                     : constant := 16#5#;         -- X.h:435
    GXxor                      : constant := 16#6#;         -- X.h:436
    GXor                       : constant := 16#7#;         -- X.h:437
    GXnor                      : constant := 16#8#;         -- X.h:438
    GXequiv                    : constant := 16#9#;         -- X.h:439
    GXinvert                   : constant := 16#a#;         -- X.h:440
    GXorReverse                : constant := 16#b#;         -- X.h:441
    GXcopyInverted             : constant := 16#c#;         -- X.h:442
    GXorInverted               : constant := 16#d#;         -- X.h:443
    GXnand                     : constant := 16#e#;         -- X.h:444
    GXset                      : constant := 16#f#;         -- X.h:445
    LineSolid                  : constant := 0;             -- X.h:449
    LineOnOffDash              : constant := 1;             -- X.h:450
    LineDoubleDash             : constant := 2;             -- X.h:451
    CapNotLast                 : constant := 0;             -- X.h:455
    CapButt                    : constant := 1;             -- X.h:456
    CapRound                   : constant := 2;             -- X.h:457
    CapProjecting              : constant := 3;             -- X.h:458
    JoinMiter                  : constant := 0;             -- X.h:462
    JoinRound                  : constant := 1;             -- X.h:463
    JoinBevel                  : constant := 2;             -- X.h:464
    FillSolid                  : constant := 0;             -- X.h:468
    FillTiled                  : constant := 1;             -- X.h:469
    FillStippled               : constant := 2;             -- X.h:470
    FillOpaqueStippled         : constant := 3;             -- X.h:471
    EvenOddRule                : constant := 0;             -- X.h:475
    WindingRule                : constant := 1;             -- X.h:476
    ClipByChildren             : constant := 0;             -- X.h:480
    IncludeInferiors           : constant := 1;             -- X.h:481
    Unsorted                   : constant := 0;             -- X.h:485
    YSorted                    : constant := 1;             -- X.h:486
    YXSorted                   : constant := 2;             -- X.h:487
    YXBanded                   : constant := 3;             -- X.h:488
    CoordModeOrigin            : constant := 0;             -- X.h:492
    CoordModePrevious          : constant := 1;             -- X.h:493
    Complex                    : constant := 0;             -- X.h:497
    Nonconvex                  : constant := 1;             -- X.h:498
    Convex                     : constant := 2;             -- X.h:499
    ArcChord                   : constant := 0;             -- X.h:503
    ArcPieSlice                : constant := 1;             -- X.h:504
    GCFunction                 : constant := 1;             -- X.h:509
    GCPlaneMask                : constant := 2;             -- X.h:510
    GCForeground               : constant := 4;             -- X.h:511
    GCBackground               : constant := 8;             -- X.h:512
    GCLineWidth                : constant := 16;            -- X.h:513
    GCLineStyle                : constant := 32;            -- X.h:514
    GCCapStyle                 : constant := 64;            -- X.h:515
    GCJoinStyle                : constant := 128;           -- X.h:516
    GCFillStyle                : constant := 256;           -- X.h:517
    GCFillRule                 : constant := 512;           -- X.h:518
    GCTile                     : constant := 1024;          -- X.h:519
    GCStipple                  : constant := 2048;          -- X.h:520
    GCTileStipXOrigin          : constant := 4096;          -- X.h:521
    GCTileStipYOrigin          : constant := 8192;          -- X.h:522
    GCFont                     : constant := 16384;         -- X.h:523
    GCSubwindowMode            : constant := 32768;         -- X.h:524
    GCGraphicsExposures        : constant := 65536;         -- X.h:525
    GCClipXOrigin              : constant := 131072;        -- X.h:526
    GCClipYOrigin              : constant := 262144;        -- X.h:527
    GCClipMask                 : constant := 524288;        -- X.h:528
    GCDashOffset               : constant := 1048576;       -- X.h:529
    GCDashList                 : constant := 2097152;       -- X.h:530
    GCArcMode                  : constant := 4194304;       -- X.h:531
    GCLastBit                  : constant := 22;            -- X.h:533
    FontLeftToRight            : constant := 0;             -- X.h:540
    FontRightToLeft            : constant := 1;             -- X.h:541
    FontChange                 : constant := 255;           -- X.h:543
    XYBitmap                   : constant := 0;             -- X.h:551
    XYPixmap                   : constant := 1;             -- X.h:552
    ZPixmap                    : constant := 2;             -- X.h:553
    AllocNone                  : constant := 0;             -- X.h:561
    AllocAll                   : constant := 1;             -- X.h:562
    DoRed                      : constant := 1;             -- X.h:567
    DoGreen                    : constant := 2;             -- X.h:568
    DoBlue                     : constant := 4;             -- X.h:569
    CursorShape                : constant := 0;             -- X.h:577
    TileShape                  : constant := 1;             -- X.h:578
    StippleShape               : constant := 2;             -- X.h:579
    AutoRepeatModeOff          : constant := 0;             -- X.h:585
    AutoRepeatModeOn           : constant := 1;             -- X.h:586
    AutoRepeatModeDefault      : constant := 2;             -- X.h:587
    LedModeOff                 : constant := 0;             -- X.h:589
    LedModeOn                  : constant := 1;             -- X.h:590
    KBKeyClickPercent          : constant := 1;             -- X.h:594
    KBBellPercent              : constant := 2;             -- X.h:595
    KBBellPitch                : constant := 4;             -- X.h:596
    KBBellDuration             : constant := 8;             -- X.h:597
    KBLed                      : constant := 16;            -- X.h:598
    KBLedMode                  : constant := 32;            -- X.h:599
    KBKey                      : constant := 64;            -- X.h:600
    KBAutoRepeatMode           : constant := 128;           -- X.h:601
    MappingSuccess             : constant := 0;             -- X.h:603
    MappingBusy                : constant := 1;             -- X.h:604
    MappingFailed              : constant := 2;             -- X.h:605
    MappingModifier            : constant := 0;             -- X.h:607
    MappingKeyboard            : constant := 1;             -- X.h:608
    MappingPointer             : constant := 2;             -- X.h:609
    DontPreferBlanking         : constant := 0;             -- X.h:615
    PreferBlanking             : constant := 1;             -- X.h:616
    DefaultBlanking            : constant := 2;             -- X.h:617
    DisableScreenSaver         : constant := 0;             -- X.h:619
    DisableScreenInterval      : constant := 0;             -- X.h:620
    DontAllowExposures         : constant := 0;             -- X.h:622
    AllowExposures             : constant := 1;             -- X.h:623
    DefaultExposures           : constant := 2;             -- X.h:624
    ScreenSaverReset           : constant := 0;             -- X.h:628
    ScreenSaverActive          : constant := 1;             -- X.h:629
    HostInsert                 : constant := 0;             -- X.h:637
    HostDelete                 : constant := 1;             -- X.h:638
    EnableAccess               : constant := 1;             -- X.h:642
    DisableAccess              : constant := 0;             -- X.h:643
    StaticGray                 : constant := 0;             -- X.h:649
    GrayScale                  : constant := 1;             -- X.h:650
    StaticColor                : constant := 2;             -- X.h:651
    PseudoColor                : constant := 3;             -- X.h:652
    TrueColor                  : constant := 4;             -- X.h:653
    DirectColor                : constant := 5;             -- X.h:654
    LSBFirst                   : constant := 0;             -- X.h:659
    MSBFirst                   : constant := 1;             -- X.h:660

    -- ****************************************
    -- Types corresponding to C built-in types:
    -- ****************************************

    subtype char         is Interfaces.C.Char;
    subtype signed_char  is Interfaces.C.signed_char;
    subtype short        is Interfaces.C.Short;
    subtype signed_short is short;
    subtype int          is Interfaces.C.Int;
    subtype signed_int   is int;
    subtype long         is Interfaces.C.Long;
    subtype signed_long  is long;

    subtype unsigned_char  is Interfaces.C.Unsigned_Char;
    subtype unsigned_short is Interfaces.C.Unsigned_Short;
    subtype unsigned_int   is Interfaces.C.Unsigned;
    subtype unsigned_long  is Interfaces.C.Unsigned_Long;

    type XID is new unsigned_long;                          -- X.h:41
    type Window is new XID;                                 -- X.h:43
    type Drawable is new XID;                               -- X.h:44
    type Font is new XID;                                   -- X.h:45
    type Pixmap is new XID;                                 -- X.h:46
    type Cursor is new XID;                                 -- X.h:47
    type Colormap is new XID;                               -- X.h:48
    type GContext is new XID;                               -- X.h:49
    type KeySym is new XID;                                 -- X.h:50
    type Mask is new unsigned_long;                         -- X.h:52
    type Atom is new unsigned_long;                         -- X.h:54
    type VisualID is new unsigned_long;                     -- X.h:56
    type Time is new unsigned_long;                         -- X.h:58
    type KeyCode is new unsigned_char;                      -- X.h:60

    -- ********************************************************
    -- In C, a variable-size array is declared as a[1] or
    -- a[ANYSIZE_ARRAY], where ANYSIZE_ARRAY is defined as 1.
    -- Then it is used as if it were bigger.
    -- In Ada we declare it as array(0..ANYSIZE_ARRAY) and then
    -- use the extensible array package.
    -- In C ANYSIZE_ARRAY is 1 and in Ada it is 0.
    -- ********************************************************
    
    ANYSIZE_ARRAY: constant := 0;                           -- winnt.h:26

    -- ****************************************************
    -- Types moved up here, to break circular dependencies,
    -- and to remove duplicate definitions:
    -- ****************************************************

    type Int_Access is access all Signed_int;
    type unsigned_char_access is access all unsigned_char;
    type const_unsigned_char_access is access constant unsigned_char;
    type wchar_access is access all interfaces.c.wchar_t;   -- wchar *
    type wchar_access_access is access all wchar_access;    -- wchar **

    -- Resource moved to x-tasking.ads 
    -- because some systems don't support tasking and don't like
    -- to have a protected type declared.

end X;
