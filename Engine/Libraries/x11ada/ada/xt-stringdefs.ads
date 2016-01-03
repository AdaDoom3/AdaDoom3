-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-stringdefs.ads,v $ 
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
-- see the accompanying file StringDefs.h.
-- --------------------------------------------------------------------------

with Interfaces.C;
with X.Strings;

package Xt.StringDefs is

    XtStrings: Interfaces.C.Char_Array(0..2200);

    pragma Import(C, XtStrings, "XtStrings");

    -- Definitions starting line 233 StringDefs.h

    subtype CCP is X.Strings.const_charp;

    XtNaccelerators  : constant CCP := XtStrings(0)'access;
    XtNallowHoriz  : constant CCP := XtStrings(13)'access;
    XtNallowVert  : constant CCP := XtStrings(24)'access;
    XtNancestorSensitive  : constant CCP := XtStrings(34)'access;
    XtNbackground  : constant CCP := XtStrings(52)'access;
    XtNbackgroundPixmap  : constant CCP := XtStrings(63)'access;
    XtNbitmap  : constant CCP := XtStrings(80)'access;
    XtNborderColor  : constant CCP := XtStrings(87)'access;
    XtNborder  : constant CCP := XtStrings(99)'access;
    XtNborderPixmap  : constant CCP := XtStrings(111)'access;
    XtNborderWidth  : constant CCP := XtStrings(124)'access;
    XtNcallback  : constant CCP := XtStrings(136)'access;
    XtNchildren  : constant CCP := XtStrings(145)'access;
    XtNcolormap  : constant CCP := XtStrings(154)'access;
    XtNdepth  : constant CCP := XtStrings(163)'access;
    XtNdestroyCallback  : constant CCP := XtStrings(169)'access;
    XtNeditType  : constant CCP := XtStrings(185)'access;
    XtNfile  : constant CCP := XtStrings(194)'access;
    XtNfont  : constant CCP := XtStrings(199)'access;
    XtNforceBars  : constant CCP := XtStrings(204)'access;
    XtNforeground  : constant CCP := XtStrings(214)'access;
    XtNfunction  : constant CCP := XtStrings(225)'access;
    XtNheight  : constant CCP := XtStrings(234)'access;
    XtNhighlight  : constant CCP := XtStrings(241)'access;
    XtNhSpace  : constant CCP := XtStrings(251)'access;
    XtNindex  : constant CCP := XtStrings(258)'access;
    XtNinitialResourcesPersistent  : constant CCP := 
	XtStrings(264)'access;
    XtNinnerHeight  : constant CCP := XtStrings(291)'access;
    XtNinnerWidth  : constant CCP := XtStrings(303)'access;
    XtNinnerWindow  : constant CCP := XtStrings(314)'access;
    XtNinsertPosition  : constant CCP := XtStrings(326)'access;
    XtNinternalHeight  : constant CCP := XtStrings(341)'access;
    XtNinternalWidth  : constant CCP := XtStrings(356)'access;
    XtNjumpProc  : constant CCP := XtStrings(370)'access;
    XtNjustify  : constant CCP := XtStrings(379)'access;
    XtNknobHeight  : constant CCP := XtStrings(387)'access;
    XtNknobIndent  : constant CCP := XtStrings(398)'access;
    XtNknobPixel  : constant CCP := XtStrings(409)'access;
    XtNknobWidth  : constant CCP := XtStrings(419)'access;
    XtNlabel  : constant CCP := XtStrings(429)'access;
    XtNlength  : constant CCP := XtStrings(435)'access;
    XtNlowerRight  : constant CCP := XtStrings(442)'access;
    XtNmappedWhenManaged  : constant CCP := XtStrings(453)'access;
    XtNmenuEntry  : constant CCP := XtStrings(471)'access;
    XtNname  : constant CCP := XtStrings(481)'access;
    XtNnotify  : constant CCP := XtStrings(486)'access;
    XtNnumChildren  : constant CCP := XtStrings(493)'access;
    XtNorientation  : constant CCP := XtStrings(505)'access;
    XtNparameter  : constant CCP := XtStrings(517)'access;
    XtNpixmap  : constant CCP := XtStrings(527)'access;
    XtNpopupCallback  : constant CCP := XtStrings(534)'access;
    XtNpopdownCallback  : constant CCP := XtStrings(548)'access;
    XtNresize  : constant CCP := XtStrings(564)'access;
    XtNreverseVideo  : constant CCP := XtStrings(571)'access;
    XtNscreen  : constant CCP := XtStrings(584)'access;
    XtNscrollProc  : constant CCP := XtStrings(591)'access;
    XtNscrollDCursor  : constant CCP := XtStrings(602)'access;
    XtNscrollHCursor  : constant CCP := XtStrings(616)'access;
    XtNscrollLCursor  : constant CCP := XtStrings(630)'access;
    XtNscrollRCursor  : constant CCP := XtStrings(644)'access;
    XtNscrollUCursor  : constant CCP := XtStrings(658)'access;
    XtNscrollVCursor  : constant CCP := XtStrings(672)'access;
    XtNselection  : constant CCP := XtStrings(686)'access;
    XtNselectionArray  : constant CCP := XtStrings(696)'access;
    XtNsensitive  : constant CCP := XtStrings(711)'access;
    XtNshown  : constant CCP := XtStrings(721)'access;
    XtNspace  : constant CCP := XtStrings(727)'access;
    XtNstring  : constant CCP := XtStrings(733)'access;
    XtNtextOptions  : constant CCP := XtStrings(740)'access;
    XtNtextSink  : constant CCP := XtStrings(752)'access;
    XtNtextSource  : constant CCP := XtStrings(761)'access;
    XtNthickness  : constant CCP := XtStrings(772)'access;
    XtNthumb  : constant CCP := XtStrings(782)'access;
    XtNthumbProc  : constant CCP := XtStrings(788)'access;
    XtNtop  : constant CCP := XtStrings(798)'access;
    XtNtranslations  : constant CCP := XtStrings(802)'access;
    XtNunrealizeCallback  : constant CCP := XtStrings(815)'access;
    XtNupdate  : constant CCP := XtStrings(833)'access;
    XtNuseBottom  : constant CCP := XtStrings(840)'access;
    XtNuseRight  : constant CCP := XtStrings(850)'access;
    XtNvalue  : constant CCP := XtStrings(859)'access;
    XtNvSpace  : constant CCP := XtStrings(865)'access;
    XtNwidth  : constant CCP := XtStrings(872)'access;
    XtNwindow  : constant CCP := XtStrings(878)'access;
    XtNx  : constant CCP := XtStrings(885)'access;
    XtNy  : constant CCP := XtStrings(887)'access;
    XtCAccelerators  : constant CCP := XtStrings(889)'access;
    XtCBackground  : constant CCP := XtStrings(902)'access;
    XtCBitmap  : constant CCP := XtStrings(913)'access;
    XtCBoolean  : constant CCP := XtStrings(920)'access;
    XtCBorderColor  : constant CCP := XtStrings(928)'access;
    XtCBorderWidth  : constant CCP := XtStrings(940)'access;
    XtCCallback  : constant CCP := XtStrings(952)'access;
    XtCColormap  : constant CCP := XtStrings(961)'access;
    XtCColor  : constant CCP := XtStrings(970)'access;
    XtCCursor  : constant CCP := XtStrings(976)'access;
    XtCDepth  : constant CCP := XtStrings(983)'access;
    XtCEditType  : constant CCP := XtStrings(989)'access;
    XtCEventBindings  : constant CCP := XtStrings(998)'access;
    XtCFile  : constant CCP := XtStrings(1012)'access;
    XtCFont  : constant CCP := XtStrings(1017)'access;
    XtCForeground  : constant CCP := XtStrings(1022)'access;
    XtCFraction  : constant CCP := XtStrings(1033)'access;
    XtCFunction  : constant CCP := XtStrings(1042)'access;
    XtCHeight  : constant CCP := XtStrings(1051)'access;
    XtCHSpace  : constant CCP := XtStrings(1058)'access;
    XtCIndex  : constant CCP := XtStrings(1065)'access;
    XtCInitialResourcesPersistent  : constant CCP := 
	XtStrings(1071)'access;
    XtCInsertPosition  : constant CCP := XtStrings(1098)'access;
    XtCInterval  : constant CCP := XtStrings(1113)'access;
    XtCJustify  : constant CCP := XtStrings(1122)'access;
    XtCKnobIndent  : constant CCP := XtStrings(1130)'access;
    XtCKnobPixel  : constant CCP := XtStrings(1141)'access;
    XtCLabel  : constant CCP := XtStrings(1151)'access;
    XtCLength  : constant CCP := XtStrings(1157)'access;
    XtCMappedWhenManaged  : constant CCP := XtStrings(1164)'access;
    XtCMargin  : constant CCP := XtStrings(1182)'access;
    XtCMenuEntry  : constant CCP := XtStrings(1189)'access;
    XtCNotify  : constant CCP := XtStrings(1199)'access;
    XtCOrientation  : constant CCP := XtStrings(1206)'access;
    XtCParameter  : constant CCP := XtStrings(1218)'access;
    XtCPixmap  : constant CCP := XtStrings(1228)'access;
    XtCPosition  : constant CCP := XtStrings(1235)'access;
    XtCReadOnly  : constant CCP := XtStrings(1244)'access;
    XtCResize  : constant CCP := XtStrings(1253)'access;
    XtCReverseVideo  : constant CCP := XtStrings(1260)'access;
    XtCScreen  : constant CCP := XtStrings(1273)'access;
    XtCScrollProc  : constant CCP := XtStrings(1280)'access;
    XtCScrollDCursor  : constant CCP := XtStrings(1291)'access;
    XtCScrollHCursor  : constant CCP := XtStrings(1305)'access;
    XtCScrollLCursor  : constant CCP := XtStrings(1319)'access;
    XtCScrollRCursor  : constant CCP := XtStrings(1333)'access;
    XtCScrollUCursor  : constant CCP := XtStrings(1347)'access;
    XtCScrollVCursor  : constant CCP := XtStrings(1361)'access;
    XtCSelection  : constant CCP := XtStrings(1375)'access;
    XtCSensitive  : constant CCP := XtStrings(1385)'access;
    XtCSelectionArray  : constant CCP := XtStrings(1395)'access;
    XtCSpace  : constant CCP := XtStrings(1410)'access;
    XtCString  : constant CCP := XtStrings(1416)'access;
    XtCTextOptions  : constant CCP := XtStrings(1423)'access;
    XtCTextPosition  : constant CCP := XtStrings(1435)'access;
    XtCTextSink  : constant CCP := XtStrings(1448)'access;
    XtCTextSource  : constant CCP := XtStrings(1457)'access;
    XtCThickness  : constant CCP := XtStrings(1468)'access;
    XtCThumb  : constant CCP := XtStrings(1478)'access;
    XtCTranslations  : constant CCP := XtStrings(1484)'access;
    XtCValue  : constant CCP := XtStrings(1497)'access;
    XtCVSpace  : constant CCP := XtStrings(1503)'access;
    XtCWidth  : constant CCP := XtStrings(1510)'access;
    XtCWindow  : constant CCP := XtStrings(1516)'access;
    XtCX  : constant CCP := XtStrings(1523)'access;
    XtCY  : constant CCP := XtStrings(1525)'access;
    XtRAcceleratorTable  : constant CCP := XtStrings(1527)'access;
    XtRAtom  : constant CCP := XtStrings(1544)'access;
    XtRBitmap  : constant CCP := XtStrings(1549)'access;
    XtRBool  : constant CCP := XtStrings(1556)'access;
    XtRBoolean  : constant CCP := XtStrings(1561)'access;
    XtRCallback  : constant CCP := XtStrings(1569)'access;
    XtRCallProc  : constant CCP := XtStrings(1578)'access;
    XtRCardinal  : constant CCP := XtStrings(1587)'access;
    XtRColor  : constant CCP := XtStrings(1596)'access;
    XtRColormap  : constant CCP := XtStrings(1602)'access;
    XtRCursor  : constant CCP := XtStrings(1611)'access;
    XtRDimension  : constant CCP := XtStrings(1618)'access;
    XtRDisplay  : constant CCP := XtStrings(1628)'access;
    XtREditMode  : constant CCP := XtStrings(1636)'access;
    XtREnum  : constant CCP := XtStrings(1645)'access;
    XtRFile  : constant CCP := XtStrings(1650)'access;
    XtRFloat  : constant CCP := XtStrings(1655)'access;
    XtRFont  : constant CCP := XtStrings(1661)'access;
    XtRFontStruct  : constant CCP := XtStrings(1666)'access;
    XtRFunction  : constant CCP := XtStrings(1677)'access;
    XtRGeometry  : constant CCP := XtStrings(1686)'access;
    XtRImmediate  : constant CCP := XtStrings(1695)'access;
    XtRInitialState  : constant CCP := XtStrings(1705)'access;
    XtRInt  : constant CCP := XtStrings(1718)'access;
    XtRJustify  : constant CCP := XtStrings(1722)'access;
    XtRLongBoolean  : constant CCP := XtStrings(1730)'access;
    XtRObject  : constant CCP := XtStrings(1735)'access;
    XtROrientation  : constant CCP := XtStrings(1742)'access;
    XtRPixel  : constant CCP := XtStrings(1754)'access;
    XtRPixmap  : constant CCP := XtStrings(1760)'access;
    XtRPointer  : constant CCP := XtStrings(1767)'access;
    XtRPosition  : constant CCP := XtStrings(1775)'access;
    XtRScreen  : constant CCP := XtStrings(1784)'access;
    XtRShort  : constant CCP := XtStrings(1791)'access;
    XtRString  : constant CCP := XtStrings(1797)'access;
    XtRStringArray  : constant CCP := XtStrings(1804)'access;
    XtRStringTable  : constant CCP := XtStrings(1816)'access;
    XtRUnsignedChar  : constant CCP := XtStrings(1828)'access;
    XtRTranslationTable  : constant CCP := XtStrings(1841)'access;
    XtRVisual  : constant CCP := XtStrings(1858)'access;
    XtRWidget  : constant CCP := XtStrings(1865)'access;
    XtRWidgetClass  : constant CCP := XtStrings(1872)'access;
    XtRWidgetList  : constant CCP := XtStrings(1884)'access;
    XtRWindow  : constant CCP := XtStrings(1895)'access;
    XtEoff  : constant CCP := XtStrings(1902)'access;
    XtEfalse  : constant CCP := XtStrings(1906)'access;
    XtEno  : constant CCP := XtStrings(1912)'access;
    XtEon  : constant CCP := XtStrings(1915)'access;
    XtEtrue  : constant CCP := XtStrings(1918)'access;
    XtEyes  : constant CCP := XtStrings(1923)'access;
    XtEvertical  : constant CCP := XtStrings(1927)'access;
    XtEhorizontal  : constant CCP := XtStrings(1936)'access;
    XtEtextRead  : constant CCP := XtStrings(1947)'access;
    XtEtextAppend  : constant CCP := XtStrings(1952)'access;
    XtEtextEdit  : constant CCP := XtStrings(1959)'access;
    XtExtdefaultbackground  : constant CCP := XtStrings(1964)'access;
    XtExtdefaultforeground  : constant CCP := XtStrings(1984)'access;
    XtExtdefaultfont  : constant CCP := XtStrings(2004)'access;
    XtNfontSet  : constant CCP := XtStrings(2018)'access;
    XtRFontSet  : constant CCP := XtStrings(2026)'access;
    XtCFontSet  : constant CCP := XtStrings(2034)'access;

end Xt.StringDefs;
