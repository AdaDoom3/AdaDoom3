-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-corep.adb,v $ 
-- $Revision: 1.2 $ $Date: 95/12/05 09:07:54 $ $Author: mg $ 

-- --------------------------------------------------------------------------
-- THIS FILE AND ANY ASSOCIATED DOCUMENTATION IS FURNISHED "AS IS" WITHOUT 
-- WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED 
-- TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A 
-- PARTICULAR PURPOSE.  The user assumes the entire risk as to the accuracy 
-- and the use of this file. 
--  
-- Copyright (c) Intermetrics, Inc. 1994 
-- Royalty-free, unlimited, worldwide, non-exclusive use, modification, 
-- reproduction and further distribution of this file is permitted. 
-- --------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

package body Xt.CoreP is

    type proc_access is access procedure;
    pragma convention(c, proc_access);

    function XtInheritTranslations return X.Strings.charp is
	function Conv is new Ada.Unchecked_Conversion(
	    X.Int_Access, X.Strings.charp);
    begin
        return Conv(XtInheritTranslations_obj'access);
    end XtInheritTranslations;

    function XtInheritRealize return Xt.IntrinsicP.XtRealizeProc is
	p: proc_access := Xt.IntrinsicP.XtInherit'access;
	function Conv is new Ada.Unchecked_Conversion(
	    proc_access, Xt.IntrinsicP.XtRealizeProc);
    begin
        return Conv(p);
        return Conv(Xt.IntrinsicP.XtInherit'access);
    end XtInheritRealize;

    function XtInheritResize return Xt.IntrinsicP.XtWidgetProc is
	function Conv is new Ada.Unchecked_Conversion(
	    proc_access, Xt.IntrinsicP.XtWidgetProc);
    begin
        return Conv(Xt.IntrinsicP.XtInherit'access);
    end XtInheritResize;

    function XtInheritExpose return Xt.IntrinsicP.XtExposeProc is
	function Conv is new Ada.Unchecked_Conversion(
	    proc_access, Xt.IntrinsicP.XtExposeProc);
    begin
        return Conv(Xt.IntrinsicP.XtInherit'access);
    end XtInheritExpose;

    function XtInheritSetValuesAlmost return Xt.IntrinsicP.XtAlmostProc is
	function Conv is new Ada.Unchecked_Conversion(
	    proc_access, Xt.IntrinsicP.XtAlmostProc);
    begin
        return Conv(Xt.IntrinsicP.XtInherit'access);
    end XtInheritSetValuesAlmost;

    function XtInheritAcceptFocus return Xt.IntrinsicP.XtAcceptFocusProc is
	function Conv is new Ada.Unchecked_Conversion(
	    proc_access, Xt.IntrinsicP.XtAcceptFocusProc);
    begin
        return Conv(Xt.IntrinsicP.XtInherit'access);
    end XtInheritAcceptFocus;

    function XtInheritQueryGeometry return Xt.IntrinsicP.XtGeometryHandler is
	function Conv is new Ada.Unchecked_Conversion(
	    proc_access, Xt.IntrinsicP.XtGeometryHandler);
    begin
        return Conv(Xt.IntrinsicP.XtInherit'access);
    end XtInheritQueryGeometry;

    function XtInheritDisplayAccelerator return Xt.IntrinsicP.XtStringProc is
	function Conv is new Ada.Unchecked_Conversion(
	    proc_access, Xt.IntrinsicP.XtStringProc);
    begin
        return Conv(Xt.IntrinsicP.XtInherit'access);
    end XtInheritDisplayAccelerator;

end Xt.CoreP;
