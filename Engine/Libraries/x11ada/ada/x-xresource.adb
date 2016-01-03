-- $Source: /home/harp/1/proto/monoBANK/xbind/x-xresource.adb,v $ 
-- $Revision: 1.8 $ $Date: 95/12/05 09:07:51 $ $Author: mg $ 

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

with Interfaces.C;

package body X.Xresource is

    function XrmStringsEqual(S1, S2: XrmString) return Boolean is
        function Strcmp(S1, S2: XrmString) return Interfaces.C.Int;
        pragma Import(C, strcmp, "strcmp");
	use type Interfaces.C.Int;
    begin
        return strcmp(S1, S2) = 0;
    end XrmStringsEqual;

    function XrmNameToString (Name: XrmName) return XrmString is
    begin
        return XrmQuarkToString(XrmQuark(Name));
    end XrmNameToString;

    function XrmStringToName (String: XrmString) return XrmName is
    begin
        return XrmName(XrmStringToQuark(X.Strings.const_charp(String)));
    end XrmStringToName;

    procedure XrmStringToNameList (
        Str         : XrmString; 
        Names_Return: XrmNameList) is

    begin
        XrmStringToQuarkList(X.Strings.const_charp(Str), 
                             XrmQuarkList(Names_Return));
    end XrmStringToNameList;

    function XrmClassToString (Class: XrmClass) return XrmString is
    begin
        return XrmQuarkToString(XrmQuark(Class));
    end XrmClassToString;

    function XrmStringToClass (String: XrmString) return XrmClass is
    begin
        return XrmClass(XrmStringToQuark(X.Strings.const_charp(String)));
    end XrmStringToClass;

    procedure XrmStringToClassList (
        Str         : XrmString;
        Class_Return: XrmClassList) is

    begin
        XrmStringToQuarkList(X.Strings.const_charp(Str), 
                             XrmQuarkList(Class_Return));
    end XrmStringToClassList;

    function XrmStringToRepresentation (String: XrmString) 
        return XrmRepresentation is
    begin
        return XrmRepresentation(XrmStringToQuark(
                   X.Strings.const_charp(String)));
    end XrmStringToRepresentation;

    function XrmRepresentationToString (Rep: XrmRepresentation) 
        return XrmString is
    begin
        return XrmQuarkToString(XrmQuark(Rep));
    end XrmRepresentationToString;

    -- ************************************
    -- functions with Char_Array parameters
    -- ************************************

    function XrmStringToQuark(
                string: Interfaces.C.Char_Array) return XrmQuark is

        use type Interfaces.C.Char_Array;
        Tmp_string: constant Interfaces.C.Char_Array := 
            string & Interfaces.C.Nul;
    begin
        return XrmStringToQuark(
                Tmp_string(Tmp_string'First)'unchecked_access);
    end XrmStringToQuark;                                   -- Xresource.h:72

    function XrmPermStringToQuark(
                string: Interfaces.C.Char_Array) return XrmQuark is

        use type Interfaces.C.Char_Array;
        Tmp_string: constant Interfaces.C.Char_Array := 
            string & Interfaces.C.Nul;
    begin
        return XrmPermStringToQuark(
                Tmp_string(Tmp_string'First)'unchecked_access);
    end XrmPermStringToQuark;                               -- Xresource.h:78

    procedure XrmStringToQuarkList(
                string       : Interfaces.C.Char_Array;
                quarks_return: XrmQuarkList) is

        use type Interfaces.C.Char_Array;
        Tmp_string: constant Interfaces.C.Char_Array := 
            string & Interfaces.C.Nul;
    begin
        XrmStringToQuarkList(
                Tmp_string(Tmp_string'First)'unchecked_access,
                quarks_return);
    end XrmStringToQuarkList;                               -- Xresource.h:108

    procedure XrmStringToBindingQuarkList(
                string         : Interfaces.C.Char_Array;
                bindings_return: XrmBindingList;
                quarks_return  : XrmQuarkList) is

        use type Interfaces.C.Char_Array;
        Tmp_string: constant Interfaces.C.Char_Array := 
            string & Interfaces.C.Nul;
    begin
        XrmStringToBindingQuarkList(
                Tmp_string(Tmp_string'First)'unchecked_access,
                bindings_return,
                quarks_return);
    end XrmStringToBindingQuarkList;                        -- Xresource.h:115

    procedure XrmPutResource(
                database : access XrmDatabase;
                specifier: Interfaces.C.Char_Array;
                c_type   : Interfaces.C.Char_Array;
                value    : XrmValuePtr) is

        use type Interfaces.C.Char_Array;
        Tmp_specifier: constant Interfaces.C.Char_Array := 
            specifier & Interfaces.C.Nul;
        Tmp_c_type: constant Interfaces.C.Char_Array := 
            c_type & Interfaces.C.Nul;
    begin
        XrmPutResource(
                database,
                Tmp_specifier(Tmp_specifier'First)'unchecked_access,
                Tmp_c_type(Tmp_c_type'First)'unchecked_access,
                value);
    end XrmPutResource;                                     -- Xresource.h:187

    procedure XrmQPutStringResource(
                database: access XrmDatabase;
                bindings: XrmBindingList;
                quarks  : XrmQuarkList;
                value   : Interfaces.C.Char_Array) is

        use type Interfaces.C.Char_Array;
        Tmp_value: constant Interfaces.C.Char_Array := 
            value & Interfaces.C.Nul;
    begin
        XrmQPutStringResource(
                database,
                bindings,
                quarks,
                Tmp_value(Tmp_value'First)'unchecked_access);
    end XrmQPutStringResource;                              -- Xresource.h:196

    procedure XrmPutStringResource(
                database : access XrmDatabase;
                specifier: Interfaces.C.Char_Array;
                value    : Interfaces.C.Char_Array) is

        use type Interfaces.C.Char_Array;
        Tmp_specifier: constant Interfaces.C.Char_Array := 
            specifier & Interfaces.C.Nul;
        Tmp_value: constant Interfaces.C.Char_Array := 
            value & Interfaces.C.Nul;
    begin
        XrmPutStringResource(
                database,
                Tmp_specifier(Tmp_specifier'First)'unchecked_access,
                Tmp_value(Tmp_value'First)'unchecked_access);
    end XrmPutStringResource;                               -- Xresource.h:205

    procedure XrmPutLineResource(
                database: access XrmDatabase;
                line    : Interfaces.C.Char_Array) is

        use type Interfaces.C.Char_Array;
        Tmp_line: constant Interfaces.C.Char_Array := 
            line & Interfaces.C.Nul;
    begin
        XrmPutLineResource(
                database,
                Tmp_line(Tmp_line'First)'unchecked_access);
    end XrmPutLineResource;                                 -- Xresource.h:213

    function XrmGetResource(
                database       : XrmDatabase;
                str_name       : Interfaces.C.Char_Array;
                str_class      : Interfaces.C.Char_Array;
                str_type_return: X.Strings.charp_vector;
                value_return   : XrmValuePtr) return X.signed_int is

        use type Interfaces.C.Char_Array;
        Tmp_str_name: constant Interfaces.C.Char_Array := 
            str_name & Interfaces.C.Nul;
        Tmp_str_class: constant Interfaces.C.Char_Array := 
            str_class & Interfaces.C.Nul;
    begin
        return XrmGetResource(
                database,
                Tmp_str_name(Tmp_str_name'First)'unchecked_access,
                Tmp_str_class(Tmp_str_class'First)'unchecked_access,
                str_type_return,
                value_return);
    end XrmGetResource;                                     -- Xresource.h:230

    function XrmGetFileDatabase(
                filename: Interfaces.C.Char_Array) return XrmDatabase is

        use type Interfaces.C.Char_Array;
        Tmp_filename: constant Interfaces.C.Char_Array := 
            filename & Interfaces.C.Nul;
    begin
        return XrmGetFileDatabase(
                Tmp_filename(Tmp_filename'First)'unchecked_access);
    end XrmGetFileDatabase;                                 -- Xresource.h:279

    function XrmCombineFileDatabase(
                filename: Interfaces.C.Char_Array;
                target  : access XrmHashBucket;
                override: X.signed_int) return X.signed_int is

        use type Interfaces.C.Char_Array;
        Tmp_filename: constant Interfaces.C.Char_Array := 
            filename & Interfaces.C.Nul;
    begin
        return XrmCombineFileDatabase(
                Tmp_filename(Tmp_filename'First)'unchecked_access,
                target,
                override);
    end XrmCombineFileDatabase;                             -- Xresource.h:285

    function XrmGetStringDatabase(
                data: Interfaces.C.Char_Array) return XrmDatabase is

        use type Interfaces.C.Char_Array;
        Tmp_data: constant Interfaces.C.Char_Array := 
            data & Interfaces.C.Nul;
    begin
        return XrmGetStringDatabase(
                Tmp_data(Tmp_data'First)'unchecked_access);
    end XrmGetStringDatabase;                               -- Xresource.h:293

    procedure XrmPutFileDatabase(
                database: XrmDatabase;
                filename: Interfaces.C.Char_Array) is

        use type Interfaces.C.Char_Array;
        Tmp_filename: constant Interfaces.C.Char_Array := 
            filename & Interfaces.C.Nul;
    begin
        XrmPutFileDatabase(
                database,
                Tmp_filename(Tmp_filename'First)'unchecked_access);
    end XrmPutFileDatabase;                                 -- Xresource.h:299

    procedure XrmParseCommand(
                database   : access XrmDatabase;
                table      : XrmOptionDescList;
                table_count: X.signed_int;
                name       : Interfaces.C.Char_Array;
                argc_in_out: access X.signed_int;
                argv_in_out: X.Strings.charp_vector) is

        use type Interfaces.C.Char_Array;
        Tmp_name: constant Interfaces.C.Char_Array := 
            name & Interfaces.C.Nul;
    begin
        XrmParseCommand(
                database,
                table,
                table_count,
                Tmp_name(Tmp_name'First)'unchecked_access,
                argc_in_out,
                argv_in_out);
    end XrmParseCommand;                                    -- Xresource.h:377

end X.Xresource;
