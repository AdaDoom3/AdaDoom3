-- $Source: /home/harp/1/proto/monoBANK/xbind/x-xresource.ads,v $ 
-- $Revision: 1.12 $ $Date: 95/12/19 15:35:27 $ $Author: mg $ 

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
-- see the accompanying file Xresource.h.
-- --------------------------------------------------------------------------

with X;
with X.Strings;
with X.Xlib;

package X.Xresource is

    type XrmQuark is new X.signed_int;                      -- Xresource.h:65
    type XrmString is new X.Strings.charp;                  -- Xresource.h:68

    NULLQUARK          : constant XrmQuark := 0;            -- Xresource.h:66
    NULLSTRING         : constant XrmString := null;        -- Xresource.h:69
    XrmEnumAllLevels   : constant := 0;                     -- Xresource.h:321
    XrmEnumOneLevel    : constant := 1;                     -- Xresource.h:322

    type XrmBinding is (                                    -- Xresource.h:106
        XrmBindTightly,                                     -- Xresource.h:106
        XrmBindLoosely                                      -- Xresource.h:106
    );
    for XrmBinding'Size use X.Int'Size;                     -- Xresource.h:106

    type XrmName is new XrmQuark;                           -- Xresource.h:129
    type XrmClass is new XrmQuark;                          -- Xresource.h:135
    type XrmRepresentation is new XrmQuark;                 -- Xresource.h:149

    type XrmOptionKind is (                                 -- Xresource.h:367
        XrmoptionNoArg,                                     -- Xresource.h:358
        XrmoptionIsArg,                                     -- Xresource.h:359
        XrmoptionStickyArg,                                 -- Xresource.h:360
        XrmoptionSepArg,                                    -- Xresource.h:361
        XrmoptionResArg,                                    -- Xresource.h:362
        XrmoptionSkipArg,                                   -- Xresource.h:363
        XrmoptionSkipLine,                                  -- Xresource.h:364
        XrmoptionSkipNArgs                                  -- Xresource.h:367
    );
    for XrmOptionKind'Size use X.Int'Size;                  -- Xresource.h:367

    type XrmQuarkList is access all XrmQuark;               -- Xresource.h:65
    type XrmNameList is new XrmQuarkList;                   -- Xresource.h:130
    type XrmClassList is new XrmQuarkList;                  -- Xresource.h:136

    type XrmValue;                                          -- Xresource.h:156
    type XrmOptionDescRec;                                  -- Xresource.h:374

    type XrmValuePtr is access all XrmValue;                -- Xresource.h:156
    type XrmHashBucket is access all X.Xlib.XrmHashBucketRec;
                                                            -- Xresource.h:165
    type XrmHashTable is access all XrmHashBucket;          -- Xresource.h:166
    type XrmDatabase is access all X.Xlib.XrmHashBucketRec; -- Xresource.h:168
    type XrmOptionDescList is access all XrmOptionDescRec;  -- Xresource.h:374

    type XrmBindingList is access all XrmBinding;           -- Xresource.h:106

    type XrmValue is                                        -- Xresource.h:156
        record
            size: X.unsigned_int;                           -- Xresource.h:154
            addr: X.Xlib.XPointer;                          -- Xresource.h:155
        end record;

    type XrmSearchList is                                   -- Xresource.h:167
        array(integer range 0..X.Anysize_Array) of aliased XrmHashTable;

    type XrmOptionDescRec is                                -- Xresource.h:374
        record
            option   : X.Strings.charp;                     -- Xresource.h:370
            specifier: X.Strings.charp;                     -- Xresource.h:371
            argKind  : XrmOptionKind;                       -- Xresource.h:372
            value    : X.Xlib.XPointer;                     -- Xresource.h:373
        end record;

    function Xpermalloc(
                size: X.unsigned_int)
               return X.Strings.charp;                      -- Xresource.h:53

    function XrmStringToQuark(
                string: X.Strings.const_charp)
               return XrmQuark;                             -- Xresource.h:72

    pragma Import(C, XrmStringToQuark, "XrmStringToQuark");

    function XrmStringToQuark(
                string: Interfaces.C.Char_Array)
               return XrmQuark;                             -- Xresource.h:72

    pragma Inline(XrmStringToQuark);

    function XrmPermStringToQuark(
                string: X.Strings.const_charp)
               return XrmQuark;                             -- Xresource.h:78

    pragma Import(C, XrmPermStringToQuark, "XrmPermStringToQuark");

    function XrmPermStringToQuark(
                string: Interfaces.C.Char_Array)
               return XrmQuark;                             -- Xresource.h:78

    pragma Inline(XrmPermStringToQuark);

    function XrmQuarkToString(
                quark: XrmQuark)
               return XrmString;                            -- Xresource.h:85

    function XrmUniqueQuark return XrmQuark;                -- Xresource.h:91

    procedure XrmStringToQuarkList(
                string       : X.Strings.const_charp;
                quarks_return: XrmQuarkList);               -- Xresource.h:108

    pragma Import(C, XrmStringToQuarkList, "XrmStringToQuarkList");

    procedure XrmStringToQuarkList(
                string       : Interfaces.C.Char_Array;
                quarks_return: XrmQuarkList);               -- Xresource.h:108

    pragma Inline(XrmStringToQuarkList);

    procedure XrmStringToBindingQuarkList(
                string         : X.Strings.const_charp;
                bindings_return: XrmBindingList;
                quarks_return  : XrmQuarkList);             -- Xresource.h:115

    pragma Import(C, XrmStringToBindingQuarkList, 
                     "XrmStringToBindingQuarkList");

    procedure XrmStringToBindingQuarkList(
                string         : Interfaces.C.Char_Array;
                bindings_return: XrmBindingList;
                quarks_return  : XrmQuarkList);             -- Xresource.h:115

    pragma Inline(XrmStringToBindingQuarkList);

    procedure XrmDestroyDatabase(
                database: XrmDatabase);                     -- Xresource.h:171

    procedure XrmQPutResource(
                database: access XrmDatabase;
                bindings: XrmBindingList;
                quarks  : XrmQuarkList;
                c_type  : XrmRepresentation;
                value   : XrmValuePtr);                     -- Xresource.h:177

    procedure XrmPutResource(
                database : access XrmDatabase;
                specifier: X.Strings.const_charp;
                c_type   : X.Strings.const_charp;
                value    : XrmValuePtr);                    -- Xresource.h:187

    pragma Import(C, XrmPutResource, "XrmPutResource");

    procedure XrmPutResource(
                database : access XrmDatabase;
                specifier: Interfaces.C.Char_Array;
                c_type   : Interfaces.C.Char_Array;
                value    : XrmValuePtr);                    -- Xresource.h:187

    pragma Inline(XrmPutResource);

    procedure XrmQPutStringResource(
                database: access XrmDatabase;
                bindings: XrmBindingList;
                quarks  : XrmQuarkList;
                value   : X.Strings.const_charp);           -- Xresource.h:196

    pragma Import(C, XrmQPutStringResource, "XrmQPutStringResource");

    procedure XrmQPutStringResource(
                database: access XrmDatabase;
                bindings: XrmBindingList;
                quarks  : XrmQuarkList;
                value   : Interfaces.C.Char_Array);         -- Xresource.h:196

    pragma Inline(XrmQPutStringResource);

    procedure XrmPutStringResource(
                database : access XrmDatabase;
                specifier: X.Strings.const_charp;
                value    : X.Strings.const_charp);          -- Xresource.h:205

    pragma Import(C, XrmPutStringResource, "XrmPutStringResource");

    procedure XrmPutStringResource(
                database : access XrmDatabase;
                specifier: Interfaces.C.Char_Array;
                value    : Interfaces.C.Char_Array);        -- Xresource.h:205

    pragma Inline(XrmPutStringResource);

    procedure XrmPutLineResource(
                database: access XrmDatabase;
                line    : X.Strings.const_charp);           -- Xresource.h:213

    pragma Import(C, XrmPutLineResource, "XrmPutLineResource");

    procedure XrmPutLineResource(
                database: access XrmDatabase;
                line    : Interfaces.C.Char_Array);         -- Xresource.h:213

    pragma Inline(XrmPutLineResource);

    procedure XrmQGetResource(
                database         : XrmDatabase;
                quark_name       : XrmNameList;
                quark_class      : XrmClassList;
                quark_type_return: access XrmRepresentation;
                value_return     : XrmValuePtr);            -- Xresource.h:220

    function XrmGetResource(
                database       : XrmDatabase;
                str_name       : X.Strings.const_charp;
                str_class      : X.Strings.const_charp;
                str_type_return: X.Strings.charp_vector;
                value_return   : XrmValuePtr)
               return X.Xlib.Bool;                          -- Xresource.h:230

    pragma Import(C, XrmGetResource, "XrmGetResource");

    function XrmGetResource(
                database       : XrmDatabase;
                str_name       : Interfaces.C.Char_Array;
                str_class      : Interfaces.C.Char_Array;
                str_type_return: X.Strings.charp_vector;
                value_return   : XrmValuePtr)
               return X.signed_int;                         -- Xresource.h:230

    pragma Inline(XrmGetResource);

    function XrmQGetSearchList(
                database   : XrmDatabase;
                names      : XrmNameList;
                classes    : XrmClassList;
                list_return: access XrmHashTable;
                list_length: X.signed_int)
               return X.Xlib.Bool;                          -- Xresource.h:240

    function XrmQGetSearchResource(
                list        : access XrmHashTable;
                name        : XrmName;
                class       : XrmClass;
                type_return : access XrmRepresentation;
                value_return: XrmValuePtr)
               return X.Xlib.Bool;                          -- Xresource.h:250

    procedure XrmSetDatabase(
                display : access X.Xlib.Display;
                database: XrmDatabase);                     -- Xresource.h:266

    function XrmGetDatabase(
                display: access X.Xlib.Display)
               return XrmDatabase;                          -- Xresource.h:273

    function XrmGetFileDatabase(
                filename: X.Strings.const_charp)
               return XrmDatabase;                          -- Xresource.h:279

    pragma Import(C, XrmGetFileDatabase, "XrmGetFileDatabase");

    function XrmGetFileDatabase(
                filename: Interfaces.C.Char_Array)
               return XrmDatabase;                          -- Xresource.h:279

    pragma Inline(XrmGetFileDatabase);

    function XrmCombineFileDatabase(
                filename: X.Strings.const_charp;
                target  : access XrmHashBucket;
                override: X.Xlib.Bool)
               return X.Xlib.Status;                        -- Xresource.h:285

    pragma Import(C, XrmCombineFileDatabase, "XrmCombineFileDatabase");

    function XrmCombineFileDatabase(
                filename: Interfaces.C.Char_Array;
                target  : access XrmHashBucket;
                override: X.signed_int)
               return X.signed_int;                         -- Xresource.h:285

    pragma Inline(XrmCombineFileDatabase);

    function XrmGetStringDatabase(
                data: X.Strings.const_charp)
               return XrmDatabase;                          -- Xresource.h:293

    pragma Import(C, XrmGetStringDatabase, "XrmGetStringDatabase");

    function XrmGetStringDatabase(
                data: Interfaces.C.Char_Array)
               return XrmDatabase;                          -- Xresource.h:293

    pragma Inline(XrmGetStringDatabase);

    procedure XrmPutFileDatabase(
                database: XrmDatabase;
                filename: X.Strings.const_charp);           -- Xresource.h:299

    pragma Import(C, XrmPutFileDatabase, "XrmPutFileDatabase");

    procedure XrmPutFileDatabase(
                database: XrmDatabase;
                filename: Interfaces.C.Char_Array);         -- Xresource.h:299

    pragma Inline(XrmPutFileDatabase);

    procedure XrmMergeDatabases(
                source_db: XrmDatabase;
                target_db: access XrmDatabase);             -- Xresource.h:306

    procedure XrmCombineDatabase(
                source_db: XrmDatabase;
                target_db: access XrmDatabase;
                override : X.Xlib.Bool);                    -- Xresource.h:313

    type proc_access is access function (
                db      : access XrmDatabase;
                bindings: XrmBindingList;
                quarks  : XrmQuarkList;
                c_type  : access XrmRepresentation;
                value   : access XrmValue;
                closure : X.Xlib.XPointer)
               return X.Xlib.Bool;                          -- Xresource.h:330

    function XrmEnumerateDatabase(
                db          : XrmDatabase;
                name_prefix : XrmNameList;
                class_prefix: XrmClassList;
                mode        : X.signed_int;
                proc        : proc_access;
                closure     : X.Xlib.XPointer)
               return X.Xlib.Bool;                          -- Xresource.h:324

    function XrmLocaleOfDatabase(
                database: XrmDatabase)
               return X.Strings.charp;                      -- Xresource.h:344

    procedure XrmParseCommand(
                database   : access XrmDatabase;
                table      : XrmOptionDescList;
                table_count: X.signed_int;
                name       : X.Strings.const_charp;
                argc_in_out: access X.signed_int;
                argv_in_out: X.Strings.charp_vector);       -- Xresource.h:377

    pragma Import(C, XrmParseCommand, "XrmParseCommand");

    procedure XrmParseCommand(
                database   : access XrmDatabase;
                table      : XrmOptionDescList;
                table_count: X.signed_int;
                name       : Interfaces.C.Char_Array;
                argc_in_out: access X.signed_int;
                argv_in_out: X.Strings.charp_vector);       -- Xresource.h:377

    pragma Inline(XrmParseCommand);

    -- ******************
    -- Xresource.h macros
    -- ******************

    function XrmStringsEqual(S1, S2: XrmString) return Boolean;
                                                            -- Xresource.h:97

    function XrmNameToString (Name: XrmName) return XrmString;
                                                            -- Xresource.h:131

    function XrmStringToName (String: XrmString) return XrmName;
                                                            -- Xresource.h:132

    procedure XrmStringToNameList (                         -- Xresource.h:133
        Str         : XrmString; 
        Names_Return: XrmNameList);

    function XrmClassToString (Class: XrmClass) return XrmString;
                                                            -- Xresource.h:137

    function XrmStringToClass (String: XrmString) return XrmClass;
                                                            -- Xresource.h:138

    procedure XrmStringToClassList (                        -- Xresource.h:139
        Str         : XrmString;
        Class_Return: XrmClassList);

    function XrmStringToRepresentation (String: XrmString)  -- Xresource.h:150
        return XrmRepresentation;

    function XrmRepresentationToString (Rep: XrmRepresentation)
        return XrmString;                                   -- Xresource.h:151

private

    pragma Import(C, Xpermalloc, "Xpermalloc");             -- Xresource.h:53

    pragma Import(C, XrmQuarkToString, "XrmQuarkToString"); -- Xresource.h:85
    pragma Import(C, XrmUniqueQuark, "XrmUniqueQuark");     -- Xresource.h:91
    pragma Import(C, XrmDestroyDatabase, "XrmDestroyDatabase");
                                                            -- Xresource.h:171
    pragma Import(C, XrmQPutResource, "XrmQPutResource");   -- Xresource.h:177
    pragma Import(C, XrmQGetResource, "XrmQGetResource");   -- Xresource.h:220
    pragma Import(C, XrmQGetSearchList, "XrmQGetSearchList");
                                                            -- Xresource.h:240
    pragma Import(C, XrmQGetSearchResource, "XrmQGetSearchResource");
                                                            -- Xresource.h:250
    pragma Import(C, XrmSetDatabase, "XrmSetDatabase");     -- Xresource.h:266
    pragma Import(C, XrmGetDatabase, "XrmGetDatabase");     -- Xresource.h:273
    pragma Import(C, XrmMergeDatabases, "XrmMergeDatabases");
                                                            -- Xresource.h:306
    pragma Import(C, XrmCombineDatabase, "XrmCombineDatabase");
                                                            -- Xresource.h:313
    pragma Import(C, XrmEnumerateDatabase, "XrmEnumerateDatabase");
                                                            -- Xresource.h:324

    pragma Import(C, XrmLocaleOfDatabase, "XrmLocaleOfDatabase");
                                                            -- Xresource.h:344

    pragma Convention(C, XrmValue);
    pragma Convention(C, XrmOptionDescRec);

    pragma Inline(XrmStringsEqual);
    pragma Inline(XrmNameToString);
    pragma Inline(XrmStringToName);
    pragma Inline(XrmStringToNameList);
    pragma Inline(XrmClassToString);
    pragma Inline(XrmStringToClass);
    pragma Inline(XrmStringToClassList);
    pragma Inline(XrmStringToRepresentation);
    pragma Inline(XrmRepresentationToString);

    pragma Convention(C, proc_access);

end X.Xresource;
