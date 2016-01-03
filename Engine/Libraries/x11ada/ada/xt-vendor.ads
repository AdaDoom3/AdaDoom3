-- $Source: /home/harp/1/proto/monoBANK/xbind/xt-vendor.ads,v $ 
-- $Revision: 1.2 $ $Date: 95/12/05 08:53:54 $ $Author: mg $ 

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
-- see the accompanying file Vendor.h.
-- --------------------------------------------------------------------------

with Xt.Intrinsic;

package Xt.Vendor is

    type VendorShellClassRec is private;                    -- Vendor.h:44

    type VendorShellWidgetClass is access all VendorShellClassRec;
                                                            -- Vendor.h:44

    vendorShellWidgetClass_obj: Xt.Intrinsic.WidgetClass;   -- Vendor.h:46

private

    type VendorShellClassRec is null record;                -- Vendor.h:44
    pragma Import(C, vendorShellWidgetClass_obj, "vendorShellWidgetClass");
                                                            -- Vendor.h:46

end Xt.Vendor;
