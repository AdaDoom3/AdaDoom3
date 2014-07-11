-- /***************************************************************************/
-- /*                                                                         */
-- /*  t1driver.c                                                             */
-- /*                                                                         */
-- /*    Type 1 driver interface (body).                                      */
-- /*                                                                         */
-- /*  Copyright 1996-2004, 2006, 2007, 2009, 2011, 2013 by                   */
-- /*  David Turner, Robert Wilhelm, and Werner Lemberg.                      */
-- /*                                                                         */
-- /*  This file is part of the FreeType project, and may only be used,       */
-- /*  modified, and distributed under the terms of the FreeType project      */
-- /*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
-- /*  this file you indicate that you have read the license and              */
-- /*  understand and accept it fully.                                        */
-- /*                                                                         */
-- /***************************************************************************/


-- #include <ft2build.h>
-- #include "t1driver.h"
-- #include "t1gload.h"
-- #include "t1load.h"

-- #include "t1errors.h"

-- #ifndef T1_CONFIG_OPTION_NO_AFM
-- #include "t1afm.h"
-- #endif

-- #include FT_INTERNAL_DEBUG_H
-- #include FT_INTERNAL_STREAM_H

-- #include FT_SERVICE_MULTIPLE_MASTERS_H
-- #include FT_SERVICE_GLYPH_DICT_H
-- #include FT_SERVICE_XFREE86_NAME_H
-- #include FT_SERVICE_POSTSCRIPT_NAME_H
-- #include FT_SERVICE_POSTSCRIPT_CMAPS_H
-- #include FT_SERVICE_POSTSCRIPT_INFO_H
-- #include FT_SERVICE_KERNING_H


--   /*************************************************************************/
--   /*                                                                       */
--   /* The macro FT_COMPONENT is used in trace mode.  It is an implicit      */
--   /* parameter of the FT_TRACE() and FT_ERROR() macros, used to print/log  */
--   /* messages during execution.                                            */
--   /*                                                                       */
-- #undef  FT_COMPONENT
-- #define FT_COMPONENT  trace_t1driver

--  /*
--   *  GLYPH DICT SERVICE
--   *
--   */

--   static FT_Error
--   t1_get_glyph_name( T1_Face     face,
--                      FT_UInt     glyph_index,
--                      FT_Pointer  buffer,
--                      FT_UInt     buffer_max )
--   {
--     FT_STRCPYN( buffer, face->type1.glyph_names[glyph_index], buffer_max );

--     return FT_Err_Ok;
--   }


--   static FT_UInt
--   t1_get_name_index( T1_Face     face,
--                      FT_String*  glyph_name )
--   {
--     FT_Int  i;


--     for ( i = 0; i < face->type1.num_glyphs; i++ )
--     {
--       FT_String*  gname = face->type1.glyph_names[i];


--       if ( !ft_strcmp( glyph_name, gname ) )
--         return (FT_UInt)i;
--     }

--     return 0;
--   }


--   static const FT_Service_GlyphDictRec  t1_service_glyph_dict =
--   {
--     (FT_GlyphDict_GetNameFunc)  t1_get_glyph_name,
--     (FT_GlyphDict_NameIndexFunc)t1_get_name_index
--   };


--   /*
--    *  POSTSCRIPT NAME SERVICE
--    *
--    */

--   static const char*
--   t1_get_ps_name( T1_Face  face )
--   {
--     return (const char*) face->type1.font_name;
--   }


--   static const FT_Service_PsFontNameRec  t1_service_ps_name =
--   {
--     (FT_PsName_GetFunc)t1_get_ps_name
--   };


--   /*
--    *  MULTIPLE MASTERS SERVICE
--    *
--    */

-- #ifndef T1_CONFIG_OPTION_NO_MM_SUPPORT
--   static const FT_Service_MultiMastersRec  t1_service_multi_masters =
--   {
--     (FT_Get_MM_Func)        T1_Get_Multi_Master,
--     (FT_Set_MM_Design_Func) T1_Set_MM_Design,
--     (FT_Set_MM_Blend_Func)  T1_Set_MM_Blend,
--     (FT_Get_MM_Var_Func)    T1_Get_MM_Var,
--     (FT_Set_Var_Design_Func)T1_Set_Var_Design
--   };
-- #endif


--   /*
--    *  POSTSCRIPT INFO SERVICE
--    *
--    */

--   static FT_Error
--   t1_ps_get_font_info( FT_Face          face,
--                        PS_FontInfoRec*  afont_info )
--   {
--     *afont_info = ((T1_Face)face)->type1.font_info;

--     return FT_Err_Ok;
--   }


--   static FT_Error
--   t1_ps_get_font_extra( FT_Face           face,
--                         PS_FontExtraRec*  afont_extra )
--   {
--     *afont_extra = ((T1_Face)face)->type1.font_extra;

--     return FT_Err_Ok;
--   }


--   static FT_Int
--   t1_ps_has_glyph_names( FT_Face  face )
--   {
--     FT_UNUSED( face );

--     return 1;
--   }


--   static FT_Error
--   t1_ps_get_font_private( FT_Face         face,
--                           PS_PrivateRec*  afont_private )
--   {
--     *afont_private = ((T1_Face)face)->type1.private_dict;

--     return FT_Err_Ok;
--   }


--   static FT_Long
--   t1_ps_get_font_value( FT_Face       face,
--                         PS_Dict_Keys  key,
--                         FT_UInt       idx,
--                         void         *value,
--                         FT_Long       value_len )
--   {
--     FT_Long  retval = -1;
--     T1_Face  t1face = (T1_Face)face;
--     T1_Font  type1  = &t1face->type1;


--     switch ( key )
--     {
--     case PS_DICT_FONT_TYPE:
--       retval = sizeof ( type1->font_type );
--       if ( value && value_len >= retval )
--         *((FT_Byte *)value) = type1->font_type;
--       break;

--     case PS_DICT_FONT_MATRIX:
--       if ( idx < sizeof ( type1->font_matrix ) /
--                    sizeof ( type1->font_matrix.xx ) )
--       {
--         FT_Fixed  val = 0;


--         retval = sizeof ( val );
--         if ( value && value_len >= retval )
--         {
--           switch ( idx )
--           {
--           case 0:
--             val = type1->font_matrix.xx;
--             break;
--           case 1:
--             val = type1->font_matrix.xy;
--             break;
--           case 2:
--             val = type1->font_matrix.yx;
--             break;
--           case 3:
--             val = type1->font_matrix.yy;
--             break;
--           }
--           *((FT_Fixed *)value) = val;
--         }
--       }
--       break;

--     case PS_DICT_FONT_BBOX:
--       if ( idx < sizeof ( type1->font_bbox ) /
--                    sizeof ( type1->font_bbox.xMin ) )
--       {
--         FT_Fixed val = 0;


--         retval = sizeof ( val );
--         if ( value && value_len >= retval )
--         {
--           switch ( idx )
--           {
--           case 0:
--             val = type1->font_bbox.xMin;
--             break;
--           case 1:
--             val = type1->font_bbox.yMin;
--             break;
--           case 2:
--             val = type1->font_bbox.xMax;
--             break;
--           case 3:
--             val = type1->font_bbox.yMax;
--             break;
--           }
--           *((FT_Fixed *)value) = val;
--         }
--       }
--       break;

--     case PS_DICT_PAINT_TYPE:
--       retval = sizeof ( type1->paint_type );
--       if ( value && value_len >= retval )
--         *((FT_Byte *)value) = type1->paint_type;
--       break;

--     case PS_DICT_FONT_NAME:
--       retval = (FT_Long)( ft_strlen( type1->font_name ) + 1 );
--       if ( value && value_len >= retval )
--         ft_memcpy( value, (void *)( type1->font_name ), retval );
--       break;

--     case PS_DICT_UNIQUE_ID:
--       retval = sizeof ( type1->private_dict.unique_id );
--       if ( value && value_len >= retval )
--         *((FT_Int *)value) = type1->private_dict.unique_id;
--       break;

--     case PS_DICT_NUM_CHAR_STRINGS:
--       retval = sizeof ( type1->num_glyphs );
--       if ( value && value_len >= retval )
--         *((FT_Int *)value) = type1->num_glyphs;
--       break;

--     case PS_DICT_CHAR_STRING_KEY:
--       if ( idx < (FT_UInt)type1->num_glyphs )
--       {
--         retval = (FT_Long)( ft_strlen( type1->glyph_names[idx] ) + 1 );
--         if ( value && value_len >= retval )
--         {
--           ft_memcpy( value, (void *)( type1->glyph_names[idx] ), retval );
--           ((FT_Char *)value)[retval - 1] = (FT_Char)'\0';
--         }
--       }
--       break;

--     case PS_DICT_CHAR_STRING:
--       if ( idx < (FT_UInt)type1->num_glyphs )
--       {
--         retval = (FT_Long)( type1->charstrings_len[idx] + 1 );
--         if ( value && value_len >= retval )
--         {
--           ft_memcpy( value, (void *)( type1->charstrings[idx] ),
--                      retval - 1 );
--           ((FT_Char *)value)[retval - 1] = (FT_Char)'\0';
--         }
--       }
--       break;

--     case PS_DICT_ENCODING_TYPE:
--       retval = sizeof ( type1->encoding_type );
--       if ( value && value_len >= retval )
--         *((T1_EncodingType *)value) = type1->encoding_type;
--       break;

--     case PS_DICT_ENCODING_ENTRY:
--       if ( type1->encoding_type == T1_ENCODING_TYPE_ARRAY &&
--            idx < (FT_UInt)type1->encoding.num_chars       )
--       {
--         retval = (FT_Long)( ft_strlen( type1->encoding.char_name[idx] ) + 1 );
--         if ( value && value_len >= retval )
--         {
--           ft_memcpy( value, (void *)( type1->encoding.char_name[idx] ),
--                      retval - 1 );
--           ((FT_Char *)value)[retval - 1] = (FT_Char)'\0';
--         }
--       }
--       break;

--     case PS_DICT_NUM_SUBRS:
--       retval = sizeof ( type1->num_subrs );
--       if ( value && value_len >= retval )
--         *((FT_Int *)value) = type1->num_subrs;
--       break;

--     case PS_DICT_SUBR:
--       if ( idx < (FT_UInt)type1->num_subrs )
--       {
--         retval = (FT_Long)( type1->subrs_len[idx] + 1 );
--         if ( value && value_len >= retval )
--         {
--           ft_memcpy( value, (void *)( type1->subrs[idx] ), retval - 1 );
--           ((FT_Char *)value)[retval - 1] = (FT_Char)'\0';
--         }
--       }
--       break;

--     case PS_DICT_STD_HW:
--       retval = sizeof ( type1->private_dict.standard_width[0] );
--       if ( value && value_len >= retval )
--         *((FT_UShort *)value) = type1->private_dict.standard_width[0];
--       break;

--     case PS_DICT_STD_VW:
--       retval = sizeof ( type1->private_dict.standard_height[0] );
--       if ( value && value_len >= retval )
--         *((FT_UShort *)value) = type1->private_dict.standard_height[0];
--       break;

--     case PS_DICT_NUM_BLUE_VALUES:
--       retval = sizeof ( type1->private_dict.num_blue_values );
--       if ( value && value_len >= retval )
--         *((FT_Byte *)value) = type1->private_dict.num_blue_values;
--       break;

--     case PS_DICT_BLUE_VALUE:
--       if ( idx < type1->private_dict.num_blue_values )
--       {
--         retval = sizeof ( type1->private_dict.blue_values[idx] );
--         if ( value && value_len >= retval )
--           *((FT_Short *)value) = type1->private_dict.blue_values[idx];
--       }
--       break;

--     case PS_DICT_BLUE_SCALE:
--       retval = sizeof ( type1->private_dict.blue_scale );
--       if ( value && value_len >= retval )
--         *((FT_Fixed *)value) = type1->private_dict.blue_scale;
--       break;

--     case PS_DICT_BLUE_FUZZ:
--       retval = sizeof ( type1->private_dict.blue_fuzz );
--       if ( value && value_len >= retval )
--         *((FT_Int *)value) = type1->private_dict.blue_fuzz;
--       break;

--     case PS_DICT_BLUE_SHIFT:
--       retval = sizeof ( type1->private_dict.blue_shift );
--       if ( value && value_len >= retval )
--         *((FT_Int *)value) = type1->private_dict.blue_shift;
--       break;

--     case PS_DICT_NUM_OTHER_BLUES:
--       retval = sizeof ( type1->private_dict.num_other_blues );
--       if ( value && value_len >= retval )
--         *((FT_Byte *)value) = type1->private_dict.num_other_blues;
--       break;

--     case PS_DICT_OTHER_BLUE:
--       if ( idx < type1->private_dict.num_other_blues )
--       {
--         retval = sizeof ( type1->private_dict.other_blues[idx] );
--         if ( value && value_len >= retval )
--           *((FT_Short *)value) = type1->private_dict.other_blues[idx];
--       }
--       break;

--     case PS_DICT_NUM_FAMILY_BLUES:
--       retval = sizeof ( type1->private_dict.num_family_blues );
--       if ( value && value_len >= retval )
--         *((FT_Byte *)value) = type1->private_dict.num_family_blues;
--       break;

--     case PS_DICT_FAMILY_BLUE:
--       if ( idx < type1->private_dict.num_family_blues )
--       {
--         retval = sizeof ( type1->private_dict.family_blues[idx] );
--         if ( value && value_len >= retval )
--           *((FT_Short *)value) = type1->private_dict.family_blues[idx];
--       }
--       break;

--     case PS_DICT_NUM_FAMILY_OTHER_BLUES:
--       retval = sizeof ( type1->private_dict.num_family_other_blues );
--       if ( value && value_len >= retval )
--         *((FT_Byte *)value) = type1->private_dict.num_family_other_blues;
--       break;

--     case PS_DICT_FAMILY_OTHER_BLUE:
--       if ( idx < type1->private_dict.num_family_other_blues )
--       {
--         retval = sizeof ( type1->private_dict.family_other_blues[idx] );
--         if ( value && value_len >= retval )
--           *((FT_Short *)value) = type1->private_dict.family_other_blues[idx];
--       }
--       break;

--     case PS_DICT_NUM_STEM_SNAP_H:
--       retval = sizeof ( type1->private_dict.num_snap_widths );
--       if ( value && value_len >= retval )
--         *((FT_Byte *)value) = type1->private_dict.num_snap_widths;
--       break;

--     case PS_DICT_STEM_SNAP_H:
--       if ( idx < type1->private_dict.num_snap_widths )
--       {
--         retval = sizeof ( type1->private_dict.snap_widths[idx] );
--         if ( value && value_len >= retval )
--           *((FT_Short *)value) = type1->private_dict.snap_widths[idx];
--       }
--       break;

--     case PS_DICT_NUM_STEM_SNAP_V:
--       retval = sizeof ( type1->private_dict.num_snap_heights );
--       if ( value && value_len >= retval )
--         *((FT_Byte *)value) = type1->private_dict.num_snap_heights;
--       break;

--     case PS_DICT_STEM_SNAP_V:
--       if ( idx < type1->private_dict.num_snap_heights )
--       {
--         retval = sizeof ( type1->private_dict.snap_heights[idx] );
--         if ( value && value_len >= retval )
--           *((FT_Short *)value) = type1->private_dict.snap_heights[idx];
--       }
--       break;

--     case PS_DICT_RND_STEM_UP:
--       retval = sizeof ( type1->private_dict.round_stem_up );
--       if ( value && value_len >= retval )
--         *((FT_Bool *)value) = type1->private_dict.round_stem_up;
--       break;

--     case PS_DICT_FORCE_BOLD:
--       retval = sizeof ( type1->private_dict.force_bold );
--       if ( value && value_len >= retval )
--         *((FT_Bool *)value) = type1->private_dict.force_bold;
--       break;

--     case PS_DICT_MIN_FEATURE:
--       if ( idx < sizeof ( type1->private_dict.min_feature ) /
--                    sizeof ( type1->private_dict.min_feature[0] ) )
--       {
--         retval = sizeof ( type1->private_dict.min_feature[idx] );
--         if ( value && value_len >= retval )
--           *((FT_Short *)value) = type1->private_dict.min_feature[idx];
--       }
--       break;

--     case PS_DICT_LEN_IV:
--       retval = sizeof ( type1->private_dict.lenIV );
--       if ( value && value_len >= retval )
--         *((FT_Int *)value) = type1->private_dict.lenIV;
--       break;

--     case PS_DICT_PASSWORD:
--       retval = sizeof ( type1->private_dict.password );
--       if ( value && value_len >= retval )
--         *((FT_Long *)value) = type1->private_dict.password;
--       break;

--     case PS_DICT_LANGUAGE_GROUP:
--       retval = sizeof ( type1->private_dict.language_group );
--       if ( value && value_len >= retval )
--         *((FT_Long *)value) = type1->private_dict.language_group;
--       break;

--     case PS_DICT_IS_FIXED_PITCH:
--       retval = sizeof ( type1->font_info.is_fixed_pitch );
--       if ( value && value_len >= retval )
--         *((FT_Bool *)value) = type1->font_info.is_fixed_pitch;
--       break;

--     case PS_DICT_UNDERLINE_POSITION:
--       retval = sizeof ( type1->font_info.underline_position );
--       if ( value && value_len >= retval )
--         *((FT_Short *)value) = type1->font_info.underline_position;
--       break;

--     case PS_DICT_UNDERLINE_THICKNESS:
--       retval = sizeof ( type1->font_info.underline_thickness );
--       if ( value && value_len >= retval )
--         *((FT_UShort *)value) = type1->font_info.underline_thickness;
--       break;

--     case PS_DICT_FS_TYPE:
--       retval = sizeof ( type1->font_extra.fs_type );
--       if ( value && value_len >= retval )
--         *((FT_UShort *)value) = type1->font_extra.fs_type;
--       break;

--     case PS_DICT_VERSION:
--       retval = (FT_Long)( ft_strlen( type1->font_info.version ) + 1 );
--       if ( value && value_len >= retval )
--         ft_memcpy( value, (void *)( type1->font_info.version ), retval );
--       break;

--     case PS_DICT_NOTICE:
--       retval = (FT_Long)( ft_strlen( type1->font_info.notice ) + 1 );
--       if ( value && value_len >= retval )
--         ft_memcpy( value, (void *)( type1->font_info.notice ), retval );
--       break;

--     case PS_DICT_FULL_NAME:
--       retval = (FT_Long)( ft_strlen( type1->font_info.full_name ) + 1 );
--       if ( value && value_len >= retval )
--         ft_memcpy( value, (void *)( type1->font_info.full_name ), retval );
--       break;

--     case PS_DICT_FAMILY_NAME:
--       retval = (FT_Long)( ft_strlen( type1->font_info.family_name ) + 1 );
--       if ( value && value_len >= retval )
--         ft_memcpy( value, (void *)( type1->font_info.family_name ), retval );
--       break;

--     case PS_DICT_WEIGHT:
--       retval = (FT_Long)( ft_strlen( type1->font_info.weight ) + 1 );
--       if ( value && value_len >= retval )
--         ft_memcpy( value, (void *)( type1->font_info.weight ), retval );
--       break;

--     case PS_DICT_ITALIC_ANGLE:
--       retval = sizeof ( type1->font_info.italic_angle );
--       if ( value && value_len >= retval )
--         *((FT_Long *)value) = type1->font_info.italic_angle;
--       break;

--     default:
--       break;
--     }

--     return retval;
--   }


--   static const FT_Service_PsInfoRec  t1_service_ps_info =
--   {
--     (PS_GetFontInfoFunc)   t1_ps_get_font_info,
--     (PS_GetFontExtraFunc)  t1_ps_get_font_extra,
--     (PS_HasGlyphNamesFunc) t1_ps_has_glyph_names,
--     (PS_GetFontPrivateFunc)t1_ps_get_font_private,
--     (PS_GetFontValueFunc)  t1_ps_get_font_value,
--   };


-- #ifndef T1_CONFIG_OPTION_NO_AFM
--   static const FT_Service_KerningRec  t1_service_kerning =
--   {
--     T1_Get_Track_Kerning,
--   };
-- #endif


--   /*
--    *  SERVICE LIST
--    *
--    */

--   static const FT_ServiceDescRec  t1_services[] =
--   {
--     { FT_SERVICE_ID_POSTSCRIPT_FONT_NAME, &t1_service_ps_name },
--     { FT_SERVICE_ID_GLYPH_DICT,           &t1_service_glyph_dict },
--     { FT_SERVICE_ID_XF86_NAME,            FT_XF86_FORMAT_TYPE_1 },
--     { FT_SERVICE_ID_POSTSCRIPT_INFO,      &t1_service_ps_info },

-- #ifndef T1_CONFIG_OPTION_NO_AFM
--     { FT_SERVICE_ID_KERNING,              &t1_service_kerning },
-- #endif

-- #ifndef T1_CONFIG_OPTION_NO_MM_SUPPORT
--     { FT_SERVICE_ID_MULTI_MASTERS,        &t1_service_multi_masters },
-- #endif
--     { NULL, NULL }
--   };


--   FT_CALLBACK_DEF( FT_Module_Interface )
--   Get_Interface( FT_Module         module,
--                  const FT_String*  t1_interface )
--   {
--     FT_UNUSED( module );

--     return ft_service_list_lookup( t1_services, t1_interface );
--   }


-- #ifndef T1_CONFIG_OPTION_NO_AFM

--   /*************************************************************************/
--   /*                                                                       */
--   /* <Function>                                                            */
--   /*    Get_Kerning                                                        */
--   /*                                                                       */
--   /* <Description>                                                         */
--   /*    A driver method used to return the kerning vector between two      */
--   /*    glyphs of the same face.                                           */
--   /*                                                                       */
--   /* <Input>                                                               */
--   /*    face        :: A handle to the source face object.                 */
--   /*                                                                       */
--   /*    left_glyph  :: The index of the left glyph in the kern pair.       */
--   /*                                                                       */
--   /*    right_glyph :: The index of the right glyph in the kern pair.      */
--   /*                                                                       */
--   /* <Output>                                                              */
--   /*    kerning     :: The kerning vector.  This is in font units for      */
--   /*                   scalable formats, and in pixels for fixed-sizes     */
--   /*                   formats.                                            */
--   /*                                                                       */
--   /* <Return>                                                              */
--   /*    FreeType error code.  0 means success.                             */
--   /*                                                                       */
--   /* <Note>                                                                */
--   /*    Only horizontal layouts (left-to-right & right-to-left) are        */
--   /*    supported by this function.  Other layouts, or more sophisticated  */
--   /*    kernings are out of scope of this method (the basic driver         */
--   /*    interface is meant to be simple).                                  */
--   /*                                                                       */
--   /*    They can be implemented by format-specific interfaces.             */
--   /*                                                                       */
--   static FT_Error
--   Get_Kerning( FT_Face     t1face,        /* T1_Face */
--                FT_UInt     left_glyph,
--                FT_UInt     right_glyph,
--                FT_Vector*  kerning )
--   {
--     T1_Face  face = (T1_Face)t1face;


--     kerning->x = 0;
--     kerning->y = 0;

--     if ( face->afm_data )
--       T1_Get_Kerning( (AFM_FontInfo)face->afm_data,
--                       left_glyph,
--                       right_glyph,
--                       kerning );

--     return FT_Err_Ok;
--   }


-- #endif /* T1_CONFIG_OPTION_NO_AFM */


--   FT_CALLBACK_TABLE_DEF
--   const FT_Driver_ClassRec  t1_driver_class =
--   {
--     {
--       FT_MODULE_FONT_DRIVER       |
--       FT_MODULE_DRIVER_SCALABLE   |
--       FT_MODULE_DRIVER_HAS_HINTER,

--       sizeof ( FT_DriverRec ),

--       "type1",
--       0x10000L,
--       0x20000L,

--       0,   /* format interface */

--       T1_Driver_Init,
--       T1_Driver_Done,
--       Get_Interface,
--     },

--     sizeof ( T1_FaceRec ),
--     sizeof ( T1_SizeRec ),
--     sizeof ( T1_GlyphSlotRec ),

--     T1_Face_Init,
--     T1_Face_Done,
--     T1_Size_Init,
--     T1_Size_Done,
--     T1_GlyphSlot_Init,
--     T1_GlyphSlot_Done,

--     T1_Load_Glyph,

-- #ifdef T1_CONFIG_OPTION_NO_AFM
--     0,                     /* FT_Face_GetKerningFunc */
--     0,                     /* FT_Face_AttachFunc     */
-- #else
--     Get_Kerning,
--     T1_Read_Metrics,
-- #endif
--     T1_Get_Advances,
--     T1_Size_Request,
--     0                      /* FT_Size_SelectFunc     */
--   };


-- /* END */
-- /***************************************************************************/
-- /*                                                                         */
-- /*  t1afm.c                                                                */
-- /*                                                                         */
-- /*    AFM support for Type 1 fonts (body).                                 */
-- /*                                                                         */
-- /*  Copyright 1996-2011, 2013 by                                           */
-- /*  David Turner, Robert Wilhelm, and Werner Lemberg.                      */
-- /*                                                                         */
-- /*  This file is part of the FreeType project, and may only be used,       */
-- /*  modified, and distributed under the terms of the FreeType project      */
-- /*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
-- /*  this file you indicate that you have read the license and              */
-- /*  understand and accept it fully.                                        */
-- /*                                                                         */
-- /***************************************************************************/


-- #include <ft2build.h>
-- #include "t1afm.h"
-- #include FT_INTERNAL_DEBUG_H
-- #include FT_INTERNAL_STREAM_H
-- #include FT_INTERNAL_POSTSCRIPT_AUX_H
-- #include "t1errors.h"


--   /*************************************************************************/
--   /*                                                                       */
--   /* The macro FT_COMPONENT is used in trace mode.  It is an implicit      */
--   /* parameter of the FT_TRACE() and FT_ERROR() macros, used to print/log  */
--   /* messages during execution.                                            */
--   /*                                                                       */
-- #undef  FT_COMPONENT
-- #define FT_COMPONENT  trace_t1afm


--   FT_LOCAL_DEF( void )
--   T1_Done_Metrics( FT_Memory     memory,
--                    AFM_FontInfo  fi )
--   {
--     FT_FREE( fi->KernPairs );
--     fi->NumKernPair = 0;

--     FT_FREE( fi->TrackKerns );
--     fi->NumTrackKern = 0;

--     FT_FREE( fi );
--   }


--   /* read a glyph name and return the equivalent glyph index */
--   static FT_Int
--   t1_get_index( const char*  name,
--                 FT_Offset    len,
--                 void*        user_data )
--   {
--     T1_Font  type1 = (T1_Font)user_data;
--     FT_Int   n;


--     /* PS string/name length must be < 16-bit */
--     if ( len > 0xFFFFU )
--       return 0;

--     for ( n = 0; n < type1->num_glyphs; n++ )
--     {
--       char*  gname = (char*)type1->glyph_names[n];


--       if ( gname && gname[0] == name[0]        &&
--            ft_strlen( gname ) == len           &&
--            ft_strncmp( gname, name, len ) == 0 )
--         return n;
--     }

--     return 0;
--   }


-- #undef  KERN_INDEX
-- #define KERN_INDEX( g1, g2 )  ( ( (FT_ULong)(g1) << 16 ) | (g2) )


--   /* compare two kerning pairs */
--   FT_CALLBACK_DEF( int )
--   compare_kern_pairs( const void*  a,
--                       const void*  b )
--   {
--     AFM_KernPair  pair1 = (AFM_KernPair)a;
--     AFM_KernPair  pair2 = (AFM_KernPair)b;

--     FT_ULong  index1 = KERN_INDEX( pair1->index1, pair1->index2 );
--     FT_ULong  index2 = KERN_INDEX( pair2->index1, pair2->index2 );


--     if ( index1 > index2 )
--       return 1;
--     else if ( index1 < index2 )
--       return -1;
--     else
--       return 0;
--   }


--   /* parse a PFM file -- for now, only read the kerning pairs */
--   static FT_Error
--   T1_Read_PFM( FT_Face       t1_face,
--                FT_Stream     stream,
--                AFM_FontInfo  fi )
--   {
--     FT_Error      error  = FT_Err_Ok;
--     FT_Memory     memory = stream->memory;
--     FT_Byte*      start;
--     FT_Byte*      limit;
--     FT_Byte*      p;
--     AFM_KernPair  kp;
--     FT_Int        width_table_length;
--     FT_CharMap    oldcharmap;
--     FT_CharMap    charmap;
--     FT_Int        n;


--     start = (FT_Byte*)stream->cursor;
--     limit = (FT_Byte*)stream->limit;

--     /* Figure out how long the width table is.          */
--     /* This info is a little-endian short at offset 99. */
--     p = start + 99;
--     if ( p + 2 > limit )
--     {
--       error = FT_THROW( Unknown_File_Format );
--       goto Exit;
--     }
--     width_table_length = FT_PEEK_USHORT_LE( p );

--     p += 18 + width_table_length;
--     if ( p + 0x12 > limit || FT_PEEK_USHORT_LE( p ) < 0x12 )
--       /* extension table is probably optional */
--       goto Exit;

--     /* Kerning offset is 14 bytes from start of extensions table. */
--     p += 14;
--     p = start + FT_PEEK_ULONG_LE( p );

--     if ( p == start )
--       /* zero offset means no table */
--       goto Exit;

--     if ( p + 2 > limit )
--     {
--       error = FT_THROW( Unknown_File_Format );
--       goto Exit;
--     }

--     fi->NumKernPair = FT_PEEK_USHORT_LE( p );
--     p += 2;
--     if ( p + 4 * fi->NumKernPair > limit )
--     {
--       error = FT_THROW( Unknown_File_Format );
--       goto Exit;
--     }

--     /* Actually, kerning pairs are simply optional! */
--     if ( fi->NumKernPair == 0 )
--       goto Exit;

--     /* allocate the pairs */
--     if ( FT_QNEW_ARRAY( fi->KernPairs, fi->NumKernPair ) )
--       goto Exit;

--     /* now, read each kern pair */
--     kp             = fi->KernPairs;
--     limit          = p + 4 * fi->NumKernPair;

--     /* PFM kerning data are stored by encoding rather than glyph index, */
--     /* so find the PostScript charmap of this font and install it       */
--     /* temporarily.  If we find no PostScript charmap, then just use    */
--     /* the default and hope it is the right one.                        */
--     oldcharmap = t1_face->charmap;
--     charmap    = NULL;

--     for ( n = 0; n < t1_face->num_charmaps; n++ )
--     {
--       charmap = t1_face->charmaps[n];
--       /* check against PostScript pseudo platform */
--       if ( charmap->platform_id == 7 )
--       {
--         error = FT_Set_Charmap( t1_face, charmap );
--         if ( error )
--           goto Exit;
--         break;
--       }
--     }

--     /* Kerning info is stored as:             */
--     /*                                        */
--     /*   encoding of first glyph (1 byte)     */
--     /*   encoding of second glyph (1 byte)    */
--     /*   offset (little-endian short)         */
--     for ( ; p < limit ; p += 4 )
--     {
--       kp->index1 = FT_Get_Char_Index( t1_face, p[0] );
--       kp->index2 = FT_Get_Char_Index( t1_face, p[1] );

--       kp->x = (FT_Int)FT_PEEK_SHORT_LE(p + 2);
--       kp->y = 0;

--       kp++;
--     }

--     if ( oldcharmap != NULL )
--       error = FT_Set_Charmap( t1_face, oldcharmap );
--     if ( error )
--       goto Exit;

--     /* now, sort the kern pairs according to their glyph indices */
--     ft_qsort( fi->KernPairs, fi->NumKernPair, sizeof ( AFM_KernPairRec ),
--               compare_kern_pairs );

--   Exit:
--     if ( error )
--     {
--       FT_FREE( fi->KernPairs );
--       fi->NumKernPair = 0;
--     }

--     return error;
--   }


--   /* parse a metrics file -- either AFM or PFM depending on what */
--   /* it turns out to be                                          */
--   FT_LOCAL_DEF( FT_Error )
--   T1_Read_Metrics( FT_Face    t1_face,
--                    FT_Stream  stream )
--   {
--     PSAux_Service  psaux;
--     FT_Memory      memory  = stream->memory;
--     AFM_ParserRec  parser;
--     AFM_FontInfo   fi      = NULL;
--     FT_Error       error   = FT_ERR( Unknown_File_Format );
--     T1_Font        t1_font = &( (T1_Face)t1_face )->type1;


--     if ( FT_NEW( fi )                   ||
--          FT_FRAME_ENTER( stream->size ) )
--       goto Exit;

--     fi->FontBBox  = t1_font->font_bbox;
--     fi->Ascender  = t1_font->font_bbox.yMax;
--     fi->Descender = t1_font->font_bbox.yMin;

--     psaux = (PSAux_Service)( (T1_Face)t1_face )->psaux;
--     if ( psaux->afm_parser_funcs )
--     {
--       error = psaux->afm_parser_funcs->init( &parser,
--                                              stream->memory,
--                                              stream->cursor,
--                                              stream->limit );

--       if ( !error )
--       {
--         parser.FontInfo  = fi;
--         parser.get_index = t1_get_index;
--         parser.user_data = t1_font;

--         error = psaux->afm_parser_funcs->parse( &parser );
--         psaux->afm_parser_funcs->done( &parser );
--       }
--     }

--     if ( FT_ERR_EQ( error, Unknown_File_Format ) )
--     {
--       FT_Byte*  start = stream->cursor;


--       /* MS Windows allows versions up to 0x3FF without complaining */
--       if ( stream->size > 6                              &&
--            start[1] < 4                                  &&
--            FT_PEEK_ULONG_LE( start + 2 ) == stream->size )
--         error = T1_Read_PFM( t1_face, stream, fi );
--     }

--     if ( !error )
--     {
--       t1_font->font_bbox = fi->FontBBox;

--       t1_face->bbox.xMin =   fi->FontBBox.xMin            >> 16;
--       t1_face->bbox.yMin =   fi->FontBBox.yMin            >> 16;
--       /* no `U' suffix here to 0xFFFF! */
--       t1_face->bbox.xMax = ( fi->FontBBox.xMax + 0xFFFF ) >> 16;
--       t1_face->bbox.yMax = ( fi->FontBBox.yMax + 0xFFFF ) >> 16;

--       /* no `U' suffix here to 0x8000! */
--       t1_face->ascender  = (FT_Short)( ( fi->Ascender  + 0x8000 ) >> 16 );
--       t1_face->descender = (FT_Short)( ( fi->Descender + 0x8000 ) >> 16 );

--       if ( fi->NumKernPair )
--       {
--         t1_face->face_flags |= FT_FACE_FLAG_KERNING;
--         ( (T1_Face)t1_face )->afm_data = fi;
--         fi = NULL;
--       }
--     }

--     FT_FRAME_EXIT();

--   Exit:
--     if ( fi != NULL )
--       T1_Done_Metrics( memory, fi );

--     return error;
--   }


--   /* find the kerning for a given glyph pair */
--   FT_LOCAL_DEF( void )
--   T1_Get_Kerning( AFM_FontInfo  fi,
--                   FT_UInt       glyph1,
--                   FT_UInt       glyph2,
--                   FT_Vector*    kerning )
--   {
--     AFM_KernPair  min, mid, max;
--     FT_ULong      idx = KERN_INDEX( glyph1, glyph2 );


--     /* simple binary search */
--     min = fi->KernPairs;
--     max = min + fi->NumKernPair - 1;

--     while ( min <= max )
--     {
--       FT_ULong  midi;


--       mid  = min + ( max - min ) / 2;
--       midi = KERN_INDEX( mid->index1, mid->index2 );

--       if ( midi == idx )
--       {
--         kerning->x = mid->x;
--         kerning->y = mid->y;

--         return;
--       }

--       if ( midi < idx )
--         min = mid + 1;
--       else
--         max = mid - 1;
--     }

--     kerning->x = 0;
--     kerning->y = 0;
--   }


--   FT_LOCAL_DEF( FT_Error )
--   T1_Get_Track_Kerning( FT_Face    face,
--                         FT_Fixed   ptsize,
--                         FT_Int     degree,
--                         FT_Fixed*  kerning )
--   {
--     AFM_FontInfo  fi = (AFM_FontInfo)( (T1_Face)face )->afm_data;
--     FT_Int        i;


--     if ( !fi )
--       return FT_THROW( Invalid_Argument );

--     for ( i = 0; i < fi->NumTrackKern; i++ )
--     {
--       AFM_TrackKern  tk = fi->TrackKerns + i;


--       if ( tk->degree != degree )
--         continue;

--       if ( ptsize < tk->min_ptsize )
--         *kerning = tk->min_kern;
--       else if ( ptsize > tk->max_ptsize )
--         *kerning = tk->max_kern;
--       else
--       {
--         *kerning = FT_MulDiv( ptsize - tk->min_ptsize,
--                               tk->max_kern - tk->min_kern,
--                               tk->max_ptsize - tk->min_ptsize ) +
--                    tk->min_kern;
--       }
--     }

--     return FT_Err_Ok;
--   }


-- /* END */
-- /***************************************************************************/
-- /*                                                                         */
-- /*  t1load.c                                                               */
-- /*                                                                         */
-- /*    Type 1 font loader (body).                                           */
-- /*                                                                         */
-- /*  Copyright 1996-2013 by                                                 */
-- /*  David Turner, Robert Wilhelm, and Werner Lemberg.                      */
-- /*                                                                         */
-- /*  This file is part of the FreeType project, and may only be used,       */
-- /*  modified, and distributed under the terms of the FreeType project      */
-- /*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
-- /*  this file you indicate that you have read the license and              */
-- /*  understand and accept it fully.                                        */
-- /*                                                                         */
-- /***************************************************************************/


--   /*************************************************************************/
--   /*                                                                       */
--   /* This is the new and improved Type 1 data loader for FreeType 2.  The  */
--   /* old loader has several problems: it is slow, complex, difficult to    */
--   /* maintain, and contains incredible hacks to make it accept some        */
--   /* ill-formed Type 1 fonts without hiccup-ing.  Moreover, about 5% of    */
--   /* the Type 1 fonts on my machine still aren't loaded correctly by it.   */
--   /*                                                                       */
--   /* This version is much simpler, much faster and also easier to read and */
--   /* maintain by a great order of magnitude.  The idea behind it is to     */
--   /* _not_ try to read the Type 1 token stream with a state machine (i.e.  */
--   /* a Postscript-like interpreter) but rather to perform simple pattern   */
--   /* matching.                                                             */
--   /*                                                                       */
--   /* Indeed, nearly all data definitions follow a simple pattern like      */
--   /*                                                                       */
--   /*  ... /Field <data> ...                                                */
--   /*                                                                       */
--   /* where <data> can be a number, a boolean, a string, or an array of     */
--   /* numbers.  There are a few exceptions, namely the encoding, font name, */
--   /* charstrings, and subrs; they are handled with a special pattern       */
--   /* matching routine.                                                     */
--   /*                                                                       */
--   /* All other common cases are handled very simply.  The matching rules   */
--   /* are defined in the file `t1tokens.h' through the use of several       */
--   /* macros calls PARSE_XXX.  This file is included twice here; the first  */
--   /* time to generate parsing callback functions, the second time to       */
--   /* generate a table of keywords (with pointers to the associated         */
--   /* callback functions).                                                  */
--   /*                                                                       */
--   /* The function `parse_dict' simply scans *linearly* a given dictionary  */
--   /* (either the top-level or private one) and calls the appropriate       */
--   /* callback when it encounters an immediate keyword.                     */
--   /*                                                                       */
--   /* This is by far the fastest way one can find to parse and read all     */
--   /* data.                                                                 */
--   /*                                                                       */
--   /* This led to tremendous code size reduction.  Note that later, the     */
--   /* glyph loader will also be _greatly_ simplified, and the automatic     */
--   /* hinter will replace the clumsy `t1hinter'.                            */
--   /*                                                                       */
--   /*************************************************************************/


-- #include <ft2build.h>
-- #include FT_INTERNAL_DEBUG_H
-- #include FT_CONFIG_CONFIG_H
-- #include FT_MULTIPLE_MASTERS_H
-- #include FT_INTERNAL_TYPE1_TYPES_H
-- #include FT_INTERNAL_CALC_H

-- #include "t1load.h"
-- #include "t1errors.h"


-- #ifdef FT_CONFIG_OPTION_INCREMENTAL
-- #define IS_INCREMENTAL  (FT_Bool)( face->root.internal->incremental_interface != 0 )
-- #else
-- #define IS_INCREMENTAL  0
-- #endif


--   /*************************************************************************/
--   /*                                                                       */
--   /* The macro FT_COMPONENT is used in trace mode.  It is an implicit      */
--   /* parameter of the FT_TRACE() and FT_ERROR() macros, used to print/log  */
--   /* messages during execution.                                            */
--   /*                                                                       */
-- #undef  FT_COMPONENT
-- #define FT_COMPONENT  trace_t1load


-- #ifndef T1_CONFIG_OPTION_NO_MM_SUPPORT


--   /*************************************************************************/
--   /*************************************************************************/
--   /*****                                                               *****/
--   /*****                    MULTIPLE MASTERS SUPPORT                   *****/
--   /*****                                                               *****/
--   /*************************************************************************/
--   /*************************************************************************/

--   static FT_Error
--   t1_allocate_blend( T1_Face  face,
--                      FT_UInt  num_designs,
--                      FT_UInt  num_axis )
--   {
--     PS_Blend   blend;
--     FT_Memory  memory = face->root.memory;
--     FT_Error   error  = FT_Err_Ok;


--     blend = face->blend;
--     if ( !blend )
--     {
--       if ( FT_NEW( blend ) )
--         goto Exit;

--       blend->num_default_design_vector = 0;

--       face->blend = blend;
--     }

--     /* allocate design data if needed */
--     if ( num_designs > 0 )
--     {
--       if ( blend->num_designs == 0 )
--       {
--         FT_UInt  nn;


--         /* allocate the blend `private' and `font_info' dictionaries */
--         if ( FT_NEW_ARRAY( blend->font_infos[1], num_designs     ) ||
--              FT_NEW_ARRAY( blend->privates  [1], num_designs     ) ||
--              FT_NEW_ARRAY( blend->bboxes    [1], num_designs     ) ||
--              FT_NEW_ARRAY( blend->weight_vector, num_designs * 2 ) )
--           goto Exit;

--         blend->default_weight_vector = blend->weight_vector + num_designs;

--         blend->font_infos[0] = &face->type1.font_info;
--         blend->privates  [0] = &face->type1.private_dict;
--         blend->bboxes    [0] = &face->type1.font_bbox;

--         for ( nn = 2; nn <= num_designs; nn++ )
--         {
--           blend->font_infos[nn] = blend->font_infos[nn - 1] + 1;
--           blend->privates  [nn] = blend->privates  [nn - 1] + 1;
--           blend->bboxes    [nn] = blend->bboxes    [nn - 1] + 1;
--         }

--         blend->num_designs = num_designs;
--       }
--       else if ( blend->num_designs != num_designs )
--         goto Fail;
--     }

--     /* allocate axis data if needed */
--     if ( num_axis > 0 )
--     {
--       if ( blend->num_axis != 0 && blend->num_axis != num_axis )
--         goto Fail;

--       blend->num_axis = num_axis;
--     }

--     /* allocate the blend design pos table if needed */
--     num_designs = blend->num_designs;
--     num_axis    = blend->num_axis;
--     if ( num_designs && num_axis && blend->design_pos[0] == 0 )
--     {
--       FT_UInt  n;


--       if ( FT_NEW_ARRAY( blend->design_pos[0], num_designs * num_axis ) )
--         goto Exit;

--       for ( n = 1; n < num_designs; n++ )
--         blend->design_pos[n] = blend->design_pos[0] + num_axis * n;
--     }

--   Exit:
--     return error;

--   Fail:
--     error = FT_THROW( Invalid_File_Format );
--     goto Exit;
--   }


--   FT_LOCAL_DEF( FT_Error )
--   T1_Get_Multi_Master( T1_Face           face,
--                        FT_Multi_Master*  master )
--   {
--     PS_Blend  blend = face->blend;
--     FT_UInt   n;
--     FT_Error  error;


--     error = FT_THROW( Invalid_Argument );

--     if ( blend )
--     {
--       master->num_axis    = blend->num_axis;
--       master->num_designs = blend->num_designs;

--       for ( n = 0; n < blend->num_axis; n++ )
--       {
--         FT_MM_Axis*   axis = master->axis + n;
--         PS_DesignMap  map = blend->design_map + n;


--         axis->name    = blend->axis_names[n];
--         axis->minimum = map->design_points[0];
--         axis->maximum = map->design_points[map->num_points - 1];
--       }

--       error = FT_Err_Ok;
--     }

--     return error;
--   }


--   /*************************************************************************/
--   /*                                                                       */
--   /* Given a normalized (blend) coordinate, figure out the design          */
--   /* coordinate appropriate for that value.                                */
--   /*                                                                       */
--   FT_LOCAL_DEF( FT_Fixed )
--   mm_axis_unmap( PS_DesignMap  axismap,
--                  FT_Fixed      ncv )
--   {
--     int  j;


--     if ( ncv <= axismap->blend_points[0] )
--       return INT_TO_FIXED( axismap->design_points[0] );

--     for ( j = 1; j < axismap->num_points; ++j )
--     {
--       if ( ncv <= axismap->blend_points[j] )
--         return INT_TO_FIXED( axismap->design_points[j - 1] ) +
--                ( axismap->design_points[j] - axismap->design_points[j - 1] ) *
--                FT_DivFix( ncv - axismap->blend_points[j - 1],
--                           axismap->blend_points[j] -
--                             axismap->blend_points[j - 1] );
--     }

--     return INT_TO_FIXED( axismap->design_points[axismap->num_points - 1] );
--   }


--   /*************************************************************************/
--   /*                                                                       */
--   /* Given a vector of weights, one for each design, figure out the        */
--   /* normalized axis coordinates which gave rise to those weights.         */
--   /*                                                                       */
--   FT_LOCAL_DEF( void )
--   mm_weights_unmap( FT_Fixed*  weights,
--                     FT_Fixed*  axiscoords,
--                     FT_UInt    axis_count )
--   {
--     FT_ASSERT( axis_count <= T1_MAX_MM_AXIS );

--     if ( axis_count == 1 )
--       axiscoords[0] = weights[1];

--     else if ( axis_count == 2 )
--     {
--       axiscoords[0] = weights[3] + weights[1];
--       axiscoords[1] = weights[3] + weights[2];
--     }

--     else if ( axis_count == 3 )
--     {
--       axiscoords[0] = weights[7] + weights[5] + weights[3] + weights[1];
--       axiscoords[1] = weights[7] + weights[6] + weights[3] + weights[2];
--       axiscoords[2] = weights[7] + weights[6] + weights[5] + weights[4];
--     }

--     else
--     {
--       axiscoords[0] = weights[15] + weights[13] + weights[11] + weights[9] +
--                         weights[7] + weights[5] + weights[3] + weights[1];
--       axiscoords[1] = weights[15] + weights[14] + weights[11] + weights[10] +
--                         weights[7] + weights[6] + weights[3] + weights[2];
--       axiscoords[2] = weights[15] + weights[14] + weights[13] + weights[12] +
--                         weights[7] + weights[6] + weights[5] + weights[4];
--       axiscoords[3] = weights[15] + weights[14] + weights[13] + weights[12] +
--                         weights[11] + weights[10] + weights[9] + weights[8];
--     }
--   }


--   /*************************************************************************/
--   /*                                                                       */
--   /* Just a wrapper around T1_Get_Multi_Master to support the different    */
--   /*  arguments needed by the GX var distortable fonts.                    */
--   /*                                                                       */
--   FT_LOCAL_DEF( FT_Error )
--   T1_Get_MM_Var( T1_Face      face,
--                  FT_MM_Var*  *master )
--   {
--     FT_Memory        memory = face->root.memory;
--     FT_MM_Var       *mmvar = NULL;
--     FT_Multi_Master  mmaster;
--     FT_Error         error;
--     FT_UInt          i;
--     FT_Fixed         axiscoords[T1_MAX_MM_AXIS];
--     PS_Blend         blend = face->blend;


--     error = T1_Get_Multi_Master( face, &mmaster );
--     if ( error )
--       goto Exit;
--     if ( FT_ALLOC( mmvar,
--                    sizeof ( FT_MM_Var ) +
--                      mmaster.num_axis * sizeof ( FT_Var_Axis ) ) )
--       goto Exit;

--     mmvar->num_axis        = mmaster.num_axis;
--     mmvar->num_designs     = mmaster.num_designs;
--     mmvar->num_namedstyles = ~0U;                        /* Does not apply */
--     mmvar->axis            = (FT_Var_Axis*)&mmvar[1];
--                                       /* Point to axes after MM_Var struct */
--     mmvar->namedstyle      = NULL;

--     for ( i = 0 ; i < mmaster.num_axis; ++i )
--     {
--       mmvar->axis[i].name    = mmaster.axis[i].name;
--       mmvar->axis[i].minimum = INT_TO_FIXED( mmaster.axis[i].minimum);
--       mmvar->axis[i].maximum = INT_TO_FIXED( mmaster.axis[i].maximum);
--       mmvar->axis[i].def     = ( mmvar->axis[i].minimum +
--                                    mmvar->axis[i].maximum ) / 2;
--                             /* Does not apply.  But this value is in range */
--       mmvar->axis[i].strid   = ~0U;                      /* Does not apply */
--       mmvar->axis[i].tag     = ~0U;                      /* Does not apply */

--       if ( ft_strcmp( mmvar->axis[i].name, "Weight" ) == 0 )
--         mmvar->axis[i].tag = FT_MAKE_TAG( 'w', 'g', 'h', 't' );
--       else if ( ft_strcmp( mmvar->axis[i].name, "Width" ) == 0 )
--         mmvar->axis[i].tag = FT_MAKE_TAG( 'w', 'd', 't', 'h' );
--       else if ( ft_strcmp( mmvar->axis[i].name, "OpticalSize" ) == 0 )
--         mmvar->axis[i].tag = FT_MAKE_TAG( 'o', 'p', 's', 'z' );
--     }

--     if ( blend->num_designs == ( 1U << blend->num_axis ) )
--     {
--       mm_weights_unmap( blend->default_weight_vector,
--                         axiscoords,
--                         blend->num_axis );

--       for ( i = 0; i < mmaster.num_axis; ++i )
--         mmvar->axis[i].def = mm_axis_unmap( &blend->design_map[i],
--                                             axiscoords[i] );
--     }

--     *master = mmvar;

--   Exit:
--     return error;
--   }


--   FT_LOCAL_DEF( FT_Error )
--   T1_Set_MM_Blend( T1_Face    face,
--                    FT_UInt    num_coords,
--                    FT_Fixed*  coords )
--   {
--     PS_Blend  blend = face->blend;
--     FT_Error  error;
--     FT_UInt   n, m;


--     error = FT_ERR( Invalid_Argument );

--     if ( blend && blend->num_axis == num_coords )
--     {
--       /* recompute the weight vector from the blend coordinates */
--       error = FT_Err_Ok;

--       for ( n = 0; n < blend->num_designs; n++ )
--       {
--         FT_Fixed  result = 0x10000L;  /* 1.0 fixed */


--         for ( m = 0; m < blend->num_axis; m++ )
--         {
--           FT_Fixed  factor;


--           /* get current blend axis position */
--           factor = coords[m];
--           if ( factor < 0 )
--             factor = 0;
--           if ( factor > 0x10000L )
--             factor = 0x10000L;

--           if ( ( n & ( 1 << m ) ) == 0 )
--             factor = 0x10000L - factor;

--           result = FT_MulFix( result, factor );
--         }
--         blend->weight_vector[n] = result;
--       }

--       error = FT_Err_Ok;
--     }

--     return error;
--   }


--   FT_LOCAL_DEF( FT_Error )
--   T1_Set_MM_Design( T1_Face   face,
--                     FT_UInt   num_coords,
--                     FT_Long*  coords )
--   {
--     PS_Blend  blend = face->blend;
--     FT_Error  error;
--     FT_UInt   n, p;


--     error = FT_ERR( Invalid_Argument );
--     if ( blend && blend->num_axis == num_coords )
--     {
--       /* compute the blend coordinates through the blend design map */
--       FT_Fixed  final_blends[T1_MAX_MM_DESIGNS];


--       for ( n = 0; n < blend->num_axis; n++ )
--       {
--         FT_Long       design  = coords[n];
--         FT_Fixed      the_blend;
--         PS_DesignMap  map     = blend->design_map + n;
--         FT_Long*      designs = map->design_points;
--         FT_Fixed*     blends  = map->blend_points;
--         FT_Int        before  = -1, after = -1;


--         for ( p = 0; p < (FT_UInt)map->num_points; p++ )
--         {
--           FT_Long  p_design = designs[p];


--           /* exact match? */
--           if ( design == p_design )
--           {
--             the_blend = blends[p];
--             goto Found;
--           }

--           if ( design < p_design )
--           {
--             after = p;
--             break;
--           }

--           before = p;
--         }

--         /* now interpolate if necessary */
--         if ( before < 0 )
--           the_blend = blends[0];

--         else if ( after < 0 )
--           the_blend = blends[map->num_points - 1];

--         else
--           the_blend = FT_MulDiv( design         - designs[before],
--                                  blends [after] - blends [before],
--                                  designs[after] - designs[before] );

--       Found:
--         final_blends[n] = the_blend;
--       }

--       error = T1_Set_MM_Blend( face, num_coords, final_blends );
--     }

--     return error;
--   }


--   /*************************************************************************/
--   /*                                                                       */
--   /* Just a wrapper around T1_Set_MM_Design to support the different       */
--   /* arguments needed by the GX var distortable fonts.                     */
--   /*                                                                       */
--   FT_LOCAL_DEF( FT_Error )
--   T1_Set_Var_Design( T1_Face    face,
--                      FT_UInt    num_coords,
--                      FT_Fixed*  coords )
--   {
--      FT_Long   lcoords[4];          /* maximum axis count is 4 */
--      FT_UInt   i;
--      FT_Error  error;


--      error = FT_ERR( Invalid_Argument );
--      if ( num_coords <= 4 && num_coords > 0 )
--      {
--        for ( i = 0; i < num_coords; ++i )
--          lcoords[i] = FIXED_TO_INT( coords[i] );
--        error = T1_Set_MM_Design( face, num_coords, lcoords );
--      }

--      return error;
--   }


--   FT_LOCAL_DEF( void )
--   T1_Done_Blend( T1_Face  face )
--   {
--     FT_Memory  memory = face->root.memory;
--     PS_Blend   blend  = face->blend;


--     if ( blend )
--     {
--       FT_UInt  num_designs = blend->num_designs;
--       FT_UInt  num_axis    = blend->num_axis;
--       FT_UInt  n;


--       /* release design pos table */
--       FT_FREE( blend->design_pos[0] );
--       for ( n = 1; n < num_designs; n++ )
--         blend->design_pos[n] = NULL;

--       /* release blend `private' and `font info' dictionaries */
--       FT_FREE( blend->privates[1] );
--       FT_FREE( blend->font_infos[1] );
--       FT_FREE( blend->bboxes[1] );

--       for ( n = 0; n < num_designs; n++ )
--       {
--         blend->privates  [n] = NULL;
--         blend->font_infos[n] = NULL;
--         blend->bboxes    [n] = NULL;
--       }

--       /* release weight vectors */
--       FT_FREE( blend->weight_vector );
--       blend->default_weight_vector = NULL;

--       /* release axis names */
--       for ( n = 0; n < num_axis; n++ )
--         FT_FREE( blend->axis_names[n] );

--       /* release design map */
--       for ( n = 0; n < num_axis; n++ )
--       {
--         PS_DesignMap  dmap = blend->design_map + n;


--         FT_FREE( dmap->design_points );
--         dmap->num_points = 0;
--       }

--       FT_FREE( face->blend );
--     }
--   }


--   static void
--   parse_blend_axis_types( T1_Face    face,
--                           T1_Loader  loader )
--   {
--     T1_TokenRec  axis_tokens[T1_MAX_MM_AXIS];
--     FT_Int       n, num_axis;
--     FT_Error     error = FT_Err_Ok;
--     PS_Blend     blend;
--     FT_Memory    memory;


--     /* take an array of objects */
--     T1_ToTokenArray( &loader->parser, axis_tokens,
--                      T1_MAX_MM_AXIS, &num_axis );
--     if ( num_axis < 0 )
--     {
--       error = FT_ERR( Ignore );
--       goto Exit;
--     }
--     if ( num_axis == 0 || num_axis > T1_MAX_MM_AXIS )
--     {
--       FT_ERROR(( "parse_blend_axis_types: incorrect number of axes: %d\n",
--                  num_axis ));
--       error = FT_THROW( Invalid_File_Format );
--       goto Exit;
--     }

--     /* allocate blend if necessary */
--     error = t1_allocate_blend( face, 0, (FT_UInt)num_axis );
--     if ( error )
--       goto Exit;

--     blend  = face->blend;
--     memory = face->root.memory;

--     /* each token is an immediate containing the name of the axis */
--     for ( n = 0; n < num_axis; n++ )
--     {
--       T1_Token    token = axis_tokens + n;
--       FT_Byte*    name;
--       FT_PtrDist  len;


--       /* skip first slash, if any */
--       if ( token->start[0] == '/' )
--         token->start++;

--       len = token->limit - token->start;
--       if ( len == 0 )
--       {
--         error = FT_THROW( Invalid_File_Format );
--         goto Exit;
--       }

--       if ( FT_ALLOC( blend->axis_names[n], (FT_Long)( len + 1 ) ) )
--         goto Exit;

--       name = (FT_Byte*)blend->axis_names[n];
--       FT_MEM_COPY( name, token->start, len );
--       name[len] = '\0';
--     }

--   Exit:
--     loader->parser.root.error = error;
--   }


--   static void
--   parse_blend_design_positions( T1_Face    face,
--                                 T1_Loader  loader )
--   {
--     T1_TokenRec  design_tokens[T1_MAX_MM_DESIGNS];
--     FT_Int       num_designs;
--     FT_Int       num_axis;
--     T1_Parser    parser = &loader->parser;

--     FT_Error     error = FT_Err_Ok;
--     PS_Blend     blend;


--     /* get the array of design tokens -- compute number of designs */
--     T1_ToTokenArray( parser, design_tokens,
--                      T1_MAX_MM_DESIGNS, &num_designs );
--     if ( num_designs < 0 )
--     {
--       error = FT_ERR( Ignore );
--       goto Exit;
--     }
--     if ( num_designs == 0 || num_designs > T1_MAX_MM_DESIGNS )
--     {
--       FT_ERROR(( "parse_blend_design_positions:"
--                  " incorrect number of designs: %d\n",
--                  num_designs ));
--       error = FT_THROW( Invalid_File_Format );
--       goto Exit;
--     }

--     {
--       FT_Byte*  old_cursor = parser->root.cursor;
--       FT_Byte*  old_limit  = parser->root.limit;
--       FT_Int    n;


--       blend    = face->blend;
--       num_axis = 0;  /* make compiler happy */

--       for ( n = 0; n < num_designs; n++ )
--       {
--         T1_TokenRec  axis_tokens[T1_MAX_MM_AXIS];
--         T1_Token     token;
--         FT_Int       axis, n_axis;


--         /* read axis/coordinates tokens */
--         token = design_tokens + n;
--         parser->root.cursor = token->start;
--         parser->root.limit  = token->limit;
--         T1_ToTokenArray( parser, axis_tokens, T1_MAX_MM_AXIS, &n_axis );

--         if ( n == 0 )
--         {
--           if ( n_axis <= 0 || n_axis > T1_MAX_MM_AXIS )
--           {
--             FT_ERROR(( "parse_blend_design_positions:"
--                        " invalid number of axes: %d\n",
--                        n_axis ));
--             error = FT_THROW( Invalid_File_Format );
--             goto Exit;
--           }

--           num_axis = n_axis;
--           error = t1_allocate_blend( face, num_designs, num_axis );
--           if ( error )
--             goto Exit;
--           blend = face->blend;
--         }
--         else if ( n_axis != num_axis )
--         {
--           FT_ERROR(( "parse_blend_design_positions: incorrect table\n" ));
--           error = FT_THROW( Invalid_File_Format );
--           goto Exit;
--         }

--         /* now read each axis token into the design position */
--         for ( axis = 0; axis < n_axis; axis++ )
--         {
--           T1_Token  token2 = axis_tokens + axis;


--           parser->root.cursor = token2->start;
--           parser->root.limit  = token2->limit;
--           blend->design_pos[n][axis] = T1_ToFixed( parser, 0 );
--         }
--       }

--       loader->parser.root.cursor = old_cursor;
--       loader->parser.root.limit  = old_limit;
--     }

--   Exit:
--     loader->parser.root.error = error;
--   }


--   static void
--   parse_blend_design_map( T1_Face    face,
--                           T1_Loader  loader )
--   {
--     FT_Error     error  = FT_Err_Ok;
--     T1_Parser    parser = &loader->parser;
--     PS_Blend     blend;
--     T1_TokenRec  axis_tokens[T1_MAX_MM_AXIS];
--     FT_Int       n, num_axis;
--     FT_Byte*     old_cursor;
--     FT_Byte*     old_limit;
--     FT_Memory    memory = face->root.memory;


--     T1_ToTokenArray( parser, axis_tokens,
--                      T1_MAX_MM_AXIS, &num_axis );
--     if ( num_axis < 0 )
--     {
--       error = FT_ERR( Ignore );
--       goto Exit;
--     }
--     if ( num_axis == 0 || num_axis > T1_MAX_MM_AXIS )
--     {
--       FT_ERROR(( "parse_blend_design_map: incorrect number of axes: %d\n",
--                  num_axis ));
--       error = FT_THROW( Invalid_File_Format );
--       goto Exit;
--     }

--     old_cursor = parser->root.cursor;
--     old_limit  = parser->root.limit;

--     error = t1_allocate_blend( face, 0, num_axis );
--     if ( error )
--       goto Exit;
--     blend = face->blend;

--     /* now read each axis design map */
--     for ( n = 0; n < num_axis; n++ )
--     {
--       PS_DesignMap  map = blend->design_map + n;
--       T1_Token      axis_token;
--       T1_TokenRec   point_tokens[T1_MAX_MM_MAP_POINTS];
--       FT_Int        p, num_points;


--       axis_token = axis_tokens + n;

--       parser->root.cursor = axis_token->start;
--       parser->root.limit  = axis_token->limit;
--       T1_ToTokenArray( parser, point_tokens,
--                        T1_MAX_MM_MAP_POINTS, &num_points );

--       if ( num_points <= 0 || num_points > T1_MAX_MM_MAP_POINTS )
--       {
--         FT_ERROR(( "parse_blend_design_map: incorrect table\n" ));
--         error = FT_THROW( Invalid_File_Format );
--         goto Exit;
--       }

--       /* allocate design map data */
--       if ( FT_NEW_ARRAY( map->design_points, num_points * 2 ) )
--         goto Exit;
--       map->blend_points = map->design_points + num_points;
--       map->num_points   = (FT_Byte)num_points;

--       for ( p = 0; p < num_points; p++ )
--       {
--         T1_Token  point_token;


--         point_token = point_tokens + p;

--         /* don't include delimiting brackets */
--         parser->root.cursor = point_token->start + 1;
--         parser->root.limit  = point_token->limit - 1;

--         map->design_points[p] = T1_ToInt( parser );
--         map->blend_points [p] = T1_ToFixed( parser, 0 );
--       }
--     }

--     parser->root.cursor = old_cursor;
--     parser->root.limit  = old_limit;

--   Exit:
--     parser->root.error = error;
--   }


--   static void
--   parse_weight_vector( T1_Face    face,
--                        T1_Loader  loader )
--   {
--     T1_TokenRec  design_tokens[T1_MAX_MM_DESIGNS];
--     FT_Int       num_designs;
--     FT_Error     error  = FT_Err_Ok;
--     T1_Parser    parser = &loader->parser;
--     PS_Blend     blend  = face->blend;
--     T1_Token     token;
--     FT_Int       n;
--     FT_Byte*     old_cursor;
--     FT_Byte*     old_limit;


--     T1_ToTokenArray( parser, design_tokens,
--                      T1_MAX_MM_DESIGNS, &num_designs );
--     if ( num_designs < 0 )
--     {
--       error = FT_ERR( Ignore );
--       goto Exit;
--     }
--     if ( num_designs == 0 || num_designs > T1_MAX_MM_DESIGNS )
--     {
--       FT_ERROR(( "parse_weight_vector:"
--                  " incorrect number of designs: %d\n",
--                  num_designs ));
--       error = FT_THROW( Invalid_File_Format );
--       goto Exit;
--     }

--     if ( !blend || !blend->num_designs )
--     {
--       error = t1_allocate_blend( face, num_designs, 0 );
--       if ( error )
--         goto Exit;
--       blend = face->blend;
--     }
--     else if ( blend->num_designs != (FT_UInt)num_designs )
--     {
--       FT_ERROR(( "parse_weight_vector:"
--                  " /BlendDesignPosition and /WeightVector have\n"
--                  "                    "
--                  " different number of elements\n" ));
--       error = FT_THROW( Invalid_File_Format );
--       goto Exit;
--     }

--     old_cursor = parser->root.cursor;
--     old_limit  = parser->root.limit;

--     for ( n = 0; n < num_designs; n++ )
--     {
--       token = design_tokens + n;
--       parser->root.cursor = token->start;
--       parser->root.limit  = token->limit;

--       blend->default_weight_vector[n] =
--       blend->weight_vector[n]         = T1_ToFixed( parser, 0 );
--     }

--     parser->root.cursor = old_cursor;
--     parser->root.limit  = old_limit;

--   Exit:
--     parser->root.error = error;
--   }


--   /* e.g., /BuildCharArray [0 0 0 0 0 0 0 0] def           */
--   /* we're only interested in the number of array elements */
--   static void
--   parse_buildchar( T1_Face    face,
--                    T1_Loader  loader )
--   {
--     face->len_buildchar = T1_ToFixedArray( &loader->parser, 0, NULL, 0 );

--     return;
--   }


-- #endif /* !T1_CONFIG_OPTION_NO_MM_SUPPORT */




--   /*************************************************************************/
--   /*************************************************************************/
--   /*****                                                               *****/
--   /*****                      TYPE 1 SYMBOL PARSING                    *****/
--   /*****                                                               *****/
--   /*************************************************************************/
--   /*************************************************************************/

--   static FT_Error
--   t1_load_keyword( T1_Face         face,
--                    T1_Loader       loader,
--                    const T1_Field  field )
--   {
--     FT_Error  error;
--     void*     dummy_object;
--     void**    objects;
--     FT_UInt   max_objects;
--     PS_Blend  blend = face->blend;


--     if ( blend && blend->num_designs == 0 )
--       blend = NULL;

--     /* if the keyword has a dedicated callback, call it */
--     if ( field->type == T1_FIELD_TYPE_CALLBACK )
--     {
--       field->reader( (FT_Face)face, loader );
--       error = loader->parser.root.error;
--       goto Exit;
--     }

--     /* now, the keyword is either a simple field, or a table of fields; */
--     /* we are now going to take care of it                              */
--     switch ( field->location )
--     {
--     case T1_FIELD_LOCATION_FONT_INFO:
--       dummy_object = &face->type1.font_info;
--       objects      = &dummy_object;
--       max_objects  = 0;

--       if ( blend )
--       {
--         objects     = (void**)blend->font_infos;
--         max_objects = blend->num_designs;
--       }
--       break;

--     case T1_FIELD_LOCATION_FONT_EXTRA:
--       dummy_object = &face->type1.font_extra;
--       objects      = &dummy_object;
--       max_objects  = 0;
--       break;

--     case T1_FIELD_LOCATION_PRIVATE:
--       dummy_object = &face->type1.private_dict;
--       objects      = &dummy_object;
--       max_objects  = 0;

--       if ( blend )
--       {
--         objects     = (void**)blend->privates;
--         max_objects = blend->num_designs;
--       }
--       break;

--     case T1_FIELD_LOCATION_BBOX:
--       dummy_object = &face->type1.font_bbox;
--       objects      = &dummy_object;
--       max_objects  = 0;

--       if ( blend )
--       {
--         objects     = (void**)blend->bboxes;
--         max_objects = blend->num_designs;
--       }
--       break;

--     case T1_FIELD_LOCATION_LOADER:
--       dummy_object = loader;
--       objects      = &dummy_object;
--       max_objects  = 0;
--       break;

--     case T1_FIELD_LOCATION_FACE:
--       dummy_object = face;
--       objects      = &dummy_object;
--       max_objects  = 0;
--       break;

-- #ifndef T1_CONFIG_OPTION_NO_MM_SUPPORT
--     case T1_FIELD_LOCATION_BLEND:
--       dummy_object = face->blend;
--       objects      = &dummy_object;
--       max_objects  = 0;
--       break;
-- #endif

--     default:
--       dummy_object = &face->type1;
--       objects      = &dummy_object;
--       max_objects  = 0;
--     }

--     if ( *objects )
--     {
--       if ( field->type == T1_FIELD_TYPE_INTEGER_ARRAY ||
--            field->type == T1_FIELD_TYPE_FIXED_ARRAY   )
--         error = T1_Load_Field_Table( &loader->parser, field,
--                                      objects, max_objects, 0 );
--       else
--         error = T1_Load_Field( &loader->parser, field,
--                                objects, max_objects, 0 );
--     }
--     else
--     {
--       FT_TRACE1(( "t1_load_keyword: ignoring keyword `%s'"
--                   " which is not valid at this point\n"
--                   "                 (probably due to missing keywords)\n",
--                  field->ident ));
--       error = FT_Err_Ok;
--     }

--   Exit:
--     return error;
--   }


--   static void
--   parse_private( T1_Face    face,
--                  T1_Loader  loader )
--   {
--     FT_UNUSED( face );

--     loader->keywords_encountered |= T1_PRIVATE;
--   }


--   static int
--   read_binary_data( T1_Parser  parser,
--                     FT_Long*   size,
--                     FT_Byte**  base,
--                     FT_Bool    incremental )
--   {
--     FT_Byte*  cur;
--     FT_Byte*  limit = parser->root.limit;


--     /* the binary data has one of the following formats */
--     /*                                                  */
--     /*   `size' [white*] RD white ....... ND            */
--     /*   `size' [white*] -| white ....... |-            */
--     /*                                                  */

--     T1_Skip_Spaces( parser );

--     cur = parser->root.cursor;

--     if ( cur < limit && ft_isdigit( *cur ) )
--     {
--       FT_Long  s = T1_ToInt( parser );


--       T1_Skip_PS_Token( parser );   /* `RD' or `-|' or something else */

--       /* there is only one whitespace char after the */
--       /* `RD' or `-|' token                          */
--       *base = parser->root.cursor + 1;

--       if ( s >= 0 && s < limit - *base )
--       {
--         parser->root.cursor += s + 1;
--         *size = s;
--         return !parser->root.error;
--       }
--     }

--     if( !incremental )
--     {
--       FT_ERROR(( "read_binary_data: invalid size field\n" ));
--       parser->root.error = FT_THROW( Invalid_File_Format );
--     }

--     return 0;
--   }


--   /* We now define the routines to handle the `/Encoding', `/Subrs', */
--   /* and `/CharStrings' dictionaries.                                */

--   static void
--   t1_parse_font_matrix( T1_Face    face,
--                         T1_Loader  loader )
--   {
--     T1_Parser   parser = &loader->parser;
--     FT_Matrix*  matrix = &face->type1.font_matrix;
--     FT_Vector*  offset = &face->type1.font_offset;
--     FT_Face     root   = (FT_Face)&face->root;
--     FT_Fixed    temp[6];
--     FT_Fixed    temp_scale;
--     FT_Int      result;


--     result = T1_ToFixedArray( parser, 6, temp, 3 );

--     if ( result < 0 )
--     {
--       parser->root.error = FT_THROW( Invalid_File_Format );
--       return;
--     }

--     temp_scale = FT_ABS( temp[3] );

--     if ( temp_scale == 0 )
--     {
--       FT_ERROR(( "t1_parse_font_matrix: invalid font matrix\n" ));
--       parser->root.error = FT_THROW( Invalid_File_Format );
--       return;
--     }

--     /* Set Units per EM based on FontMatrix values.  We set the value to */
--     /* 1000 / temp_scale, because temp_scale was already multiplied by   */
--     /* 1000 (in t1_tofixed, from psobjs.c).                              */

--     root->units_per_EM = (FT_UShort)FT_DivFix( 1000, temp_scale );

--     /* we need to scale the values by 1.0/temp_scale */
--     if ( temp_scale != 0x10000L )
--     {
--       temp[0] = FT_DivFix( temp[0], temp_scale );
--       temp[1] = FT_DivFix( temp[1], temp_scale );
--       temp[2] = FT_DivFix( temp[2], temp_scale );
--       temp[4] = FT_DivFix( temp[4], temp_scale );
--       temp[5] = FT_DivFix( temp[5], temp_scale );
--       temp[3] = temp[3] < 0 ? -0x10000L : 0x10000L;
--     }

--     matrix->xx = temp[0];
--     matrix->yx = temp[1];
--     matrix->xy = temp[2];
--     matrix->yy = temp[3];

--     /* note that the offsets must be expressed in integer font units */
--     offset->x = temp[4] >> 16;
--     offset->y = temp[5] >> 16;
--   }


--   static void
--   parse_encoding( T1_Face    face,
--                   T1_Loader  loader )
--   {
--     T1_Parser  parser = &loader->parser;
--     FT_Byte*   cur;
--     FT_Byte*   limit  = parser->root.limit;

--     PSAux_Service  psaux = (PSAux_Service)face->psaux;


--     T1_Skip_Spaces( parser );
--     cur = parser->root.cursor;
--     if ( cur >= limit )
--     {
--       FT_ERROR(( "parse_encoding: out of bounds\n" ));
--       parser->root.error = FT_THROW( Invalid_File_Format );
--       return;
--     }

--     /* if we have a number or `[', the encoding is an array, */
--     /* and we must load it now                               */
--     if ( ft_isdigit( *cur ) || *cur == '[' )
--     {
--       T1_Encoding  encode          = &face->type1.encoding;
--       FT_Int       count, n;
--       PS_Table     char_table      = &loader->encoding_table;
--       FT_Memory    memory          = parser->root.memory;
--       FT_Error     error;
--       FT_Bool      only_immediates = 0;


--       /* read the number of entries in the encoding; should be 256 */
--       if ( *cur == '[' )
--       {
--         count           = 256;
--         only_immediates = 1;
--         parser->root.cursor++;
--       }
--       else
--         count = (FT_Int)T1_ToInt( parser );

--       T1_Skip_Spaces( parser );
--       if ( parser->root.cursor >= limit )
--         return;

--       /* we use a T1_Table to store our charnames */
--       loader->num_chars = encode->num_chars = count;
--       if ( FT_NEW_ARRAY( encode->char_index, count )     ||
--            FT_NEW_ARRAY( encode->char_name,  count )     ||
--            FT_SET_ERROR( psaux->ps_table_funcs->init(
--                            char_table, count, memory ) ) )
--       {
--         parser->root.error = error;
--         return;
--       }

--       /* We need to `zero' out encoding_table.elements */
--       for ( n = 0; n < count; n++ )
--       {
--         char*  notdef = (char *)".notdef";


--         T1_Add_Table( char_table, n, notdef, 8 );
--       }

--       /* Now we need to read records of the form                */
--       /*                                                        */
--       /*   ... charcode /charname ...                           */
--       /*                                                        */
--       /* for each entry in our table.                           */
--       /*                                                        */
--       /* We simply look for a number followed by an immediate   */
--       /* name.  Note that this ignores correctly the sequence   */
--       /* that is often seen in type1 fonts:                     */
--       /*                                                        */
--       /*   0 1 255 { 1 index exch /.notdef put } for dup        */
--       /*                                                        */
--       /* used to clean the encoding array before anything else. */
--       /*                                                        */
--       /* Alternatively, if the array is directly given as       */
--       /*                                                        */
--       /*   /Encoding [ ... ]                                    */
--       /*                                                        */
--       /* we only read immediates.                               */

--       n = 0;
--       T1_Skip_Spaces( parser );

--       while ( parser->root.cursor < limit )
--       {
--         cur = parser->root.cursor;

--         /* we stop when we encounter a `def' or `]' */
--         if ( *cur == 'd' && cur + 3 < limit )
--         {
--           if ( cur[1] == 'e'         &&
--                cur[2] == 'f'         &&
--                IS_PS_DELIM( cur[3] ) )
--           {
--             FT_TRACE6(( "encoding end\n" ));
--             cur += 3;
--             break;
--           }
--         }
--         if ( *cur == ']' )
--         {
--           FT_TRACE6(( "encoding end\n" ));
--           cur++;
--           break;
--         }

--         /* check whether we've found an entry */
--         if ( ft_isdigit( *cur ) || only_immediates )
--         {
--           FT_Int  charcode;


--           if ( only_immediates )
--             charcode = n;
--           else
--           {
--             charcode = (FT_Int)T1_ToInt( parser );
--             T1_Skip_Spaces( parser );
--           }

--           cur = parser->root.cursor;

--           if ( cur + 2 < limit && *cur == '/' && n < count )
--           {
--             FT_PtrDist  len;


--             cur++;

--             parser->root.cursor = cur;
--             T1_Skip_PS_Token( parser );
--             if ( parser->root.cursor >= limit )
--               return;
--             if ( parser->root.error )
--               return;

--             len = parser->root.cursor - cur;

--             parser->root.error = T1_Add_Table( char_table, charcode,
--                                                cur, len + 1 );
--             if ( parser->root.error )
--               return;
--             char_table->elements[charcode][len] = '\0';

--             n++;
--           }
--           else if ( only_immediates )
--           {
--             /* Since the current position is not updated for           */
--             /* immediates-only mode we would get an infinite loop if   */
--             /* we don't do anything here.                              */
--             /*                                                         */
--             /* This encoding array is not valid according to the type1 */
--             /* specification (it might be an encoding for a CID type1  */
--             /* font, however), so we conclude that this font is NOT a  */
--             /* type1 font.                                             */
--             parser->root.error = FT_THROW( Unknown_File_Format );
--             return;
--           }
--         }
--         else
--         {
--           T1_Skip_PS_Token( parser );
--           if ( parser->root.error )
--             return;
--         }

--         T1_Skip_Spaces( parser );
--       }

--       face->type1.encoding_type = T1_ENCODING_TYPE_ARRAY;
--       parser->root.cursor       = cur;
--     }

--     /* Otherwise, we should have either `StandardEncoding', */
--     /* `ExpertEncoding', or `ISOLatin1Encoding'             */
--     else
--     {
--       if ( cur + 17 < limit                                            &&
--            ft_strncmp( (const char*)cur, "StandardEncoding", 16 ) == 0 )
--         face->type1.encoding_type = T1_ENCODING_TYPE_STANDARD;

--       else if ( cur + 15 < limit                                          &&
--                 ft_strncmp( (const char*)cur, "ExpertEncoding", 14 ) == 0 )
--         face->type1.encoding_type = T1_ENCODING_TYPE_EXPERT;

--       else if ( cur + 18 < limit                                             &&
--                 ft_strncmp( (const char*)cur, "ISOLatin1Encoding", 17 ) == 0 )
--         face->type1.encoding_type = T1_ENCODING_TYPE_ISOLATIN1;

--       else
--         parser->root.error = FT_ERR( Ignore );
--     }
--   }


--   static void
--   parse_subrs( T1_Face    face,
--                T1_Loader  loader )
--   {
--     T1_Parser  parser = &loader->parser;
--     PS_Table   table  = &loader->subrs;
--     FT_Memory  memory = parser->root.memory;
--     FT_Error   error;
--     FT_Int     num_subrs;

--     PSAux_Service  psaux = (PSAux_Service)face->psaux;


--     T1_Skip_Spaces( parser );

--     /* test for empty array */
--     if ( parser->root.cursor < parser->root.limit &&
--          *parser->root.cursor == '['              )
--     {
--       T1_Skip_PS_Token( parser );
--       T1_Skip_Spaces  ( parser );
--       if ( parser->root.cursor >= parser->root.limit ||
--            *parser->root.cursor != ']'               )
--         parser->root.error = FT_THROW( Invalid_File_Format );
--       return;
--     }

--     num_subrs = (FT_Int)T1_ToInt( parser );

--     /* position the parser right before the `dup' of the first subr */
--     T1_Skip_PS_Token( parser );         /* `array' */
--     if ( parser->root.error )
--       return;
--     T1_Skip_Spaces( parser );

--     /* initialize subrs array -- with synthetic fonts it is possible */
--     /* we get here twice                                             */
--     if ( !loader->num_subrs )
--     {
--       error = psaux->ps_table_funcs->init( table, num_subrs, memory );
--       if ( error )
--         goto Fail;
--     }

--     /* the format is simple:   */
--     /*                         */
--     /*   `index' + binary data */
--     /*                         */
--     for (;;)
--     {
--       FT_Long   idx, size;
--       FT_Byte*  base;


--       /* If we are out of data, or if the next token isn't `dup', */
--       /* we are done.                                             */
--       if ( parser->root.cursor + 4 >= parser->root.limit          ||
--           ft_strncmp( (char*)parser->root.cursor, "dup", 3 ) != 0 )
--         break;

--       T1_Skip_PS_Token( parser );       /* `dup' */

--       idx = T1_ToInt( parser );

--       if ( !read_binary_data( parser, &size, &base, IS_INCREMENTAL ) )
--         return;

--       /* The binary string is followed by one token, e.g. `NP' */
--       /* (bound to `noaccess put') or by two separate tokens:  */
--       /* `noaccess' & `put'.  We position the parser right     */
--       /* before the next `dup', if any.                        */
--       T1_Skip_PS_Token( parser );   /* `NP' or `|' or `noaccess' */
--       if ( parser->root.error )
--         return;
--       T1_Skip_Spaces  ( parser );

--       if ( parser->root.cursor + 4 < parser->root.limit            &&
--            ft_strncmp( (char*)parser->root.cursor, "put", 3 ) == 0 )
--       {
--         T1_Skip_PS_Token( parser ); /* skip `put' */
--         T1_Skip_Spaces  ( parser );
--       }

--       /* with synthetic fonts it is possible we get here twice */
--       if ( loader->num_subrs )
--         continue;

--       /* some fonts use a value of -1 for lenIV to indicate that */
--       /* the charstrings are unencoded                           */
--       /*                                                         */
--       /* thanks to Tom Kacvinsky for pointing this out           */
--       /*                                                         */
--       if ( face->type1.private_dict.lenIV >= 0 )
--       {
--         FT_Byte*  temp;


--         /* some fonts define empty subr records -- this is not totally */
--         /* compliant to the specification (which says they should at   */
--         /* least contain a `return'), but we support them anyway       */
--         if ( size < face->type1.private_dict.lenIV )
--         {
--           error = FT_THROW( Invalid_File_Format );
--           goto Fail;
--         }

--         /* t1_decrypt() shouldn't write to base -- make temporary copy */
--         if ( FT_ALLOC( temp, size ) )
--           goto Fail;
--         FT_MEM_COPY( temp, base, size );
--         psaux->t1_decrypt( temp, size, 4330 );
--         size -= face->type1.private_dict.lenIV;
--         error = T1_Add_Table( table, (FT_Int)idx,
--                               temp + face->type1.private_dict.lenIV, size );
--         FT_FREE( temp );
--       }
--       else
--         error = T1_Add_Table( table, (FT_Int)idx, base, size );
--       if ( error )
--         goto Fail;
--     }

--     if ( !loader->num_subrs )
--       loader->num_subrs = num_subrs;

--     return;

--   Fail:
--     parser->root.error = error;
--   }


-- #define TABLE_EXTEND  5


--   static void
--   parse_charstrings( T1_Face    face,
--                      T1_Loader  loader )
--   {
--     T1_Parser      parser       = &loader->parser;
--     PS_Table       code_table   = &loader->charstrings;
--     PS_Table       name_table   = &loader->glyph_names;
--     PS_Table       swap_table   = &loader->swap_table;
--     FT_Memory      memory       = parser->root.memory;
--     FT_Error       error;

--     PSAux_Service  psaux        = (PSAux_Service)face->psaux;

--     FT_Byte*       cur;
--     FT_Byte*       limit        = parser->root.limit;
--     FT_Int         n, num_glyphs;
--     FT_UInt        notdef_index = 0;
--     FT_Byte        notdef_found = 0;


--     num_glyphs = (FT_Int)T1_ToInt( parser );
--     if ( num_glyphs < 0 )
--     {
--       error = FT_THROW( Invalid_File_Format );
--       goto Fail;
--     }

--     /* some fonts like Optima-Oblique not only define the /CharStrings */
--     /* array but access it also                                        */
--     if ( num_glyphs == 0 || parser->root.error )
--       return;

--     /* initialize tables, leaving space for addition of .notdef, */
--     /* if necessary, and a few other glyphs to handle buggy      */
--     /* fonts which have more glyphs than specified.              */

--     /* for some non-standard fonts like `Optima' which provides  */
--     /* different outlines depending on the resolution it is      */
--     /* possible to get here twice                                */
--     if ( !loader->num_glyphs )
--     {
--       error = psaux->ps_table_funcs->init(
--                 code_table, num_glyphs + 1 + TABLE_EXTEND, memory );
--       if ( error )
--         goto Fail;

--       error = psaux->ps_table_funcs->init(
--                 name_table, num_glyphs + 1 + TABLE_EXTEND, memory );
--       if ( error )
--         goto Fail;

--       /* Initialize table for swapping index notdef_index and */
--       /* index 0 names and codes (if necessary).              */

--       error = psaux->ps_table_funcs->init( swap_table, 4, memory );
--       if ( error )
--         goto Fail;
--     }

--     n = 0;

--     for (;;)
--     {
--       FT_Long   size;
--       FT_Byte*  base;


--       /* the format is simple:        */
--       /*   `/glyphname' + binary data */

--       T1_Skip_Spaces( parser );

--       cur = parser->root.cursor;
--       if ( cur >= limit )
--         break;

--       /* we stop when we find a `def' or `end' keyword */
--       if ( cur + 3 < limit && IS_PS_DELIM( cur[3] ) )
--       {
--         if ( cur[0] == 'd' &&
--              cur[1] == 'e' &&
--              cur[2] == 'f' )
--         {
--           /* There are fonts which have this: */
--           /*                                  */
--           /*   /CharStrings 118 dict def      */
--           /*   Private begin                  */
--           /*   CharStrings begin              */
--           /*   ...                            */
--           /*                                  */
--           /* To catch this we ignore `def' if */
--           /* no charstring has actually been  */
--           /* seen.                            */
--           if ( n )
--             break;
--         }

--         if ( cur[0] == 'e' &&
--              cur[1] == 'n' &&
--              cur[2] == 'd' )
--           break;
--       }

--       T1_Skip_PS_Token( parser );
--       if ( parser->root.error )
--         return;

--       if ( *cur == '/' )
--       {
--         FT_PtrDist  len;


--         if ( cur + 1 >= limit )
--         {
--           error = FT_THROW( Invalid_File_Format );
--           goto Fail;
--         }

--         cur++;                              /* skip `/' */
--         len = parser->root.cursor - cur;

--         if ( !read_binary_data( parser, &size, &base, IS_INCREMENTAL ) )
--           return;

--         /* for some non-standard fonts like `Optima' which provides */
--         /* different outlines depending on the resolution it is     */
--         /* possible to get here twice                               */
--         if ( loader->num_glyphs )
--           continue;

--         error = T1_Add_Table( name_table, n, cur, len + 1 );
--         if ( error )
--           goto Fail;

--         /* add a trailing zero to the name table */
--         name_table->elements[n][len] = '\0';

--         /* record index of /.notdef */
--         if ( *cur == '.'                                              &&
--              ft_strcmp( ".notdef",
--                         (const char*)(name_table->elements[n]) ) == 0 )
--         {
--           notdef_index = n;
--           notdef_found = 1;
--         }

--         if ( face->type1.private_dict.lenIV >= 0 &&
--              n < num_glyphs + TABLE_EXTEND       )
--         {
--           FT_Byte*  temp;


--           if ( size <= face->type1.private_dict.lenIV )
--           {
--             error = FT_THROW( Invalid_File_Format );
--             goto Fail;
--           }

--           /* t1_decrypt() shouldn't write to base -- make temporary copy */
--           if ( FT_ALLOC( temp, size ) )
--             goto Fail;
--           FT_MEM_COPY( temp, base, size );
--           psaux->t1_decrypt( temp, size, 4330 );
--           size -= face->type1.private_dict.lenIV;
--           error = T1_Add_Table( code_table, n,
--                                 temp + face->type1.private_dict.lenIV, size );
--           FT_FREE( temp );
--         }
--         else
--           error = T1_Add_Table( code_table, n, base, size );
--         if ( error )
--           goto Fail;

--         n++;
--       }
--     }

--     loader->num_glyphs = n;

--     /* if /.notdef is found but does not occupy index 0, do our magic. */
--     if ( notdef_found                                                 &&
--          ft_strcmp( ".notdef", (const char*)name_table->elements[0] ) )
--     {
--       /* Swap glyph in index 0 with /.notdef glyph.  First, add index 0  */
--       /* name and code entries to swap_table.  Then place notdef_index   */
--       /* name and code entries into swap_table.  Then swap name and code */
--       /* entries at indices notdef_index and 0 using values stored in    */
--       /* swap_table.                                                     */

--       /* Index 0 name */
--       error = T1_Add_Table( swap_table, 0,
--                             name_table->elements[0],
--                             name_table->lengths [0] );
--       if ( error )
--         goto Fail;

--       /* Index 0 code */
--       error = T1_Add_Table( swap_table, 1,
--                             code_table->elements[0],
--                             code_table->lengths [0] );
--       if ( error )
--         goto Fail;

--       /* Index notdef_index name */
--       error = T1_Add_Table( swap_table, 2,
--                             name_table->elements[notdef_index],
--                             name_table->lengths [notdef_index] );
--       if ( error )
--         goto Fail;

--       /* Index notdef_index code */
--       error = T1_Add_Table( swap_table, 3,
--                             code_table->elements[notdef_index],
--                             code_table->lengths [notdef_index] );
--       if ( error )
--         goto Fail;

--       error = T1_Add_Table( name_table, notdef_index,
--                             swap_table->elements[0],
--                             swap_table->lengths [0] );
--       if ( error )
--         goto Fail;

--       error = T1_Add_Table( code_table, notdef_index,
--                             swap_table->elements[1],
--                             swap_table->lengths [1] );
--       if ( error )
--         goto Fail;

--       error = T1_Add_Table( name_table, 0,
--                             swap_table->elements[2],
--                             swap_table->lengths [2] );
--       if ( error )
--         goto Fail;

--       error = T1_Add_Table( code_table, 0,
--                             swap_table->elements[3],
--                             swap_table->lengths [3] );
--       if ( error )
--         goto Fail;

--     }
--     else if ( !notdef_found )
--     {
--       /* notdef_index is already 0, or /.notdef is undefined in   */
--       /* charstrings dictionary.  Worry about /.notdef undefined. */
--       /* We take index 0 and add it to the end of the table(s)    */
--       /* and add our own /.notdef glyph to index 0.               */

--       /* 0 333 hsbw endchar */
--       FT_Byte  notdef_glyph[] = { 0x8B, 0xF7, 0xE1, 0x0D, 0x0E };
--       char*    notdef_name    = (char *)".notdef";


--       error = T1_Add_Table( swap_table, 0,
--                             name_table->elements[0],
--                             name_table->lengths [0] );
--       if ( error )
--         goto Fail;

--       error = T1_Add_Table( swap_table, 1,
--                             code_table->elements[0],
--                             code_table->lengths [0] );
--       if ( error )
--         goto Fail;

--       error = T1_Add_Table( name_table, 0, notdef_name, 8 );
--       if ( error )
--         goto Fail;

--       error = T1_Add_Table( code_table, 0, notdef_glyph, 5 );

--       if ( error )
--         goto Fail;

--       error = T1_Add_Table( name_table, n,
--                             swap_table->elements[0],
--                             swap_table->lengths [0] );
--       if ( error )
--         goto Fail;

--       error = T1_Add_Table( code_table, n,
--                             swap_table->elements[1],
--                             swap_table->lengths [1] );
--       if ( error )
--         goto Fail;

--       /* we added a glyph. */
--       loader->num_glyphs += 1;
--     }

--     return;

--   Fail:
--     parser->root.error = error;
--   }


--   /*************************************************************************/
--   /*                                                                       */
--   /* Define the token field static variables.  This is a set of            */
--   /* T1_FieldRec variables.                                                */
--   /*                                                                       */
--   /*************************************************************************/


--   static
--   const T1_FieldRec  t1_keywords[] =
--   {

-- #include "t1tokens.h"

--     /* now add the special functions... */
--     T1_FIELD_CALLBACK( "FontMatrix",           t1_parse_font_matrix,
--                        T1_FIELD_DICT_FONTDICT )
--     T1_FIELD_CALLBACK( "Encoding",             parse_encoding,
--                        T1_FIELD_DICT_FONTDICT )
--     T1_FIELD_CALLBACK( "Subrs",                parse_subrs,
--                        T1_FIELD_DICT_PRIVATE )
--     T1_FIELD_CALLBACK( "CharStrings",          parse_charstrings,
--                        T1_FIELD_DICT_PRIVATE )
--     T1_FIELD_CALLBACK( "Private",              parse_private,
--                        T1_FIELD_DICT_FONTDICT )

-- #ifndef T1_CONFIG_OPTION_NO_MM_SUPPORT
--     T1_FIELD_CALLBACK( "BlendDesignPositions", parse_blend_design_positions,
--                        T1_FIELD_DICT_FONTDICT )
--     T1_FIELD_CALLBACK( "BlendDesignMap",       parse_blend_design_map,
--                        T1_FIELD_DICT_FONTDICT )
--     T1_FIELD_CALLBACK( "BlendAxisTypes",       parse_blend_axis_types,
--                        T1_FIELD_DICT_FONTDICT )
--     T1_FIELD_CALLBACK( "WeightVector",         parse_weight_vector,
--                        T1_FIELD_DICT_FONTDICT )
--     T1_FIELD_CALLBACK( "BuildCharArray",       parse_buildchar,
--                        T1_FIELD_DICT_PRIVATE )
-- #endif

--     { 0, T1_FIELD_LOCATION_CID_INFO, T1_FIELD_TYPE_NONE, 0, 0, 0, 0, 0, 0 }
--   };


-- #define T1_FIELD_COUNT                                           \
--           ( sizeof ( t1_keywords ) / sizeof ( t1_keywords[0] ) )


--   static FT_Error
--   parse_dict( T1_Face    face,
--               T1_Loader  loader,
--               FT_Byte*   base,
--               FT_Long    size )
--   {
--     T1_Parser  parser = &loader->parser;
--     FT_Byte   *limit, *start_binary = NULL;
--     FT_Bool    have_integer = 0;


--     parser->root.cursor = base;
--     parser->root.limit  = base + size;
--     parser->root.error  = FT_Err_Ok;

--     limit = parser->root.limit;

--     T1_Skip_Spaces( parser );

--     while ( parser->root.cursor < limit )
--     {
--       FT_Byte*  cur;


--       cur = parser->root.cursor;

--       /* look for `eexec' */
--       if ( IS_PS_TOKEN( cur, limit, "eexec" ) )
--         break;

--       /* look for `closefile' which ends the eexec section */
--       else if ( IS_PS_TOKEN( cur, limit, "closefile" ) )
--         break;

--       /* in a synthetic font the base font starts after a           */
--       /* `FontDictionary' token that is placed after a Private dict */
--       else if ( IS_PS_TOKEN( cur, limit, "FontDirectory" ) )
--       {
--         if ( loader->keywords_encountered & T1_PRIVATE )
--           loader->keywords_encountered |=
--             T1_FONTDIR_AFTER_PRIVATE;
--         parser->root.cursor += 13;
--       }

--       /* check whether we have an integer */
--       else if ( ft_isdigit( *cur ) )
--       {
--         start_binary = cur;
--         T1_Skip_PS_Token( parser );
--         if ( parser->root.error )
--           goto Exit;
--         have_integer = 1;
--       }

--       /* in valid Type 1 fonts we don't see `RD' or `-|' directly */
--       /* since those tokens are handled by parse_subrs and        */
--       /* parse_charstrings                                        */
--       else if ( *cur == 'R' && cur + 6 < limit && *(cur + 1) == 'D' &&
--                 have_integer )
--       {
--         FT_Long   s;
--         FT_Byte*  b;


--         parser->root.cursor = start_binary;
--         if ( !read_binary_data( parser, &s, &b, IS_INCREMENTAL ) )
--           return FT_THROW( Invalid_File_Format );
--         have_integer = 0;
--       }

--       else if ( *cur == '-' && cur + 6 < limit && *(cur + 1) == '|' &&
--                 have_integer )
--       {
--         FT_Long   s;
--         FT_Byte*  b;


--         parser->root.cursor = start_binary;
--         if ( !read_binary_data( parser, &s, &b, IS_INCREMENTAL ) )
--           return FT_THROW( Invalid_File_Format );
--         have_integer = 0;
--       }

--       /* look for immediates */
--       else if ( *cur == '/' && cur + 2 < limit )
--       {
--         FT_PtrDist  len;


--         cur++;

--         parser->root.cursor = cur;
--         T1_Skip_PS_Token( parser );
--         if ( parser->root.error )
--           goto Exit;

--         len = parser->root.cursor - cur;

--         if ( len > 0 && len < 22 && parser->root.cursor < limit )
--         {
--           /* now compare the immediate name to the keyword table */
--           T1_Field  keyword = (T1_Field)t1_keywords;


--           for (;;)
--           {
--             FT_Byte*  name;


--             name = (FT_Byte*)keyword->ident;
--             if ( !name )
--               break;

--             if ( cur[0] == name[0]                                  &&
--                  len == (FT_PtrDist)ft_strlen( (const char *)name ) &&
--                  ft_memcmp( cur, name, len ) == 0                   )
--             {
--               /* We found it -- run the parsing callback!     */
--               /* We record every instance of every field      */
--               /* (until we reach the base font of a           */
--               /* synthetic font) to deal adequately with      */
--               /* multiple master fonts; this is also          */
--               /* necessary because later PostScript           */
--               /* definitions override earlier ones.           */

--               /* Once we encounter `FontDirectory' after      */
--               /* `/Private', we know that this is a synthetic */
--               /* font; except for `/CharStrings' we are not   */
--               /* interested in anything that follows this     */
--               /* `FontDirectory'.                             */

--               /* MM fonts have more than one /Private token at */
--               /* the top level; let's hope that all the junk   */
--               /* that follows the first /Private token is not  */
--               /* interesting to us.                            */

--               /* According to Adobe Tech Note #5175 (CID-Keyed */
--               /* Font Installation for ATM Software) a `begin' */
--               /* must be followed by exactly one `end', and    */
--               /* `begin' -- `end' pairs must be accurately     */
--               /* paired.  We could use this to distinguish     */
--               /* between the global Private and the Private    */
--               /* dict that is a member of the Blend dict.      */

--               const FT_UInt dict =
--                 ( loader->keywords_encountered & T1_PRIVATE )
--                     ? T1_FIELD_DICT_PRIVATE
--                     : T1_FIELD_DICT_FONTDICT;

--               if ( !( dict & keyword->dict ) )
--               {
--                 FT_TRACE1(( "parse_dict: found `%s' but ignoring it"
--                             " since it is in the wrong dictionary\n",
--                             keyword->ident ));
--                 break;
--               }

--               if ( !( loader->keywords_encountered &
--                       T1_FONTDIR_AFTER_PRIVATE     )                  ||
--                    ft_strcmp( (const char*)name, "CharStrings" ) == 0 )
--               {
--                 parser->root.error = t1_load_keyword( face,
--                                                       loader,
--                                                       keyword );
--                 if ( parser->root.error != FT_Err_Ok )
--                 {
--                   if ( FT_ERR_EQ( parser->root.error, Ignore ) )
--                     parser->root.error = FT_Err_Ok;
--                   else
--                     return parser->root.error;
--                 }
--               }
--               break;
--             }

--             keyword++;
--           }
--         }

--         have_integer = 0;
--       }
--       else
--       {
--         T1_Skip_PS_Token( parser );
--         if ( parser->root.error )
--           goto Exit;
--         have_integer = 0;
--       }

--       T1_Skip_Spaces( parser );
--     }

--   Exit:
--     return parser->root.error;
--   }


--   static void
--   t1_init_loader( T1_Loader  loader,
--                   T1_Face    face )
--   {
--     FT_UNUSED( face );

--     FT_MEM_ZERO( loader, sizeof ( *loader ) );
--     loader->num_glyphs = 0;
--     loader->num_chars  = 0;

--     /* initialize the tables -- simply set their `init' field to 0 */
--     loader->encoding_table.init  = 0;
--     loader->charstrings.init     = 0;
--     loader->glyph_names.init     = 0;
--     loader->subrs.init           = 0;
--     loader->swap_table.init      = 0;
--     loader->fontdata             = 0;
--     loader->keywords_encountered = 0;
--   }


--   static void
--   t1_done_loader( T1_Loader  loader )
--   {
--     T1_Parser  parser = &loader->parser;


--     /* finalize tables */
--     T1_Release_Table( &loader->encoding_table );
--     T1_Release_Table( &loader->charstrings );
--     T1_Release_Table( &loader->glyph_names );
--     T1_Release_Table( &loader->swap_table );
--     T1_Release_Table( &loader->subrs );

--     /* finalize parser */
--     T1_Finalize_Parser( parser );
--   }


--   FT_LOCAL_DEF( FT_Error )
--   T1_Open_Face( T1_Face  face )
--   {
--     T1_LoaderRec   loader;
--     T1_Parser      parser;
--     T1_Font        type1 = &face->type1;
--     PS_Private     priv  = &type1->private_dict;
--     FT_Error       error;

--     PSAux_Service  psaux = (PSAux_Service)face->psaux;


--     t1_init_loader( &loader, face );

--     /* default values */
--     face->ndv_idx          = -1;
--     face->cdv_idx          = -1;
--     face->len_buildchar    = 0;

--     priv->blue_shift       = 7;
--     priv->blue_fuzz        = 1;
--     priv->lenIV            = 4;
--     priv->expansion_factor = (FT_Fixed)( 0.06 * 0x10000L );
--     priv->blue_scale       = (FT_Fixed)( 0.039625 * 0x10000L * 1000 );

--     parser = &loader.parser;
--     error  = T1_New_Parser( parser,
--                             face->root.stream,
--                             face->root.memory,
--                             psaux );
--     if ( error )
--       goto Exit;

--     error = parse_dict( face, &loader,
--                         parser->base_dict, parser->base_len );
--     if ( error )
--       goto Exit;

--     error = T1_Get_Private_Dict( parser, psaux );
--     if ( error )
--       goto Exit;

--     error = parse_dict( face, &loader,
--                         parser->private_dict, parser->private_len );
--     if ( error )
--       goto Exit;

--     /* ensure even-ness of `num_blue_values' */
--     priv->num_blue_values &= ~1;

-- #ifndef T1_CONFIG_OPTION_NO_MM_SUPPORT

--     if ( face->blend                                                     &&
--          face->blend->num_default_design_vector != 0                     &&
--          face->blend->num_default_design_vector != face->blend->num_axis )
--     {
--       /* we don't use it currently so just warn, reset, and ignore */
--       FT_ERROR(( "T1_Open_Face(): /DesignVector contains %u entries "
--                  "while there are %u axes.\n",
--                  face->blend->num_default_design_vector,
--                  face->blend->num_axis ));

--       face->blend->num_default_design_vector = 0;
--     }

--     /* the following can happen for MM instances; we then treat the */
--     /* font as a normal PS font                                     */
--     if ( face->blend                                             &&
--          ( !face->blend->num_designs || !face->blend->num_axis ) )
--       T1_Done_Blend( face );

--     /* another safety check */
--     if ( face->blend )
--     {
--       FT_UInt  i;


--       for ( i = 0; i < face->blend->num_axis; i++ )
--         if ( !face->blend->design_map[i].num_points )
--         {
--           T1_Done_Blend( face );
--           break;
--         }
--     }

--     if ( face->blend )
--     {
--       if ( face->len_buildchar > 0 )
--       {
--         FT_Memory  memory = face->root.memory;


--         if ( FT_NEW_ARRAY( face->buildchar, face->len_buildchar ) )
--         {
--           FT_ERROR(( "T1_Open_Face: cannot allocate BuildCharArray\n" ));
--           face->len_buildchar = 0;
--           goto Exit;
--         }
--       }
--     }
--     else
--       face->len_buildchar = 0;

-- #endif /* !T1_CONFIG_OPTION_NO_MM_SUPPORT */

--     /* now, propagate the subrs, charstrings, and glyphnames tables */
--     /* to the Type1 data                                            */
--     type1->num_glyphs = loader.num_glyphs;

--     if ( loader.subrs.init )
--     {
--       loader.subrs.init  = 0;
--       type1->num_subrs   = loader.num_subrs;
--       type1->subrs_block = loader.subrs.block;
--       type1->subrs       = loader.subrs.elements;
--       type1->subrs_len   = loader.subrs.lengths;
--     }

--     if ( !IS_INCREMENTAL )
--       if ( !loader.charstrings.init )
--       {
--         FT_ERROR(( "T1_Open_Face: no `/CharStrings' array in face\n" ));
--         error = FT_THROW( Invalid_File_Format );
--       }

--     loader.charstrings.init  = 0;
--     type1->charstrings_block = loader.charstrings.block;
--     type1->charstrings       = loader.charstrings.elements;
--     type1->charstrings_len   = loader.charstrings.lengths;

--     /* we copy the glyph names `block' and `elements' fields; */
--     /* the `lengths' field must be released later             */
--     type1->glyph_names_block    = loader.glyph_names.block;
--     type1->glyph_names          = (FT_String**)loader.glyph_names.elements;
--     loader.glyph_names.block    = 0;
--     loader.glyph_names.elements = 0;

--     /* we must now build type1.encoding when we have a custom array */
--     if ( type1->encoding_type == T1_ENCODING_TYPE_ARRAY )
--     {
--       FT_Int    charcode, idx, min_char, max_char;
--       FT_Byte*  char_name;
--       FT_Byte*  glyph_name;


--       /* OK, we do the following: for each element in the encoding  */
--       /* table, look up the index of the glyph having the same name */
--       /* the index is then stored in type1.encoding.char_index, and */
--       /* the name to type1.encoding.char_name                       */

--       min_char = 0;
--       max_char = 0;

--       charcode = 0;
--       for ( ; charcode < loader.encoding_table.max_elems; charcode++ )
--       {
--         type1->encoding.char_index[charcode] = 0;
--         type1->encoding.char_name [charcode] = (char *)".notdef";

--         char_name = loader.encoding_table.elements[charcode];
--         if ( char_name )
--           for ( idx = 0; idx < type1->num_glyphs; idx++ )
--           {
--             glyph_name = (FT_Byte*)type1->glyph_names[idx];
--             if ( ft_strcmp( (const char*)char_name,
--                             (const char*)glyph_name ) == 0 )
--             {
--               type1->encoding.char_index[charcode] = (FT_UShort)idx;
--               type1->encoding.char_name [charcode] = (char*)glyph_name;

--               /* Change min/max encoded char only if glyph name is */
--               /* not /.notdef                                      */
--               if ( ft_strcmp( (const char*)".notdef",
--                               (const char*)glyph_name ) != 0 )
--               {
--                 if ( charcode < min_char )
--                   min_char = charcode;
--                 if ( charcode >= max_char )
--                   max_char = charcode + 1;
--               }
--               break;
--             }
--           }
--       }

--       type1->encoding.code_first = min_char;
--       type1->encoding.code_last  = max_char;
--       type1->encoding.num_chars  = loader.num_chars;
--     }

--   Exit:
--     t1_done_loader( &loader );
--     return error;
--   }


-- /* END */

-- /***************************************************************************/
-- /*                                                                         */
-- /*  t1objs.c                                                               */
-- /*                                                                         */
-- /*    Type 1 objects manager (body).                                       */
-- /*                                                                         */
-- /*  Copyright 1996-2009, 2011, 2013 by                                     */
-- /*  David Turner, Robert Wilhelm, and Werner Lemberg.                      */
-- /*                                                                         */
-- /*  This file is part of the FreeType project, and may only be used,       */
-- /*  modified, and distributed under the terms of the FreeType project      */
-- /*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
-- /*  this file you indicate that you have read the license and              */
-- /*  understand and accept it fully.                                        */
-- /*                                                                         */
-- /***************************************************************************/


-- #include <ft2build.h>
-- #include FT_INTERNAL_CALC_H
-- #include FT_INTERNAL_DEBUG_H
-- #include FT_INTERNAL_STREAM_H
-- #include FT_TRUETYPE_IDS_H

-- #include "t1gload.h"
-- #include "t1load.h"

-- #include "t1errors.h"

-- #ifndef T1_CONFIG_OPTION_NO_AFM
-- #include "t1afm.h"
-- #endif

-- #include FT_SERVICE_POSTSCRIPT_CMAPS_H
-- #include FT_INTERNAL_POSTSCRIPT_AUX_H


--   /*************************************************************************/
--   /*                                                                       */
--   /* The macro FT_COMPONENT is used in trace mode.  It is an implicit      */
--   /* parameter of the FT_TRACE() and FT_ERROR() macros, used to print/log  */
--   /* messages during execution.                                            */
--   /*                                                                       */
-- #undef  FT_COMPONENT
-- #define FT_COMPONENT  trace_t1objs


--   /*************************************************************************/
--   /*                                                                       */
--   /*                            SIZE FUNCTIONS                             */
--   /*                                                                       */
--   /*  note that we store the global hints in the size's "internal" root    */
--   /*  field                                                                */
--   /*                                                                       */
--   /*************************************************************************/


--   static PSH_Globals_Funcs
--   T1_Size_Get_Globals_Funcs( T1_Size  size )
--   {
--     T1_Face           face     = (T1_Face)size->root.face;
--     PSHinter_Service  pshinter = (PSHinter_Service)face->pshinter;
--     FT_Module         module;


--     module = FT_Get_Module( size->root.face->driver->root.library,
--                             "pshinter" );
--     return ( module && pshinter && pshinter->get_globals_funcs )
--            ? pshinter->get_globals_funcs( module )
--            : 0 ;
--   }


--   FT_LOCAL_DEF( void )
--   T1_Size_Done( FT_Size  t1size )          /* T1_Size */
--   {
--     T1_Size  size = (T1_Size)t1size;


--     if ( size->root.internal )
--     {
--       PSH_Globals_Funcs  funcs;


--       funcs = T1_Size_Get_Globals_Funcs( size );
--       if ( funcs )
--         funcs->destroy( (PSH_Globals)size->root.internal );

--       size->root.internal = 0;
--     }
--   }


--   FT_LOCAL_DEF( FT_Error )
--   T1_Size_Init( FT_Size  t1size )      /* T1_Size */
--   {
--     T1_Size            size  = (T1_Size)t1size;
--     FT_Error           error = FT_Err_Ok;
--     PSH_Globals_Funcs  funcs = T1_Size_Get_Globals_Funcs( size );


--     if ( funcs )
--     {
--       PSH_Globals  globals;
--       T1_Face      face = (T1_Face)size->root.face;


--       error = funcs->create( size->root.face->memory,
--                              &face->type1.private_dict, &globals );
--       if ( !error )
--         size->root.internal = (FT_Size_Internal)(void*)globals;
--     }

--     return error;
--   }


--   FT_LOCAL_DEF( FT_Error )
--   T1_Size_Request( FT_Size          t1size,     /* T1_Size */
--                    FT_Size_Request  req )
--   {
--     T1_Size            size  = (T1_Size)t1size;
--     PSH_Globals_Funcs  funcs = T1_Size_Get_Globals_Funcs( size );


--     FT_Request_Metrics( size->root.face, req );

--     if ( funcs )
--       funcs->set_scale( (PSH_Globals)size->root.internal,
--                         size->root.metrics.x_scale,
--                         size->root.metrics.y_scale,
--                         0, 0 );

--     return FT_Err_Ok;
--   }


--   /*************************************************************************/
--   /*                                                                       */
--   /*                            SLOT  FUNCTIONS                            */
--   /*                                                                       */
--   /*************************************************************************/

--   FT_LOCAL_DEF( void )
--   T1_GlyphSlot_Done( FT_GlyphSlot  slot )
--   {
--     slot->internal->glyph_hints = 0;
--   }


--   FT_LOCAL_DEF( FT_Error )
--   T1_GlyphSlot_Init( FT_GlyphSlot  slot )
--   {
--     T1_Face           face;
--     PSHinter_Service  pshinter;


--     face     = (T1_Face)slot->face;
--     pshinter = (PSHinter_Service)face->pshinter;

--     if ( pshinter )
--     {
--       FT_Module  module;


--       module = FT_Get_Module( slot->face->driver->root.library,
--                               "pshinter" );
--       if ( module )
--       {
--         T1_Hints_Funcs  funcs;


--         funcs = pshinter->get_t1_funcs( module );
--         slot->internal->glyph_hints = (void*)funcs;
--       }
--     }

--     return 0;
--   }


--   /*************************************************************************/
--   /*                                                                       */
--   /*                            FACE  FUNCTIONS                            */
--   /*                                                                       */
--   /*************************************************************************/


--   /*************************************************************************/
--   /*                                                                       */
--   /* <Function>                                                            */
--   /*    T1_Face_Done                                                       */
--   /*                                                                       */
--   /* <Description>                                                         */
--   /*    The face object destructor.                                        */
--   /*                                                                       */
--   /* <Input>                                                               */
--   /*    face :: A typeless pointer to the face object to destroy.          */
--   /*                                                                       */
--   FT_LOCAL_DEF( void )
--   T1_Face_Done( FT_Face  t1face )         /* T1_Face */
--   {
--     T1_Face    face = (T1_Face)t1face;
--     FT_Memory  memory;
--     T1_Font    type1;


--     if ( !face )
--       return;

--     memory = face->root.memory;
--     type1  = &face->type1;

-- #ifndef T1_CONFIG_OPTION_NO_MM_SUPPORT
--     /* release multiple masters information */
--     FT_ASSERT( ( face->len_buildchar == 0 ) == ( face->buildchar == NULL ) );

--     if ( face->buildchar )
--     {
--       FT_FREE( face->buildchar );

--       face->buildchar     = NULL;
--       face->len_buildchar = 0;
--     }

--     T1_Done_Blend( face );
--     face->blend = 0;
-- #endif

--     /* release font info strings */
--     {
--       PS_FontInfo  info = &type1->font_info;


--       FT_FREE( info->version );
--       FT_FREE( info->notice );
--       FT_FREE( info->full_name );
--       FT_FREE( info->family_name );
--       FT_FREE( info->weight );
--     }

--     /* release top dictionary */
--     FT_FREE( type1->charstrings_len );
--     FT_FREE( type1->charstrings );
--     FT_FREE( type1->glyph_names );

--     FT_FREE( type1->subrs );
--     FT_FREE( type1->subrs_len );

--     FT_FREE( type1->subrs_block );
--     FT_FREE( type1->charstrings_block );
--     FT_FREE( type1->glyph_names_block );

--     FT_FREE( type1->encoding.char_index );
--     FT_FREE( type1->encoding.char_name );
--     FT_FREE( type1->font_name );

-- #ifndef T1_CONFIG_OPTION_NO_AFM
--     /* release afm data if present */
--     if ( face->afm_data )
--       T1_Done_Metrics( memory, (AFM_FontInfo)face->afm_data );
-- #endif

--     /* release unicode map, if any */
-- #if 0
--     FT_FREE( face->unicode_map_rec.maps );
--     face->unicode_map_rec.num_maps = 0;
--     face->unicode_map              = NULL;
-- #endif

--     face->root.family_name = NULL;
--     face->root.style_name  = NULL;
--   }


--   /*************************************************************************/
--   /*                                                                       */
--   /* <Function>                                                            */
--   /*    T1_Face_Init                                                       */
--   /*                                                                       */
--   /* <Description>                                                         */
--   /*    The face object constructor.                                       */
--   /*                                                                       */
--   /* <Input>                                                               */
--   /*    stream     ::  input stream where to load font data.               */
--   /*                                                                       */
--   /*    face_index :: The index of the font face in the resource.          */
--   /*                                                                       */
--   /*    num_params :: Number of additional generic parameters.  Ignored.   */
--   /*                                                                       */
--   /*    params     :: Additional generic parameters.  Ignored.             */
--   /*                                                                       */
--   /* <InOut>                                                               */
--   /*    face       :: The face record to build.                            */
--   /*                                                                       */
--   /* <Return>                                                              */
--   /*    FreeType error code.  0 means success.                             */
--   /*                                                                       */
--   FT_LOCAL_DEF( FT_Error )
--   T1_Face_Init( FT_Stream      stream,
--                 FT_Face        t1face,          /* T1_Face */
--                 FT_Int         face_index,
--                 FT_Int         num_params,
--                 FT_Parameter*  params )
--   {
--     T1_Face             face = (T1_Face)t1face;
--     FT_Error            error;
--     FT_Service_PsCMaps  psnames;
--     PSAux_Service       psaux;
--     T1_Font             type1 = &face->type1;
--     PS_FontInfo         info = &type1->font_info;

--     FT_UNUSED( num_params );
--     FT_UNUSED( params );
--     FT_UNUSED( stream );


--     face->root.num_faces = 1;

--     FT_FACE_FIND_GLOBAL_SERVICE( face, psnames, POSTSCRIPT_CMAPS );
--     face->psnames = psnames;

--     face->psaux = FT_Get_Module_Interface( FT_FACE_LIBRARY( face ),
--                                            "psaux" );
--     psaux = (PSAux_Service)face->psaux;
--     if ( !psaux )
--     {
--       FT_ERROR(( "T1_Face_Init: cannot access `psaux' module\n" ));
--       error = FT_THROW( Missing_Module );
--       goto Exit;
--     }

--     face->pshinter = FT_Get_Module_Interface( FT_FACE_LIBRARY( face ),
--                                               "pshinter" );

--     FT_TRACE2(( "Type 1 driver\n" ));

--     /* open the tokenizer; this will also check the font format */
--     error = T1_Open_Face( face );
--     if ( error )
--       goto Exit;

--     /* if we just wanted to check the format, leave successfully now */
--     if ( face_index < 0 )
--       goto Exit;

--     /* check the face index */
--     if ( face_index > 0 )
--     {
--       FT_ERROR(( "T1_Face_Init: invalid face index\n" ));
--       error = FT_THROW( Invalid_Argument );
--       goto Exit;
--     }

--     /* now load the font program into the face object */

--     /* initialize the face object fields */

--     /* set up root face fields */
--     {
--       FT_Face  root = (FT_Face)&face->root;


--       root->num_glyphs = type1->num_glyphs;
--       root->face_index = 0;

--       root->face_flags = FT_FACE_FLAG_SCALABLE    |
--                          FT_FACE_FLAG_HORIZONTAL  |
--                          FT_FACE_FLAG_GLYPH_NAMES |
--                          FT_FACE_FLAG_HINTER;

--       if ( info->is_fixed_pitch )
--         root->face_flags |= FT_FACE_FLAG_FIXED_WIDTH;

--       if ( face->blend )
--         root->face_flags |= FT_FACE_FLAG_MULTIPLE_MASTERS;

--       /* XXX: TODO -- add kerning with .afm support */


--       /* The following code to extract the family and the style is very   */
--       /* simplistic and might get some things wrong.  For a full-featured */
--       /* algorithm you might have a look at the whitepaper given at       */
--       /*                                                                  */
--       /*   http://blogs.msdn.com/text/archive/2007/04/23/wpf-font-selection-model.aspx */

--       /* get style name -- be careful, some broken fonts only */
--       /* have a `/FontName' dictionary entry!                 */
--       root->family_name = info->family_name;
--       root->style_name  = NULL;

--       if ( root->family_name )
--       {
--         char*  full   = info->full_name;
--         char*  family = root->family_name;


--         if ( full )
--         {
--           FT_Bool  the_same = TRUE;


--           while ( *full )
--           {
--             if ( *full == *family )
--             {
--               family++;
--               full++;
--             }
--             else
--             {
--               if ( *full == ' ' || *full == '-' )
--                 full++;
--               else if ( *family == ' ' || *family == '-' )
--                 family++;
--               else
--               {
--                 the_same = FALSE;

--                 if ( !*family )
--                   root->style_name = full;
--                 break;
--               }
--             }
--           }

--           if ( the_same )
--             root->style_name = (char *)"Regular";
--         }
--       }
--       else
--       {
--         /* do we have a `/FontName'? */
--         if ( type1->font_name )
--           root->family_name = type1->font_name;
--       }

--       if ( !root->style_name )
--       {
--         if ( info->weight )
--           root->style_name = info->weight;
--         else
--           /* assume `Regular' style because we don't know better */
--           root->style_name = (char *)"Regular";
--       }

--       /* compute style flags */
--       root->style_flags = 0;
--       if ( info->italic_angle )
--         root->style_flags |= FT_STYLE_FLAG_ITALIC;
--       if ( info->weight )
--       {
--         if ( !ft_strcmp( info->weight, "Bold"  ) ||
--              !ft_strcmp( info->weight, "Black" ) )
--           root->style_flags |= FT_STYLE_FLAG_BOLD;
--       }

--       /* no embedded bitmap support */
--       root->num_fixed_sizes = 0;
--       root->available_sizes = 0;

--       root->bbox.xMin =   type1->font_bbox.xMin            >> 16;
--       root->bbox.yMin =   type1->font_bbox.yMin            >> 16;
--       /* no `U' suffix here to 0xFFFF! */
--       root->bbox.xMax = ( type1->font_bbox.xMax + 0xFFFF ) >> 16;
--       root->bbox.yMax = ( type1->font_bbox.yMax + 0xFFFF ) >> 16;

--       /* Set units_per_EM if we didn't set it in t1_parse_font_matrix. */
--       if ( !root->units_per_EM )
--         root->units_per_EM = 1000;

--       root->ascender  = (FT_Short)( root->bbox.yMax );
--       root->descender = (FT_Short)( root->bbox.yMin );

--       root->height = (FT_Short)( ( root->units_per_EM * 12 ) / 10 );
--       if ( root->height < root->ascender - root->descender )
--         root->height = (FT_Short)( root->ascender - root->descender );

--       /* now compute the maximum advance width */
--       root->max_advance_width =
--         (FT_Short)( root->bbox.xMax );
--       {
--         FT_Pos  max_advance;


--         error = T1_Compute_Max_Advance( face, &max_advance );

--         /* in case of error, keep the standard width */
--         if ( !error )
--           root->max_advance_width = (FT_Short)FIXED_TO_INT( max_advance );
--         else
--           error = FT_Err_Ok;   /* clear error */
--       }

--       root->max_advance_height = root->height;

--       root->underline_position  = (FT_Short)info->underline_position;
--       root->underline_thickness = (FT_Short)info->underline_thickness;
--     }

--     {
--       FT_Face  root = &face->root;


--       if ( psnames )
--       {
--         FT_CharMapRec    charmap;
--         T1_CMap_Classes  cmap_classes = psaux->t1_cmap_classes;
--         FT_CMap_Class    clazz;


--         charmap.face = root;

--         /* first of all, try to synthesize a Unicode charmap */
--         charmap.platform_id = TT_PLATFORM_MICROSOFT;
--         charmap.encoding_id = TT_MS_ID_UNICODE_CS;
--         charmap.encoding    = FT_ENCODING_UNICODE;

--         error = FT_CMap_New( cmap_classes->unicode, NULL, &charmap, NULL );
--         if ( error                                      &&
--              FT_ERR_NEQ( error, No_Unicode_Glyph_Name ) )
--           goto Exit;
--         error = FT_Err_Ok;

--         /* now, generate an Adobe Standard encoding when appropriate */
--         charmap.platform_id = TT_PLATFORM_ADOBE;
--         clazz               = NULL;

--         switch ( type1->encoding_type )
--         {
--         case T1_ENCODING_TYPE_STANDARD:
--           charmap.encoding    = FT_ENCODING_ADOBE_STANDARD;
--           charmap.encoding_id = TT_ADOBE_ID_STANDARD;
--           clazz               = cmap_classes->standard;
--           break;

--         case T1_ENCODING_TYPE_EXPERT:
--           charmap.encoding    = FT_ENCODING_ADOBE_EXPERT;
--           charmap.encoding_id = TT_ADOBE_ID_EXPERT;
--           clazz               = cmap_classes->expert;
--           break;

--         case T1_ENCODING_TYPE_ARRAY:
--           charmap.encoding    = FT_ENCODING_ADOBE_CUSTOM;
--           charmap.encoding_id = TT_ADOBE_ID_CUSTOM;
--           clazz               = cmap_classes->custom;
--           break;

--         case T1_ENCODING_TYPE_ISOLATIN1:
--           charmap.encoding    = FT_ENCODING_ADOBE_LATIN_1;
--           charmap.encoding_id = TT_ADOBE_ID_LATIN_1;
--           clazz               = cmap_classes->unicode;
--           break;

--         default:
--           ;
--         }

--         if ( clazz )
--           error = FT_CMap_New( clazz, NULL, &charmap, NULL );

-- #if 0
--         /* Select default charmap */
--         if (root->num_charmaps)
--           root->charmap = root->charmaps[0];
-- #endif
--       }
--     }

--   Exit:
--     return error;
--   }


--   /*************************************************************************/
--   /*                                                                       */
--   /* <Function>                                                            */
--   /*    T1_Driver_Init                                                     */
--   /*                                                                       */
--   /* <Description>                                                         */
--   /*    Initializes a given Type 1 driver object.                          */
--   /*                                                                       */
--   /* <Input>                                                               */
--   /*    driver :: A handle to the target driver object.                    */
--   /*                                                                       */
--   /* <Return>                                                              */
--   /*    FreeType error code.  0 means success.                             */
--   /*                                                                       */
--   FT_LOCAL_DEF( FT_Error )
--   T1_Driver_Init( FT_Module  driver )
--   {
--     FT_UNUSED( driver );

--     return FT_Err_Ok;
--   }


--   /*************************************************************************/
--   /*                                                                       */
--   /* <Function>                                                            */
--   /*    T1_Driver_Done                                                     */
--   /*                                                                       */
--   /* <Description>                                                         */
--   /*    Finalizes a given Type 1 driver.                                   */
--   /*                                                                       */
--   /* <Input>                                                               */
--   /*    driver :: A handle to the target Type 1 driver.                    */
--   /*                                                                       */
--   FT_LOCAL_DEF( void )
--   T1_Driver_Done( FT_Module  driver )
--   {
--     FT_UNUSED( driver );
--   }


-- /* END */

-- /***************************************************************************/
-- /*                                                                         */
-- /*  t1parse.c                                                              */
-- /*                                                                         */
-- /*    Type 1 parser (body).                                                */
-- /*                                                                         */
-- /*  Copyright 1996-2005, 2008, 2009, 2012, 2013 by                         */
-- /*  David Turner, Robert Wilhelm, and Werner Lemberg.                      */
-- /*                                                                         */
-- /*  This file is part of the FreeType project, and may only be used,       */
-- /*  modified, and distributed under the terms of the FreeType project      */
-- /*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
-- /*  this file you indicate that you have read the license and              */
-- /*  understand and accept it fully.                                        */
-- /*                                                                         */
-- /***************************************************************************/


--   /*************************************************************************/
--   /*                                                                       */
--   /* The Type 1 parser is in charge of the following:                      */
--   /*                                                                       */
--   /*  - provide an implementation of a growing sequence of objects called  */
--   /*    a `T1_Table' (used to build various tables needed by the loader).  */
--   /*                                                                       */
--   /*  - opening .pfb and .pfa files to extract their top-level and private */
--   /*    dictionaries.                                                      */
--   /*                                                                       */
--   /*  - read numbers, arrays & strings from any dictionary.                */
--   /*                                                                       */
--   /* See `t1load.c' to see how data is loaded from the font file.          */
--   /*                                                                       */
--   /*************************************************************************/


-- #include <ft2build.h>
-- #include FT_INTERNAL_DEBUG_H
-- #include FT_INTERNAL_STREAM_H
-- #include FT_INTERNAL_POSTSCRIPT_AUX_H

-- #include "t1parse.h"

-- #include "t1errors.h"


--   /*************************************************************************/
--   /*                                                                       */
--   /* The macro FT_COMPONENT is used in trace mode.  It is an implicit      */
--   /* parameter of the FT_TRACE() and FT_ERROR() macros, used to print/log  */
--   /* messages during execution.                                            */
--   /*                                                                       */
-- #undef  FT_COMPONENT
-- #define FT_COMPONENT  trace_t1parse


--   /*************************************************************************/
--   /*************************************************************************/
--   /*************************************************************************/
--   /*****                                                               *****/
--   /*****                   INPUT STREAM PARSER                         *****/
--   /*****                                                               *****/
--   /*************************************************************************/
--   /*************************************************************************/
--   /*************************************************************************/


--   /* see Adobe Technical Note 5040.Download_Fonts.pdf */

--   static FT_Error
--   read_pfb_tag( FT_Stream   stream,
--                 FT_UShort  *atag,
--                 FT_ULong   *asize )
--   {
--     FT_Error   error;
--     FT_UShort  tag;
--     FT_ULong   size;


--     *atag  = 0;
--     *asize = 0;

--     if ( !FT_READ_USHORT( tag ) )
--     {
--       if ( tag == 0x8001U || tag == 0x8002U )
--       {
--         if ( !FT_READ_ULONG_LE( size ) )
--           *asize = size;
--       }

--       *atag = tag;
--     }

--     return error;
--   }


--   static FT_Error
--   check_type1_format( FT_Stream    stream,
--                       const char*  header_string,
--                       size_t       header_length )
--   {
--     FT_Error   error;
--     FT_UShort  tag;
--     FT_ULong   dummy;


--     if ( FT_STREAM_SEEK( 0 ) )
--       goto Exit;

--     error = read_pfb_tag( stream, &tag, &dummy );
--     if ( error )
--       goto Exit;

--     /* We assume that the first segment in a PFB is always encoded as   */
--     /* text.  This might be wrong (and the specification doesn't insist */
--     /* on that), but we have never seen a counterexample.               */
--     if ( tag != 0x8001U && FT_STREAM_SEEK( 0 ) )
--       goto Exit;

--     if ( !FT_FRAME_ENTER( header_length ) )
--     {
--       error = FT_Err_Ok;

--       if ( ft_memcmp( stream->cursor, header_string, header_length ) != 0 )
--         error = FT_THROW( Unknown_File_Format );

--       FT_FRAME_EXIT();
--     }

--   Exit:
--     return error;
--   }


--   FT_LOCAL_DEF( FT_Error )
--   T1_New_Parser( T1_Parser      parser,
--                  FT_Stream      stream,
--                  FT_Memory      memory,
--                  PSAux_Service  psaux )
--   {
--     FT_Error   error;
--     FT_UShort  tag;
--     FT_ULong   size;


--     psaux->ps_parser_funcs->init( &parser->root, 0, 0, memory );

--     parser->stream       = stream;
--     parser->base_len     = 0;
--     parser->base_dict    = 0;
--     parser->private_len  = 0;
--     parser->private_dict = 0;
--     parser->in_pfb       = 0;
--     parser->in_memory    = 0;
--     parser->single_block = 0;

--     /* check the header format */
--     error = check_type1_format( stream, "%!PS-AdobeFont", 14 );
--     if ( error )
--     {
--       if ( FT_ERR_NEQ( error, Unknown_File_Format ) )
--         goto Exit;

--       error = check_type1_format( stream, "%!FontType", 10 );
--       if ( error )
--       {
--         FT_TRACE2(( "  not a Type 1 font\n" ));
--         goto Exit;
--       }
--     }

--     /******************************************************************/
--     /*                                                                */
--     /* Here a short summary of what is going on:                      */
--     /*                                                                */
--     /*   When creating a new Type 1 parser, we try to locate and load */
--     /*   the base dictionary if this is possible (i.e., for PFB       */
--     /*   files).  Otherwise, we load the whole font into memory.      */
--     /*                                                                */
--     /*   When `loading' the base dictionary, we only setup pointers   */
--     /*   in the case of a memory-based stream.  Otherwise, we         */
--     /*   allocate and load the base dictionary in it.                 */
--     /*                                                                */
--     /*   parser->in_pfb is set if we are in a binary (`.pfb') font.   */
--     /*   parser->in_memory is set if we have a memory stream.         */
--     /*                                                                */

--     /* try to compute the size of the base dictionary;     */
--     /* look for a Postscript binary file tag, i.e., 0x8001 */
--     if ( FT_STREAM_SEEK( 0L ) )
--       goto Exit;

--     error = read_pfb_tag( stream, &tag, &size );
--     if ( error )
--       goto Exit;

--     if ( tag != 0x8001U )
--     {
--       /* assume that this is a PFA file for now; an error will */
--       /* be produced later when more things are checked        */
--       if ( FT_STREAM_SEEK( 0L ) )
--         goto Exit;
--       size = stream->size;
--     }
--     else
--       parser->in_pfb = 1;

--     /* now, try to load `size' bytes of the `base' dictionary we */
--     /* found previously                                          */

--     /* if it is a memory-based resource, set up pointers */
--     if ( !stream->read )
--     {
--       parser->base_dict = (FT_Byte*)stream->base + stream->pos;
--       parser->base_len  = size;
--       parser->in_memory = 1;

--       /* check that the `size' field is valid */
--       if ( FT_STREAM_SKIP( size ) )
--         goto Exit;
--     }
--     else
--     {
--       /* read segment in memory -- this is clumsy, but so does the format */
--       if ( FT_ALLOC( parser->base_dict, size )       ||
--            FT_STREAM_READ( parser->base_dict, size ) )
--         goto Exit;
--       parser->base_len = size;
--     }

--     parser->root.base   = parser->base_dict;
--     parser->root.cursor = parser->base_dict;
--     parser->root.limit  = parser->root.cursor + parser->base_len;

--   Exit:
--     if ( error && !parser->in_memory )
--       FT_FREE( parser->base_dict );

--     return error;
--   }


--   FT_LOCAL_DEF( void )
--   T1_Finalize_Parser( T1_Parser  parser )
--   {
--     FT_Memory  memory = parser->root.memory;


--     /* always free the private dictionary */
--     FT_FREE( parser->private_dict );

--     /* free the base dictionary only when we have a disk stream */
--     if ( !parser->in_memory )
--       FT_FREE( parser->base_dict );

--     parser->root.funcs.done( &parser->root );
--   }


--   FT_LOCAL_DEF( FT_Error )
--   T1_Get_Private_Dict( T1_Parser      parser,
--                        PSAux_Service  psaux )
--   {
--     FT_Stream  stream = parser->stream;
--     FT_Memory  memory = parser->root.memory;
--     FT_Error   error  = FT_Err_Ok;
--     FT_ULong   size;


--     if ( parser->in_pfb )
--     {
--       /* in the case of the PFB format, the private dictionary can be  */
--       /* made of several segments.  We thus first read the number of   */
--       /* segments to compute the total size of the private dictionary  */
--       /* then re-read them into memory.                                */
--       FT_Long    start_pos = FT_STREAM_POS();
--       FT_UShort  tag;


--       parser->private_len = 0;
--       for (;;)
--       {
--         error = read_pfb_tag( stream, &tag, &size );
--         if ( error )
--           goto Fail;

--         if ( tag != 0x8002U )
--           break;

--         parser->private_len += size;

--         if ( FT_STREAM_SKIP( size ) )
--           goto Fail;
--       }

--       /* Check that we have a private dictionary there */
--       /* and allocate private dictionary buffer        */
--       if ( parser->private_len == 0 )
--       {
--         FT_ERROR(( "T1_Get_Private_Dict:"
--                    " invalid private dictionary section\n" ));
--         error = FT_THROW( Invalid_File_Format );
--         goto Fail;
--       }

--       if ( FT_STREAM_SEEK( start_pos )                           ||
--            FT_ALLOC( parser->private_dict, parser->private_len ) )
--         goto Fail;

--       parser->private_len = 0;
--       for (;;)
--       {
--         error = read_pfb_tag( stream, &tag, &size );
--         if ( error || tag != 0x8002U )
--         {
--           error = FT_Err_Ok;
--           break;
--         }

--         if ( FT_STREAM_READ( parser->private_dict + parser->private_len,
--                              size ) )
--           goto Fail;

--         parser->private_len += size;
--       }
--     }
--     else
--     {
--       /* We have already `loaded' the whole PFA font file into memory; */
--       /* if this is a memory resource, allocate a new block to hold    */
--       /* the private dict.  Otherwise, simply overwrite into the base  */
--       /* dictionary block in the heap.                                 */

--       /* first of all, look at the `eexec' keyword */
--       FT_Byte*  cur   = parser->base_dict;
--       FT_Byte*  limit = cur + parser->base_len;
--       FT_Byte   c;


--     Again:
--       for (;;)
--       {
--         c = cur[0];
--         if ( c == 'e' && cur + 9 < limit )  /* 9 = 5 letters for `eexec' + */
--                                             /* whitespace + 4 chars        */
--         {
--           if ( cur[1] == 'e' &&
--                cur[2] == 'x' &&
--                cur[3] == 'e' &&
--                cur[4] == 'c' )
--             break;
--         }
--         cur++;
--         if ( cur >= limit )
--         {
--           FT_ERROR(( "T1_Get_Private_Dict:"
--                      " could not find `eexec' keyword\n" ));
--           error = FT_THROW( Invalid_File_Format );
--           goto Exit;
--         }
--       }

--       /* check whether `eexec' was real -- it could be in a comment */
--       /* or string (as e.g. in u003043t.gsf from ghostscript)       */

--       parser->root.cursor = parser->base_dict;
--       /* set limit to `eexec' + whitespace + 4 characters */
--       parser->root.limit  = cur + 10;

--       cur   = parser->root.cursor;
--       limit = parser->root.limit;

--       while ( cur < limit )
--       {
--         if ( *cur == 'e' && ft_strncmp( (char*)cur, "eexec", 5 ) == 0 )
--           goto Found;

--         T1_Skip_PS_Token( parser );
--         if ( parser->root.error )
--           break;
--         T1_Skip_Spaces  ( parser );
--         cur = parser->root.cursor;
--       }

--       /* we haven't found the correct `eexec'; go back and continue */
--       /* searching                                                  */

--       cur   = limit;
--       limit = parser->base_dict + parser->base_len;
--       goto Again;

--       /* now determine where to write the _encrypted_ binary private  */
--       /* dictionary.  We overwrite the base dictionary for disk-based */
--       /* resources and allocate a new block otherwise                 */

--     Found:
--       parser->root.limit = parser->base_dict + parser->base_len;

--       T1_Skip_PS_Token( parser );
--       cur   = parser->root.cursor;
--       limit = parser->root.limit;

--       /* according to the Type1 spec, the first cipher byte must not be  */
--       /* an ASCII whitespace character code (blank, tab, carriage return */
--       /* or line feed).  We have seen Type 1 fonts with two line feed    */
--       /* characters...  So skip now all whitespace character codes.      */
--       while ( cur < limit       &&
--               ( *cur == ' '  ||
--                 *cur == '\t' ||
--                 *cur == '\r' ||
--                 *cur == '\n' ) )
--         ++cur;
--       if ( cur >= limit )
--       {
--         FT_ERROR(( "T1_Get_Private_Dict:"
--                    " `eexec' not properly terminated\n" ));
--         error = FT_THROW( Invalid_File_Format );
--         goto Exit;
--       }

--       size = (FT_ULong)( parser->base_len - ( cur - parser->base_dict ) );

--       if ( parser->in_memory )
--       {
--         /* note that we allocate one more byte to put a terminating `0' */
--         if ( FT_ALLOC( parser->private_dict, size + 1 ) )
--           goto Fail;
--         parser->private_len = size;
--       }
--       else
--       {
--         parser->single_block = 1;
--         parser->private_dict = parser->base_dict;
--         parser->private_len  = size;
--         parser->base_dict    = 0;
--         parser->base_len     = 0;
--       }

--       /* now determine whether the private dictionary is encoded in binary */
--       /* or hexadecimal ASCII format -- decode it accordingly              */

--       /* we need to access the next 4 bytes (after the final whitespace */
--       /* following the `eexec' keyword); if they all are hexadecimal    */
--       /* digits, then we have a case of ASCII storage                   */

--       if ( cur + 3 < limit                                &&
--            ft_isxdigit( cur[0] ) && ft_isxdigit( cur[1] ) &&
--            ft_isxdigit( cur[2] ) && ft_isxdigit( cur[3] ) )
--       {
--         /* ASCII hexadecimal encoding */
--         FT_Long  len;


--         parser->root.cursor = cur;
--         (void)psaux->ps_parser_funcs->to_bytes( &parser->root,
--                                                 parser->private_dict,
--                                                 parser->private_len,
--                                                 &len,
--                                                 0 );
--         parser->private_len = len;

--         /* put a safeguard */
--         parser->private_dict[len] = '\0';
--       }
--       else
--         /* binary encoding -- copy the private dict */
--         FT_MEM_MOVE( parser->private_dict, cur, size );
--     }

--     /* we now decrypt the encoded binary private dictionary */
--     psaux->t1_decrypt( parser->private_dict, parser->private_len, 55665U );

--     if ( parser->private_len < 4 )
--     {
--       FT_ERROR(( "T1_Get_Private_Dict:"
--                  " invalid private dictionary section\n" ));
--       error = FT_THROW( Invalid_File_Format );
--       goto Fail;
--     }

--     /* replace the four random bytes at the beginning with whitespace */
--     parser->private_dict[0] = ' ';
--     parser->private_dict[1] = ' ';
--     parser->private_dict[2] = ' ';
--     parser->private_dict[3] = ' ';

--     parser->root.base   = parser->private_dict;
--     parser->root.cursor = parser->private_dict;
--     parser->root.limit  = parser->root.cursor + parser->private_len;

--   Fail:
--   Exit:
--     return error;
--   }


-- /* END */
-- /***************************************************************************/
-- /*                                                                         */
-- /*  t1afm.h                                                                */
-- /*                                                                         */
-- /*    AFM support for Type 1 fonts (specification).                        */
-- /*                                                                         */
-- /*  Copyright 1996-2001, 2002, 2006 by                                     */
-- /*  David Turner, Robert Wilhelm, and Werner Lemberg.                      */
-- /*                                                                         */
-- /*  This file is part of the FreeType project, and may only be used,       */
-- /*  modified, and distributed under the terms of the FreeType project      */
-- /*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
-- /*  this file you indicate that you have read the license and              */
-- /*  understand and accept it fully.                                        */
-- /*                                                                         */
-- /***************************************************************************/


-- #ifndef __T1AFM_H__
-- #define __T1AFM_H__

-- #include <ft2build.h>
-- #include "t1objs.h"
-- #include FT_INTERNAL_TYPE1_TYPES_H

-- FT_BEGIN_HEADER


--   FT_LOCAL( FT_Error )
--   T1_Read_Metrics( FT_Face    face,
--                    FT_Stream  stream );

--   FT_LOCAL( void )
--   T1_Done_Metrics( FT_Memory     memory,
--                    AFM_FontInfo  fi );

--   FT_LOCAL( void )
--   T1_Get_Kerning( AFM_FontInfo  fi,
--                   FT_UInt       glyph1,
--                   FT_UInt       glyph2,
--                   FT_Vector*    kerning );

--   FT_LOCAL( FT_Error )
--   T1_Get_Track_Kerning( FT_Face    face,
--                         FT_Fixed   ptsize,
--                         FT_Int     degree,
--                         FT_Fixed*  kerning );

-- FT_END_HEADER

-- #endif /* __T1AFM_H__ */


-- /* END */

-- /***************************************************************************/
-- /*                                                                         */
-- /*  t1gload.c                                                              */
-- /*                                                                         */
-- /*    Type 1 Glyph Loader (body).                                          */
-- /*                                                                         */
-- /*  Copyright 1996-2006, 2008-2010, 2013 by                                */
-- /*  David Turner, Robert Wilhelm, and Werner Lemberg.                      */
-- /*                                                                         */
-- /*  This file is part of the FreeType project, and may only be used,       */
-- /*  modified, and distributed under the terms of the FreeType project      */
-- /*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
-- /*  this file you indicate that you have read the license and              */
-- /*  understand and accept it fully.                                        */
-- /*                                                                         */
-- /***************************************************************************/


-- #include <ft2build.h>
-- #include "t1gload.h"
-- #include FT_INTERNAL_CALC_H
-- #include FT_INTERNAL_DEBUG_H
-- #include FT_INTERNAL_STREAM_H
-- #include FT_OUTLINE_H
-- #include FT_INTERNAL_POSTSCRIPT_AUX_H

-- #include "t1errors.h"


--   /*************************************************************************/
--   /*                                                                       */
--   /* The macro FT_COMPONENT is used in trace mode.  It is an implicit      */
--   /* parameter of the FT_TRACE() and FT_ERROR() macros, used to print/log  */
--   /* messages during execution.                                            */
--   /*                                                                       */
-- #undef  FT_COMPONENT
-- #define FT_COMPONENT  trace_t1gload


--   /*************************************************************************/
--   /*************************************************************************/
--   /*************************************************************************/
--   /**********                                                      *********/
--   /**********            COMPUTE THE MAXIMUM ADVANCE WIDTH         *********/
--   /**********                                                      *********/
--   /**********    The following code is in charge of computing      *********/
--   /**********    the maximum advance width of the font.  It        *********/
--   /**********    quickly processes each glyph charstring to        *********/
--   /**********    extract the value from either a `sbw' or `seac'   *********/
--   /**********    operator.                                         *********/
--   /**********                                                      *********/
--   /*************************************************************************/
--   /*************************************************************************/
--   /*************************************************************************/


--   FT_LOCAL_DEF( FT_Error )
--   T1_Parse_Glyph_And_Get_Char_String( T1_Decoder  decoder,
--                                       FT_UInt     glyph_index,
--                                       FT_Data*    char_string )
--   {
--     T1_Face   face  = (T1_Face)decoder->builder.face;
--     T1_Font   type1 = &face->type1;
--     FT_Error  error = FT_Err_Ok;

-- #ifdef FT_CONFIG_OPTION_INCREMENTAL
--     FT_Incremental_InterfaceRec *inc =
--                       face->root.internal->incremental_interface;
-- #endif


--     decoder->font_matrix = type1->font_matrix;
--     decoder->font_offset = type1->font_offset;

-- #ifdef FT_CONFIG_OPTION_INCREMENTAL

--     /* For incremental fonts get the character data using the */
--     /* callback function.                                     */
--     if ( inc )
--       error = inc->funcs->get_glyph_data( inc->object,
--                                           glyph_index, char_string );
--     else

-- #endif /* FT_CONFIG_OPTION_INCREMENTAL */

--     /* For ordinary fonts get the character data stored in the face record. */
--     {
--       char_string->pointer = type1->charstrings[glyph_index];
--       char_string->length  = (FT_Int)type1->charstrings_len[glyph_index];
--     }

--     if ( !error )
--       error = decoder->funcs.parse_charstrings(
--                 decoder, (FT_Byte*)char_string->pointer,
--                 char_string->length );

-- #ifdef FT_CONFIG_OPTION_INCREMENTAL

--     /* Incremental fonts can optionally override the metrics. */
--     if ( !error && inc && inc->funcs->get_glyph_metrics )
--     {
--       FT_Incremental_MetricsRec  metrics;


--       metrics.bearing_x = FIXED_TO_INT( decoder->builder.left_bearing.x );
--       metrics.bearing_y = 0;
--       metrics.advance   = FIXED_TO_INT( decoder->builder.advance.x );
--       metrics.advance_v = FIXED_TO_INT( decoder->builder.advance.y );

--       error = inc->funcs->get_glyph_metrics( inc->object,
--                                              glyph_index, FALSE, &metrics );

--       decoder->builder.left_bearing.x = INT_TO_FIXED( metrics.bearing_x );
--       decoder->builder.advance.x      = INT_TO_FIXED( metrics.advance );
--       decoder->builder.advance.y      = INT_TO_FIXED( metrics.advance_v );
--     }

-- #endif /* FT_CONFIG_OPTION_INCREMENTAL */

--     return error;
--   }


--   FT_CALLBACK_DEF( FT_Error )
--   T1_Parse_Glyph( T1_Decoder  decoder,
--                   FT_UInt     glyph_index )
--   {
--     FT_Data   glyph_data;
--     FT_Error  error = T1_Parse_Glyph_And_Get_Char_String(
--                         decoder, glyph_index, &glyph_data );


-- #ifdef FT_CONFIG_OPTION_INCREMENTAL

--     if ( !error )
--     {
--       T1_Face  face = (T1_Face)decoder->builder.face;


--       if ( face->root.internal->incremental_interface )
--         face->root.internal->incremental_interface->funcs->free_glyph_data(
--           face->root.internal->incremental_interface->object,
--           &glyph_data );
--     }

-- #endif /* FT_CONFIG_OPTION_INCREMENTAL */

--     return error;
--   }


--   FT_LOCAL_DEF( FT_Error )
--   T1_Compute_Max_Advance( T1_Face  face,
--                           FT_Pos*  max_advance )
--   {
--     FT_Error       error;
--     T1_DecoderRec  decoder;
--     FT_Int         glyph_index;
--     T1_Font        type1 = &face->type1;
--     PSAux_Service  psaux = (PSAux_Service)face->psaux;


--     FT_ASSERT( ( face->len_buildchar == 0 ) == ( face->buildchar == NULL ) );

--     *max_advance = 0;

--     /* initialize load decoder */
--     error = psaux->t1_decoder_funcs->init( &decoder,
--                                            (FT_Face)face,
--                                            0, /* size       */
--                                            0, /* glyph slot */
--                                            (FT_Byte**)type1->glyph_names,
--                                            face->blend,
--                                            0,
--                                            FT_RENDER_MODE_NORMAL,
--                                            T1_Parse_Glyph );
--     if ( error )
--       return error;

--     decoder.builder.metrics_only = 1;
--     decoder.builder.load_points  = 0;

--     decoder.num_subrs     = type1->num_subrs;
--     decoder.subrs         = type1->subrs;
--     decoder.subrs_len     = type1->subrs_len;

--     decoder.buildchar     = face->buildchar;
--     decoder.len_buildchar = face->len_buildchar;

--     *max_advance = 0;

--     /* for each glyph, parse the glyph charstring and extract */
--     /* the advance width                                      */
--     for ( glyph_index = 0; glyph_index < type1->num_glyphs; glyph_index++ )
--     {
--       /* now get load the unscaled outline */
--       error = T1_Parse_Glyph( &decoder, glyph_index );
--       if ( glyph_index == 0 || decoder.builder.advance.x > *max_advance )
--         *max_advance = decoder.builder.advance.x;

--       /* ignore the error if one occurred - skip to next glyph */
--     }

--     psaux->t1_decoder_funcs->done( &decoder );

--     return FT_Err_Ok;
--   }


--   FT_LOCAL_DEF( FT_Error )
--   T1_Get_Advances( FT_Face    t1face,        /* T1_Face */
--                    FT_UInt    first,
--                    FT_UInt    count,
--                    FT_Int32   load_flags,
--                    FT_Fixed*  advances )
--   {
--     T1_Face        face  = (T1_Face)t1face;
--     T1_DecoderRec  decoder;
--     T1_Font        type1 = &face->type1;
--     PSAux_Service  psaux = (PSAux_Service)face->psaux;
--     FT_UInt        nn;
--     FT_Error       error;


--     if ( load_flags & FT_LOAD_VERTICAL_LAYOUT )
--     {
--       for ( nn = 0; nn < count; nn++ )
--         advances[nn] = 0;

--       return FT_Err_Ok;
--     }

--     error = psaux->t1_decoder_funcs->init( &decoder,
--                                            (FT_Face)face,
--                                            0, /* size       */
--                                            0, /* glyph slot */
--                                            (FT_Byte**)type1->glyph_names,
--                                            face->blend,
--                                            0,
--                                            FT_RENDER_MODE_NORMAL,
--                                            T1_Parse_Glyph );
--     if ( error )
--       return error;

--     decoder.builder.metrics_only = 1;
--     decoder.builder.load_points  = 0;

--     decoder.num_subrs = type1->num_subrs;
--     decoder.subrs     = type1->subrs;
--     decoder.subrs_len = type1->subrs_len;

--     decoder.buildchar     = face->buildchar;
--     decoder.len_buildchar = face->len_buildchar;

--     for ( nn = 0; nn < count; nn++ )
--     {
--       error = T1_Parse_Glyph( &decoder, first + nn );
--       if ( !error )
--         advances[nn] = FIXED_TO_INT( decoder.builder.advance.x );
--       else
--         advances[nn] = 0;
--     }

--     return FT_Err_Ok;
--   }


--   FT_LOCAL_DEF( FT_Error )
--   T1_Load_Glyph( FT_GlyphSlot  t1glyph,          /* T1_GlyphSlot */
--                  FT_Size       t1size,           /* T1_Size      */
--                  FT_UInt       glyph_index,
--                  FT_Int32      load_flags )
--   {
--     T1_GlyphSlot            glyph = (T1_GlyphSlot)t1glyph;
--     FT_Error                error;
--     T1_DecoderRec           decoder;
--     T1_Face                 face = (T1_Face)t1glyph->face;
--     FT_Bool                 hinting;
--     T1_Font                 type1         = &face->type1;
--     PSAux_Service           psaux         = (PSAux_Service)face->psaux;
--     const T1_Decoder_Funcs  decoder_funcs = psaux->t1_decoder_funcs;

--     FT_Matrix               font_matrix;
--     FT_Vector               font_offset;
--     FT_Data                 glyph_data;
--     FT_Bool                 must_finish_decoder = FALSE;
-- #ifdef FT_CONFIG_OPTION_INCREMENTAL
--     FT_Bool                 glyph_data_loaded = 0;
-- #endif


-- #ifdef FT_CONFIG_OPTION_INCREMENTAL
--     if ( glyph_index >= (FT_UInt)face->root.num_glyphs &&
--          !face->root.internal->incremental_interface   )
-- #else
--     if ( glyph_index >= (FT_UInt)face->root.num_glyphs )
-- #endif /* FT_CONFIG_OPTION_INCREMENTAL */
--     {
--       error = FT_THROW( Invalid_Argument );
--       goto Exit;
--     }

--     FT_ASSERT( ( face->len_buildchar == 0 ) == ( face->buildchar == NULL ) );

--     if ( load_flags & FT_LOAD_NO_RECURSE )
--       load_flags |= FT_LOAD_NO_SCALE | FT_LOAD_NO_HINTING;

--     if ( t1size )
--     {
--       glyph->x_scale = t1size->metrics.x_scale;
--       glyph->y_scale = t1size->metrics.y_scale;
--     }
--     else
--     {
--       glyph->x_scale = 0x10000L;
--       glyph->y_scale = 0x10000L;
--     }

--     t1glyph->outline.n_points   = 0;
--     t1glyph->outline.n_contours = 0;

--     hinting = FT_BOOL( ( load_flags & FT_LOAD_NO_SCALE   ) == 0 &&
--                        ( load_flags & FT_LOAD_NO_HINTING ) == 0 );

--     t1glyph->format = FT_GLYPH_FORMAT_OUTLINE;

--     error = decoder_funcs->init( &decoder,
--                                  t1glyph->face,
--                                  t1size,
--                                  t1glyph,
--                                  (FT_Byte**)type1->glyph_names,
--                                  face->blend,
--                                  FT_BOOL( hinting ),
--                                  FT_LOAD_TARGET_MODE( load_flags ),
--                                  T1_Parse_Glyph );
--     if ( error )
--       goto Exit;

--     must_finish_decoder = TRUE;

--     decoder.builder.no_recurse = FT_BOOL(
--                                    ( load_flags & FT_LOAD_NO_RECURSE ) != 0 );

--     decoder.num_subrs     = type1->num_subrs;
--     decoder.subrs         = type1->subrs;
--     decoder.subrs_len     = type1->subrs_len;

--     decoder.buildchar     = face->buildchar;
--     decoder.len_buildchar = face->len_buildchar;

--     /* now load the unscaled outline */
--     error = T1_Parse_Glyph_And_Get_Char_String( &decoder, glyph_index,
--                                                 &glyph_data );
--     if ( error )
--       goto Exit;
-- #ifdef FT_CONFIG_OPTION_INCREMENTAL
--     glyph_data_loaded = 1;
-- #endif

--     font_matrix = decoder.font_matrix;
--     font_offset = decoder.font_offset;

--     /* save new glyph tables */
--     decoder_funcs->done( &decoder );

--     must_finish_decoder = FALSE;

--     /* now, set the metrics -- this is rather simple, as   */
--     /* the left side bearing is the xMin, and the top side */
--     /* bearing the yMax                                    */
--     if ( !error )
--     {
--       t1glyph->outline.flags &= FT_OUTLINE_OWNER;
--       t1glyph->outline.flags |= FT_OUTLINE_REVERSE_FILL;

--       /* for composite glyphs, return only left side bearing and */
--       /* advance width                                           */
--       if ( load_flags & FT_LOAD_NO_RECURSE )
--       {
--         FT_Slot_Internal  internal = t1glyph->internal;


--         t1glyph->metrics.horiBearingX =
--           FIXED_TO_INT( decoder.builder.left_bearing.x );
--         t1glyph->metrics.horiAdvance  =
--           FIXED_TO_INT( decoder.builder.advance.x );

--         internal->glyph_matrix      = font_matrix;
--         internal->glyph_delta       = font_offset;
--         internal->glyph_transformed = 1;
--       }
--       else
--       {
--         FT_BBox            cbox;
--         FT_Glyph_Metrics*  metrics = &t1glyph->metrics;
--         FT_Vector          advance;


--         /* copy the _unscaled_ advance width */
--         metrics->horiAdvance =
--           FIXED_TO_INT( decoder.builder.advance.x );
--         t1glyph->linearHoriAdvance =
--           FIXED_TO_INT( decoder.builder.advance.x );
--         t1glyph->internal->glyph_transformed = 0;

--         if ( load_flags & FT_LOAD_VERTICAL_LAYOUT )
--         {
--           /* make up vertical ones */
--           metrics->vertAdvance = ( face->type1.font_bbox.yMax -
--                                    face->type1.font_bbox.yMin ) >> 16;
--           t1glyph->linearVertAdvance = metrics->vertAdvance;
--         }
--         else
--         {
--           metrics->vertAdvance =
--             FIXED_TO_INT( decoder.builder.advance.y );
--           t1glyph->linearVertAdvance =
--             FIXED_TO_INT( decoder.builder.advance.y );
--         }

--         t1glyph->format = FT_GLYPH_FORMAT_OUTLINE;

--         if ( t1size && t1size->metrics.y_ppem < 24 )
--           t1glyph->outline.flags |= FT_OUTLINE_HIGH_PRECISION;

-- #if 1
--         /* apply the font matrix, if any */
--         if ( font_matrix.xx != 0x10000L || font_matrix.yy != font_matrix.xx ||
--              font_matrix.xy != 0        || font_matrix.yx != 0              )
--           FT_Outline_Transform( &t1glyph->outline, &font_matrix );

--         if ( font_offset.x || font_offset.y )
--           FT_Outline_Translate( &t1glyph->outline,
--                                 font_offset.x,
--                                 font_offset.y );

--         advance.x = metrics->horiAdvance;
--         advance.y = 0;
--         FT_Vector_Transform( &advance, &font_matrix );
--         metrics->horiAdvance = advance.x + font_offset.x;
--         advance.x = 0;
--         advance.y = metrics->vertAdvance;
--         FT_Vector_Transform( &advance, &font_matrix );
--         metrics->vertAdvance = advance.y + font_offset.y;
-- #endif

--         if ( ( load_flags & FT_LOAD_NO_SCALE ) == 0 )
--         {
--           /* scale the outline and the metrics */
--           FT_Int       n;
--           FT_Outline*  cur = decoder.builder.base;
--           FT_Vector*   vec = cur->points;
--           FT_Fixed     x_scale = glyph->x_scale;
--           FT_Fixed     y_scale = glyph->y_scale;


--           /* First of all, scale the points, if we are not hinting */
--           if ( !hinting || ! decoder.builder.hints_funcs )
--             for ( n = cur->n_points; n > 0; n--, vec++ )
--             {
--               vec->x = FT_MulFix( vec->x, x_scale );
--               vec->y = FT_MulFix( vec->y, y_scale );
--             }

--           /* Then scale the metrics */
--           metrics->horiAdvance = FT_MulFix( metrics->horiAdvance, x_scale );
--           metrics->vertAdvance = FT_MulFix( metrics->vertAdvance, y_scale );
--         }

--         /* compute the other metrics */
--         FT_Outline_Get_CBox( &t1glyph->outline, &cbox );

--         metrics->width  = cbox.xMax - cbox.xMin;
--         metrics->height = cbox.yMax - cbox.yMin;

--         metrics->horiBearingX = cbox.xMin;
--         metrics->horiBearingY = cbox.yMax;

--         if ( load_flags & FT_LOAD_VERTICAL_LAYOUT )
--         {
--           /* make up vertical ones */
--           ft_synthesize_vertical_metrics( metrics,
--                                           metrics->vertAdvance );
--         }
--       }

--       /* Set control data to the glyph charstrings.  Note that this is */
--       /* _not_ zero-terminated.                                        */
--       t1glyph->control_data = (FT_Byte*)glyph_data.pointer;
--       t1glyph->control_len  = glyph_data.length;
--     }


--   Exit:

-- #ifdef FT_CONFIG_OPTION_INCREMENTAL
--     if ( glyph_data_loaded && face->root.internal->incremental_interface )
--     {
--       face->root.internal->incremental_interface->funcs->free_glyph_data(
--         face->root.internal->incremental_interface->object,
--         &glyph_data );

--       /* Set the control data to null - it is no longer available if   */
--       /* loaded incrementally.                                         */
--       t1glyph->control_data = 0;
--       t1glyph->control_len  = 0;
--     }
-- #endif

--     if ( must_finish_decoder )
--       decoder_funcs->done( &decoder );

--     return error;
--   }


-- /* END */

-- /***************************************************************************/
-- /*                                                                         */
-- /*  t1errors.h                                                             */
-- /*                                                                         */
-- /*    Type 1 error codes (specification only).                             */
-- /*                                                                         */
-- /*  Copyright 2001, 2012 by                                                */
-- /*  David Turner, Robert Wilhelm, and Werner Lemberg.                      */
-- /*                                                                         */
-- /*  This file is part of the FreeType project, and may only be used,       */
-- /*  modified, and distributed under the terms of the FreeType project      */
-- /*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
-- /*  this file you indicate that you have read the license and              */
-- /*  understand and accept it fully.                                        */
-- /*                                                                         */
-- /***************************************************************************/


--   /*************************************************************************/
--   /*                                                                       */
--   /* This file is used to define the Type 1 error enumeration constants.   */
--   /*                                                                       */
--   /*************************************************************************/

-- #ifndef __T1ERRORS_H__
-- #define __T1ERRORS_H__

-- #include FT_MODULE_ERRORS_H

-- #undef __FTERRORS_H__

-- #undef  FT_ERR_PREFIX
-- #define FT_ERR_PREFIX  T1_Err_
-- #define FT_ERR_BASE    FT_Mod_Err_Type1

-- #include FT_ERRORS_H

-- #endif /* __T1ERRORS_H__ */


-- /* END */

-- /***************************************************************************/
-- /*                                                                         */
-- /*  t1driver.h                                                             */
-- /*                                                                         */
-- /*    High-level Type 1 driver interface (specification).                  */
-- /*                                                                         */
-- /*  Copyright 1996-2001, 2002 by                                           */
-- /*  David Turner, Robert Wilhelm, and Werner Lemberg.                      */
-- /*                                                                         */
-- /*  This file is part of the FreeType project, and may only be used,       */
-- /*  modified, and distributed under the terms of the FreeType project      */
-- /*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
-- /*  this file you indicate that you have read the license and              */
-- /*  understand and accept it fully.                                        */
-- /*                                                                         */
-- /***************************************************************************/


-- #ifndef __T1DRIVER_H__
-- #define __T1DRIVER_H__


-- #include <ft2build.h>
-- #include FT_INTERNAL_DRIVER_H


-- FT_BEGIN_HEADER

-- #ifdef FT_CONFIG_OPTION_PIC
-- #error "this module does not support PIC yet"
-- #endif


--   FT_EXPORT_VAR( const FT_Driver_ClassRec )  t1_driver_class;


-- FT_END_HEADER

-- #endif /* __T1DRIVER_H__ */


-- /* END */

-- /***************************************************************************/
-- /*                                                                         */
-- /*  type1.c                                                                */
-- /*                                                                         */
-- /*    FreeType Type 1 driver component (body only).                        */
-- /*                                                                         */
-- /*  Copyright 1996-2001 by                                                 */
-- /*  David Turner, Robert Wilhelm, and Werner Lemberg.                      */
-- /*                                                                         */
-- /*  This file is part of the FreeType project, and may only be used,       */
-- /*  modified, and distributed under the terms of the FreeType project      */
-- /*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
-- /*  this file you indicate that you have read the license and              */
-- /*  understand and accept it fully.                                        */
-- /*                                                                         */
-- /***************************************************************************/


-- #define FT_MAKE_OPTION_SINGLE_OBJECT

-- #include <ft2build.h>
-- #include "t1parse.c"
-- #include "t1load.c"
-- #include "t1objs.c"
-- #include "t1driver.c"
-- #include "t1gload.c"

-- #ifndef T1_CONFIG_OPTION_NO_AFM
-- #include "t1afm.c"
-- #endif


-- /* END */

-- /***************************************************************************/
-- /*                                                                         */
-- /*  t1load.h                                                               */
-- /*                                                                         */
-- /*    Type 1 font loader (specification).                                  */
-- /*                                                                         */
-- /*  Copyright 1996-2001, 2002, 2004, 2006, 2007 by                         */
-- /*  David Turner, Robert Wilhelm, and Werner Lemberg.                      */
-- /*                                                                         */
-- /*  This file is part of the FreeType project, and may only be used,       */
-- /*  modified, and distributed under the terms of the FreeType project      */
-- /*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
-- /*  this file you indicate that you have read the license and              */
-- /*  understand and accept it fully.                                        */
-- /*                                                                         */
-- /***************************************************************************/


-- #ifndef __T1LOAD_H__
-- #define __T1LOAD_H__


-- #include <ft2build.h>
-- #include FT_INTERNAL_STREAM_H
-- #include FT_INTERNAL_POSTSCRIPT_AUX_H
-- #include FT_MULTIPLE_MASTERS_H

-- #include "t1parse.h"


-- FT_BEGIN_HEADER


--   typedef struct  T1_Loader_
--   {
--     T1_ParserRec  parser;          /* parser used to read the stream */

--     FT_Int        num_chars;       /* number of characters in encoding */
--     PS_TableRec   encoding_table;  /* PS_Table used to store the       */
--                                    /* encoding character names         */

--     FT_Int        num_glyphs;
--     PS_TableRec   glyph_names;
--     PS_TableRec   charstrings;
--     PS_TableRec   swap_table;      /* For moving .notdef glyph to index 0. */

--     FT_Int        num_subrs;
--     PS_TableRec   subrs;
--     FT_Bool       fontdata;

--     FT_UInt       keywords_encountered; /* T1_LOADER_ENCOUNTERED_XXX */

--   } T1_LoaderRec, *T1_Loader;


--   /* treatment of some keywords differs depending on whether */
--   /* they precede or follow certain other keywords           */

-- #define T1_PRIVATE                ( 1 << 0 )
-- #define T1_FONTDIR_AFTER_PRIVATE  ( 1 << 1 )


--   FT_LOCAL( FT_Error )
--   T1_Open_Face( T1_Face  face );

-- #ifndef T1_CONFIG_OPTION_NO_MM_SUPPORT

--   FT_LOCAL( FT_Error )
--   T1_Get_Multi_Master( T1_Face           face,
--                        FT_Multi_Master*  master );

--   FT_LOCAL_DEF( FT_Error )
--   T1_Get_MM_Var( T1_Face      face,
--                  FT_MM_Var*  *master );

--   FT_LOCAL( FT_Error )
--   T1_Set_MM_Blend( T1_Face    face,
--                    FT_UInt    num_coords,
--                    FT_Fixed*  coords );

--   FT_LOCAL( FT_Error )
--   T1_Set_MM_Design( T1_Face   face,
--                     FT_UInt   num_coords,
--                     FT_Long*  coords );

--   FT_LOCAL_DEF( FT_Error )
--   T1_Set_Var_Design( T1_Face    face,
--                      FT_UInt    num_coords,
--                      FT_Fixed*  coords );

--   FT_LOCAL( void )
--   T1_Done_Blend( T1_Face  face );

-- #endif /* !T1_CONFIG_OPTION_NO_MM_SUPPORT */


-- FT_END_HEADER

-- #endif /* __T1LOAD_H__ */


-- /* END */

-- /***************************************************************************/
-- /*                                                                         */
-- /*  t1parse.h                                                              */
-- /*                                                                         */
-- /*    Type 1 parser (specification).                                       */
-- /*                                                                         */
-- /*  Copyright 1996-2001, 2002, 2003, 2008 by                               */
-- /*  David Turner, Robert Wilhelm, and Werner Lemberg.                      */
-- /*                                                                         */
-- /*  This file is part of the FreeType project, and may only be used,       */
-- /*  modified, and distributed under the terms of the FreeType project      */
-- /*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
-- /*  this file you indicate that you have read the license and              */
-- /*  understand and accept it fully.                                        */
-- /*                                                                         */
-- /***************************************************************************/


-- #ifndef __T1PARSE_H__
-- #define __T1PARSE_H__


-- #include <ft2build.h>
-- #include FT_INTERNAL_TYPE1_TYPES_H
-- #include FT_INTERNAL_STREAM_H


-- FT_BEGIN_HEADER


--   /*************************************************************************/
--   /*                                                                       */
--   /* <Struct>                                                              */
--   /*    T1_ParserRec                                                       */
--   /*                                                                       */
--   /* <Description>                                                         */
--   /*    A PS_ParserRec is an object used to parse a Type 1 fonts very      */
--   /*    quickly.                                                           */
--   /*                                                                       */
--   /* <Fields>                                                              */
--   /*    root         :: The root parser.                                   */
--   /*                                                                       */
--   /*    stream       :: The current input stream.                          */
--   /*                                                                       */
--   /*    base_dict    :: A pointer to the top-level dictionary.             */
--   /*                                                                       */
--   /*    base_len     :: The length in bytes of the top dictionary.         */
--   /*                                                                       */
--   /*    private_dict :: A pointer to the private dictionary.               */
--   /*                                                                       */
--   /*    private_len  :: The length in bytes of the private dictionary.     */
--   /*                                                                       */
--   /*    in_pfb       :: A boolean.  Indicates that we are handling a PFB   */
--   /*                    file.                                              */
--   /*                                                                       */
--   /*    in_memory    :: A boolean.  Indicates a memory-based stream.       */
--   /*                                                                       */
--   /*    single_block :: A boolean.  Indicates that the private dictionary  */
--   /*                    is stored in lieu of the base dictionary.          */
--   /*                                                                       */
--   typedef struct  T1_ParserRec_
--   {
--     PS_ParserRec  root;
--     FT_Stream     stream;

--     FT_Byte*      base_dict;
--     FT_ULong      base_len;

--     FT_Byte*      private_dict;
--     FT_ULong      private_len;

--     FT_Bool       in_pfb;
--     FT_Bool       in_memory;
--     FT_Bool       single_block;

--   } T1_ParserRec, *T1_Parser;


-- #define T1_Add_Table( p, i, o, l )  (p)->funcs.add( (p), i, o, l )
-- #define T1_Done_Table( p )          \
--           do                        \
--           {                         \
--             if ( (p)->funcs.done )  \
--               (p)->funcs.done( p ); \
--           } while ( 0 )
-- #define T1_Release_Table( p )          \
--           do                           \
--           {                            \
--             if ( (p)->funcs.release )  \
--               (p)->funcs.release( p ); \
--           } while ( 0 )


-- #define T1_Skip_Spaces( p )    (p)->root.funcs.skip_spaces( &(p)->root )
-- #define T1_Skip_PS_Token( p )  (p)->root.funcs.skip_PS_token( &(p)->root )

-- #define T1_ToInt( p )       (p)->root.funcs.to_int( &(p)->root )
-- #define T1_ToFixed( p, t )  (p)->root.funcs.to_fixed( &(p)->root, t )

-- #define T1_ToCoordArray( p, m, c )                           \
--           (p)->root.funcs.to_coord_array( &(p)->root, m, c )
-- #define T1_ToFixedArray( p, m, f, t )                           \
--           (p)->root.funcs.to_fixed_array( &(p)->root, m, f, t )
-- #define T1_ToToken( p, t )                          \
--           (p)->root.funcs.to_token( &(p)->root, t )
-- #define T1_ToTokenArray( p, t, m, c )                           \
--           (p)->root.funcs.to_token_array( &(p)->root, t, m, c )

-- #define T1_Load_Field( p, f, o, m, pf )                         \
--           (p)->root.funcs.load_field( &(p)->root, f, o, m, pf )

-- #define T1_Load_Field_Table( p, f, o, m, pf )                         \
--           (p)->root.funcs.load_field_table( &(p)->root, f, o, m, pf )


--   FT_LOCAL( FT_Error )
--   T1_New_Parser( T1_Parser      parser,
--                  FT_Stream      stream,
--                  FT_Memory      memory,
--                  PSAux_Service  psaux );

--   FT_LOCAL( FT_Error )
--   T1_Get_Private_Dict( T1_Parser      parser,
--                        PSAux_Service  psaux );

--   FT_LOCAL( void )
--   T1_Finalize_Parser( T1_Parser  parser );


-- FT_END_HEADER

-- #endif /* __T1PARSE_H__ */


-- /* END */
-- /***************************************************************************/
-- /*                                                                         */
-- /*  t1gload.h                                                              */
-- /*                                                                         */
-- /*    Type 1 Glyph Loader (specification).                                 */
-- /*                                                                         */
-- /*  Copyright 1996-2001, 2002, 2003, 2008, 2011 by                         */
-- /*  David Turner, Robert Wilhelm, and Werner Lemberg.                      */
-- /*                                                                         */
-- /*  This file is part of the FreeType project, and may only be used,       */
-- /*  modified, and distributed under the terms of the FreeType project      */
-- /*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
-- /*  this file you indicate that you have read the license and              */
-- /*  understand and accept it fully.                                        */
-- /*                                                                         */
-- /***************************************************************************/


-- #ifndef __T1GLOAD_H__
-- #define __T1GLOAD_H__


-- #include <ft2build.h>
-- #include "t1objs.h"


-- FT_BEGIN_HEADER


--   FT_LOCAL( FT_Error )
--   T1_Compute_Max_Advance( T1_Face  face,
--                           FT_Pos*  max_advance );

--   FT_LOCAL( FT_Error )
--   T1_Get_Advances( FT_Face    face,
--                    FT_UInt    first,
--                    FT_UInt    count,
--                    FT_Int32   load_flags,
--                    FT_Fixed*  advances );

--   FT_LOCAL( FT_Error )
--   T1_Load_Glyph( FT_GlyphSlot  glyph,
--                  FT_Size       size,
--                  FT_UInt       glyph_index,
--                  FT_Int32      load_flags );


-- FT_END_HEADER

-- #endif /* __T1GLOAD_H__ */


-- /* END */

-- /***************************************************************************/
-- /*                                                                         */
-- /*  t1objs.h                                                               */
-- /*                                                                         */
-- /*    Type 1 objects manager (specification).                              */
-- /*                                                                         */
-- /*  Copyright 1996-2001, 2002, 2006, 2011 by                               */
-- /*  David Turner, Robert Wilhelm, and Werner Lemberg.                      */
-- /*                                                                         */
-- /*  This file is part of the FreeType project, and may only be used,       */
-- /*  modified, and distributed under the terms of the FreeType project      */
-- /*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
-- /*  this file you indicate that you have read the license and              */
-- /*  understand and accept it fully.                                        */
-- /*                                                                         */
-- /***************************************************************************/


-- #ifndef __T1OBJS_H__
-- #define __T1OBJS_H__


-- #include <ft2build.h>
-- #include FT_INTERNAL_OBJECTS_H
-- #include FT_CONFIG_CONFIG_H
-- #include FT_INTERNAL_TYPE1_TYPES_H


-- FT_BEGIN_HEADER


--   /* The following structures must be defined by the hinter */
--   typedef struct T1_Size_Hints_   T1_Size_Hints;
--   typedef struct T1_Glyph_Hints_  T1_Glyph_Hints;


--   /*************************************************************************/
--   /*                                                                       */
--   /* <Type>                                                                */
--   /*    T1_Size                                                            */
--   /*                                                                       */
--   /* <Description>                                                         */
--   /*    A handle to a Type 1 size object.                                  */
--   /*                                                                       */
--   typedef struct T1_SizeRec_*  T1_Size;


--   /*************************************************************************/
--   /*                                                                       */
--   /* <Type>                                                                */
--   /*    T1_GlyphSlot                                                       */
--   /*                                                                       */
--   /* <Description>                                                         */
--   /*    A handle to a Type 1 glyph slot object.                            */
--   /*                                                                       */
--   typedef struct T1_GlyphSlotRec_*  T1_GlyphSlot;


--   /*************************************************************************/
--   /*                                                                       */
--   /* <Type>                                                                */
--   /*    T1_CharMap                                                         */
--   /*                                                                       */
--   /* <Description>                                                         */
--   /*    A handle to a Type 1 character mapping object.                     */
--   /*                                                                       */
--   /* <Note>                                                                */
--   /*    The Type 1 format doesn't use a charmap but an encoding table.     */
--   /*    The driver is responsible for making up charmap objects            */
--   /*    corresponding to these tables.                                     */
--   /*                                                                       */
--   typedef struct T1_CharMapRec_*   T1_CharMap;


--   /*************************************************************************/
--   /*                                                                       */
--   /*                  HERE BEGINS THE TYPE1 SPECIFIC STUFF                 */
--   /*                                                                       */
--   /*************************************************************************/


--   /*************************************************************************/
--   /*                                                                       */
--   /* <Type>                                                                */
--   /*    T1_SizeRec                                                         */
--   /*                                                                       */
--   /* <Description>                                                         */
--   /*    Type 1 size record.                                                */
--   /*                                                                       */
--   typedef struct  T1_SizeRec_
--   {
--     FT_SizeRec  root;

--   } T1_SizeRec;


--   FT_LOCAL( void )
--   T1_Size_Done( FT_Size  size );

--   FT_LOCAL( FT_Error )
--   T1_Size_Request( FT_Size          size,
--                    FT_Size_Request  req );

--   FT_LOCAL( FT_Error )
--   T1_Size_Init( FT_Size  size );


--   /*************************************************************************/
--   /*                                                                       */
--   /* <Type>                                                                */
--   /*    T1_GlyphSlotRec                                                    */
--   /*                                                                       */
--   /* <Description>                                                         */
--   /*    Type 1 glyph slot record.                                          */
--   /*                                                                       */
--   typedef struct  T1_GlyphSlotRec_
--   {
--     FT_GlyphSlotRec  root;

--     FT_Bool          hint;
--     FT_Bool          scaled;

--     FT_Int           max_points;
--     FT_Int           max_contours;

--     FT_Fixed         x_scale;
--     FT_Fixed         y_scale;

--   } T1_GlyphSlotRec;


--   FT_LOCAL( FT_Error )
--   T1_Face_Init( FT_Stream      stream,
--                 FT_Face        face,
--                 FT_Int         face_index,
--                 FT_Int         num_params,
--                 FT_Parameter*  params );

--   FT_LOCAL( void )
--   T1_Face_Done( FT_Face  face );

--   FT_LOCAL( FT_Error )
--   T1_GlyphSlot_Init( FT_GlyphSlot  slot );

--   FT_LOCAL( void )
--   T1_GlyphSlot_Done( FT_GlyphSlot  slot );

--   FT_LOCAL( FT_Error )
--   T1_Driver_Init( FT_Module  driver );

--   FT_LOCAL( void )
--   T1_Driver_Done( FT_Module  driver );


-- FT_END_HEADER

-- #endif /* __T1OBJS_H__ */


-- /* END */

-- /***************************************************************************/
-- /*                                                                         */
-- /*  t1tokens.h                                                             */
-- /*                                                                         */
-- /*    Type 1 tokenizer (specification).                                    */
-- /*                                                                         */
-- /*  Copyright 1996-2001, 2002, 2003, 2004, 2006, 2008, 2009 by             */
-- /*  David Turner, Robert Wilhelm, and Werner Lemberg.                      */
-- /*                                                                         */
-- /*  This file is part of the FreeType project, and may only be used,       */
-- /*  modified, and distributed under the terms of the FreeType project      */
-- /*  license, LICENSE.TXT.  By continuing to use, modify, or distribute     */
-- /*  this file you indicate that you have read the license and              */
-- /*  understand and accept it fully.                                        */
-- /*                                                                         */
-- /***************************************************************************/


-- #undef  FT_STRUCTURE
-- #define FT_STRUCTURE  PS_FontInfoRec
-- #undef  T1CODE
-- #define T1CODE        T1_FIELD_LOCATION_FONT_INFO

--   T1_FIELD_STRING( "version",            version,
--                    T1_FIELD_DICT_FONTDICT )
--   T1_FIELD_STRING( "Notice",             notice,
--                    T1_FIELD_DICT_FONTDICT )
--   T1_FIELD_STRING( "FullName",           full_name,
--                    T1_FIELD_DICT_FONTDICT )
--   T1_FIELD_STRING( "FamilyName",         family_name,
--                    T1_FIELD_DICT_FONTDICT )
--   T1_FIELD_STRING( "Weight",             weight,
--                    T1_FIELD_DICT_FONTDICT )

--   /* we use pointers to detect modifications made by synthetic fonts */
--   T1_FIELD_NUM   ( "ItalicAngle",        italic_angle,
--                    T1_FIELD_DICT_FONTDICT )
--   T1_FIELD_BOOL  ( "isFixedPitch",       is_fixed_pitch,
--                    T1_FIELD_DICT_FONTDICT )
--   T1_FIELD_NUM   ( "UnderlinePosition",  underline_position,
--                    T1_FIELD_DICT_FONTDICT )
--   T1_FIELD_NUM   ( "UnderlineThickness", underline_thickness,
--                    T1_FIELD_DICT_FONTDICT )

-- #undef  FT_STRUCTURE
-- #define FT_STRUCTURE  PS_FontExtraRec
-- #undef  T1CODE
-- #define T1CODE        T1_FIELD_LOCATION_FONT_EXTRA

--   T1_FIELD_NUM   ( "FSType", fs_type,
--                    T1_FIELD_DICT_FONTDICT )

-- #undef  FT_STRUCTURE
-- #define FT_STRUCTURE  PS_PrivateRec
-- #undef  T1CODE
-- #define T1CODE        T1_FIELD_LOCATION_PRIVATE

--   T1_FIELD_NUM       ( "UniqueID",         unique_id,
--                        T1_FIELD_DICT_FONTDICT | T1_FIELD_DICT_PRIVATE )
--   T1_FIELD_NUM       ( "lenIV",            lenIV,
--                        T1_FIELD_DICT_PRIVATE )
--   T1_FIELD_NUM       ( "LanguageGroup",    language_group,
--                        T1_FIELD_DICT_PRIVATE )
--   T1_FIELD_NUM       ( "password",         password,
--                        T1_FIELD_DICT_PRIVATE )

--   T1_FIELD_FIXED_1000( "BlueScale",        blue_scale,
--                        T1_FIELD_DICT_PRIVATE )
--   T1_FIELD_NUM       ( "BlueShift",        blue_shift,
--                        T1_FIELD_DICT_PRIVATE )
--   T1_FIELD_NUM       ( "BlueFuzz",         blue_fuzz,
--                        T1_FIELD_DICT_PRIVATE )

--   T1_FIELD_NUM_TABLE ( "BlueValues",       blue_values,        14,
--                        T1_FIELD_DICT_PRIVATE )
--   T1_FIELD_NUM_TABLE ( "OtherBlues",       other_blues,        10,
--                        T1_FIELD_DICT_PRIVATE )
--   T1_FIELD_NUM_TABLE ( "FamilyBlues",      family_blues,       14,
--                        T1_FIELD_DICT_PRIVATE )
--   T1_FIELD_NUM_TABLE ( "FamilyOtherBlues", family_other_blues, 10,
--                        T1_FIELD_DICT_PRIVATE )

--   T1_FIELD_NUM_TABLE2( "StdHW",            standard_width,      1,
--                        T1_FIELD_DICT_PRIVATE )
--   T1_FIELD_NUM_TABLE2( "StdVW",            standard_height,     1,
--                        T1_FIELD_DICT_PRIVATE )
--   T1_FIELD_NUM_TABLE2( "MinFeature",       min_feature,         2,
--                        T1_FIELD_DICT_PRIVATE )

--   T1_FIELD_NUM_TABLE ( "StemSnapH",        snap_widths,        12,
--                        T1_FIELD_DICT_PRIVATE )
--   T1_FIELD_NUM_TABLE ( "StemSnapV",        snap_heights,       12,
--                        T1_FIELD_DICT_PRIVATE )

--   T1_FIELD_FIXED     ( "ExpansionFactor",  expansion_factor,
--                        T1_FIELD_DICT_PRIVATE )
--   T1_FIELD_BOOL      ( "ForceBold",        force_bold,
--                        T1_FIELD_DICT_PRIVATE )


-- #undef  FT_STRUCTURE
-- #define FT_STRUCTURE  T1_FontRec
-- #undef  T1CODE
-- #define T1CODE        T1_FIELD_LOCATION_FONT_DICT

--   T1_FIELD_KEY  ( "FontName",    font_name,    T1_FIELD_DICT_FONTDICT )
--   T1_FIELD_NUM  ( "PaintType",   paint_type,   T1_FIELD_DICT_FONTDICT )
--   T1_FIELD_NUM  ( "FontType",    font_type,    T1_FIELD_DICT_FONTDICT )
--   T1_FIELD_FIXED( "StrokeWidth", stroke_width, T1_FIELD_DICT_FONTDICT )


-- #undef  FT_STRUCTURE
-- #define FT_STRUCTURE  FT_BBox
-- #undef  T1CODE
-- #define T1CODE        T1_FIELD_LOCATION_BBOX

--   T1_FIELD_BBOX( "FontBBox", xMin, T1_FIELD_DICT_FONTDICT )


-- #ifndef T1_CONFIG_OPTION_NO_MM_SUPPORT

-- #undef  FT_STRUCTURE
-- #define FT_STRUCTURE  T1_FaceRec
-- #undef  T1CODE
-- #define T1CODE        T1_FIELD_LOCATION_FACE

--   T1_FIELD_NUM( "NDV", ndv_idx, T1_FIELD_DICT_PRIVATE )
--   T1_FIELD_NUM( "CDV", cdv_idx, T1_FIELD_DICT_PRIVATE )


-- #undef  FT_STRUCTURE
-- #define FT_STRUCTURE  PS_BlendRec
-- #undef  T1CODE
-- #define T1CODE        T1_FIELD_LOCATION_BLEND

--   T1_FIELD_NUM_TABLE( "DesignVector", default_design_vector,
--                       T1_MAX_MM_DESIGNS, T1_FIELD_DICT_FONTDICT )


-- #endif /* T1_CONFIG_OPTION_NO_MM_SUPPORT */


-- /* END */
