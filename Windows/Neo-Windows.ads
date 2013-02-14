--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
--
with
  System,
  Interfaces,
  Interfaces.C,
  Ada.Command_Line,
  Ada.Unchecked_Conversion,
  Neo.Foundation.Data_Types;
use
  System,
  Interfaces,
  Interfaces.C,
  Ada.Command_Line,
  Neo.Foundation.Data_Types;
package Neo.Windows
  is
  ---------------
  -- Constants --
  ---------------
    ERROR_INSUFFICIENT_BUFFER                  : constant Integer_4_Unsigned_C := 16#0_0#; -- ???
    CODE_PAGE_UTF_8                            : constant Integer_4_Unsigned_C := 16#0000_FDE9#;
    KEY_READ                                   : constant Integer_4_Unsigned_C := 16#0002_0019#;
    HKEY_LOCAL_MACHINE                         : constant Integer_4_Unsigned_C := 16#8000_0002#;
    GENERIC_READ                               : constant Integer_4_Unsigned_C := 16#8000_0000#;
    GENERIC_WRITE                              : constant Integer_4_Unsigned_C := 16#4000_0000#;
    FILE_SHARE_READ                            : constant Integer_4_Unsigned_C := 16#0000_0001#;
    FILE_SHARE_WRITE                           : constant Integer_4_Unsigned_C := 16#0000_0002#;
    OPEN_EXISTING                              : constant Integer_4_Unsigned_C := 16#0000_0003#;
    ADD_LIST_OF_INSTALLED_DEVICES              : constant Integer_4_Unsigned_C := 16#0000_0004#;
    RESTRICT_DEVICES_TO_CURRENT_HARDWARE       : constant Integer_4_Unsigned_C := 16#0000_0002#;
    PROPERTY_FOR_DEVICE_DESCRIPTION            : constant Integer_4_Unsigned_C := 16#0000_0000#;
    PROPERTY_FOR_DEVICE_NAME                   : constant Integer_4_Unsigned_C := 16#0000_000C#;
    GET_RAW_DEVICE_NAME                        : constant Integer_4_Unsigned_C := 16#2000_0007#;
    GET_RAW_DEVICE_HEADER                      : constant Integer_4_Unsigned_C := 16#1000_0005#;
    GET_RAW_DEVICE_DATA                        : constant Integer_4_Unsigned_C := 16#1000_0003#;
    GET_RAW_DEVICE_PREPARSED_DATA              : constant Integer_4_Unsigned_C := 16#2000_0005#;
    FORMAT_MESSAGE_ALLOCATE_BUFFER             : constant Integer_4_Unsigned_C := 16#0000_0100#;
    FORMAT_MESSAGE_ARGUMENT_ARRAY              : constant Integer_4_Unsigned_C := 16#0000_2000#;
    FORMAT_MESSAGE_FROM_HMODULE                : constant Integer_4_Unsigned_C := 16#0000_0800#;
    FORMAT_MESSAGE_FROM_STRING                 : constant Integer_4_Unsigned_C := 16#0000_0400#;
    FORMAT_MESSAGE_FROM_SYSTEM                 : constant Integer_4_Unsigned_C := 16#0000_1000#;
    FORMAT_MESSAGE_IGNORE_INSERTS              : constant Integer_4_Unsigned_C := 16#0000_0200#;
    FORMAT_MESSAGE_MAX_WIDTH_MASK              : constant Integer_4_Unsigned_C := 16#0000_00FF#;
    EVENT_CLOSE                                : constant Integer_4_Unsigned_C := 16#0000_0010#;
    EVENT_KEY_UP                               : constant Integer_4_Unsigned_C := 16#0000_0101#;
    EVENT_SIZING                               : constant Integer_4_Unsigned_C := 16#0000_0214#;
    EVENT_COMMAND                              : constant Integer_4_Unsigned_C := 16#0000_0112#;
    EVENT_KEY_DOWN                             : constant Integer_4_Unsigned_C := 16#0000_0100#;
    EVENT_CHARACTER                            : constant Integer_4_Unsigned_C := 16#0000_0102#;
    EVENT_SIZE_CHANGED                         : constant Integer_4_Unsigned_C := 16#0000_0005#;
    EVENT_SYSTEM_KEY_DOWN                      : constant Integer_4_Unsigned_C := 16#0000_0104#;
    EVENT_SYSTEM_KEY_UP                        : constant Integer_4_Unsigned_C := 16#0000_0105#;
    EVENT_ACTIVATION_CHANGE                    : constant Integer_4_Unsigned_C := 16#0000_0006#;
    EVENT_MOUSE_MIDDLE_DOWN                    : constant Integer_4_Unsigned_C := 16#0000_0207#;
    EVENT_MOUSE_RIGHT_DOWN                     : constant Integer_4_Unsigned_C := 16#0000_0204#;
    EVENT_MOUSE_EXTRA_DOWN                     : constant Integer_4_Unsigned_C := 16#0000_020B#;
    EVENT_MOUSE_LEFT_DOWN                      : constant Integer_4_Unsigned_C := 16#0000_0201#;
    EVENT_MOUSE_MIDDLE_UP                      : constant Integer_4_Unsigned_C := 16#0000_0208#;
    EVENT_MOUSE_RIGHT_UP                       : constant Integer_4_Unsigned_C := 16#0000_0205#;
    EVENT_MOUSE_EXTRA_UP                       : constant Integer_4_Unsigned_C := 16#0000_020C#;
    EVENT_MOUSE_LEFT_UP                        : constant Integer_4_Unsigned_C := 16#0000_0202#;
    EVENT_MOUSE_WHEEL_VERTICAL                 : constant Integer_4_Unsigned_C := 16#0000_020A#;
    EVENT_MOUSE_WHEEL_HORIZONTAL               : constant Integer_4_Unsigned_C := 16#0000_020E#;
    EVENT_MOUSE_MOVE                           : constant Integer_4_Unsigned_C := 16#0000_0200#;
    EVENT_MOVE                                 : constant Integer_4_Unsigned_C := 16#0000_0003#;
    EVENT_CREATE                               : constant Integer_4_Unsigned_C := 16#0000_0001#;
    EVENT_INPUT_FOCUS_GAINED                   : constant Integer_4_Unsigned_C := 16#0000_0007#;
    EVENT_INPUT_FOCUS_LOST                     : constant Integer_4_Unsigned_C := 16#0000_0008#;
    STYLE_EXTRA_ALWAYS_ON_TOP                  : constant Integer_4_Unsigned_C := 16#0000_0008#;
    STYLE_EXTRA_NOTHING                        : constant Integer_4_Unsigned_C := 16#0000_0000#; 
    STYLE_NOTHING                              : constant Integer_4_Unsigned_C := 16#0000_0000#; 
    STYLE_TITLEBAR                             : constant Integer_4_Unsigned_C := 16#00C0_0000#;
    STYLE_TITLEBAR_MENU                        : constant Integer_4_Unsigned_C := 16#0008_0000#;
    STYLE_TITLEBARLESS_AND_BORDERLESS          : constant Integer_4_Unsigned_C := 16#8000_0000#;
    STYLE_VISIBLE_INITIALLY                    : constant Integer_4_Unsigned_C := 16#1000_0000#;
    STYLE_BORDER_THIN_LINE                     : constant Integer_4_Unsigned_C := 16#0080_0000#;
    STYLE_BORDER_SIZABLE                       : constant Integer_4_Unsigned_C := 16#0004_0000#;
    STYLE_BOX_FULLSCREEN                       : constant Integer_4_Unsigned_C := 16#0001_0000#;
    STYLE_BOX_ICONIZE                          : constant Integer_4_Unsigned_C := 16#0002_0000#;
    STYLE_NO_ACTIVATE                          : constant Integer_4_Unsigned_C := 16#0800_0000#;
    ICON_INFORMATION                           : constant Integer_4_Unsigned_C := 16#0000_0040#;
    ICON_WARNING                               : constant Integer_4_Unsigned_C := 16#0000_0030#;
    ICON_ERROR                                 : constant Integer_4_Unsigned_C := 16#0000_0010#;
    ICON_CUSTOM                                : constant Integer_4_Unsigned_C := 16#0000_0080#;
    LOAD_FROM_FILE                             : constant Integer_4_Unsigned_C := 16#0000_0010#;
    LOAD_ICO                                   : constant Integer_4_Unsigned_C := 16#0000_0001#;
    LOAD_CUR                                   : constant Integer_4_Unsigned_C := 16#0000_0002#;
    IGNORE_MESSAGE_FILTER_MINIMUM              : constant Integer_4_Unsigned_C := 16#0000_0000#;
    IGNORE_MESSAGE_FILTER_MAXIMUM              : constant Integer_4_Unsigned_C := 16#0000_0000#;
    BUTTON_OKAY                                : constant Integer_4_Unsigned_C := 16#0000_0000#;
    BUTTONS_YES_NO                             : constant Integer_4_Unsigned_C := 16#0000_0004#;
    BUTTONS_CANCEL_OKAY                        : constant Integer_4_Unsigned_C := 16#0000_0001#;
    BUTTONS_CANCEL_RETRY                       : constant Integer_4_Unsigned_C := 16#0000_0005#;
    MEMORY_MOVEABLE                            : constant Integer_4_Unsigned_C := 16#0000_0002#;
    MEMORY_DYNAMIC_DATA_EXCHANGE_SHARE         : constant Integer_4_Unsigned_C := 16#0000_2000#;
    MESSAGE_QUIT                               : constant Integer_4_Unsigned_C := 16#0000_0012#;
    CLIPBOARD_UNICODE_TEXT                     : constant Integer_4_Unsigned_C := 16#0000_000D#;
    REMOVE_MESSAGES_AFTER_PROCESSING           : constant Integer_4_Unsigned_C := 16#0000_0001#;
    NO_ERROR                                   : constant Integer_4_Unsigned_C := 16#0000_0000#;
    DEFAULT_TO_NEAREST_MONITOR                 : constant Integer_4_Unsigned_C := 16#0000_0002#;
    MAIL_API_TO                                : constant Integer_4_Unsigned_C := 16#0000_0001#;
    MAIL_API_DIALOG                            : constant Integer_4_Unsigned_C := 16#0000_0008#;
    ATTRIBUTE_SUB_DIRECTORY                    : constant Integer_4_Unsigned_C := 16#0000_0010#;
    GET_MOUSE                                  : constant Integer_4_Unsigned_C := 16#0000_0003#;
    SET_MOUSE                                  : constant Integer_4_Unsigned_C := 16#0000_0004#;
    SEND_CHANGE                                : constant Integer_4_Unsigned_C := 16#0000_0002#;
    GENERIC_DESKTOP_CONTROL                    : constant Integer_4_Unsigned_C := 16#0000_0001#;
    USE_RAW_MOUSE                              : constant Integer_4_Unsigned_C := 16#0000_0002#;
    USE_RAW_POINTER                            : constant Integer_4_Unsigned_C := 16#0000_0001#;
    USE_RAW_KEYBOARD                           : constant Integer_4_Unsigned_C := 16#0000_0005#;
    USE_RAW_JOYSTICK                           : constant Integer_4_Unsigned_C := 16#0000_0004#;
    USE_RAW_GAMEPAD                            : constant Integer_4_Unsigned_C := 16#0000_0006#;
    USE_RAW_KEYPAD                             : constant Integer_4_Unsigned_C := 16#0000_0004#;
    KIND_RAW_MOUSE                             : constant Integer_4_Unsigned_C := 16#0000_0000#;
    KIND_RAW_KEYBOARD                          : constant Integer_4_Unsigned_C := 16#0000_0001#;
    KIND_RAW_HUMAN_INTERFACE_DEVICE            : constant Integer_4_Unsigned_C := 16#0000_0002#;
    KIND_HUMAN_INTERFACE_DEVICE_INPUT          : constant Integer_4_Unsigned_C := 16#0000_0000#;
    SUCCESSFUL_HUMAN_INTEFACE_DEVICE_OPERATION : constant Integer_4_Unsigned_C := 16#0000_0000#;
    SUBEVENT_RAW_MOUSE_MIDDLE_DOWN             : constant Integer_4_Unsigned_C := 16#0000_0010#;
    SUBEVENT_RAW_MOUSE_MIDDLE_UP               : constant Integer_4_Unsigned_C := 16#0000_0020#;
    SUBEVENT_RAW_MOUSE_LEFT_DOWN               : constant Integer_4_Unsigned_C := 16#0000_0001#;
    SUBEVENT_RAW_MOUSE_LEFT_UP                 : constant Integer_4_Unsigned_C := 16#0000_0002#;
    SUBEVENT_RAW_MOUSE_RIGHT_DOWN              : constant Integer_4_Unsigned_C := 16#0000_0004#;
    SUBEVENT_RAW_MOUSE_RIGHT_UP                : constant Integer_4_Unsigned_C := 16#0000_0008#;
    SUBEVENT_RAW_MOUSE_EXTRA_1_DOWN            : constant Integer_4_Unsigned_C := 16#0000_0040#;
    SUBEVENT_RAW_MOUSE_EXTRA_1_UP              : constant Integer_4_Unsigned_C := 16#0000_0080#;
    SUBEVENT_RAW_MOUSE_EXTRA_2_DOWN            : constant Integer_4_Unsigned_C := 16#0000_0100#;
    SUBEVENT_RAW_MOUSE_EXTRA_2_UP              : constant Integer_4_Unsigned_C := 16#0000_0200#;
    SUBEVENT_RAW_MOUSE_WHEEL_VERTICAL          : constant Integer_4_Unsigned_C := 16#0000_0400#;
    SUBEVENT_RAW_MOUSE_WHEEL_HORIZONTAL        : constant Integer_4_Unsigned_C := 16#0000_0000#; -- ???
    SUBEVENT_ACTIVATED_BY_CLICK                : constant Integer_4_Unsigned_C := 16#0000_0002#;
    SUBEVENT_KEY_COMBINATION_ALT_ENTER         : constant Integer_4_Unsigned_C := 16#0000_000D#;
    SUBEVENT_RESIZE_BOTTOM_RIGHT               : constant Integer_4_Unsigned_C := 16#0000_0008#;
    SUBEVENT_RESIZE_BOTTOM_LEFT                : constant Integer_4_Unsigned_C := 16#0000_0007#;
    SUBEVENT_RESIZE_TOP_RIGHT                  : constant Integer_4_Unsigned_C := 16#0000_0005#;
    SUBEVENT_RESIZE_TOP_LEFT                   : constant Integer_4_Unsigned_C := 16#0000_0004#;
    SUBEVENT_RESIZE_BOTTOM                     : constant Integer_4_Unsigned_C := 16#0000_0006#;
    SUBEVENT_RESIZE_RIGHT                      : constant Integer_4_Unsigned_C := 16#0000_0002#;
    SUBEVENT_RESIZE_LEFT                       : constant Integer_4_Unsigned_C := 16#0000_0001#;
    SUBEVENT_RESIZE_TOP                        : constant Integer_4_Unsigned_C := 16#0000_0003#;
    SUBEVENT_RESIZE_SNAPBACK                   : constant Integer_4_Unsigned_C := 16#0000_0009#;
    SUBEVENT_CLICK_ACTIVATION                  : constant Integer_4_Unsigned_C := 16#0000_0002#;
    SUBEVENT_FULLSCREENED                      : constant Integer_4_Unsigned_C := 16#0000_0002#;
    SUBEVENT_RESTORED                          : constant Integer_4_Unsigned_C := 16#0000_0000#;
    SUBEVENT_ICONIZED                          : constant Integer_4_Unsigned_C := 16#0000_0001#;
    SUBEVENT_KEY_LEFT_MOUSE                    : constant Integer_4_Unsigned_C := 16#0000_0001#;
    SUBEVENT_KEY_CLEAR                         : constant Integer_4_Unsigned_C := 16#0000_00FE#;
    SUBEVENT_KEY_F10                           : constant Integer_4_Unsigned_C := 16#0000_0079#;
    SUBEVENT_KEY_F11                           : constant Integer_4_Unsigned_C := 16#0000_007A#;
    SUBEVENT_KEY_ALTERNATIVE                   : constant Integer_4_Unsigned_C := 16#0000_0012#;
    SUBEVENT_KEY_ALTERNATIVE_LEFT              : constant Integer_4_Unsigned_C := 16#0000_00A4#;
    SUBEVENT_KEY_ALTERNATIVE_RIGHT             : constant Integer_4_Unsigned_C := 16#0000_00A5#;
    SUBEVENT_KEY_CONTROL                       : constant Integer_4_Unsigned_C := 16#0000_0011#;
    SUBEVENT_KEY_CONTROL_LEFT                  : constant Integer_4_Unsigned_C := 16#0000_00A2#;
    SUBEVENT_KEY_CONTROL_RIGHT                 : constant Integer_4_Unsigned_C := 16#0000_00A3#;
    SUBEVENT_KEY_SHIFT                         : constant Integer_4_Unsigned_C := 16#0000_0010#;
    SUBEVENT_KEY_SHIFT_LEFT                    : constant Integer_4_Unsigned_C := 16#0000_00A0#;
    SUBEVENT_KEY_SHIFT_RIGHT                   : constant Integer_4_Unsigned_C := 16#0000_00A1#;
    SUBEVENT_MENU_POPOUT                       : constant Integer_4_Unsigned_C := 16#0000_F100#;
    SUBEVENT_SCREEN_SAVER_START                : constant Integer_4_Unsigned_C := 16#0000_F140#;
    SUBEVENT_WORD_LOW                          : constant Integer_4_Unsigned_C := 16#0000_FFFF#;
    SUBEVENT_WORD_HIGH                         : constant Integer_4_Unsigned_C := 16#FFFF_0000#;
    SUBEVENT_SHORT_LOW                         : constant Integer_2_Unsigned_C := 16#00FF#;
    END_OF_FILE_ON_WINDOWS                     : constant Integer_2_Unsigned_C := 16#FFFF#;
    SHORT_HIGH_BIT                             : constant Integer_2_Unsigned_C := 16#C000#;
    LANGUAGE_ARABIC                            : constant Integer_2_Unsigned_C := 16#0401#;
    LANGUAGE_BASQUE                            : constant Integer_2_Unsigned_C := 16#042D#;
    LANGUAGE_CATALAN                           : constant Integer_2_Unsigned_C := 16#0403#;
    LANGUAGE_CHINESE_SIMPLIFIED                : constant Integer_2_Unsigned_C := 16#0804#;
    LANGUAGE_CHINESE_TRADITIONAL               : constant Integer_2_Unsigned_C := 16#0404#;
    LANGUAGE_CZECH                             : constant Integer_2_Unsigned_C := 16#0405#;
    LANGUAGE_DANISH                            : constant Integer_2_Unsigned_C := 16#0406#;
    LANGUAGE_DUTCH                             : constant Integer_2_Unsigned_C := 16#0413#;
    LANGUAGE_ENGLISH                           : constant Integer_2_Unsigned_C := 16#0409#;
    LANGUAGE_FINNISH                           : constant Integer_2_Unsigned_C := 16#040B#;
    LANGUAGE_FRENCH                            : constant Integer_2_Unsigned_C := 16#040C#;
    LANGUAGE_GERMAN                            : constant Integer_2_Unsigned_C := 16#0407#;
    LANGUAGE_GREEK                             : constant Integer_2_Unsigned_C := 16#0408#;
    LANGUAGE_HEBREW                            : constant Integer_2_Unsigned_C := 16#040D#;
    LANGUAGE_HUNGARIAN                         : constant Integer_2_Unsigned_C := 16#040E#;
    LANGUAGE_ITALIAN                           : constant Integer_2_Unsigned_C := 16#0410#;
    LANGUAGE_JAPANESE                          : constant Integer_2_Unsigned_C := 16#0411#;
    LANGUAGE_KOREAN                            : constant Integer_2_Unsigned_C := 16#0412#;
    LANGUAGE_NORWEGIAN                         : constant Integer_2_Unsigned_C := 16#0414#;
    LANGUAGE_POLISH                            : constant Integer_2_Unsigned_C := 16#0415#;
    LANGUAGE_PORTUGUESE                        : constant Integer_2_Unsigned_C := 16#0816#;
    LANGUAGE_PORTUGUESE_BRAZIL                 : constant Integer_2_Unsigned_C := 16#0416#;
    LANGUAGE_RUSSIAN                           : constant Integer_2_Unsigned_C := 16#0419#;
    LANGUAGE_SLOVAKIAN                         : constant Integer_2_Unsigned_C := 16#041B#;
    LANGUAGE_SLOVENIAN                         : constant Integer_2_Unsigned_C := 16#0424#;
    LANGUAGE_SPANISH                           : constant Integer_2_Unsigned_C := 16#0C0A#;
    LANGUAGE_SWEDISH                           : constant Integer_2_Unsigned_C := 16#041D#;
    LANGUAGE_TURKISH                           : constant Integer_2_Unsigned_C := 16#041F#;
    LANGUAGE_NEUTRAL                           : constant Integer_1_Unsigned_C := 16#00#;
    SUBLANGUAGE_DEFAULT                        : constant Integer_1_Unsigned_C := 16#01#;
    GET_CLASS_CURSOR                           : constant Integer_4_Signed_C   := -12;
    SET_WINDOW_STYLE                           : constant Integer_4_Signed_C   := -16;
    SET_WINDOW_STYLE_EXTRA                     : constant Integer_4_Signed_C   := -20;
    SET_CLASS_CURSOR                           : constant Integer_4_Signed_C   := -12;
    HOOK_LOW_LEVEL_KEYBOARD                    : constant Integer_4_Signed_C   := 13;
    HOOK_LOW_LEVEL_MOUSE                       : constant Integer_4_Signed_C   := 14;
    REGION_NULL                                : constant Integer_4_Signed_C   := 0;
    REGION_COMPLEX                             : constant Integer_4_Signed_C   := 2;
    REGION_SIMPLE                              : constant Integer_4_Signed_C   := 1;
    PRESSED_OKAY                               : constant Integer_4_Signed_C   := 1;
    PRESSED_RETRY                              : constant Integer_4_Signed_C   := 4;
    PRESSED_YES                                : constant Integer_4_Signed_C   := 6;
    FAILED                                     : constant Integer_4_Signed_C   := 0;
    AN_ACTION_OCCURED                          : constant Integer_4_Signed_C   := 0;
    MAKE_WINDOW_FULLSCREEN                     : constant Integer_4_Signed_C   := 3;
    MAKE_WINDOW_HIDE                           : constant Integer_4_Signed_C   := 0;
    MAKE_WINDOW_RESTORE                        : constant Integer_4_Signed_C   := 9;
    MAKE_WINDOW_NORMALIZE                      : constant Integer_4_Signed_C   := 1;
    MAKE_WINDOW_GO_TO_ICONIC                   : constant Integer_4_Signed_C   := 2;
    DATA_HORZONTAL_RESOLUTION                  : constant Integer_4_Signed_C   := 8;
    DATA_NUMBER_OF_BITS_PER_PIXEL              : constant Integer_4_Signed_C   := 12;
    DATA_VERTICAL_RESOLUTION                   : constant Integer_4_Signed_C   := 10;
    MILLISECOND_TIMEOUT_FORCE_WRITE            : constant Integer_4_Signed_C   := 500;
    MOUSE_WHEEL_DELTA                          : constant Integer_2_Signed     := 120;
    MAXIMUM_PATH_FOR_CREATE_FILE               : constant Integer_4_Signed     := 32_767;
  -------------
  -- Arrays --
  ------------
    type Reserved_Capabilities
      is array (1..17)
      of Integer_2_Unsigned_C;
    type Reserved_Button_Capabilities
      is array (1..10)
      of Integer_4_Unsigned_C;
    type Reserved_Shit
      is array (1..5)
      of Integer_2_Unsigned_C;
  -------------
  -- Records --
  -------------
    -- typedef struct _OVERLAPPED {
    --   ULONG_PTR Internal;
    --   ULONG_PTR InternalHigh;
    --   union {
    --     struct {
    --       DWORD Offset;
    --       DWORD OffsetHigh;
    --     };
    --     PVOID  Pointer;
    --   };
    --   HANDLE    hEvent;
    -- } OVERLAPPED, *LPOVERLAPPED;
    -- type Record_Overlapped_
    --   is record
    --     Internal : ;
    --     Internal_High : ;
    --     Event  : Address;
    --   end record;
    -- type Record_Device_Attributes
    --   is record
    --     Size    : Integer_4_Unsigned_C := Record_Device_Attributes'Size / 8;
    --     Vendor  : Integer_2_Unsigned_C := 0;
    --     Product : Integer_2_Unsigned_C := 0;
    --     Version : Integer_2_Unsigned_C := 0;
    --   end record;
    -- type Record_Device_Header
    --   is record
    --     Kind        : Integer_4_Unsigned_C := 0;
    --     Size        : Integer_4_Unsigned_C := 0;
    --     Device      : Address              := NULL_ADDRESS;
    --     Data_Signed : Integer_4_Signed_C   := 0;
    --   end record;
    --   pragma Convention(C, Record_Device_Header);
    -- type Record_Device_Capabilities
    --   is record
    --     Usage                                 : Integer_2_Unsigned_C  := 0;
    --     Page                                  : Integer_2_Unsigned_C  := 0;
    --     Input_Report_Byte_Length              : Integer_2_Unsigned_C  := 0;
    --     Output_Report_Byte_Length             : Integer_2_Unsigned_C  := 0;
    --     Feature_Report_Byte_Length            : Integer_2_Unsigned_C  := 0;
    --     Reserved                              : Reserved_Capabilities := ;
    --     Number_Of_Link_Collection_Nodes       : Integer_2_Unsigned_C  := 0;
    --     Number_Of_Input_Button_Capabilities   : Integer_2_Unsigned_C  := 0;
    --     Number_Of_Input_Value_Capabilities    : Integer_2_Unsigned_C  := 0;
    --     Number_Of_Input_Data_Indices          : Integer_2_Unsigned_C  := 0;
    --     Number_Of_Output_Button_Capabilities  : Integer_2_Unsigned_C  := 0;
    --     Number_Of_Output_Value_Capabilities   : Integer_2_Unsigned_C  := 0;
    --     Number_Of_Output_Data_Indices         : Integer_2_Unsigned_C  := 0;
    --     Number_Of_Feature_Button_Capabilities : Integer_2_Unsigned_C  := 0;
    --     Number_Of_Feature_Value_Capabilities  : Integer_2_Unsigned_C  := 0;
    --     Number_Of_Feature_Data_Indices        : Integer_2_Unsigned_C  := 0;
    --   end record;
    --   pragma Convention(C, Record_Device_Capabilities);
    -- type Record_Device_Button_Capabilities
    --   is record
    --     Page                : Integer_2_Unsigned_C         := 0;
    --     Report_Identifier   : Integer_1_Unsigned_C         := 0;
    --     Is_Alias            : Integer_4_Signed_C           := 0;
    --     Bit_Field           : Integer_2_Unsigned_C         := 0;
    --     Link_Usage          : Integer_2_Unsigned_C         := 0;
    --     Link_Page           : Integer_2_Unsigned_C         := 0;
    --     Is_Range            : Integer_4_Signed_C           := 0;
    --     Is_String_Range     : Integer_4_Signed_C           := 0;
    --     Is_Designator_Range : Integer_4_Signed_C           := 0;
    --     Is_Absolute         : Integer_4_Signed_C           := 0;
    --     Reserved            : Reserved_Button_Capabilities := ;
    --     Usage_Minimum       : Integer_2_Unsigned_C         := 0;
    --     String_Minimum      : Integer_2_Unsigned_C         := 0;
    --     String_Maximum      : Integer_2_Unsigned_C         := 0;
    --     Designator_Minimum  : Integer_2_Unsigned_C         := 0;
    --     Designator_Maximum  : Integer_2_Unsigned_C         := 0;
    --     Data_Index_Minimum  : Integer_2_Unsigned_C         := 0;
    --     Data_Index_Maximum  : Integer_2_Unsigned_C         := 0;
    --   end record;
    --   pragma Convention(C, Record_Device_Button_Capabilities);
    -- type Record_Device_Capability_Values
    --   is record
    --     Page                : Integer_2_Unsigned_C := 0;
    --     Report_Identifier   : Integer_1_Unsigned_C := 0;
    --     Is_Alias            : Integer_4_Signed_C   := 0;
    --     Bit_Field           : Integer_2_Unsigned_C := 0;
    --     Link_Collection     : Integer_2_Unsigned_C := 0;
    --     Link_Usage          : Integer_2_Unsigned_C := 0;
    --     Link_Page           : Integer_2_Unsigned_C := 0;
    --     Is_Range            : Integer_4_Signed_C   := 0;
    --     Is_String_Range     : Integer_4_Signed_C   := 0;
    --     Is_Designator_Range : Integer_4_Signed_C   := 0;
    --     Is_Absolute         : Integer_4_Signed_C   := 0;
    --     Has_Null            : Integer_4_Signed_C   := 0;
    --     Reserved_A          : Integer_1_Unsigned_C := 0;
    --     Bit_Size            : Integer_2_Unsigned_C := 0;
    --     Report_Count        : Integer_2_Unsigned_C := 0;
    --     Reserved_B          : Reserved_        := ;
    --     Units_Exponent      : Integer_4_Signed_C   := 0;
    --     Logical_Minimum     : Integer_4_Signed_C   := 0;
    --     Logical_Maximum     : Integer_4_Signed_C   := 0;
    --     Physical_Minimum    : Integer_4_Signed_C   := 0;
    --     Physical_Maximum    : Integer_4_Signed_C   := 0;
    --     Usage_Minimum       : Integer_2_Unsigned_C := 0;
    --     Usage_Maximum       : Integer_2_Unsigned_C := 0;
    --     String_Minimum      : Integer_2_Unsigned_C := 0;
    --     String_Maximum      : Integer_2_Unsigned_C := 0;
    --     Designator_Minimum  : Integer_2_Unsigned_C := 0;
    --     Designator_Maximum  : Integer_2_Unsigned_C := 0;
    --   end record;
    --   pragma Convention(C, Record_Device_Capability_Values);
    type Record_Mouse
      is record
        Flags             : Integer_2_Unsigned_C := 0;
        Button_Flags      : Integer_2_Unsigned_C := 0;
        Button_Data       : Integer_2_Unsigned_C := 0;
        Buttons           : Integer_4_Unsigned_C := 0;
        Last_X            : Integer_4_Signed_C   := 0;
        Last_Y            : Integer_4_Signed_C   := 0;
        Extra_Information : Integer_4_Unsigned_C := 0;
      end record;
      pragma Convention(C, Record_Mouse);
    -- type Record_Mouse
    --   is record
    --     Point       : Record_Point;
    --     Data        : Integer_4_Unsigned_C;
    --     Flags       : Integer_4_Unsigned_C;
    --     Time        : Integer_4_Unsigned_C;
    --     Information : Address; -- Changes on 64/32 bit systems
    --   end record;
    --   pragma Convention(C, Record_Mouse);
    type Record_Keyboard
      is record
        Make_Code         : Integer_2_Unsigned_C := 0;
        Flags             : Integer_2_Unsigned_C := 0;
        Reserved          : Integer_2_Unsigned_C := 0;
        Key               : Integer_2_Unsigned_C := 0;
        Message           : Integer_4_Unsigned_C := 0;
        Extra_Information : Integer_4_Unsigned_C := 0;
      end record;
      pragma Convention(C, Record_Keyboard);
    type Record_Version_Information
      is record
        Size                : Integer_4_Unsigned_C := Record_Version_Information'Size / 8;
        Major               : Integer_4_Unsigned_C := 0;
        Minor               : Integer_4_Unsigned_C := 0;
        Build_Number        : Integer_4_Unsigned_C := 0;
        Platform_Identifier : Integer_4_Unsigned_C := 0;
        Service_Pack        : String_2(1..128)     := (others => ' ');
        Service_Pack_Major  : Integer_2_Unsigned_C := 0;
        Service_Pack_Minor  : Integer_2_Unsigned_C := 0;
        Suite_Mask          : Integer_2_Unsigned_C := 0;
        Product_Type        : Integer_1_Unsigned_C := 0;
        Reserved            : Integer_1_Unsigned_C := 0;
      end record;
      pragma Convention(C, Record_Version_Information);
    type Record_Device_Interface
      is record
        Size     : Integer_4_Unsigned_C        := Record_Device_Interface'Size / 8;
        Class_ID : Integer_4_Unsigned_C        := 0;
        Flags    : Integer_4_Unsigned_C        := 0;
        Reserved : Access_Integer_4_Unsigned_C := null; -- ULONG_PTR
      end record;
    type Record_Flash_Information
      is record
        Size     : Integer_4_Unsigned_C := Record_Flash_Information'Size / 8;
        Window   : Address              := NULL_ADDRESS;
        Flags    : Integer_4_Unsigned_C := 0;
        Count    : Integer_4_Unsigned_C := 0;
        Time_Out : Integer_4_Unsigned_C := 0;
      end record;
      pragma Convention(C, Record_Flash_Information);
    type Record_Rectangle
      is record
        Left   : Integer_4_Signed_C := 0;
        Top    : Integer_4_Signed_C := 0;
        Right  : Integer_4_Signed_C := 0;
        Bottom : Integer_4_Signed_C := 0;
      end record;
      pragma Convention(C, Record_Rectangle); 
    type Record_Monitor_Information
      is record
        Size      : Integer_4_Unsigned_C := Record_Monitor_Information'Size / 8;
        Monitor   : Record_Rectangle;--     := <>;
        Work_Area : Record_Rectangle;--     := <>;
        Flags     : Integer_4_Unsigned_C := 0;
      end record;
      pragma Convention(C, Record_Monitor_Information);
    type Record_Window_Class
      is record
        Size       : Integer_4_Unsigned_C          := Record_Window_Class'Size / 8;
        Style      : Integer_4_Unsigned_C          := 0;
        Callback   : Address                       := NULL_ADDRESS;
        Extra_A    : Integer_4_Signed_C            := 0;
        Extra_B    : Integer_4_Signed_C            := 0;
        Instance   : Address                       := NULL_ADDRESS;
        Icon_Large : Address                       := NULL_ADDRESS;
        Cursor     : Address                       := NULL_ADDRESS;
        Background : Address                       := NULL_ADDRESS;
        Menu_Name  : Access_Constant_Character_2_C := null;
        Class_Name : Access_Constant_Character_2_C := null;
        Icon_Small : Address                       := NULL_ADDRESS;
      end record;
      pragma Convention(C, Record_Window_Class); 
    type Record_Point
      is record
        X : Integer_4_Signed_C := 0;
        Y : Integer_4_Signed_C := 0;
      end record;
      pragma Convention(C, Record_Point);
    type Record_Message
      is record
        Window        : Address              := NULL_ADDRESS;
        Data          : Integer_4_Unsigned_C := 0;
        Data_Unsigned : Integer_4_Unsigned_C := 0;
        Data_Signed   : Integer_4_Signed_C   := 0;
        Time          : Integer_4_Unsigned_C := 0;
        Point         : Record_Point;--         := <>;
      end record;
      pragma Convention(C, Record_Message);
    type Record_Key
      is record
        Code        : Integer_4_Unsigned_C := 0;
        Scan_Code   : Integer_4_Unsigned_C := 0;
        Flags       : Integer_4_Unsigned_C := 0;
        Time        : Integer_4_Unsigned_C := 0;
        Information : Address              := NULL_ADDRESS; -- Changes on 64/32 bit systems
      end record;
      pragma Convention(C, Record_Key);
    type Record_Memory_Status
      is record
        Size                       : Integer_4_Unsigned_C := Record_Memory_Status'Size / 8;
        Memory_Load                : Integer_4_Unsigned_C := 0;
        Total_Physical             : Integer_8_Unsigned_C := 0;
        Available_Physical         : Integer_8_Unsigned_C := 0;
        Total_Page_File            : Integer_8_Unsigned_C := 0;
        Available_Page_File        : Integer_8_Unsigned_C := 0;
        Total_Virtual              : Integer_8_Unsigned_C := 0;
        Available_Virtual          : Integer_8_Unsigned_C := 0;
        Available_Extended_Virtual : Integer_8_Unsigned_C := 0;
      end record;
      pragma Convention(C, Record_Memory_Status);
    type Record_GUID
      is record
        First_Eight_Hex   : Integer_4_Unsigned_C             := 0;
        Second_Four_Hex   : Integer_2_Unsigned_C             := 0;
        Third_Four_Hex    : Integer_2_Unsigned_C             := 0;
        Final_Sixteen_Hex : Array_Integer_1_Unsigned_C(1..8) := (others => 0);
      end record;
      pragma Convention(C, Record_GUID);
    type Record_Device_Information
      is record
        Size       : Integer_4_Unsigned_C := Record_Device_Information'Size / 8;
        Class_GUID : Record_GUID;--          := <>;
        Instance   : Integer_4_Unsigned_C := 0;
        Reserved   : Address              := NULL_ADDRESS; -- ULONG_PTR
      end record;
      pragma Convention(C, Record_Device_Information);
    type Record_Device
      is record
        Page   : Integer_2_Unsigned_C := 0;
        Usage  : Integer_2_Unsigned_C := 0;
        Flags  : Integer_4_Unsigned_C := 0;
        Target : Address              := NULL_ADDRESS;
      end record;
      pragma Convention(C, Record_Device);
    type Record_Device_List_Element
      is record
        Handle : Address              := NULL_ADDRESS;
        Kind   : Integer_4_Unsigned_C := 0;
      end record;
      pragma Convention(C, Record_Device_List_Element);
  ------------
  -- Arrays --
  ------------
    type Array_Record_Device_List_Element
      is array (Positive range <>)
      of Record_Device_List_Element;
  ---------------
  -- Accessors --
  ---------------
    -- type Access_Record_Device_List
    --   is access all Record_Device_List;
    -- type Access_Array_Record_Device_List
    --   is access all Array_Record_Device_List;
    type Access_Record_Key
      is access all Record_Key;
    type Access_Record_Mouse
      is access all Record_Mouse;
    type Access_Record_Rectangle
      is access all Record_Rectangle;
    type Access_Record_Monitor_Information
      is access all Record_Monitor_Information;
  -----------------
  -- Subprograms --
  -----------------
    function To_Integer_4_Signed_C
      is new Ada.Unchecked_Conversion(Access_Record_Mouse, Integer_4_Signed_C);
    function To_Integer_4_Signed_C
      is new Ada.Unchecked_Conversion(Access_Record_Key, Integer_4_Signed_C);
    function To_Address
      is new Ada.Unchecked_Conversion(Integer_4_Unsigned, Address);
    function To_Character_2
      is new Ada.Unchecked_Conversion(Integer_2_Unsigned, Character_2);
    function To_Access_Record_Rectangle   
      is new Ada.Unchecked_Conversion(Address, Access_Record_Rectangle);
    function To_Access_Record_Key 
      is new Ada.Unchecked_Conversion(Integer_4_Signed_C, Access_Record_Key);
    function To_Access_Record_Rectangle   
      is new Ada.Unchecked_Conversion(Integer_4_Signed_C, Access_Record_Rectangle);
    function Get_Blank_Cursor
      return Array_Integer_1_Unsigned;
    -- procedure Get_Device_Identifier(
    --   Identifier : in Access_Device_Identifier);
    function Is_Lower(
      Item : in Character_2)
      return Integer_4_Signed;
    function Is_Upper(
      Item : in Character_2)
      return Integer_4_Signed;
    -- function Write_File(
    --   File                     : in Address;
    --   Buffer                   : in Access_Constant_Array_Integer_1_Unsigned_C;
    --   Number_Of_Bytes_To_Write : in Integer_4_Unsigned_C;
    --   Bytes_Written            : in Access_Integer_4_Unsigned_C;
    --   Over_Lapped              : in Access_Record_Overlapped)
    --   return Integer_4_Signed_C;
    -- function Convert_String_2_C_To_UTF_8(
    --   Code_Page                   : in Integer_4_Unsigned_C;
    --   Flags                       : in Integer_4_Unsigned_C;
    --   Original_String             : in Access_String_2_C;
    --   Character_Count_Of_Original : in Integer_4_Signed_C;
    --   Resulting_String            : in Access_String_1_C;
    --   Byte_Count_Of_Result        : in Integer_4_Signed_C;
    --   Default_Character           : in Access_Constant_String_1_C;
    --   Used_Default_Character      : in Access_Integer_4_Signed_C)
    --   return Integer_4_Signed_C;
    function Register_Devices(
      Devices : in Address;
      Number  : in Integer_4_Unsigned_C;
      Size    : in Integer_4_Unsigned_C)
      return Integer_4_Signed_C;
    function Set_Device_Output_Data(
      Device : in Address;
      Data   : in Address;
      Size   : in Integer_4_Unsigned_C)
      return Integer_4_Signed_C;
    -- function Get_Device_Attributes(
    --   Device     : in Address;
    --   Attributes : in Access_Device_Attributes)
    --   return Integer_4_Signed_C;
    -- function Get_Device_Interface_Detail(
    --   Information_Set       : in Record_Device_Information;--_In_       HDEVINFO 
    --   Interface             : in Record_Device_Interface;--_In_       PSP_DEVICE_INTERFACE_DATA 
    --   Interface_Detail      : in Record_Device_Detail;--_Out_opt_  PSP_DEVICE_INTERFACE_DETAIL_DATA 
    --   Interface_Detail_Size : in Integer_4_Unsigned_C;--, _In_       DWORD 
    --   Required_Size         : in Access_Integer_4_Unsigned_C;--_Out_opt_  PDWORD 
    --   Information           : in );--_Out_opt_  PSP_DEVINFO_DATA);
    --   return Integer_4_Signed;
    -- function Enumerate_Device_Interfaces(
    --   Information_Set      : in Access_Device_Information;
    --   Information_Data     : in Access_PSP_DEVINFO_DATA;
    --   Interface_Class_Guid : in Access_GUID;
    --   Member_Index         : in Integer_4_Unsigned;
    --   Interface_Data       : in Access_PSP_DEVICE_INTERFACE_DATA)
    --   return Integer_4_Signed;
    function Sleep(
      Milliseconds : in Integer_4_Unsigned_C;
      Alertable    : in Integer_4_Signed_C)
      return Integer_4_Signed_C;
    function Registry_Close_Key(
      Key : in Address)
      return Integer_4_Signed_C;
    function Registry_Open_Key(
      Key     : in Integer_4_Unsigned_C;
      Sub_Key : in String_2_C;
      Options : in Integer_4_Unsigned_C;
      Desired : in Integer_4_Unsigned_C;
      Result  : in Address)
      return Integer_4_Unsigned_C;
    function Registry_Query_Value(
      Key        : in Address;
      Value_Name : in String_2_C;
      Reserved   : in Address;
      Kind       : in Address;
      Data       : in Address;
      Data_Size  : in Address)
      return Integer_4_Unsigned_C;
    function Get_Device_Usages(
      Kind            : in Integer_4_Signed_C; -- Enumerated
      Page            : in Integer_2_Unsigned_C;
      Link_Collection : in Integer_2_Unsigned_C;
      Usage           : in Address;
      Usage_Length    : in Address; -- PULONG
      Preparsed_Data  : in Address;
      Report          : in Access_Array_Integer_1_Unsigned_C;
      Report_Length   : in Integer_4_Unsigned_C)
      return Integer_4_Unsigned_C;
    function Get_Device_Button_Capabilities(
      Kind                : in Integer_4_Signed_C;
      Button_Capabilities : in Address;
      Length              : in Address;
      Preparsed_Data      : in Address)
      return Integer_4_Unsigned_C;
    function Create_File(
      Name                 : in Address;
      Desired_Access       : in Integer_4_Unsigned_C;
      Share_Mode           : in Integer_4_Unsigned_C;
      Security_Attributes  : in Address;
      Creation_Desposition : in Integer_4_Unsigned_C;
      Flags_And_Attributes : in Integer_4_Unsigned_C;
      Template_File        : in Address)
      return Address;
    function Get_Device_Manufacturer(
      File   : in Address;
      Buffer : in Address;
      Size   : in Integer_4_Unsigned_C)
      return Integer_4_Signed_C;
    function Get_Device_Description(
      File   : in Address;
      Buffer : in Address;
      Size   : in Integer_4_Unsigned_C)
      return Integer_4_Signed_C;
    function Get_Device_Product(
      File   : in Address;
      Buffer : in Address;
      Size   : in Integer_4_Unsigned_C)
      return Integer_4_Signed_C;
    -- function Get_Device_Information(
    --   Device  : in Address;
    --   Command : in Integer_4_Unsigned_C;
    --   Data    : in Address;
    --   Size    : in Address)
    --   return Integer_4_Unsigned_C;
    function Get_Device_Input_Data(
      Raw_Input   : in Address;
      Command     : in Integer_4_Unsigned_C;
      Data        : in Address;
      Size        : in Address;
      Header_Size : in Integer_4_Unsigned_C)
      return Integer_4_Unsigned_C;
    -- function Get_Device_Information(
    --   Guid          : in Address;
    --   Enumerator    : in Address;
    --   Window_Parent : in Address;
    --   Flags         : in Integer_4_Unsigned_C)
    --   return Address;
    function Get_Device_Registry_Property(
      Set           : in Address;
      Data          : in Address;
      Property      : in Integer_4_Unsigned_C;
      Registry_Kind : in Address;
      Buffer        : in Address;
      Size          : in Integer_4_Unsigned_C;
      Required_Size : in Address)
      return Integer_4_Signed_C;
    function Get_Device_Instance_Id(
      Set           : in Address;
      Data          : in Address;
      Id            : in Address;
      Size          : in Integer_4_Unsigned_C;
      Required_Size : in Address)
      return Integer_4_Signed_C;
    function Get_Device_Enumeration(
      Set    : in Address;
      Member : in Integer_4_Unsigned_C;
      Data   : in Address)
      return Integer_4_Signed_C;
    function Get_Device_List(
      List  : in Address;
      Count : in Address;
      Size  : in Integer_4_Unsigned_C)
      return Integer_4_Unsigned_C;
    function Destroy_Device_List(
      List : in Address)
      return Integer_4_Signed_C;
    function Enumerate_Display_Monitor(
      Device_Context : in Address;
      Clip           : in Address;
      Callback       : in Address;
      Data           : in Integer_4_Signed_C)
      return Integer_4_Signed_C;
    function Get_Clip_Box(
      Device_Context : in Address;
      Rectangle      : in Address)
      return Integer_4_Signed_C;
    function Find_Intersecting_Rectangle(
      Destination : in Address;
      Rectangle_A : in Address;
      Rectangle_B : in Address)
      return Integer_4_Signed_C;
    function Get_Client_Rectangle(
      Window    : in Address;
      Rectangle : in Address)
      return Integer_4_Signed_C;
    function Rectangles_Are_Equal(
      Rectangle_A : Address;
      Rectangle_B : Address)
      return Integer_4_Signed_C;
    function Get_Class_Setting(
      Window : in Address;
      Index  : in Integer_4_Signed_C)
      return Integer_4_Unsigned_C;
    function Get_Version(
      Version_Information : in Address)
      return Integer_4_Signed_C;
    function Get_User_Name(
      Buffer : Address;
      Size   : Address)
      return Integer_4_Signed_C;
    function Create_Cursor(
      Instance   : in Address;
      Hot_Spot_X : in Integer_4_Signed_C;
      Hot_Spot_Y : in Integer_4_Signed_C;
      Width      : in Integer_4_Signed_C;
      Height     : in Integer_4_Signed_C;
      Bits_AND   : in Address;
      Bits_XOR   : in Address)
      return Address;
    function Change_Class_Setting(
      Window  : in Address;
      Command : in Integer_4_Signed_C;
      Setting : in Integer_4_Unsigned_C)
      return Integer_4_Unsigned_C;
    function Flash_Window(
      Flash_Information : in Address)
      return Integer_4_Signed_C;
    function Get_Clip_Cursor_Area(
      Rectangle : in Address)--Record_Rectangle)
      return Integer_4_Signed_C;
    function Clip_Cursor(
      Rectangle : in Address)--Record_Rectangle)
      return Integer_4_Signed_C;
    function System_Parameter_Information(
      Action       : in Integer_4_Unsigned_C;
      Parameter_A  : in Integer_4_Unsigned_C;
      Parameter_B  : in Address;
      User_Profile : in Integer_4_Unsigned_C)
      return Integer_4_Signed_C;
    function Change_Window_Setting( -- 64 bit difference, SetWindowLongPtr
      Window  : in Address;
      Command : in Integer_4_Signed_C;
      Setting : in Integer_4_Unsigned_C)
      return Integer_4_Unsigned_C;
    function Get_Procedure_Address(
      Module         : in Address;
      Procedure_Name : in Access_Constant_Character_2_C)
      return Address;
    function Get_Key_State(
      Virtual_Key : in Integer_4_Unsigned_C)
      return Integer_2_Unsigned_C;
    function Free_Library(
      Module : in Address)
      return Integer_4_Signed_C;
    function Load_Library(
      Name : in Access_Constant_Character_2_C) 
      return Address; 
    function Get_Foreground_Window
      return Address;
    function Get_Disk_Free_Space(
      Directory                  : in Access_Constant_Character_2_C;
      Free_Bytes_Available       : in Address;
      Total_Number_Of_Bytes      : in Address;
      Total_Number_Of_Free_Bytes : in Address)
      return Integer_4_Signed_C;
    function Shell_Execute(
      Window       : in Address;
      Operation    : in Access_Constant_Character_2_C;
      File         : in Access_Constant_Character_2_C;
      Parameters   : in Access_Constant_Character_2_C;
      Directory    : in Access_Constant_Character_2_C;
      Show_Command : in Integer_4_Signed_C)
      return Address;
    function Set_Process_Working_Set_Size(
      Process : in Address;
      Minimum : in Integer_Size_C;
      Maximum : in Integer_Size_C)
      return Integer_4_Signed_C;
    function Virtual_Unlock(
      Data : in Address;
      Size : in Integer_Size_C)
      return Integer_4_Signed_C;
    function Virtual_Lock(
      Data : in Address;
      Size : in Integer_Size_C)
      return Integer_4_Signed_C;
    function Global_Allocate(
      Flags : Integer_4_Unsigned_C;
      Bytes : Integer_Size_C)
      return Address;
    function Get_Clipboard_Data(
      Format : in Integer_4_Unsigned_C)
      return Address;
    function Global_Lock(
      Memory : in Address)
      return Address;
    function Global_Unlock(
      Memory : in Address)
      return Integer_4_Signed_C;
    function Global_Free(
      Memory : in Address)
      return Address;
    function Open_Clipboard(
      Window : in Address)
      return Integer_4_Signed_C;
    function Close_Clipboard
      return Integer_4_Signed_C;
    function Empty_Clipboard
      return Integer_4_Signed_C;
    function Set_Clipboard_Data(
      Format : in Integer_4_Unsigned_C;
      Memory : in Address)
      return Address;
    function Format_Message(
      Flags      : in Integer_4_Unsigned_C;
      Source     : in Address;
      Identifier : in Integer_4_Unsigned_C;
      Language   : in Integer_4_Unsigned_C;
      Buffer     : in Access_Constant_Character_2_C;
      Size       : in Integer_4_Unsigned_C;
      Arguments  : in Address)
      return Integer_4_Unsigned_C;
    function Global_Memory_Status(
      Buffer : Address)
      return Integer_4_Signed_C;
    function Get_System_Default_Language
      return Integer_2_Unsigned_C;
    function Get_Cursor_Position(
      Point : in Address)
      return Integer_4_Signed_C;
    function Bring_Window_To_Top(
      Window : in Address)
      return Integer_4_Signed_C;
    function Set_Window_Position(
      Window       : in Address;
      Insert_After : in Address;
      X            : in Integer_4_Signed_C;
      Y            : in Integer_4_Signed_C;
      Width        : in Integer_4_Signed_C;
      Height       : in Integer_4_Signed_C;
      Flags        : in Integer_4_Unsigned_C)
      return Integer_4_Signed_C;
    procedure Post_Quit_Message(
      Exit_Code : in Integer_4_Signed_C);
    function Monitor_From_Window(
      Window : in Address;
      Flags  : in Integer_4_Unsigned_C)
      return Address;
    function Send_Message(
      Window        : in Address;
      Message       : in Integer_4_Unsigned_C;
      Data_Unsigned : in Integer_4_Unsigned_C;
      Data_Signed   : in Integer_4_Signed_C)
      return Integer_4_Signed_C;
    function Get_Desktop_Window
      return Address; 
    function Get_Current_Process
      return Address;
    function Get_Current_Instance
      return Address;
    function Get_Process_Affinity_Mask(
      Process               : in Address;
      Process_Affinity_Mask : in Address;
      System_Affinity_Mask  : in Address)
      return Integer_4_Signed_C;
    function Query_Performance_Counter(
      Performance_Count : in Address) 
      return Integer_4_Signed_C;
    function Query_Performance_Frequency(
      Frequency : in Address) 
      return Integer_4_Signed_C;
    function Get_Last_Error
      return Integer_4_Unsigned_C;
    function Create_Mutex(
      Attributes    : in Address;
      Initial_Owner : in Integer_4_Signed_C;
      Name          : in String_2_C)
      return Address;
    function Release_Mutex(
      Mutex : in Address)
      return Integer_4_Signed_C;
    function Get_Module_Handle(
      Module_Name : Access_Constant_Character_2_C)
      return Address;
    function Set_Cursor_Position(
      X : in Integer_4_Signed_C;
      Y : in Integer_4_Signed_C)
      return Integer_4_Signed_C;
    function Show_Cursor(
      Do_Show : in Integer_4_Signed_C)
      return Integer_4_Signed_C;
    function Load_Cursor(
      Instance    : in Address;
      Cursor_Name : in Address)
      return Address;
    function Load_Icon(
      Instance  : in Address;
      Icon_Name : in Address)
      return Address;
    function Load_Image(
      Instance  : in Address;
      Name      : in Access_Constant_Character_2_C;
      Kind      : in Integer_4_Unsigned_C;
      Desired_X : in Integer_4_Signed_C;
      Desired_Y : in Integer_4_Signed_C;
      Load      : in Integer_4_Unsigned_C)
      return Address;
    function Message_Box(
      Window  : in Address;
      Text    : in String_2_C;
      Caption : in String_2_C;
      Kind    : in Integer_4_Unsigned_C)
      return Integer_4_Signed_C;
    function Get_Current_Thread_Id
      return Integer_4_Unsigned_C;
    function Unhook_Windows_Hook(
      Hook : in Address)
      return Integer_4_Signed_C;
    function Call_Next_Hook(
      Hook          : in Address;
      Code          : in Integer_4_Signed_C;
      Data_Signed   : in Integer_4_Signed_C;
      Data_Unsigned : in Integer_4_Unsigned_C)
      return Integer_4_Signed_C;
    function Set_Windows_Hook(
      Hook      : in Integer_4_Signed_C;
      Callback  : in Address;
      Modifier  : in Address;
      Thread_Id : in Integer_4_Unsigned_C)
      return Address;
    function Set_Focus(
      Window : in Address)
      return Address;
    function Set_Foreground_Window(
      Window : in Address)
      return Integer_4_Signed_C;
    function Set_Active_Window(
      Window : in Address)
      return Address;
    function Set_Window_Text(
      Window : in Address;
      Text   : in Access_Constant_Character_2_C)
      return Integer_4_Signed_C;
    function Register_Class(
      Window_Class : in Address)
      return Integer_2_Unsigned_C;
    function Unregister_Class(
      Class_Name   : in String_2_C;
      Window_Class : in Address)
      return Integer_4_Signed_C;
    function Define_Window_Procedure(
      Window        : in Address;
      Message       : in Integer_4_Unsigned_C;
      Data_Unsigned : in Integer_4_Unsigned_C;
      Data_Signed   : in Integer_4_Signed_C)
      return Integer_4_Signed_C;
    function Create_Window(
      Style_Extra : in Integer_4_Unsigned_C;
      Class_Name  : in String_2_C;
      Window_Name : in String_2_C;
      Style       : in Integer_4_Unsigned_C;
      X           : in Integer_4_Signed_C;
      Y           : in Integer_4_Signed_C;
      Width       : in Integer_4_Signed_C;
      Height      : in Integer_4_Signed_C;
      Parent      : in Address;
      Menu        : in Address;
      Instance    : in Address;
      Parameter   : in Address)
      return Address;
    function Show_Window(
      Window  : in Address;
      Command : in Integer_4_Signed_C)
      return Integer_4_Signed_C;
    function Update_Window(
      Window : in Address)
      return Integer_4_Signed_C;
    function Destroy_Window(
      Window : in Address)
      return Integer_4_Signed_C;
    function Find_Window(
      Class_Name  : in String_2_C;
      Window_Name : in Address)
      return Address;
    function Get_Window_Rectangle(
      Window    : in Address;
      Rectangle : in Address)
      return Integer_4_Signed_C;
    function Adjust_Window_Rectangle(
      Rectangle   : in Address;
      Style       : in Integer_4_Unsigned_C;
      Menu        : in Integer_4_Signed_C;
      Extra_Style : in Integer_4_Unsigned_C)
      return Integer_4_Signed_C;
    function Peek_Message(
      Message        : in Address;
      Window         : in Address;
      Filter_Minimum : in Integer_4_Unsigned_C;
      Filter_Maximum : in Integer_4_Unsigned_C;
      Command        : in Integer_4_Unsigned_C)
      return Integer_4_Signed_C;
    function Translate_Message(
      Message : in Address)
      return Integer_4_Signed_C;
    function Dispatch_Message(
      Message : in Address)
      return Integer_4_Signed_C;
    function Get_Message_Time
      return Integer_4_Unsigned_C;
    function Get_Monitor_Information(
      Monitor     : in Address;
      Information : in Address)
      return Integer_4_Signed_C;
    function Get_Device_Context(
      Window : in Address)
      return Address;
    function Release_Device_Context(
      Window         : in Address;
      Device_Context : in Address)
      return Integer_4_Signed_C;
    function Get_Device_Capabilities(
      Device_Context : in Address;
      Capability     : in Integer_4_Signed_C)
      return Integer_4_Signed_C;
-------
private
-------
  ----------------
  -- Directives --
  ----------------
    pragma Linker_Options("C:\Windows\System32\IPHLPAPI.DLL");
    pragma Linker_Options("-lgdi32");
    pragma Linker_Options("-lhid");
    pragma Linker_Options("-lsetupapi");
    pragma Import(C,       Get_Current_Instance,           "rts_get_hInstance");
    pragma Import(Stdcall, Is_Lower,                       "iswlower");
    pragma Import(Stdcall, Is_Upper,                       "iswupper");
    --pragma Import(Stdcall, Write_File,                     "WriteFile");
    --pragma Import(Stdcall, Convert_String_2_C_To_UTF_8,    "WideCharToMultiByte");
    pragma Import(Stdcall, Sleep,                          "SleepEx");
    --pragma Import(Stdcall, Enumerate_Device_Interfaces,    "SetupDiEnumDeviceInterfaces");
    pragma Import(Stdcall, Registry_Close_Key,             "RegCloseKey");
    pragma Import(Stdcall, Registry_Query_Value,           "RegQueryValueExW");
    pragma Import(Stdcall, Registry_Open_Key,              "RegOpenKeyExW");
    pragma Import(Stdcall, Create_File,                    "CreateFileW");
    pragma Import(Stdcall, Destroy_Device_List,            "SetupDiDestroyDeviceInfoList");
    --pragma Import(Stdcall, Get_Device_Interface_Detail,    "SetupDiGetDeviceInterfaceDetail");
    pragma Import(Stdcall, Get_Device_Registry_Property,   "SetupDiGetDeviceRegistryPropertyW");
    pragma Import(Stdcall, Get_Device_Enumeration,         "SetupDiEnumDeviceInfo");
    pragma Import(Stdcall, Get_Device_Instance_Id,         "SetupDiGetDeviceInstanceIdW");
    --pragma Import(Stdcall, Get_Device_Class_Information,   "SetupDiGetClassDevsW");
    --pragma Import(Stdcall, Get_Device_Identifier,          "HidD_GetHidGuid");
    --pragma Import(Stdcall, Get_Device_Attributes,          "HidD_GetAttributes");
    pragma Import(Stdcall, Set_Device_Output_Data,         "HidD_SetOutputReport");
    pragma Import(Stdcall, Get_Device_Manufacturer,        "HidD_GetManufacturerString");
    pragma Import(Stdcall, Get_Device_Product,             "HidD_GetProductString");
    pragma Import(Stdcall, Get_Device_Description,         "HidD_GetPhysicalDescriptor");
    pragma Import(Stdcall, Get_Device_Usages,              "HidP_GetUsages");
    pragma Import(Stdcall, Get_Device_Button_Capabilities, "HidP_GetButtonCaps");
    pragma Import(Stdcall, Get_Device_Input_Data,          "GetRawInputData");
    pragma Import(Stdcall, Get_Device_List,                "GetRawInputDeviceList");
    --pragma Import(Stdcall, Get_Device_Raw_Information,     "GetRawInputDeviceInfoW");
    pragma Import(Stdcall, Register_Devices,               "RegisterRawInputDevices");
    pragma Import(Stdcall, Enumerate_Display_Monitor,      "EnumDisplayMonitors");
    pragma Import(Stdcall, Get_Clip_Box,                   "GetClipBox");
    pragma Import(Stdcall, Rectangles_Are_Equal,           "EqualRect");
    pragma Import(Stdcall, Find_Intersecting_Rectangle,    "IntersectRect");
    pragma Import(Stdcall, Get_Client_Rectangle,           "GetClientRect");
    pragma Import(Stdcall, Get_Class_Setting,              "GetClassLongW");
    pragma Import(Stdcall, Get_Version,                    "GetVersionExW");
    pragma Import(Stdcall, Get_User_Name,                  "GetUserNameW");
    pragma Import(Stdcall, Create_Cursor,                  "CreateCursor");
    pragma Import(Stdcall, Change_Class_Setting,           "SetClassLongW");
    pragma Import(Stdcall, Flash_Window,                   "FlashWindowEx");
    pragma Import(Stdcall, Get_Clip_Cursor_Area,           "GetClipCursor");
    pragma Import(Stdcall, Clip_Cursor,                    "ClipCursor");
    pragma Import(Stdcall, System_Parameter_Information,   "SystemParametersInfoW");
    pragma Import(Stdcall, Get_Current_Thread_Id,          "GetCurrentThreadId");
    pragma Import(Stdcall, Get_Module_Handle,              "GetModuleHandleW");
    pragma Import(Stdcall, Get_Key_State,                  "GetKeyState");
    pragma Import(Stdcall, Set_Window_Text,                "SetWindowTextW");
    pragma Import(Stdcall, Change_Window_Setting,          "SetWindowLongW");
    pragma Import(Stdcall, Get_Foreground_Window,          "GetForegroundWindow");
    pragma Import(Stdcall, Free_Library,                   "FreeLibrary");
    pragma Import(Stdcall, Load_Library,                   "LoadLibraryW");
    pragma Import(Stdcall, Get_Procedure_Address,          "GetProcAddress");
    pragma Import(Stdcall, Shell_Execute,                  "ShellExecuteW");
    pragma Import(Stdcall, Set_Process_Working_Set_Size,   "SetProcessWorkingSetSize");
    pragma Import(Stdcall, Get_Disk_Free_Space,            "GetDiskFreeSpaceExW");
    pragma Import(Stdcall, Virtual_Lock,                   "VirtualLock");
    pragma Import(Stdcall, Virtual_Unlock,                 "VirtualUnlock");
    pragma Import(Stdcall, Set_Clipboard_Data,             "SetClipboardData");
    pragma Import(Stdcall, Get_Clipboard_Data,             "GetClipboardData");
    pragma Import(Stdcall, Empty_Clipboard,                "EmptyClipboard");
    pragma Import(Stdcall, Open_Clipboard,                 "OpenClipboard");
    pragma Import(Stdcall, Close_Clipboard,                "CloseClipboard");
    pragma Import(Stdcall, Global_Lock,                    "GlobalLock");
    pragma Import(Stdcall, Global_Unlock,                  "GlobalUnlock");
    pragma Import(Stdcall, Global_Free,                    "GlobalFree");
    pragma Import(Stdcall, Global_Allocate,                "GlobalAlloc");
    pragma Import(Stdcall, Format_Message,                 "FormatMessageW");
    pragma Import(Stdcall, Get_System_Default_Language,    "GetSystemDefaultUILanguage");
    pragma Import(Stdcall, Global_Memory_Status,           "GlobalMemoryStatusEx");
    pragma Import(Stdcall, Get_Cursor_Position,            "GetCursorPos");
    pragma Import(Stdcall, Bring_Window_To_Top,            "BringWindowToTop");
    pragma Import(Stdcall, Set_Window_Position,            "SetWindowPos");
    pragma Import(Stdcall, Send_Message,                   "SendMessageW");
    pragma Import(Stdcall, Unregister_Class,               "UnregisterClassW");
    pragma Import(Stdcall, Message_Box,                    "MessageBoxW");
    pragma Import(Stdcall, Get_Current_Process,            "GetCurrentProcess");
    pragma Import(Stdcall, Monitor_From_Window,            "MonitorFromWindow");
    pragma Import(Stdcall, Post_Quit_Message,              "PostQuitMessage"); 
    pragma Import(Stdcall, Get_Desktop_Window,             "GetDesktopWindow"); 
    pragma Import(Stdcall, Get_Process_Affinity_Mask,      "GetProcessAffinityMask");
    pragma Import(Stdcall, Query_Performance_Counter,      "QueryPerformanceCounter");
    pragma Import(Stdcall, Query_Performance_Frequency,    "QueryPerformanceFrequency");
    pragma Import(Stdcall, Get_Last_Error,                 "GetLastError");
    pragma Import(Stdcall, Create_Mutex,                   "CreateMutexW");
    pragma Import(Stdcall, Release_Mutex,                  "ReleaseMutex");
    pragma Import(Stdcall, Set_Cursor_Position,            "SetCursorPos");
    pragma Import(Stdcall, Show_Cursor,                    "ShowCursor");
    pragma Import(Stdcall, Load_Cursor,                    "LoadCursorW");
    pragma Import(Stdcall, Load_Icon,                      "LoadIconW");
    pragma Import(Stdcall, Load_Image,                     "LoadImageW");
    pragma Import(Stdcall, Call_Next_Hook,                 "CallNextHookEx");
    pragma Import(Stdcall, Unhook_Windows_Hook,            "UnhookWindowsHookEx");
    pragma Import(Stdcall, Set_Windows_Hook,               "SetWindowsHookExW");
    pragma Import(Stdcall, Set_Focus,                      "SetFocus");
    pragma Import(Stdcall, Set_Foreground_Window,          "SetForegroundWindow");
    pragma Import(Stdcall, Set_Active_Window,              "SetActiveWindow");
    pragma Import(Stdcall, Register_Class,                 "RegisterClassExW");
    pragma Import(Stdcall, Define_Window_Procedure,        "DefWindowProcW");
    pragma Import(Stdcall, Create_Window,                  "CreateWindowExW");
    pragma Import(Stdcall, Show_Window,                    "ShowWindow");
    pragma Import(Stdcall, Update_Window,                  "UpdateWindow");
    pragma Import(Stdcall, Destroy_Window,                 "DestroyWindow");
    pragma Import(Stdcall, Find_Window,                    "FindWindowW");
    pragma Import(Stdcall, Get_Window_Rectangle,           "GetWindowRect");
    pragma Import(Stdcall, Adjust_Window_Rectangle,        "AdjustWindowRectEx");
    pragma Import(Stdcall, Peek_Message,                   "PeekMessageW");
    pragma Import(Stdcall, Translate_Message,              "TranslateMessage");
    pragma Import(Stdcall, Dispatch_Message,               "DispatchMessageW");
    pragma Import(Stdcall, Get_Message_Time,               "GetMessageTime");
    pragma Import(Stdcall, Get_Monitor_Information,        "GetMonitorInfoW");
    pragma Import(Stdcall, Get_Device_Context,             "GetDC");
    pragma Import(Stdcall, Release_Device_Context,         "ReleaseDC");
    pragma Import(Stdcall, Get_Device_Capabilities,        "GetDeviceCaps");
  end Neo.Windows;
