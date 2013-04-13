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
WITH
  System,
  Interfaces,
  Interfaces.C,
  Ada.Command_Line,
  Ada.Unchecked_Conversion,
  Neo.Foundation.Data_Types;
USE
  System,
  Interfaces,
  Interfaces.C,
  Ada.Command_Line,
  Neo.Foundation.Data_Types;
PACKAGE Neo.Windows
  IS
  ---------------
  -- Constants --
  ---------------
    GENERIC_CURSOR                             : CONSTANT Address := To_Unchecked_Address(Integer_4_Unsigned_C(16#0000_7F00#));
    GENERIC_ICON                               : CONSTANT Address := To_Unchecked_Address(Integer_4_Unsigned_C(16#0000_7F00#));
    BRUSH_GRAY                                 : CONSTANT Address := To_Unchecked_Address(Integer_4_Unsigned_C(16#0000_0011#));
    ERROR_INSUFFICIENT_BUFFER                  : CONSTANT Integer_4_Unsigned_C := 16#0_0#; -- ???
    STOP_READING_TOP_LEVEL_DEVICES             : CONSTANT Integer_4_Unsigned_C := 16#0000_0001#;
    TAKE_INPUT_ON_NON_ACTIVE                   : CONSTANT Integer_4_Unsigned_C := 16#0000_0100#;
    CODE_PAGE_UTF_8                            : CONSTANT Integer_4_Unsigned_C := 16#0000_FDE9#;
    KEY_READ                                   : CONSTANT Integer_4_Unsigned_C := 16#0002_0019#;
    HKEY_LOCAL_MACHINE                         : CONSTANT Integer_4_Unsigned_C := 16#8000_0002#;
    GENERIC_READ                               : CONSTANT Integer_4_Unsigned_C := 16#8000_0000#;
    GENERIC_WRITE                              : CONSTANT Integer_4_Unsigned_C := 16#4000_0000#;
    FILE_SHARE_READ                            : CONSTANT Integer_4_Unsigned_C := 16#0000_0001#;
    FILE_SHARE_WRITE                           : CONSTANT Integer_4_Unsigned_C := 16#0000_0002#;
    OPEN_EXISTING                              : CONSTANT Integer_4_Unsigned_C := 16#0000_0003#;
    ADD_LIST_OF_INSTALLED_DEVICES              : CONSTANT Integer_4_Unsigned_C := 16#0000_0004#;
    RESTRICT_DEVICES_TO_CURRENT_HARDWARE       : CONSTANT Integer_4_Unsigned_C := 16#0000_0002#;
    PROPERTY_FOR_DEVICE_DESCRIPTION            : CONSTANT Integer_4_Unsigned_C := 16#0000_0000#;
    PROPERTY_FOR_DEVICE_NAME                   : CONSTANT Integer_4_Unsigned_C := 16#0000_000C#;
    GET_DEVICE_NAME                            : CONSTANT Integer_4_Unsigned_C := 16#2000_0007#;
    GET_DEVICE_HEADER                          : CONSTANT Integer_4_Unsigned_C := 16#1000_0005#;
    GET_DEVICE_DATA                            : CONSTANT Integer_4_Unsigned_C := 16#1000_0003#;
    GET_DEVICE_PREPARSED_DATA                  : CONSTANT Integer_4_Unsigned_C := 16#2000_0005#;
    FORMAT_MESSAGE_ALLOCATE_BUFFER             : CONSTANT Integer_4_Unsigned_C := 16#0000_0100#;
    FORMAT_MESSAGE_ARGUMENT_ARRAY              : CONSTANT Integer_4_Unsigned_C := 16#0000_2000#;
    FORMAT_MESSAGE_FROM_HMODULE                : CONSTANT Integer_4_Unsigned_C := 16#0000_0800#;
    FORMAT_MESSAGE_FROM_STRING                 : CONSTANT Integer_4_Unsigned_C := 16#0000_0400#;
    FORMAT_MESSAGE_FROM_SYSTEM                 : CONSTANT Integer_4_Unsigned_C := 16#0000_1000#;
    FORMAT_MESSAGE_IGNORE_INSERTS              : CONSTANT Integer_4_Unsigned_C := 16#0000_0200#;
    FORMAT_MESSAGE_MAX_WIDTH_MASK              : CONSTANT Integer_4_Unsigned_C := 16#0000_00FF#;
    EVENT_CLOSE                                : CONSTANT Integer_4_Unsigned_C := 16#0000_0010#;
    EVENT_KEY_UP                               : CONSTANT Integer_4_Unsigned_C := 16#0000_0101#;
    EVENT_SIZING                               : CONSTANT Integer_4_Unsigned_C := 16#0000_0214#;
    EVENT_COMMAND                              : CONSTANT Integer_4_Unsigned_C := 16#0000_0112#;
    EVENT_KEY_DOWN                             : CONSTANT Integer_4_Unsigned_C := 16#0000_0100#;
    EVENT_CHARACTER                            : CONSTANT Integer_4_Unsigned_C := 16#0000_0102#;
    EVENT_SIZE_CHANGED                         : CONSTANT Integer_4_Unsigned_C := 16#0000_0005#;
    EVENT_SYSTEM_KEY_DOWN                      : CONSTANT Integer_4_Unsigned_C := 16#0000_0104#;
    EVENT_SYSTEM_KEY_UP                        : CONSTANT Integer_4_Unsigned_C := 16#0000_0105#;
    EVENT_ACTIVATION_CHANGE                    : CONSTANT Integer_4_Unsigned_C := 16#0000_0006#;
    EVENT_INPUT                                : CONSTANT Integer_4_Unsigned_C := 16#0000_00FF#;
    EVENT_MOUSE_MIDDLE_DOWN                    : CONSTANT Integer_4_Unsigned_C := 16#0000_0207#;
    EVENT_MOUSE_RIGHT_DOWN                     : CONSTANT Integer_4_Unsigned_C := 16#0000_0204#;
    EVENT_MOUSE_EXTRA_DOWN                     : CONSTANT Integer_4_Unsigned_C := 16#0000_020B#;
    EVENT_MOUSE_LEFT_DOWN                      : CONSTANT Integer_4_Unsigned_C := 16#0000_0201#;
    EVENT_MOUSE_MIDDLE_UP                      : CONSTANT Integer_4_Unsigned_C := 16#0000_0208#;
    EVENT_MOUSE_RIGHT_UP                       : CONSTANT Integer_4_Unsigned_C := 16#0000_0205#;
    EVENT_MOUSE_EXTRA_UP                       : CONSTANT Integer_4_Unsigned_C := 16#0000_020C#;
    EVENT_MOUSE_LEFT_UP                        : CONSTANT Integer_4_Unsigned_C := 16#0000_0202#;
    EVENT_MOUSE_WHEEL_VERTICAL                 : CONSTANT Integer_4_Unsigned_C := 16#0000_020A#;
    EVENT_MOUSE_WHEEL_HORIZONTAL               : CONSTANT Integer_4_Unsigned_C := 16#0000_020E#;
    EVENT_MOUSE_MOVE                           : CONSTANT Integer_4_Unsigned_C := 16#0000_0200#;
    EVENT_MOVE                                 : CONSTANT Integer_4_Unsigned_C := 16#0000_0003#;
    EVENT_CREATE                               : CONSTANT Integer_4_Unsigned_C := 16#0000_0001#;
    EVENT_INPUT_FOCUS_GAINED                   : CONSTANT Integer_4_Unsigned_C := 16#0000_0007#;
    EVENT_INPUT_FOCUS_LOST                     : CONSTANT Integer_4_Unsigned_C := 16#0000_0008#;
    EVENT_DEVICE_CHANGE                        : CONSTANT Integer_4_Unsigned_C := 16#0000_00FE#;
    STYLE_EXTRA_ALWAYS_ON_TOP                  : CONSTANT Integer_4_Unsigned_C := 16#0000_0008#;
    STYLE_EXTRA_NOTHING                        : CONSTANT Integer_4_Unsigned_C := 16#0000_0000#; 
    STYLE_NOTHING                              : CONSTANT Integer_4_Unsigned_C := 16#0000_0000#; 
    STYLE_TITLEBAR                             : CONSTANT Integer_4_Unsigned_C := 16#00C0_0000#;
    STYLE_TITLEBAR_MENU                        : CONSTANT Integer_4_Unsigned_C := 16#0008_0000#;
    STYLE_TITLEBARLESS_AND_BORDERLESS          : CONSTANT Integer_4_Unsigned_C := 16#8000_0000#;
    STYLE_VISIBLE_INITIALLY                    : CONSTANT Integer_4_Unsigned_C := 16#1000_0000#;
    STYLE_BORDER_THIN_LINE                     : CONSTANT Integer_4_Unsigned_C := 16#0080_0000#;
    STYLE_BORDER_SIZABLE                       : CONSTANT Integer_4_Unsigned_C := 16#0004_0000#;
    STYLE_BOX_FULLSCREEN                       : CONSTANT Integer_4_Unsigned_C := 16#0001_0000#;
    STYLE_BOX_ICONIZE                          : CONSTANT Integer_4_Unsigned_C := 16#0002_0000#;
    STYLE_NO_ACTIVATE                          : CONSTANT Integer_4_Unsigned_C := 16#0800_0000#;
    ICON_INFORMATION                           : CONSTANT Integer_4_Unsigned_C := 16#0000_0040#;
    ICON_WARNING                               : CONSTANT Integer_4_Unsigned_C := 16#0000_0030#;
    ICON_ERROR                                 : CONSTANT Integer_4_Unsigned_C := 16#0000_0010#;
    ICON_CUSTOM                                : CONSTANT Integer_4_Unsigned_C := 16#0000_0080#;
    LOAD_FROM_FILE                             : CONSTANT Integer_4_Unsigned_C := 16#0000_0010#;
    LOAD_ICO                                   : CONSTANT Integer_4_Unsigned_C := 16#0000_0001#;
    LOAD_CUR                                   : CONSTANT Integer_4_Unsigned_C := 16#0000_0002#;
    IGNORE_MESSAGE_FILTER_MINIMUM              : CONSTANT Integer_4_Unsigned_C := 16#0000_0000#;
    IGNORE_MESSAGE_FILTER_MAXIMUM              : CONSTANT Integer_4_Unsigned_C := 16#0000_0000#;
    BUTTON_OKAY                                : CONSTANT Integer_4_Unsigned_C := 16#0000_0000#;
    BUTTONS_YES_NO                             : CONSTANT Integer_4_Unsigned_C := 16#0000_0004#;
    BUTTONS_CANCEL_OKAY                        : CONSTANT Integer_4_Unsigned_C := 16#0000_0001#;
    BUTTONS_CANCEL_RETRY                       : CONSTANT Integer_4_Unsigned_C := 16#0000_0005#;
    MEMORY_MOVEABLE                            : CONSTANT Integer_4_Unsigned_C := 16#0000_0002#;
    MEMORY_DYNAMIC_DATA_EXCHANGE_SHARE         : CONSTANT Integer_4_Unsigned_C := 16#0000_2000#;
    MESSAGE_QUIT                               : CONSTANT Integer_4_Unsigned_C := 16#0000_0012#;
    CLIPBOARD_UNICODE_TEXT                     : CONSTANT Integer_4_Unsigned_C := 16#0000_000D#;
    REMOVE_MESSAGES_AFTER_PROCESSING           : CONSTANT Integer_4_Unsigned_C := 16#0000_0001#;
    NO_ERROR                                   : CONSTANT Integer_4_Unsigned_C := 16#0000_0000#;
    DEFAULT_TO_NEAREST_MONITOR                 : CONSTANT Integer_4_Unsigned_C := 16#0000_0002#;
    ATTRIBUTE_SUB_DIRECTORY                    : CONSTANT Integer_4_Unsigned_C := 16#0000_0010#;
    GET_MOUSE                                  : CONSTANT Integer_4_Unsigned_C := 16#0000_0003#;
    SET_MOUSE                                  : CONSTANT Integer_4_Unsigned_C := 16#0000_0004#;
    SEND_CHANGE                                : CONSTANT Integer_4_Unsigned_C := 16#0000_0002#;
    KIND_IS_MOUSE                              : CONSTANT Integer_4_Unsigned_C := 16#0000_0000#;
    KIND_IS_KEYBOARD                           : CONSTANT Integer_4_Unsigned_C := 16#0000_0001#;
    KIND_IS_HUMAN_INTERFACE_DEVICE             : CONSTANT Integer_4_Unsigned_C := 16#0000_0002#;
    KIND_HUMAN_INTERFACE_DEVICE_USAGE          : CONSTANT Integer_4_Unsigned_C := 16#0000_0000#;
    SUCCESSFUL_HUMAN_INTEFACE_DEVICE_OPERATION : CONSTANT Integer_4_Unsigned_C := 16#0000_0000#;
    SUBEVENT_MOUSE_BUTTON_LEFT_DOWN            : CONSTANT Integer_4_Unsigned_C := 16#0000_0001#;
    SUBEVENT_MOUSE_BUTTON_LEFT_UP              : CONSTANT Integer_4_Unsigned_C := 16#0000_0002#;
    SUBEVENT_MOUSE_BUTTON_RIGHT_DOWN           : CONSTANT Integer_4_Unsigned_C := 16#0000_0004#;
    SUBEVENT_MOUSE_BUTTON_RIGHT_UP             : CONSTANT Integer_4_Unsigned_C := 16#0000_0008#;
    SUBEVENT_MOUSE_BUTTON_MIDDLE_DOWN          : CONSTANT Integer_4_Unsigned_C := 16#0000_0020#;
    SUBEVENT_MOUSE_BUTTON_MIDDLE_UP            : CONSTANT Integer_4_Unsigned_C := 16#0000_0010#;
    SUBEVENT_MOUSE_BUTTON_MIDDLE_HORIZONTAL    : CONSTANT Integer_4_Unsigned_C := 16#0000_0800#;
    SUBEVENT_MOUSE_BUTTON_MIDDLE_VERTICAL      : CONSTANT Integer_4_Unsigned_C := 16#0000_0400#;
    SUBEVENT_MOUSE_BUTTON_EXTRA_1_DOWN         : CONSTANT Integer_4_Unsigned_C := 16#0000_0040#;
    SUBEVENT_MOUSE_BUTTON_EXTRA_1_UP           : CONSTANT Integer_4_Unsigned_C := 16#0000_0080#;
    SUBEVENT_MOUSE_BUTTON_EXTRA_2_DOWN         : CONSTANT Integer_4_Unsigned_C := 16#0000_0100#;
    SUBEVENT_MOUSE_BUTTON_EXTRA_2_UP           : CONSTANT Integer_4_Unsigned_C := 16#0000_0200#;
    SUBEVENT_ACTIVATED_BY_CLICK                : CONSTANT Integer_4_Unsigned_C := 16#0000_0002#;
    SUBEVENT_KEY_COMBINATION_ALT_ENTER         : CONSTANT Integer_4_Unsigned_C := 16#0000_000D#;
    SUBEVENT_RESIZE_BOTTOM_RIGHT               : CONSTANT Integer_4_Unsigned_C := 16#0000_0008#;
    SUBEVENT_RESIZE_BOTTOM_LEFT                : CONSTANT Integer_4_Unsigned_C := 16#0000_0007#;
    SUBEVENT_RESIZE_TOP_RIGHT                  : CONSTANT Integer_4_Unsigned_C := 16#0000_0005#;
    SUBEVENT_RESIZE_TOP_LEFT                   : CONSTANT Integer_4_Unsigned_C := 16#0000_0004#;
    SUBEVENT_RESIZE_BOTTOM                     : CONSTANT Integer_4_Unsigned_C := 16#0000_0006#;
    SUBEVENT_RESIZE_RIGHT                      : CONSTANT Integer_4_Unsigned_C := 16#0000_0002#;
    SUBEVENT_RESIZE_LEFT                       : CONSTANT Integer_4_Unsigned_C := 16#0000_0001#;
    SUBEVENT_RESIZE_TOP                        : CONSTANT Integer_4_Unsigned_C := 16#0000_0003#;
    SUBEVENT_RESIZE_SNAPBACK                   : CONSTANT Integer_4_Unsigned_C := 16#0000_0009#;
    SUBEVENT_CLICK_ACTIVATION                  : CONSTANT Integer_4_Unsigned_C := 16#0000_0002#;
    SUBEVENT_FULLSCREENED                      : CONSTANT Integer_4_Unsigned_C := 16#0000_0002#;
    SUBEVENT_RESTORED                          : CONSTANT Integer_4_Unsigned_C := 16#0000_0000#;
    SUBEVENT_ICONIZED                          : CONSTANT Integer_4_Unsigned_C := 16#0000_0001#;
    SUBEVENT_KEY_IS_LEFT_SIDED                 : CONSTANT Integer_2_Unsigned_C := 16#0002#;
    SUBEVENT_KEY_IS_RIGHT_SIDED                : CONSTANT Integer_2_Unsigned_C := 16#0004#;
    VIRTUAL_KEY_LEFT_MOUSE                     : CONSTANT Integer_2_Unsigned_C := 16#0001#;
    VIRTUAL_KEY_CLEAR                          : CONSTANT Integer_2_Unsigned_C := 16#00FE#;
    SUBEVENT_MENU_POPOUT                       : CONSTANT Integer_4_Unsigned_C := 16#0000_F100#;
    SUBEVENT_SCREEN_SAVER_START                : CONSTANT Integer_4_Unsigned_C := 16#0000_F140#;
    SUBEVENT_WORD_LOW                          : CONSTANT Integer_4_Unsigned_C := 16#0000_FFFF#;
    SUBEVENT_WORD_HIGH                         : CONSTANT Integer_4_Unsigned_C := 16#FFFF_0000#;
    SUBEVENT_SHORT_LOW                         : CONSTANT Integer_2_Unsigned_C := 16#00FF#;
    KEY_IS_DOWN                                : CONSTANT Integer_2_Unsigned_C := 16#0000#;
    KEY_MAKE_CODE_FOR_LEFT                     : CONSTANT Integer_2_Unsigned_C := 16#002A#;
    KEY_MAKE_CODE_FOR_RIGHT                    : CONSTANT Integer_2_Unsigned_C := 16#0036#;
    SHORT_HIGH_BIT                             : CONSTANT Integer_2_Unsigned_C := 16#C000#;
    END_OF_FILE_ON_WINDOWS                     : CONSTANT Integer_2_Unsigned_C := 16#FFFF#;
    USE_RAW_MOUSE                              : CONSTANT Integer_2_Unsigned_C := 16#0002#;
    USE_RAW_POINTER                            : CONSTANT Integer_2_Unsigned_C := 16#0001#;
    USE_RAW_KEYBOARD                           : CONSTANT Integer_2_Unsigned_C := 16#0006#;
    USE_RAW_JOYSTICK                           : CONSTANT Integer_2_Unsigned_C := 16#0004#;
    USE_RAW_GAME_PAD                           : CONSTANT Integer_2_Unsigned_C := 16#0005#;
    USE_RAW_KEYPAD                             : CONSTANT Integer_2_Unsigned_C := 16#0004#;
    GENERIC_DESKTOP_CONTROL                    : CONSTANT Integer_2_Unsigned_C := 16#0001#;
    LANGUAGE_ARABIC                            : CONSTANT Integer_2_Unsigned_C := 16#0401#;
    LANGUAGE_BASQUE                            : CONSTANT Integer_2_Unsigned_C := 16#042D#;
    LANGUAGE_CATALAN                           : CONSTANT Integer_2_Unsigned_C := 16#0403#;
    LANGUAGE_CHINESE_SIMPLIFIED                : CONSTANT Integer_2_Unsigned_C := 16#0804#;
    LANGUAGE_CHINESE_TRADITIONAL               : CONSTANT Integer_2_Unsigned_C := 16#0404#;
    LANGUAGE_CZECH                             : CONSTANT Integer_2_Unsigned_C := 16#0405#;
    LANGUAGE_DANISH                            : CONSTANT Integer_2_Unsigned_C := 16#0406#;
    LANGUAGE_DUTCH                             : CONSTANT Integer_2_Unsigned_C := 16#0413#;
    LANGUAGE_ENGLISH                           : CONSTANT Integer_2_Unsigned_C := 16#0409#;
    LANGUAGE_FINNISH                           : CONSTANT Integer_2_Unsigned_C := 16#040B#;
    LANGUAGE_FRENCH                            : CONSTANT Integer_2_Unsigned_C := 16#040C#;
    LANGUAGE_GERMAN                            : CONSTANT Integer_2_Unsigned_C := 16#0407#;
    LANGUAGE_GREEK                             : CONSTANT Integer_2_Unsigned_C := 16#0408#;
    LANGUAGE_HEBREW                            : CONSTANT Integer_2_Unsigned_C := 16#040D#;
    LANGUAGE_HUNGARIAN                         : CONSTANT Integer_2_Unsigned_C := 16#040E#;
    LANGUAGE_ITALIAN                           : CONSTANT Integer_2_Unsigned_C := 16#0410#;
    LANGUAGE_JAPANESE                          : CONSTANT Integer_2_Unsigned_C := 16#0411#;
    LANGUAGE_KOREAN                            : CONSTANT Integer_2_Unsigned_C := 16#0412#;
    LANGUAGE_NORWEGIAN                         : CONSTANT Integer_2_Unsigned_C := 16#0414#;
    LANGUAGE_POLISH                            : CONSTANT Integer_2_Unsigned_C := 16#0415#;
    LANGUAGE_PORTUGUESE                        : CONSTANT Integer_2_Unsigned_C := 16#0816#;
    LANGUAGE_PORTUGUESE_BRAZIL                 : CONSTANT Integer_2_Unsigned_C := 16#0416#;
    LANGUAGE_RUSSIAN                           : CONSTANT Integer_2_Unsigned_C := 16#0419#;
    LANGUAGE_SLOVAKIAN                         : CONSTANT Integer_2_Unsigned_C := 16#041B#;
    LANGUAGE_SLOVENIAN                         : CONSTANT Integer_2_Unsigned_C := 16#0424#;
    LANGUAGE_SPANISH                           : CONSTANT Integer_2_Unsigned_C := 16#0C0A#;
    LANGUAGE_SWEDISH                           : CONSTANT Integer_2_Unsigned_C := 16#041D#;
    LANGUAGE_TURKISH                           : CONSTANT Integer_2_Unsigned_C := 16#041F#;
    LANGUAGE_NEUTRAL                           : CONSTANT Integer_1_Unsigned_C := 16#00#;
    SUBLANGUAGE_DEFAULT                        : CONSTANT Integer_1_Unsigned_C := 16#01#;
    GET_CLASS_CURSOR                           : CONSTANT Integer_4_Signed_C   := -12;
    SET_WINDOW_STYLE                           : CONSTANT Integer_4_Signed_C   := -16;
    SET_WINDOW_STYLE_EXTRA                     : CONSTANT Integer_4_Signed_C   := -20;
    SET_CLASS_CURSOR                           : CONSTANT Integer_4_Signed_C   := -12;
    HOOK_LOW_LEVEL_KEYBOARD                    : CONSTANT Integer_4_Signed_C   := 13;
    HOOK_LOW_LEVEL_MOUSE                       : CONSTANT Integer_4_Signed_C   := 14;
    REGION_NULL                                : CONSTANT Integer_4_Signed_C   := 0;
    REGION_COMPLEX                             : CONSTANT Integer_4_Signed_C   := 2;
    REGION_SIMPLE                              : CONSTANT Integer_4_Signed_C   := 1;
    PRESSED_OKAY                               : CONSTANT Integer_4_Signed_C   := 1;
    PRESSED_RETRY                              : CONSTANT Integer_4_Signed_C   := 4;
    PRESSED_YES                                : CONSTANT Integer_4_Signed_C   := 6;
    FAILED                                     : CONSTANT Integer_4_Signed_C   := 0;
    AN_ACTION_OCCURED                          : CONSTANT Integer_4_Signed_C   := 0;
    MAKE_WINDOW_FULLSCREEN                     : CONSTANT Integer_4_Signed_C   := 3;
    MAKE_WINDOW_HIDE                           : CONSTANT Integer_4_Signed_C   := 0;
    MAKE_WINDOW_RESTORE                        : CONSTANT Integer_4_Signed_C   := 9;
    MAKE_WINDOW_NORMALIZE                      : CONSTANT Integer_4_Signed_C   := 1;
    MAKE_WINDOW_GO_TO_ICONIC                   : CONSTANT Integer_4_Signed_C   := 2;
    DATA_HORZONTAL_RESOLUTION                  : CONSTANT Integer_4_Signed_C   := 8;
    DATA_NUMBER_OF_BITS_PER_PIXEL              : CONSTANT Integer_4_Signed_C   := 12;
    DATA_VERTICAL_RESOLUTION                   : CONSTANT Integer_4_Signed_C   := 10;
    MILLISECOND_TIMEOUT_FORCE_WRITE            : CONSTANT Integer_4_Signed_C   := 500;
    MOUSE_WHEEL_DELTA                          : CONSTANT Integer_2_Signed     := 120;
    MAXIMUM_PATH_FOR_CREATE_FILE               : CONSTANT Integer_4_Signed     := 32_767;
  -------------
  -- Arrays --
  ------------
    TYPE Reserved_Capabilities
      IS ARRAY(1..17)
      OF Integer_2_Unsigned_C;
    TYPE Reserved_Button_Capabilities
      IS ARRAY(1..10)
      OF Integer_4_Unsigned_C;
    TYPE Reserved_Shit
      IS ARRAY(1..5)
      OF Integer_2_Unsigned_C;
  -------------
  -- Records --
  -------------
-- #ifndef _MSC_VER
-- typedef BOOL (WINAPI *LPFN_ISWOW64PROCESS) (HANDLE, PBOOL);
-- #endif

-- typedef void* (CALLBACK *KbdLayerDescriptor) (VOID);

-- #define CAPLOK          0x01
-- #define WCH_NONE        0xF000
-- #define WCH_DEAD        0xF001

-- typedef struct _VK_TO_WCHARS {
--         BYTE VirtualKey;
--         BYTE Attributes;
--         WCHAR wch[];
-- } VK_TO_WCHARS, *PVK_TO_WCHARS;

-- typedef struct _LIGATURE {
--         BYTE VirtualKey;
--         WORD ModificationNumber;
--         WCHAR wch[];
-- } LIGATURE, *PLIGATURE;

-- typedef struct _VK_TO_BIT {
--         BYTE Vk;
--         BYTE ModBits;
-- } VK_TO_BIT, *PVK_TO_BIT;

-- typedef struct _MODIFIERS {
--         PVK_TO_BIT pVkToBit; // __ptr64
--         WORD wMaxModBits;
--         BYTE ModNumber[];
-- } MODIFIERS, *PMODIFIERS;

-- typedef struct _VSC_VK {
--         BYTE Vsc;
--         USHORT Vk;
-- } VSC_VK, *PVSC_VK;

-- typedef struct _VK_TO_WCHAR_TABLE {
--         //PVK_TO_WCHARS1 pVkToWchars; // __ptr64
--         PVK_TO_WCHARS pVkToWchars; // __ptr64
--         BYTE nModifications;
--         BYTE cbSize;
-- } VK_TO_WCHAR_TABLE, *PVK_TO_WCHAR_TABLE;

-- typedef struct _DEADKEY {
--         DWORD dwBoth;
--         WCHAR wchComposed;
--         USHORT uFlags;
-- } DEADKEY, *PDEADKEY;

-- typedef struct _VSC_LPWSTR {
--         BYTE vsc;
--         WCHAR *pwsz; // __ptr64
-- } VSC_LPWSTR, *PVSC_LPWSTR;

-- typedef struct tagKbdLayer {
--         PMODIFIERS pCharModifiers; // __ptr64
--         PVK_TO_WCHAR_TABLE pVkToWcharTable; // __ptr64
--         PDEADKEY pDeadKey; // __ptr64
--         PVSC_LPWSTR pKeyNames; // __ptr64
--         PVSC_LPWSTR pKeyNamesExt; // __ptr64
--         WCHAR **pKeyNamesDead; // __ptr64
--         USHORT *pusVSCtoVK; // __ptr64
--         BYTE bMaxVSCtoVK;
--         PVSC_VK pVSCtoVK_E0; // __ptr64
--         PVSC_VK pVSCtoVK_E1; // __ptr64
--         DWORD fLocaleFlags;
--         BYTE nLgMax;
--         BYTE cbLgEntry;
--         //PLIGATURE1 pLigature; // __ptr64
--         PLIGATURE pLigature; // __ptr64
--         DWORD dwType;
--         DWORD dwSubType;
-- } KBDTABLES, *PKBDTABLES; // __ptr64
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
    -- TYPE Record_Overlapped_
    --   IS RECORD
    --     Internal : ;
    --     Internal_High : ;
    --     Event  : Address;
    --   END RECORD;
    -- TYPE Record_Device_Attributes
    --   IS RECORD
    --     Size    : Integer_4_Unsigned_C := Record_Device_Attributes'Size / 8;
    --     Vendor  : Integer_2_Unsigned_C := 0;
    --     Product : Integer_2_Unsigned_C := 0;
    --     Version : Integer_2_Unsigned_C := 0;
    --   END RECORD;
    TYPE Record_Device_Header
      IS RECORD
        Kind        : Integer_4_Unsigned_C := 0;
        Size        : Integer_4_Unsigned_C := 0;
        Device      : Address              := NULL_ADDRESS;
        Data_Signed : Integer_4_Signed_C   := 0;
      END RECORD;
      PRAGMA Convention(C, Record_Device_Header);
    -- TYPE Record_Device_Capabilities
    --   IS RECORD
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
    --   END RECORD;
    --   PRAGMA Convention(C, Record_Device_Capabilities);
    -- TYPE Record_Device_Button_Capabilities
    --   IS RECORD
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
    --   END RECORD;
    --   PRAGMA Convention(C, Record_Device_Button_Capabilities);
    -- TYPE Record_Device_Capability_Values
    --   IS RECORD
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
    --   END RECORD;
    --   PRAGMA Convention(C, Record_Device_Capability_Values);
    TYPE Record_Mouse
      IS RECORD
        Flags             : Integer_2_Unsigned_C := 0;
        Button_Flags      : Integer_4_Unsigned_C := 0;
        Buttons           : Integer_4_Unsigned_C := 0;
        Last_X            : Integer_4_Signed_C   := 0;
        Last_Y            : Integer_4_Signed_C   := 0;
        Extra_Information : Integer_4_Unsigned_C := 0;
      END RECORD;
      PRAGMA Convention(C, Record_Mouse);
    -- TYPE Record_Mouse
    --   IS RECORD
    --     Point       : Record_Point;
    --     Data        : Integer_4_Unsigned_C;
    --     Flags       : Integer_4_Unsigned_C;
    --     Time        : Integer_4_Unsigned_C;
    --     Information : Address; -- Changes on 64/32 bit systems
    --   END RECORD;
    --   PRAGMA Convention(C, Record_Mouse);
    TYPE Record_Keyboard
      IS RECORD
        Make_Code         : Integer_2_Unsigned_C := 0;
        Flags             : Integer_2_Unsigned_C := 0;
        Reserved          : Integer_2_Unsigned_C := 0;
        Key               : Integer_2_Unsigned_C := 0;
        Message           : Integer_4_Unsigned_C := 0;
        Extra_Information : Integer_4_Unsigned_C := 0;
      END RECORD;
      PRAGMA Convention(C, Record_Keyboard);
    TYPE Record_Version_Information
      IS RECORD
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
      END RECORD;
      PRAGMA Convention(C, Record_Version_Information);
    TYPE Record_Device_Interface
      IS RECORD
        Size     : Integer_4_Unsigned_C        := Record_Device_Interface'Size / 8;
        Class_ID : Integer_4_Unsigned_C        := 0;
        Flags    : Integer_4_Unsigned_C        := 0;
        Reserved : Access_Integer_4_Unsigned_C := NULL; -- ULONG_PTR
      END RECORD;
    TYPE Record_Flash_Information
      IS RECORD
        Size     : Integer_4_Unsigned_C := Record_Flash_Information'Size / 8;
        Window   : Address              := NULL_ADDRESS;
        Flags    : Integer_4_Unsigned_C := 0;
        Count    : Integer_4_Unsigned_C := 0;
        Time_Out : Integer_4_Unsigned_C := 0;
      END RECORD;
      PRAGMA Convention(C, Record_Flash_Information);
    TYPE Record_Rectangle
      IS RECORD
        Left   : Integer_4_Signed_C := 0;
        Top    : Integer_4_Signed_C := 0;
        Right  : Integer_4_Signed_C := 0;
        Bottom : Integer_4_Signed_C := 0;
      END RECORD;
      PRAGMA Convention(C, Record_Rectangle); 
    TYPE Record_Monitor_Information
      IS RECORD
        Size      : Integer_4_Unsigned_C := Record_Monitor_Information'Size / 8;
        Monitor   : Record_Rectangle     := (others => <>);
        Work_Area : Record_Rectangle     := (others => <>);
        Flags     : Integer_4_Unsigned_C := 0;
      END RECORD;
      PRAGMA Convention(C, Record_Monitor_Information);
    TYPE Record_Window_Class
      IS RECORD
        Size       : Integer_4_Unsigned_C          := Record_Window_Class'Size / 8;
        Style      : Integer_4_Unsigned_C          := 0;
        Callback   : Address                       := NULL_ADDRESS;
        Extra_A    : Integer_4_Signed_C            := 0;
        Extra_B    : Integer_4_Signed_C            := 0;
        Instance   : Address                       := NULL_ADDRESS;
        Icon_Large : Address                       := NULL_ADDRESS;
        Cursor     : Address                       := NULL_ADDRESS;
        Background : Address                       := NULL_ADDRESS;
        Menu_Name  : Access_Constant_Character_2_C := NULL;
        Class_Name : Access_Constant_Character_2_C := NULL;
        Icon_Small : Address                       := NULL_ADDRESS;
      END RECORD;
      PRAGMA Convention(C, Record_Window_Class); 
    TYPE Record_Point
      IS RECORD
        X : Integer_4_Signed_C := 0;
        Y : Integer_4_Signed_C := 0;
      END RECORD;
      PRAGMA Convention(C, Record_Point);
    TYPE Record_Message
      IS RECORD
        Window        : Address              := NULL_ADDRESS;
        Data          : Integer_4_Unsigned_C := 0;
        Data_Unsigned : Integer_4_Unsigned_C := 0;
        Data_Signed   : Integer_4_Signed_C   := 0;
        Time          : Integer_4_Unsigned_C := 0;
        Point         : Record_Point         := (others => <>);
      END RECORD;
      PRAGMA Convention(C, Record_Message);
    TYPE Record_Key
      IS RECORD
        Code        : Integer_4_Unsigned_C := 0;
        Scan_Code   : Integer_4_Unsigned_C := 0;
        Flags       : Integer_4_Unsigned_C := 0;
        Time        : Integer_4_Unsigned_C := 0;
        Information : Address              := NULL_ADDRESS; -- Changes on 64/32 bit systems
      END RECORD;
      PRAGMA Convention(C, Record_Key);
    TYPE Record_Memory_Status
      IS RECORD
        Size                       : Integer_4_Unsigned_C := Record_Memory_Status'Size / 8;
        Memory_Load                : Integer_4_Unsigned_C := 0;
        Total_Physical             : Integer_8_Unsigned_C := 0;
        Available_Physical         : Integer_8_Unsigned_C := 0;
        Total_Page_File            : Integer_8_Unsigned_C := 0;
        Available_Page_File        : Integer_8_Unsigned_C := 0;
        Total_Virtual              : Integer_8_Unsigned_C := 0;
        Available_Virtual          : Integer_8_Unsigned_C := 0;
        Available_Extended_Virtual : Integer_8_Unsigned_C := 0;
      END RECORD;
      PRAGMA Convention(C, Record_Memory_Status);
    TYPE Record_GUID
      IS RECORD
        First_Eight_Hex   : Integer_4_Unsigned_C             := 0;
        Second_Four_Hex   : Integer_2_Unsigned_C             := 0;
        Third_Four_Hex    : Integer_2_Unsigned_C             := 0;
        Final_Sixteen_Hex : Array_Integer_1_Unsigned_C(1..8) := (others => 0);
      END RECORD;
      PRAGMA Convention(C, Record_GUID);
    TYPE Record_Device_Information
      IS RECORD
        Size       : Integer_4_Unsigned_C := Record_Device_Information'Size / 8;
        Class_GUID : Record_GUID          := (others => <>);
        Instance   : Integer_4_Unsigned_C := 0;
        Reserved   : Address              := NULL_ADDRESS; -- ULONG_PTR
      END RECORD;
      PRAGMA Convention(C, Record_Device_Information);
    TYPE Record_Device_Setup
      IS RECORD
        Page   : Integer_2_Unsigned_C := 0;
        Usage  : Integer_2_Unsigned_C := 0;
        Flags  : Integer_4_Unsigned_C := 0;
        Target : Address              := NULL_ADDRESS;
      END RECORD;
      PRAGMA Convention(C, Record_Device_Setup);
    TYPE Record_Device_List_Element
      IS RECORD
        Handle : Address              := NULL_ADDRESS;
        Kind   : Integer_4_Unsigned_C := 0;
      END RECORD;
      PRAGMA Convention(C, Record_Device_List_Element);
    TYPE Record_Device_Keyboard
      IS RECORD
        Header : Record_Device_Header := (others => <>);
        Data   : Record_Keyboard      := (others => <>);
      END RECORD;
    TYPE Record_Device_Mouse
      IS RECORD
        Header : Record_Device_Header := (others => <>);
        Data   : Record_Mouse         := (others => <>);
      END RECORD;
-- typedef struct _XINPUT_GAMEPAD
-- {
--     WORD                                wButtons;
--     BYTE                                bLeftTrigger;
--     BYTE                                bRightTrigger;
--     SHORT                               sThumbLX;
--     SHORT                               sThumbLY;
--     SHORT                               sThumbRX;
--     SHORT                               sThumbRY;
-- } XINPUT_GAMEPAD, *PXINPUT_GAMEPAD;

-- typedef struct _XINPUT_STATE
-- {
--     DWORD                               dwPacketNumber;
--     XINPUT_GAMEPAD                      Gamepad;
-- } XINPUT_STATE, *PXINPUT_STATE;

-- typedef struct _XINPUT_VIBRATION
-- {
--     WORD                                wLeftMotorSpeed;
--     WORD                                wRightMotorSpeed;
-- } XINPUT_VIBRATION, *PXINPUT_VIBRATION;

-- typedef struct _XINPUT_CAPABILITIES
-- {
--     BYTE                                TYPE;
--     BYTE                                SubType;
--     WORD                                Flags;
--     XINPUT_GAMEPAD                      Gamepad;
--     XINPUT_VIBRATION                    Vibration;
-- } XINPUT_CAPABILITIES, *PXINPUT_CAPABILITIES;

-- #ifndef XINPUT_USE_9_1_0

-- typedef struct _XINPUT_BATTERY_INFORMATION
-- {
--     BYTE BatteryType;
--     BYTE BatteryLevel;
-- } XINPUT_BATTERY_INFORMATION, *PXINPUT_BATTERY_INFORMATION;

-- typedef struct _XINPUT_KEYSTROKE
-- {
--     WORD    VirtualKey;
--     WCHAR   Unicode;
--     WORD    Flags;
--     BYTE    UserIndex;
--     BYTE    HidCode;
-- } XINPUT_KEYSTROKE, *PXINPUT_KEYSTROKE;
  ------------
  -- Arrays --
  ------------
    TYPE Array_Record_Device_List_Element
      IS ARRAY (Positive range <>)
      OF Record_Device_List_Element;
    TYPE Array_Record_Device_Setup
      IS ARRAY (Positive range <>)
      OF Record_Device_Setup;
  ---------------
  -- Accessors --
  ---------------
    -- TYPE Access_Record_Device_List
    --   IS access all Record_Device_List;
    -- TYPE Access_Array_Record_Device_List
    --   IS access all Array_Record_Device_List;
    TYPE Access_Record_Key
      IS access all Record_Key;
    TYPE Access_Record_Mouse
      IS access all Record_Mouse;
    TYPE Access_Record_Rectangle
      IS access all Record_Rectangle;
    TYPE Access_Record_Monitor_Information
      IS access all Record_Monitor_Information;
  -----------------
  -- Subprograms --
  -----------------
    FUNCTION To_Integer_4_Signed_C
      IS NEW Ada.Unchecked_Conversion(Access_Record_Mouse, Integer_4_Signed_C);
    FUNCTION To_Integer_4_Signed_C
      IS NEW Ada.Unchecked_Conversion(Access_Record_Key, Integer_4_Signed_C);
    FUNCTION To_Access_Record_Rectangle   
      IS NEW Ada.Unchecked_Conversion(Address, Access_Record_Rectangle);
    FUNCTION To_Access_Record_Key 
      IS NEW Ada.Unchecked_Conversion(Integer_4_Signed_C, Access_Record_Key);
    FUNCTION To_Access_Record_Rectangle   
      IS NEW Ada.Unchecked_Conversion(Integer_4_Signed_C, Access_Record_Rectangle);
    FUNCTION Get_Blank_Cursor
      RETURN Array_Integer_1_Unsigned;
-- DWORD WINAPI XInputGetState
-- (
--     DWORD         dwUserIndex,  // Index OF the gamer associated WITH the device
--     XINPUT_STATE* pState        // Receives the current state
-- );

-- DWORD WINAPI XInputSetState
-- (
--     DWORD             dwUserIndex,  // Index OF the gamer associated WITH the device
--     XINPUT_VIBRATION* pVibration    // The vibration information to send to the controller
-- );


-- DWORD WINAPI XInputGetDSoundAudioDeviceGuids
-- (
--     DWORD dwUserIndex,          // Index OF the gamer associated WITH the device
--     GUID* pDSoundRenderGuid,    // DSound device ID for render
--     GUID* pDSoundCaptureGuid    // DSound device ID for capture
-- );

    -- procedure Get_Device_Identifier(
    --   Identifier : IN Access_Device_Identifier);
    FUNCTION Is_Lower(
      Item : IN Character_2)
      RETURN Integer_4_Signed;
    FUNCTION Is_Upper(
      Item : IN Character_2)
      RETURN Integer_4_Signed;
    -- FUNCTION Write_File(
    --   File                     : IN Address;
    --   Buffer                   : IN Access_Constant_Array_Integer_1_Unsigned_C;
    --   Number_Of_Bytes_To_Write : IN Integer_4_Unsigned_C;
    --   Bytes_Written            : IN Access_Integer_4_Unsigned_C;
    --   Over_Lapped              : IN Access_Record_Overlapped)
    --   RETURN Integer_4_Signed_C;
    -- FUNCTION Convert_String_2_C_To_UTF_8(
    --   Code_Page                   : IN Integer_4_Unsigned_C;
    --   Flags                       : IN Integer_4_Unsigned_C;
    --   Original_String             : IN Access_String_2_C;
    --   Character_Count_Of_Original : IN Integer_4_Signed_C;
    --   Resulting_String            : IN Access_String_1_C;
    --   Byte_Count_Of_Result        : IN Integer_4_Signed_C;
    --   Default_Character           : IN Access_Constant_String_1_C;
    --   Used_Default_Character      : IN Access_Integer_4_Signed_C)
    --   RETURN Integer_4_Signed_C;
    FUNCTION Register_Devices(
      Devices : IN Address;
      Number  : IN Integer_4_Unsigned_C;
      Size    : IN Integer_4_Unsigned_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Set_Device_Output_Data(
      Device : IN Address;
      Data   : IN Address;
      Size   : IN Integer_4_Unsigned_C)
      RETURN Integer_4_Signed_C;
    -- FUNCTION Get_Device_Attributes(
    --   Device     : IN Address;
    --   Attributes : IN Access_Device_Attributes)
    --   RETURN Integer_4_Signed_C;
    -- FUNCTION Get_Device_Interface_Detail(
    --   Information_Set       : IN Record_Device_Information;--_In_       HDEVINFO 
    --   Interface             : IN Record_Device_Interface;--_In_       PSP_DEVICE_INTERFACE_DATA 
    --   Interface_Detail      : IN Record_Device_Detail;--_Out_opt_  PSP_DEVICE_INTERFACE_DETAIL_DATA 
    --   Interface_Detail_Size : IN Integer_4_Unsigned_C;--, _In_       DWORD 
    --   Required_Size         : IN Access_Integer_4_Unsigned_C;--_Out_opt_  PDWORD 
    --   Information           : IN );--_Out_opt_  PSP_DEVINFO_DATA);
    --   RETURN Integer_4_Signed;
    -- FUNCTION Enumerate_Device_Interfaces(
    --   Information_Set      : IN Access_Device_Information;
    --   Information_Data     : IN Access_PSP_DEVINFO_DATA;
    --   Interface_Class_Guid : IN Access_GUID;
    --   Member_Index         : IN Integer_4_Unsigned;
    --   Interface_Data       : IN Access_PSP_DEVICE_INTERFACE_DATA)
    --   RETURN Integer_4_Signed;
    FUNCTION Registry_Close_Key(
      Key : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Registry_Open_Key(
      Key     : IN Integer_4_Unsigned_C;
      Sub_Key : IN String_2_C;
      Options : IN Integer_4_Unsigned_C;
      Desired : IN Integer_4_Unsigned_C;
      Result  : IN Address)
      RETURN Integer_4_Unsigned_C;
    FUNCTION Registry_Query_Value(
      Key        : IN Address;
      Value_Name : IN String_2_C;
      Reserved   : IN Address;
      Kind       : IN Address;
      Data       : IN Address;
      Data_Size  : IN Address)
      RETURN Integer_4_Unsigned_C;
    FUNCTION Get_Device_Usages(
      Kind            : IN Integer_4_Signed_C; -- Enumerated
      Page            : IN Integer_2_Unsigned_C;
      Link_Collection : IN Integer_2_Unsigned_C;
      Usage           : IN Address;
      Usage_Length    : IN Address; -- PULONG
      Preparsed_Data  : IN Address;
      Report          : IN Access_Array_Integer_1_Unsigned_C;
      Report_Length   : IN Integer_4_Unsigned_C)
      RETURN Integer_4_Unsigned_C;
    FUNCTION Get_Device_Button_Capabilities(
      Kind                : IN Integer_4_Signed_C;
      Button_Capabilities : IN Address;
      Length              : IN Address;
      Preparsed_Data      : IN Address)
      RETURN Integer_4_Unsigned_C;
    FUNCTION Create_File(
      Name                 : IN Address;
      Desired_Access       : IN Integer_4_Unsigned_C;
      Share_Mode           : IN Integer_4_Unsigned_C;
      Security_Attributes  : IN Address;
      Creation_Desposition : IN Integer_4_Unsigned_C;
      Flags_And_Attributes : IN Integer_4_Unsigned_C;
      Template_File        : IN Address)
      RETURN Address;
    FUNCTION Get_Device_Manufacturer(
      File   : IN Address;
      Buffer : IN Address;
      Size   : IN Integer_4_Unsigned_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_Device_Description(
      File   : IN Address;
      Buffer : IN Address;
      Size   : IN Integer_4_Unsigned_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_Device_Product(
      File   : IN Address;
      Buffer : IN Address;
      Size   : IN Integer_4_Unsigned_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_Device_Information(
      Device  : IN Address;
      Command : IN Integer_4_Unsigned_C;
      Data    : IN Address;
      Size    : IN Address)
      RETURN Integer_4_Unsigned_C;
    FUNCTION Get_Device_Input_Data(
      Device      : IN Address;
      Command     : IN Integer_4_Unsigned_C;
      Data        : IN Address;
      Size        : IN Address;
      Header_Size : IN Integer_4_Unsigned_C)
      RETURN Integer_4_Unsigned_C;
    -- FUNCTION Get_Device_Information(
    --   Guid          : IN Address;
    --   Enumerator    : IN Address;
    --   Window_Parent : IN Address;
    --   Flags         : IN Integer_4_Unsigned_C)
    --   RETURN Address;
    FUNCTION Get_Device_Registry_Property(
      Set           : IN Address;
      Data          : IN Address;
      Property      : IN Integer_4_Unsigned_C;
      Registry_Kind : IN Address;
      Buffer        : IN Address;
      Size          : IN Integer_4_Unsigned_C;
      Required_Size : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_Device_Instance_Id(
      Set           : IN Address;
      Data          : IN Address;
      Id            : IN Address;
      Size          : IN Integer_4_Unsigned_C;
      Required_Size : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_Device_Enumeration(
      Set    : IN Address;
      Member : IN Integer_4_Unsigned_C;
      Data   : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_Device_List(
      List  : IN Address;
      Count : IN Address;
      Size  : IN Integer_4_Unsigned_C)
      RETURN Integer_4_Unsigned_C;
    FUNCTION Destroy_Device_List(
      List : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Enumerate_Display_Monitor(
      Device_Context : IN Address;
      Clip           : IN Address;
      Callback       : IN Address;
      Data           : IN Integer_4_Signed_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_Clip_Box(
      Device_Context : IN Address;
      Rectangle      : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Find_Intersecting_Rectangle(
      Destination : IN Address;
      Rectangle_A : IN Address;
      Rectangle_B : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_Client_Rectangle(
      Window    : IN Address;
      Rectangle : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Rectangles_Are_Equal(
      Rectangle_A : Address;
      Rectangle_B : Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_Class_Setting(
      Window : IN Address;
      Index  : IN Integer_4_Signed_C)
      RETURN Integer_4_Unsigned_C;
    FUNCTION Get_Version(
      Version_Information : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_User_Name(
      Buffer : Address;
      Size   : Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Create_Cursor(
      Instance   : IN Address;
      Hot_Spot_X : IN Integer_4_Signed_C;
      Hot_Spot_Y : IN Integer_4_Signed_C;
      Width      : IN Integer_4_Signed_C;
      Height     : IN Integer_4_Signed_C;
      Bits_AND   : IN Address;
      Bits_XOR   : IN Address)
      RETURN Address;
    FUNCTION Change_Class_Setting(
      Window  : IN Address;
      Command : IN Integer_4_Signed_C;
      Setting : IN Integer_4_Unsigned_C)
      RETURN Integer_4_Unsigned_C;
    FUNCTION Flash_Window(
      Flash_Information : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_Clip_Cursor_Area(
      Rectangle : IN Address)--Record_Rectangle)
      RETURN Integer_4_Signed_C;
    FUNCTION Clip_Cursor(
      Rectangle : IN Address)--Record_Rectangle)
      RETURN Integer_4_Signed_C;
    FUNCTION System_Parameter_Information(
      Action       : IN Integer_4_Unsigned_C;
      Parameter_A  : IN Integer_4_Unsigned_C;
      Parameter_B  : IN Address;
      User_Profile : IN Integer_4_Unsigned_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Change_Window_Setting( -- 64 bit difference, SetWindowLongPtr
      Window  : IN Address;
      Command : IN Integer_4_Signed_C;
      Setting : IN Integer_4_Unsigned_C)
      RETURN Integer_4_Unsigned_C;
    FUNCTION Get_Procedure_Address(
      Module         : IN Address;
      Procedure_Name : IN Access_Constant_Character_2_C)
      RETURN Address;
    FUNCTION Get_Key_State(
      Virtual_Key : IN Integer_4_Unsigned_C)
      RETURN Integer_2_Unsigned_C;
    FUNCTION Free_Library(
      Module : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Load_Library(
      Name : IN Access_Constant_Character_2_C) 
      RETURN Address; 
    FUNCTION Get_Foreground_Window
      RETURN Address;
    FUNCTION Get_Disk_Free_Space(
      Directory                  : IN Access_Constant_Character_2_C;
      Free_Bytes_Available       : IN Address;
      Total_Number_Of_Bytes      : IN Address;
      Total_Number_Of_Free_Bytes : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Shell_Execute(
      Window       : IN Address;
      Operation    : IN Access_Constant_Character_2_C;
      File         : IN Access_Constant_Character_2_C;
      Parameters   : IN Access_Constant_Character_2_C;
      Directory    : IN Access_Constant_Character_2_C;
      Show_Command : IN Integer_4_Signed_C)
      RETURN Address;
    FUNCTION Set_Process_Working_Set_Size(
      Process : IN Address;
      Minimum : IN Integer_Size_C;
      Maximum : IN Integer_Size_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Virtual_Unlock(
      Data : IN Address;
      Size : IN Integer_Size_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Virtual_Lock(
      Data : IN Address;
      Size : IN Integer_Size_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Global_Allocate(
      Flags : Integer_4_Unsigned_C;
      Bytes : Integer_Size_C)
      RETURN Address;
    FUNCTION Get_Clipboard_Data(
      Format : IN Integer_4_Unsigned_C)
      RETURN Address;
    FUNCTION Global_Lock(
      Memory : IN Address)
      RETURN Address;
    FUNCTION Global_Unlock(
      Memory : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Global_Free(
      Memory : IN Address)
      RETURN Address;
    FUNCTION Open_Clipboard(
      Window : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Close_Clipboard
      RETURN Integer_4_Signed_C;
    FUNCTION Empty_Clipboard
      RETURN Integer_4_Signed_C;
    FUNCTION Set_Clipboard_Data(
      Format : IN Integer_4_Unsigned_C;
      Memory : IN Address)
      RETURN Address;
    FUNCTION Format_Message(
      Flags      : IN Integer_4_Unsigned_C;
      Source     : IN Address;
      Identifier : IN Integer_4_Unsigned_C;
      Language   : IN Integer_4_Unsigned_C;
      Buffer     : IN Access_Constant_Character_2_C;
      Size       : IN Integer_4_Unsigned_C;
      Arguments  : IN Address)
      RETURN Integer_4_Unsigned_C;
    FUNCTION Global_Memory_Status(
      Buffer : Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_System_Default_Language
      RETURN Integer_2_Unsigned_C;
    FUNCTION Get_Cursor_Position(
      Point : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Bring_Window_To_Top(
      Window : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Set_Window_Position(
      Window       : IN Address;
      Insert_After : IN Address;
      X            : IN Integer_4_Signed_C;
      Y            : IN Integer_4_Signed_C;
      Width        : IN Integer_4_Signed_C;
      Height       : IN Integer_4_Signed_C;
      Flags        : IN Integer_4_Unsigned_C)
      RETURN Integer_4_Signed_C;
    procedure Post_Quit_Message(
      Exit_Code : IN Integer_4_Signed_C);
    FUNCTION Monitor_From_Window(
      Window : IN Address;
      Flags  : IN Integer_4_Unsigned_C)
      RETURN Address;
    FUNCTION Send_Message(
      Window        : IN Address;
      Message       : IN Integer_4_Unsigned_C;
      Data_Unsigned : IN Integer_4_Unsigned_C;
      Data_Signed   : IN Integer_4_Signed_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_Desktop_Window
      RETURN Address; 
    FUNCTION Get_Current_Process
      RETURN Address;
    FUNCTION Get_Current_Instance
      RETURN Address;
    FUNCTION Get_Process_Affinity_Mask(
      Process               : IN Address;
      Process_Affinity_Mask : IN Address;
      System_Affinity_Mask  : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Query_Performance_Counter(
      Performance_Count : IN Address) 
      RETURN Integer_4_Signed_C;
    FUNCTION Query_Performance_Frequency(
      Frequency : IN Address) 
      RETURN Integer_4_Signed_C;
    FUNCTION Get_Last_Error
      RETURN Integer_4_Unsigned_C;
    FUNCTION Create_Mutex(
      Attributes    : IN Address;
      Initial_Owner : IN Integer_4_Signed_C;
      Name          : IN String_2_C)
      RETURN Address;
    FUNCTION Release_Mutex(
      Mutex : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_Module_Handle(
      Module_Name : Access_Constant_Character_2_C)
      RETURN Address;
    FUNCTION Set_Cursor_Position(
      X : IN Integer_4_Signed_C;
      Y : IN Integer_4_Signed_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Show_Cursor(
      Do_Show : IN Integer_4_Signed_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Load_Cursor(
      Instance    : IN Address;
      Cursor_Name : IN Address)
      RETURN Address;
    FUNCTION Load_Icon(
      Instance  : IN Address;
      Icon_Name : IN Address)
      RETURN Address;
    FUNCTION Load_Image(
      Instance  : IN Address;
      Name      : IN Access_Constant_Character_2_C;
      Kind      : IN Integer_4_Unsigned_C;
      Desired_X : IN Integer_4_Signed_C;
      Desired_Y : IN Integer_4_Signed_C;
      Load      : IN Integer_4_Unsigned_C)
      RETURN Address;
    FUNCTION Message_Box(
      Window  : IN Address;
      Text    : IN String_2_C;
      Caption : IN String_2_C;
      Kind    : IN Integer_4_Unsigned_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_Current_Thread_Id
      RETURN Integer_4_Unsigned_C;
    FUNCTION Unhook_Windows_Hook(
      Hook : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Call_Next_Hook(
      Hook          : IN Address;
      Code          : IN Integer_4_Signed_C;
      Data_Signed   : IN Integer_4_Signed_C;
      Data_Unsigned : IN Integer_4_Unsigned_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Set_Windows_Hook(
      Hook      : IN Integer_4_Signed_C;
      Callback  : IN Address;
      Modifier  : IN Address;
      Thread_Id : IN Integer_4_Unsigned_C)
      RETURN Address;
    FUNCTION Set_Focus(
      Window : IN Address)
      RETURN Address;
    FUNCTION Set_Foreground_Window(
      Window : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Set_Active_Window(
      Window : IN Address)
      RETURN Address;
    FUNCTION Set_Window_Text(
      Window : IN Address;
      Text   : IN Access_Constant_Character_2_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Register_Class(
      Window_Class : IN Address)
      RETURN Integer_2_Unsigned_C;
    FUNCTION Unregister_Class(
      Class_Name   : IN String_2_C;
      Window_Class : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Define_Window_Procedure(
      Window        : IN Address;
      Message       : IN Integer_4_Unsigned_C;
      Data_Unsigned : IN Integer_4_Unsigned_C;
      Data_Signed   : IN Integer_4_Signed_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Create_Window(
      Style_Extra : IN Integer_4_Unsigned_C;
      Class_Name  : IN String_2_C;
      Window_Name : IN String_2_C;
      Style       : IN Integer_4_Unsigned_C;
      X           : IN Integer_4_Signed_C;
      Y           : IN Integer_4_Signed_C;
      Width       : IN Integer_4_Signed_C;
      Height      : IN Integer_4_Signed_C;
      Parent      : IN Address;
      Menu        : IN Address;
      Instance    : IN Address;
      Parameter   : IN Address)
      RETURN Address;
    FUNCTION Show_Window(
      Window  : IN Address;
      Command : IN Integer_4_Signed_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Update_Window(
      Window : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Destroy_Window(
      Window : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Find_Window(
      Class_Name  : IN String_2_C;
      Window_Name : IN Address)
      RETURN Address;
    FUNCTION Get_Window_Rectangle(
      Window    : IN Address;
      Rectangle : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Adjust_Window_Rectangle(
      Rectangle   : IN Address;
      Style       : IN Integer_4_Unsigned_C;
      Menu        : IN Integer_4_Signed_C;
      Extra_Style : IN Integer_4_Unsigned_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Peek_Message(
      Message        : IN Address;
      Window         : IN Address;
      Filter_Minimum : IN Integer_4_Unsigned_C;
      Filter_Maximum : IN Integer_4_Unsigned_C;
      Command        : IN Integer_4_Unsigned_C)
      RETURN Integer_4_Signed_C;
    FUNCTION Translate_Message(
      Message : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Dispatch_Message(
      Message : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_Message_Time
      RETURN Integer_4_Unsigned_C;
    FUNCTION Get_Monitor_Information(
      Monitor     : IN Address;
      Information : IN Address)
      RETURN Integer_4_Signed_C;
    FUNCTION Get_Device_Context(
      Window : IN Address)
      RETURN Address;
    FUNCTION Release_Device_Context(
      Window         : IN Address;
      Device_Context : IN Address)
      RETURN Integer_4_Signed_C;
    -- FUNCTION Get_Device_Capabilities(
    --   Device_Context : IN Address;
    --   Capability     : IN Integer_4_Signed_C)
    --   RETURN Integer_4_Signed_C;
    FUNCTION Close_Handle(
      Object : IN Address)
      RETURN Integer_4_Signed_C;
-------
private
-------
  ----------------
  -- Directives --
  ----------------
    --PRAGMA Linker_Options("C:\Windows\System32\xinput9_0_1.dll");
    --PRAGMA Linker_Options("C:\Windows\SysWOW64\xinput9_0_1.dll");
    PRAGMA Linker_Options("-lgdi32");
    PRAGMA Linker_Options("-lhid");
    PRAGMA Linker_Options("-lsetupapi");
    PRAGMA Import(C,       Get_Current_Instance,           "rts_get_hInstance");
    PRAGMA Import(Stdcall, Is_Lower,                       "iswlower");
    PRAGMA Import(Stdcall, Is_Upper,                       "iswupper");
    --PRAGMA Import(Stdcall, Write_File,                     "WriteFile");
    --PRAGMA Import(Stdcall, Convert_String_2_C_To_UTF_8,    "WideCharToMultiByte");
    --PRAGMA Import(Stdcall, Enumerate_Device_Interfaces,    "SetupDiEnumDeviceInterfaces");
    PRAGMA Import(Stdcall, Registry_Close_Key,             "RegCloseKey");
    PRAGMA Import(Stdcall, Registry_Query_Value,           "RegQueryValueExW");
    PRAGMA Import(Stdcall, Registry_Open_Key,              "RegOpenKeyExW");
    PRAGMA Import(Stdcall, Create_File,                    "CreateFileW");
    PRAGMA Import(Stdcall, Destroy_Device_List,            "SetupDiDestroyDeviceInfoList");
    --PRAGMA Import(Stdcall, Get_Device_Interface_Detail,    "SetupDiGetDeviceInterfaceDetail");
    PRAGMA Import(Stdcall, Get_Device_Registry_Property,   "SetupDiGetDeviceRegistryPropertyW");
    PRAGMA Import(Stdcall, Get_Device_Enumeration,         "SetupDiEnumDeviceInfo");
    PRAGMA Import(Stdcall, Get_Device_Instance_Id,         "SetupDiGetDeviceInstanceIdW");
    --PRAGMA Import(Stdcall, Get_Device_Class_Information,   "SetupDiGetClassDevsW");
    --PRAGMA Import(Stdcall, Get_Device_Identifier,          "HidD_GetHidGuid");
    --PRAGMA Import(Stdcall, Get_Device_Attributes,          "HidD_GetAttributes");
    --PRAGMA Import(Stdcall, Set_Device_Output_Data,         "HidD_SetOutputReport");
    PRAGMA Import(Stdcall, Get_Device_Manufacturer,        "HidD_GetManufacturerString");
    PRAGMA Import(Stdcall, Get_Device_Product,             "HidD_GetProductString");
    PRAGMA Import(Stdcall, Get_Device_Description,         "HidD_GetPhysicalDescriptor");
    PRAGMA Import(Stdcall, Get_Device_Usages,              "HidP_GetUsages");
    --PRAGMA Import(Stdcall, Get_Device_Button_Capabilities, "HidP_GetButtonCaps");
    PRAGMA Import(Stdcall, Get_Device_Input_Data,          "GetRawInputData");
    PRAGMA Import(Stdcall, Get_Device_List,                "GetRawInputDeviceList");
    PRAGMA Import(Stdcall, Get_Device_Information,         "GetRawInputDeviceInfoW");
    PRAGMA Import(Stdcall, Register_Devices,               "RegisterRawInputDevices");
    PRAGMA Import(Stdcall, Close_Handle,                   "CloseHandle");
    PRAGMA Import(Stdcall, Enumerate_Display_Monitor,      "EnumDisplayMonitors");
    PRAGMA Import(Stdcall, Get_Clip_Box,                   "GetClipBox");
    PRAGMA Import(Stdcall, Rectangles_Are_Equal,           "EqualRect");
    PRAGMA Import(Stdcall, Find_Intersecting_Rectangle,    "IntersectRect");
    PRAGMA Import(Stdcall, Get_Client_Rectangle,           "GetClientRect");
    PRAGMA Import(Stdcall, Get_Class_Setting,              "GetClassLongW");
    PRAGMA Import(Stdcall, Get_Version,                    "GetVersionExW");
    PRAGMA Import(Stdcall, Get_User_Name,                  "GetUserNameW");
    PRAGMA Import(Stdcall, Create_Cursor,                  "CreateCursor");
    PRAGMA Import(Stdcall, Change_Class_Setting,           "SetClassLongW");
    PRAGMA Import(Stdcall, Flash_Window,                   "FlashWindowEx");
    PRAGMA Import(Stdcall, Get_Clip_Cursor_Area,           "GetClipCursor");
    PRAGMA Import(Stdcall, Clip_Cursor,                    "ClipCursor");
    PRAGMA Import(Stdcall, System_Parameter_Information,   "SystemParametersInfoW");
    PRAGMA Import(Stdcall, Get_Current_Thread_Id,          "GetCurrentThreadId");
    PRAGMA Import(Stdcall, Get_Module_Handle,              "GetModuleHandleW");
    PRAGMA Import(Stdcall, Get_Key_State,                  "GetKeyState");
    PRAGMA Import(Stdcall, Set_Window_Text,                "SetWindowTextW");
    PRAGMA Import(Stdcall, Change_Window_Setting,          "SetWindowLongW");
    PRAGMA Import(Stdcall, Get_Foreground_Window,          "GetForegroundWindow");
    PRAGMA Import(Stdcall, Free_Library,                   "FreeLibrary");
    PRAGMA Import(Stdcall, Load_Library,                   "LoadLibraryW");
    PRAGMA Import(Stdcall, Get_Procedure_Address,          "GetProcAddress");
    PRAGMA Import(Stdcall, Shell_Execute,                  "ShellExecuteW");
    PRAGMA Import(Stdcall, Set_Process_Working_Set_Size,   "SetProcessWorkingSetSize");
    PRAGMA Import(Stdcall, Get_Disk_Free_Space,            "GetDiskFreeSpaceExW");
    PRAGMA Import(Stdcall, Virtual_Lock,                   "VirtualLock");
    PRAGMA Import(Stdcall, Virtual_Unlock,                 "VirtualUnlock");
    PRAGMA Import(Stdcall, Set_Clipboard_Data,             "SetClipboardData");
    PRAGMA Import(Stdcall, Get_Clipboard_Data,             "GetClipboardData");
    PRAGMA Import(Stdcall, Empty_Clipboard,                "EmptyClipboard");
    PRAGMA Import(Stdcall, Open_Clipboard,                 "OpenClipboard");
    PRAGMA Import(Stdcall, Close_Clipboard,                "CloseClipboard");
    PRAGMA Import(Stdcall, Global_Lock,                    "GlobalLock");
    PRAGMA Import(Stdcall, Global_Unlock,                  "GlobalUnlock");
    PRAGMA Import(Stdcall, Global_Free,                    "GlobalFree");
    PRAGMA Import(Stdcall, Global_Allocate,                "GlobalAlloc");
    PRAGMA Import(Stdcall, Format_Message,                 "FormatMessageW");
    PRAGMA Import(Stdcall, Get_System_Default_Language,    "GetSystemDefaultUILanguage");
    PRAGMA Import(Stdcall, Global_Memory_Status,           "GlobalMemoryStatusEx");
    PRAGMA Import(Stdcall, Get_Cursor_Position,            "GetCursorPos");
    PRAGMA Import(Stdcall, Bring_Window_To_Top,            "BringWindowToTop");
    PRAGMA Import(Stdcall, Set_Window_Position,            "SetWindowPos");
    PRAGMA Import(Stdcall, Send_Message,                   "SendMessageW");
    PRAGMA Import(Stdcall, Unregister_Class,               "UnregisterClassW");
    PRAGMA Import(Stdcall, Message_Box,                    "MessageBoxW");
    PRAGMA Import(Stdcall, Get_Current_Process,            "GetCurrentProcess");
    PRAGMA Import(Stdcall, Monitor_From_Window,            "MonitorFromWindow");
    PRAGMA Import(Stdcall, Post_Quit_Message,              "PostQuitMessage"); 
    PRAGMA Import(Stdcall, Get_Desktop_Window,             "GetDesktopWindow"); 
    PRAGMA Import(Stdcall, Get_Process_Affinity_Mask,      "GetProcessAffinityMask");
    PRAGMA Import(Stdcall, Query_Performance_Counter,      "QueryPerformanceCounter");
    PRAGMA Import(Stdcall, Query_Performance_Frequency,    "QueryPerformanceFrequency");
    PRAGMA Import(Stdcall, Get_Last_Error,                 "GetLastError");
    PRAGMA Import(Stdcall, Create_Mutex,                   "CreateMutexW");
    PRAGMA Import(Stdcall, Release_Mutex,                  "ReleaseMutex");
    PRAGMA Import(Stdcall, Set_Cursor_Position,            "SetCursorPos");
    PRAGMA Import(Stdcall, Show_Cursor,                    "ShowCursor");
    PRAGMA Import(Stdcall, Load_Cursor,                    "LoadCursorW");
    PRAGMA Import(Stdcall, Load_Icon,                      "LoadIconW");
    PRAGMA Import(Stdcall, Load_Image,                     "LoadImageW");
    PRAGMA Import(Stdcall, Call_Next_Hook,                 "CallNextHookEx");
    PRAGMA Import(Stdcall, Unhook_Windows_Hook,            "UnhookWindowsHookEx");
    PRAGMA Import(Stdcall, Set_Windows_Hook,               "SetWindowsHookExW");
    PRAGMA Import(Stdcall, Set_Focus,                      "SetFocus");
    PRAGMA Import(Stdcall, Set_Foreground_Window,          "SetForegroundWindow");
    PRAGMA Import(Stdcall, Set_Active_Window,              "SetActiveWindow");
    PRAGMA Import(Stdcall, Register_Class,                 "RegisterClassExW");
    PRAGMA Import(Stdcall, Define_Window_Procedure,        "DefWindowProcW");
    PRAGMA Import(Stdcall, Create_Window,                  "CreateWindowExW");
    PRAGMA Import(Stdcall, Show_Window,                    "ShowWindow");
    PRAGMA Import(Stdcall, Update_Window,                  "UpdateWindow");
    PRAGMA Import(Stdcall, Destroy_Window,                 "DestroyWindow");
    PRAGMA Import(Stdcall, Find_Window,                    "FindWindowW");
    PRAGMA Import(Stdcall, Get_Window_Rectangle,           "GetWindowRect");
    PRAGMA Import(Stdcall, Adjust_Window_Rectangle,        "AdjustWindowRectEx");
    PRAGMA Import(Stdcall, Peek_Message,                   "PeekMessageW");
    PRAGMA Import(Stdcall, Translate_Message,              "TranslateMessage");
    PRAGMA Import(Stdcall, Dispatch_Message,               "DispatchMessageW");
    PRAGMA Import(Stdcall, Get_Message_Time,               "GetMessageTime");
    PRAGMA Import(Stdcall, Get_Monitor_Information,        "GetMonitorInfoW");
    PRAGMA Import(Stdcall, Get_Device_Context,             "GetDC");
    PRAGMA Import(Stdcall, Release_Device_Context,         "ReleaseDC");
    PRAGMA Import(Stdcall, Get_Device_Capabilities,        "GetDeviceCaps");
  END Neo.Windows;

