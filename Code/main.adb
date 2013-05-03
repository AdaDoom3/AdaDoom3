-- Sample output:
-- C:\Users\user0\Downloads\AdaDoom3-master (16)\AdaDoom3-master\main
--
-- ________________________________________________________________________________
--
--                         S  Y  S  T  E  M      T  E  S  T  
-- ________________________________________________________________________________
--
-- Language ENGLISH_LANGUAGE
-- Version WINDOWS_2_6_1_SYSTEM
-- Username "user0"
--
-- >> 5
--
-- ________________________________________________________________________________
--
--                            T  E  X  T      T  E  S  T  
-- ________________________________________________________________________________
--
--
--
--
-- +-----------------------------------------------------------------------------+
-- | |       |\                                           -~ /     \  /          |
-- |~~__     | \                                         | \/       /\          /|
-- |    --   |  \                                        | / \    /    \     /   |
-- |      |~_|   \                                   \___|/    \/         /      |
-- |--__  |   -- |\________________________________/~~\~~|    /  \     /     \   |
-- |   |~~--__  |~_|____|____|____|____|____|____|/ /  \/|\ /      \/          \/|
-- |   |      |~--_|__|____|____|____|____|____|_/ /|    |/ \    /   \       /   |
-- |___|______|__|_||____|____|____|____|____|__[]/_|----|    \/       \  /      |
-- |  \mmmm :   | _|___|____|____|____|____|____|___|  /\|   /  \      /  \      |
-- |      B :_--~~ |_|____|____|____|____|____|____|  |  |\/      \ /        \   |
-- |  __--P :  |  /                                /  /  | \     /  \          /\|
-- |~~  |   :  | /                                 ~~~   |  \  /      \      /   |
-- |    |      |/                        .-.             |  /\          \  /     |
-- |    |      /                        |   |            |/   \          /\      |
-- |    |     /                        |     |            -_   \       /    \    |
-- +-----------------------------------------------------------------------------+
-- |          |  /|  |   |  2  3  4  | /~~~~~\ |       /|    |_| ....  ......... |
-- |          |  ~|~ | % |           | | ~J~ | |       ~|~ % |_| ....  ......... |
-- |   AMMO   |  HEALTH  |  5  6  7  |  \===/  |    ARMOR    |#| ....  ......... |
-- +-----------------------------------------------------------------------------+
--           "From the people who gave you VI - NEW Ascii DOOM!"     -Beth Mayo
-- But does it work? Yes!
--
-- >> 5
--
-- ________________________________________________________________________________
--
--                         M  E  M  O  R  Y      T  E  S  T  
-- ________________________________________________________________________________
--
-- Load 4.70000E+01
-- Physical_Total 8192
-- Physical_Available 4508577792
-- Page_File_Total 17174138880
-- Page_File_Available 11318198272
-- Virtual_Total 4294836224
-- Virtual_Available 4237250560
-- Virtual_Available_Extended 0

-- >> 5
--
-- ________________________________________________________________________________
--
--                     P  R  O  C  E  S  S  O  R      T  E  S  T
-- ________________________________________________________________________________
--
-- Clock ticks:  216825906872
-- Sleep...
-- Clock ticks:  216827385041
-- Sleep...
-- Clock ticks:  216830338365
-- Number of cores: 4
-- Speed in megahertz: 2942861
-- Vendor: ADVANCED_MICRO_DEVICES_VENDOR
-- Has 3DNow!
-- Has 3DNow!+
-- Has SSE4a
-- Has MMX+
-- Has MMX
-- Has SSE
-- Has SSE2
-- Has SSE3
-- Has POPCNT
-- Has LZCNT
-- Has FXSR
-- Has HTT
-- Has CMOV
-- Stack is empty!
-- Specifics: 1001111111000000
-- Control word: 37F
-- Status word: 100000
-- Selector: 100011
-- Tags: 1111111111111111
-- Data offset: 1010001111100100100000
-- Data selector: 101011
-- Operation code: 10110011100
-- Program counter: 10000000110111111010011
-- Trace:
--
--      1: 0043B61F
--      2: 00404F12 neo.system.processor.implementation_for_compiler.put_trace
--               At neo-system-processor-implementation_for_compiler.adb:40
--      3: 0040B19D neo.system.processor.put_trace
--               At neo-system-processor.adb:333
--      4: 0040AE8E neo.system.processor.test.a
--               At neo-system-processor.adb:38
--      5: 0040AEA2 neo.system.processor.test.b
--               At neo-system-processor.adb:45
--      6: 0040AEB6 neo.system.processor.test.c
--               At neo-system-processor.adb:52
--      7: 0040AECA neo.system.processor.test.d
--               At neo-system-processor.adb:59
--      8: 0040ACCD neo.system.processor.test
--               At neo-system-processor.adb:215
--      9: 00404241 main
--               At main.adb:34
--     10: 0040192B main
--               At b~main.adb:333
--     11: 004010B9
--     12: 004012A6
--     13: 74D433A8
--     14: 771B9EF0
--     15: 771B9EC3
--
-- >> d
-- [2013-04-23 23:48:48] process terminated successfully (elapsed time: 24.65s)
with
  Neo.Foundation.Text_IO,
  Neo.Foundation.Data_Types,
  Neo.Foundation.Package_Testing,
  Neo.System,
  Neo.System.Text,
  Neo.System.Memory,
  Neo.System.Processor;--,
  --Neo.System.Network,
  --Neo.System.Window,
  --Neo.System.Exception_Handling,
  --Neo.System.Input;
procedure Main
  is
  begin
    --Neo.Foundation.Text_IO.Test;
    --Neo.Foundation.Package_Testing.Test;
    Neo.System.Test;
    Neo.System.Processor.Initialize;
    Neo.System.Processor.Test;
    Neo.System.Memory.Test;
    Neo.System.Text.Test;
  end Main;
