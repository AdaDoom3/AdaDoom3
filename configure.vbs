'configure.vbs'
  '''''''''''''
  ' Constants '
  '''''''''''''
    const TEMPORARY_FILE_NAME     = build_vbs_DirectoryList.txt
    const ENVIRONMENT_VARIABLES   = ("gnat", "mingw", "gcc", "gnatmake")
    const EXECUTABLE_NAME         = "\gnatmake.exe"
    const EXECUTABLE_PATH_GUESSES =( _
      )
  '''''''''''
  ' Objects '
  '''''''''''
    set FileSystem  = CreateObject("Scripting.FileSystemObject")
    set WindowShell = CreateObject("WScript.Shell")
    set CurrentFile = null
  '''''''''''''
  ' Variables '
  '''''''''''''
    
    
    Names         = 
    PossiblePaths = EXECUTABLE_PATH_GUESSES
  ''''''''''''''''''
  ' FastFileSearch '
  ''''''''''''''''''
    sub FastFileSearch(String ItemName)
      Directory   = WindowShell.ExpandEnvironmentStrings("%temp%\" & TEMPORARY_FILE_NAME)
      CurrentFile = 
      if FileSystem.FileExists(directory) then
        FileSystem.DeleteFile(directory)
      end if
      for each Drive in FileSystem.Drives
        if Drive.DriveType = 2 then
          search Drive.DriveLetter
        end if
      next
      CurrentFile = FileSystem.OpenTextFile(directory, 1)
      for each ItemName in Split(CurrentFile.ReadAll, VbCrLf)
        if InStr(1, ItemName, FileName, 1) > 0 then
          WScript.Echo ItemName
        end if
      next
      CurrentFile.Close
      sub Search(Drive)
        'WScript.Echo "Scanning drive " & Drive & ":"'
        'WindowShell.Run "cmd /c dir /s /b " & Drive & ":\" & itemName & " >>" & directory, 0, True'
      end sub
    end sub
  ''''''''
  ' Main '
  ''''''''
    'If no environment variables are set, search for the gnat executables'
    'First search by EXECUTABLE_PATH_GUESSES, and if that fails search everywhere'
    if WindowShell.ExpandEnvironmentStrings("%GNAT%")

    'Once it is found, make the script edit itself (if its possible?) and add'
    'the valid path to the EXECUTABLE_PATH_GUESSES string array. If some of the path'
    'guesses are invalid directories then they should be removed.'

    '....'
    
    
...pass to gprbuild
32-bit
windows
x86-64
optimize yes
saftey yes
GNAT

...also build 64-bit if compiler exists
