'build.vbs'
  '''''''''''''
  ' Constants '
  '''''''''''''
    const EXECUTABLE_PATH_GUESSES =(
      );
  '''''''''''
  ' Objects '
  '''''''''''
    set FileSystem  = CreateObject("Scripting.FileSystemObject")
    set WindowShell = CreateObject("WScript.Shell")
    set CurrentFile = null
  '''''''''''''
  ' Variables '
  '''''''''''''
    directory   = WindowShell.ExpandEnvironmentStrings("%temp%\dir.txt")
    fileName    = "\Example.txt"
    currentFile = 
    names       = 
  ''''''''
  ' Main '
  ''''''''
    'If no environment variables are set, search for the gnat executables'
    'First search by EXECUTABLE_PATH_GUESSES, and if that fails search everywhere'
    if FileSystem.FileExists(sDir) then
      FileSystem.DeleteFile(sDir)
    end if
    for each drive in FileSystem.Drives
      if drive.DriveType = 2 then
        Search drive.DriveLetter
      end if
    Next
    CurrentFile = fileSystem.OpenTextFile(directory, 1)
    for each itemName in Split(CurrentFile.ReadAll, VbCrLf)
      if InStr(1, itemName, fileName, 1) > 0 then
        WScript.Echo itemName
      end if
    next
    CurrentFile.Close
    sub Search(sDrive)
      'WScript.Echo "Scanning drive " & sDrive & ":"'
      'WindowShell.Run "cmd /c dir /s /b " & sDrive & ":\" & sName & " >>" & sDir, 0, True'
    end sub
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
