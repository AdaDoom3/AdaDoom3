with POSIX;
with POSIX.Process_Identification;

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Directories;

separate(Neo.System) 

package body Import is
   
   use  Ada.Strings.Fixed;
   
   use type POSIX.POSIX_String;

   function Get_Last_Error return Integer_4_Unsigned is
   begin
      raise Unimplemented_Feature;  
      return 0;
   end Get_Last_Error;
   
   
   procedure Open_Text(Path : in String_2) is
   begin
      raise Unimplemented_Feature;  
   end Open_Text;
   
   
   procedure Open_Webpage(Path : in String_2) is
   begin
      raise Unimplemented_Feature;  
   end Open_Webpage;
   
   
   procedure Set_Alert(Value : in Boolean) is
   begin
      raise Unimplemented_Feature;  
   end Set_Alert;
   
   
   procedure Execute(Path : in String_2; Do_Fullscreen : in Boolean) is
   begin
      raise Unimplemented_Feature;  
   end Execute;
   
   
   function Is_Okay(Name, Message : in String_2; Buttons : in Enumerated_Buttons; Icon : in Enumerated_Icon) return Boolean is
   begin
      raise Unimplemented_Feature;  
      return False;
   end Is_Okay;
   
   
   function Get_Specifics return Record_Specifics is
      
      Proc_Self_Exe  : constant String := "/proc/self/exe";

      Login_Name : constant POSIX.POSIX_String := POSIX.Process_Identification.Get_Login_Name;   
      Release    : constant String     := POSIX.To_String(POSIX.Release);      

      Path            : String_2_Unbounded;
      Executable_Name : String_2_Unbounded;
      System          : System_Type                := Unknown_System;
      System_Version  : System_Version_Type        := (0,0);
      
      
      Idx_First_Dot   : Natural := 0;
      Idx_Second_Dot   : Natural := 0;
      
   begin
      
      if POSIX.System_Name /= "Linux" then
	 
	 raise Unimplemented_Feature;  	 
	 
      else
	 
	 System := Linux_System;
	 
	 -- proc/self/exe is available only on Linux
	 -- For other systems see the following question on stack overflow:
	 -- http://stackoverflow.com/questions/1023306/finding-current-executables-path-without-proc-self-exe
	 Path := To_String_2_Unbounded(To_String_2(Ada.Directories.Full_Name(Proc_Self_Exe))); 	    
	 
      end if;
      
      -- Executable name based on the Path calculated earlier
      --
      Executable_Name := Delete(Path, 1, Index(Path, "\", Backward));
      
      -- Release is assumed to be well formatted (eg "4.2.0-18-generic")
      --
      Idx_First_Dot := Index(Source  => Release,
			     Set     => Ada.Strings.Maps.To_Set('.'),
			     From    => Release'First);
      
      System_Version(Major_Version) := Natural'Value(Release(Release'First .. Idx_First_Dot - 1));
      
      Idx_Second_Dot := Index(Source  => Release,
			      Set     => Ada.Strings.Maps.To_Set('.'),
			      From    => Idx_First_Dot + 1);
      
      System_Version(Minor_Version) := Natural'Value(Release(Idx_First_Dot + 1 .. Idx_Second_Dot - 1));
      
      return 
	( Username  => To_String_2_Unbounded(To_String_2(POSIX.To_String(Login_Name))), -- It is probably a good idea to write conversion function from String to String_2_Unbounded
	  System    => System,
	  Version   => System_Version,
	  Name      => Executable_Name,
	  Path      => Path,
	  Bit_Size  => (if POSIX.Machine = "x86_64" then 64 else 32),
	  Separator => '/'  
	);
	  
   end Get_Specifics;
end Import;
