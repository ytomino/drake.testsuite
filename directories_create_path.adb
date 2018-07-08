-- { dg-do run }
with Ada.Directories.Temporary;
procedure directories_create_path is
	package AD renames Ada.Directories;
	Temp : constant String :=
		AD.Full_Name (AD.Temporary.Create_Temporary_Directory);
	procedure Try_Create_Path (New_Directory : String) is
	begin
		AD.Create_Path (New_Directory);
		pragma Assert (AD.Exists (New_Directory));
		AD.Delete_Tree (Temp & "/A");
	end Try_Create_Path;
begin
	-- absolute
	Try_Create_Path (Temp & "/A");
	Try_Create_Path (Temp & "/A/");
	Try_Create_Path (Temp & "/A/B/");
	-- relative
	declare
		Current : constant String := AD.Current_Directory;
	begin
		AD.Set_Directory (Temp);
		Try_Create_Path ("A");
		Try_Create_Path ("A/");
		Try_Create_Path ("A/B/");
		AD.Create_Path ("A");
		Try_Create_Path ("A/B/");
		AD.Set_Directory (Current);
	end;
	-- cleanup
	AD.Delete_Tree (Temp);
	pragma Debug (Ada.Debug.Put ("OK"));
end directories_create_path;
