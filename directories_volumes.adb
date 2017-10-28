-- { dg-do run }
with Ada.Directories.Volumes;
procedure directories_volumes is
begin
	declare
		FS : Ada.Directories.Volumes.File_System;
	begin
		pragma Assert (not Ada.Directories.Volumes.Is_Assigned (FS));
		null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end directories_volumes;
