-- { dg-do run }
with ada.text_io;
with ada.directories.temporary;
procedure directories_replace_file is
	S : constant String := ada.directories.temporary.Create_Temporary_File;
	T : constant String := ada.directories.temporary.Create_Temporary_File;
begin
	Ada.Debug.Put (S);
	Ada.Debug.Put (T);
	for Overwrite in Boolean loop
		declare
			use ada.text_io;
			file : file_type;
		begin
			create (file, Name => S);
			put_line (file, "SOURCE");
			close (file);
			if Overwrite then
				create (file, Name => T);
				put_line (file, "TARGET");
				close (file);
			elsif Ada.Directories.Exists (T) then
				Ada.Directories.Delete_File (T);
			end if;
		end;
		ada.directories.replace_file (Source_Name => S, Target_Name => T);
		pragma Assert (Ada.Directories.Exists (T));
		pragma Assert (not Ada.Directories.Exists (S));
		declare
			use ada.text_io;
			file : file_type;
		begin
			open (file, In_File, Name => T);
			declare
				Line : constant String := Get_Line (file);
			begin
				pragma Assert (Line = "SOURCE");
				null;
			end;
			pragma Assert (End_Of_File (file));
			close (file);
		end;
		ada.directories.Delete_File (T);
		pragma Assert (not Ada.Directories.Exists (S));
		pragma Assert (not Ada.Directories.Exists (T));
	end loop;
	-- finished
	pragma Debug (Ada.Debug.Put ("OK"));
end directories_replace_file;
