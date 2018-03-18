-- { dg-do run }
with Ada.Directories.Temporary;
with Ada.Storage_Mapped_IO;
with Ada.Streams.Stream_IO;
procedure storage_mapped_io is
	use type Ada.Storage_Mapped_IO.File_Mode;
begin
	declare
		P : Ada.Storage_Mapped_IO.Storage_Type;
	begin
		pragma Assert (not Ada.Storage_Mapped_IO.Is_Mapped (P));
		null;
	end;
	declare -- Out_File
		Name : constant String :=
			Ada.Directories.Temporary.Create_Temporary_File;
		P : Ada.Storage_Mapped_IO.Storage_Type;
	begin
		for Size in Ada.Streams.Stream_Element_Count range 0 .. 1 loop
			for I in Ada.Storage_Mapped_IO.File_Mode loop
				begin
					Ada.Storage_Mapped_IO.Map (P, I, Name => Name, Size => Size);
					Ada.Storage_Mapped_IO.Unmap (P);
				exception
					when Ada.Storage_Mapped_IO.Use_Error =>
						if I = Ada.Storage_Mapped_IO.Out_File then
							Ada.Debug.Put (
								"read-access is always required in this machine");
						else
							raise;
						end if;
				end;
			end loop;
		end loop;
		Ada.Directories.Delete_File (Name);
	end;
	declare -- Private_Copy
		Name : constant String :=
			Ada.Directories.Temporary.Create_Temporary_File;
		P : Ada.Storage_Mapped_IO.Storage_Type;
		File : Ada.Streams.Stream_IO.File_Type;
	begin
		for Mode in
			Ada.Storage_Mapped_IO.In_File .. Ada.Storage_Mapped_IO.Inout_File
		loop
			for Private_Copy in Boolean loop
				-- initialize contents
				Ada.Streams.Stream_IO.Create (File, Name => Name);
				String'Write (Ada.Streams.Stream_IO.Stream (File), "Ada!");
				Ada.Streams.Stream_IO.Close (File);
				-- modify by storage mapping
				Ada.Storage_Mapped_IO.Map (P, Mode,
					Name => Name, Private_Copy => Private_Copy);
				declare
					Contents : String (1 .. 4);
					for Contents'Address use
						Ada.Storage_Mapped_IO.Storage_Address (P);
				begin
					pragma Assert (Contents = "Ada!");
					if Mode = Ada.Storage_Mapped_IO.Inout_File
						or else Private_Copy
					then
						Contents (1 .. 3) := "C++";
					end if;
				end;
				Ada.Storage_Mapped_IO.Unmap (P);
				-- check contents
				if Mode = Ada.Storage_Mapped_IO.Inout_File
					or else Private_Copy
				then
					Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File,
						Name => Name);
					declare
						Contents : String (1 .. 4);
					begin
						String'Read (Ada.Streams.Stream_IO.Stream (File), Contents);
						pragma Assert ((Contents = "Ada!") = Private_Copy);
						pragma Assert ((Contents = "C++!") /= Private_Copy);
					end;
					Ada.Streams.Stream_IO.Close (File);
				end if;
			end loop;
		end loop;
		Ada.Directories.Delete_File (Name);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end storage_mapped_io;
