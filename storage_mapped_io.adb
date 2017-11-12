-- { dg-do run }
with Ada.Directories.Temporary;
with Ada.Storage_Mapped_IO;
with Ada.Streams;
procedure storage_mapped_io is
	use type Ada.Storage_Mapped_IO.File_Mode;
begin
	declare
		P : Ada.Storage_Mapped_IO.Storage_Type;
	begin
		pragma Assert (not Ada.Storage_Mapped_IO.Is_Mapped (P));
		null;
	end;
	declare
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
	pragma Debug (Ada.Debug.Put ("OK"));
end storage_mapped_io;
