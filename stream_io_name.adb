-- { dg-do run }
with Ada.Hierarchical_File_Names;
with Ada.Directories.Temporary;
with Ada.Streams.Stream_IO.Pipes;
with Ada.Streams.Stream_IO.Standard_Files;
with Ada.Strings.Unbounded;
procedure stream_io_name is
	use Ada.Streams.Stream_IO;
	use type Ada.Strings.Unbounded.Unbounded_String;
	type Kind_Type is (
		Ordinary,
		Ordinary_And_Reset,
		Ordinary_And_Set_Mode,
		Temporary,
		Temporary_And_Reset,
		Temporary_And_Set_Mode,
		Pipe);
	type Finish_Type is (Close, Delete);
	Current_Directory : constant String := Ada.Directories.Current_Directory;
begin
	-- standard files
	Ada.Debug.Put (Name (Standard_Files.Standard_Input.all));
	-- normal files
	for Kind in Kind_Type loop
		for Finish in Finish_Type loop
			for Name_Query in Boolean loop
				if Kind = Pipe and then Finish = Delete then
					goto Continue;
				end if;
				Ada.Debug.Put (
					Kind_Type'Image (Kind) & ", " &
					Finish_Type'Image (Finish) & ", " &
					"Name_Query=" & Boolean'Image (Name_Query));
				declare
					Opened_Name : Ada.Strings.Unbounded.Unbounded_String;
					File : File_Type;
				begin
					case Kind is
						when Ordinary | Ordinary_And_Reset | Ordinary_And_Set_Mode =>
							declare
								Name : constant String :=
									Ada.Directories.Temporary.Create_Temporary_File;
							begin
								Ada.Debug.Put (Name);
								Create (File, Name => Name);
								Opened_Name := +Name;
							end;
						when Temporary
							| Temporary_And_Reset | Temporary_And_Set_Mode =>
							Create (File);
						when Pipe =>
							declare
								The_Pipe : File_Type;
							begin
								Pipes.Create (File, The_Pipe);
							end;
					end case;
					Ada.Directories.Set_Directory ("..");
					case Kind is
						when Ordinary_And_Reset | Temporary_And_Reset =>
							Reset (File);
						when Ordinary_And_Set_Mode | Temporary_And_Set_Mode =>
							Set_Mode (File, In_File);
						when Ordinary | Temporary | Pipe =>
							null;
					end case;
					if Name_Query then
						declare
							N : constant String := Name (File);
						begin
							Ada.Debug.Put (N);
							pragma Assert (
								N (N'First) = '*'
								or else (
									Ada.Hierarchical_File_Names.Is_Full_Name (N)
									and then Ada.Directories.Exists (N)));
						end;
					end if;
					case Finish is
						when Close =>
							Close (File);
						when Delete =>
							Delete (File);
							pragma Assert (
								not Ada.Directories.Exists (
									Opened_Name.Constant_Reference.Element.all));
					end case;
					Ada.Directories.Set_Directory (Current_Directory);
					if Finish = Close and then not Opened_Name.Is_Null then
						Ada.Directories.Delete_File (
							Opened_Name.Constant_Reference.Element.all);
					end if;
				end;
			<<Continue>>
				null;
			end loop;
		end loop;
	end loop;
	pragma Debug (Ada.Debug.Put ("OK"));
end stream_io_name;
