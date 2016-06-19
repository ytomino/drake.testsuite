-- { dg-do run }
with Ada.Environment_Encoding.Encoding_Streams;
with Ada.Environment_Encoding.Generic_Strings;
with Ada.Environment_Encoding.Names;
with Ada.Environment_Encoding.Strings;
with Ada.Environment_Encoding.Wide_Strings;
with Ada.Environment_Encoding.Wide_Wide_Strings;
with Ada.Streams.Overlaps_Storage_IO;
with Ada.Streams.Stream_IO;
with Ada.Streams.Unbounded_Storage_IO;
with System.Storage_Elements;
procedure streams_sea_thresholds is
	use Ada.Streams;
	In_First_Zero_SEA : constant
		Stream_Element_Array (
			Stream_Element_Offset'First + 1 .. Stream_Element_Offset'First) :=
		(others => <>);
	In_First_One_SEA : constant
		Stream_Element_Array (
			Stream_Element_Offset'First .. Stream_Element_Offset'First) :=
		(Stream_Element_Offset'First => Character'Pos ('A'));
	Out_First_Zero_SEA :
		Stream_Element_Array (
			Stream_Element_Offset'First + 1 .. Stream_Element_Offset'First);
	Out_First_One_SEA :
		Stream_Element_Array (
			Stream_Element_Offset'First .. Stream_Element_Offset'First);
	In_Last_Zero_SEA : constant
		Stream_Element_Array (
			Stream_Element_Offset'Last .. Stream_Element_Offset'Last - 1) :=
		(others => <>);
	In_Last_One_SEA : constant
		Stream_Element_Array (
			Stream_Element_Offset'Last .. Stream_Element_Offset'Last) :=
		(Stream_Element_Offset'Last => Character'Pos ('Z'));
	Out_Last_Zero_SEA :
		Stream_Element_Array (
			Stream_Element_Offset'Last .. Stream_Element_Offset'Last - 1);
	Out_Last_One_SEA :
		Stream_Element_Array (
			Stream_Element_Offset'Last .. Stream_Element_Offset'Last);
begin
	-- streaming with Ada.Streams.Block_Transmission
	declare
		Buffer : Unbounded_Storage_IO.Buffer_Type;
	begin
		-- output
		Stream_Element_Array'Write (
			Unbounded_Storage_IO.Stream (Buffer),
			In_First_Zero_SEA);
		Stream_Element_Array'Output (
			Unbounded_Storage_IO.Stream (Buffer),
			In_First_Zero_SEA);
		Stream_Element_Array'Write (
			Unbounded_Storage_IO.Stream (Buffer),
			In_First_One_SEA);
		Stream_Element_Array'Output (
			Unbounded_Storage_IO.Stream (Buffer),
			In_First_One_SEA);
		Stream_Element_Array'Write (
			Unbounded_Storage_IO.Stream (Buffer),
			In_Last_Zero_SEA);
		Stream_Element_Array'Output (
			Unbounded_Storage_IO.Stream (Buffer),
			In_Last_Zero_SEA);
		Stream_Element_Array'Write (
			Unbounded_Storage_IO.Stream (Buffer),
			In_Last_One_SEA);
		Stream_Element_Array'Output (
			Unbounded_Storage_IO.Stream (Buffer),
			In_Last_One_SEA);
		-- reset the index to the first
		Unbounded_Storage_IO.Reset (Buffer);
		-- input
		Out_First_Zero_SEA := (others => 0);
		Stream_Element_Array'Read (
			Unbounded_Storage_IO.Stream (Buffer),
			Out_First_Zero_SEA);
		pragma Assert (Out_First_Zero_SEA = In_First_Zero_SEA);
		Out_First_Zero_SEA := (others => 0);
		Out_First_Zero_SEA := Stream_Element_Array'Input (
			Unbounded_Storage_IO.Stream (Buffer));
		pragma Assert (Out_First_Zero_SEA = In_First_Zero_SEA);
		Out_First_One_SEA := (others => 0);
		Stream_Element_Array'Read (
			Unbounded_Storage_IO.Stream (Buffer),
			Out_First_One_SEA);
		pragma Assert (Out_First_One_SEA = In_First_One_SEA);
		Out_First_One_SEA := Stream_Element_Array'Input (
			Unbounded_Storage_IO.Stream (Buffer));
		pragma Assert (Out_First_One_SEA = In_First_One_SEA);
		Out_Last_Zero_SEA := (others => 0);
		Stream_Element_Array'Read (
			Unbounded_Storage_IO.Stream (Buffer),
			Out_Last_Zero_SEA);
		pragma Assert (Out_Last_Zero_SEA = In_Last_Zero_SEA);
		Out_Last_Zero_SEA := Stream_Element_Array'Input (
			Unbounded_Storage_IO.Stream (Buffer));
		pragma Assert (Out_Last_Zero_SEA = In_Last_Zero_SEA);
		Out_Last_One_SEA := (others => 0);
		Stream_Element_Array'Read (
			Unbounded_Storage_IO.Stream (Buffer),
			Out_Last_One_SEA);
		pragma Assert (Out_Last_One_SEA = In_Last_One_SEA);
		Out_Last_One_SEA := Stream_Element_Array'Input (
			Unbounded_Storage_IO.Stream (Buffer));
		pragma Assert (Out_Last_One_SEA = In_Last_One_SEA);
	end;
	-- Ada.Environment_Encodings
	declare
		use Ada.Environment_Encoding;
		procedure Try_Conversions (
			From_Id, To_Id : Encoding_Id;
			Converter : in out Ada.Environment_Encoding.Converter)
		is
			pragma Unreferenced (From_Id);
			In_Last : Stream_Element_Offset;
			Out_Last : Stream_Element_Offset;
			Subsequence_Status : Subsequence_Status_Type;
			Continuing_Status : Continuing_Status_Type;
			Finishing_Status : Finishing_Status_Type;
			Status : Status_Type;
			Substituting_Status : Substituting_Status_Type;
		begin
			-- Set_Substitute
			Set_Substitute (Converter, In_First_Zero_SEA);
			pragma Assert (Substitute (Converter) = In_First_Zero_SEA);
			Set_Substitute (Converter, In_First_One_SEA);
			pragma Assert (Substitute (Converter) = In_First_One_SEA);
			Set_Substitute (Converter, In_Last_Zero_SEA);
			pragma Assert (Substitute (Converter) = In_Last_Zero_SEA);
			Set_Substitute (Converter, In_Last_One_SEA);
			pragma Assert (Substitute (Converter) = In_Last_One_SEA);
			-- reset the substitute
			Set_Substitute (Converter, Default_Substitute (To_Id));
			-- Convert with subsequence
			for Finish in Boolean loop
				Convert (
					Converter,
					In_First_Zero_SEA,
					In_Last,
					Out_First_Zero_SEA,
					Out_Last,
					Finish => Finish,
					Status => Subsequence_Status);
				pragma Assert (In_Last = Stream_Element_Offset'First);
				pragma Assert (Out_Last = Stream_Element_Offset'First);
				Convert (
					Converter,
					In_Last_Zero_SEA,
					In_Last,
					Out_Last_Zero_SEA,
					Out_Last,
					Finish => Finish,
					Status => Subsequence_Status);
				pragma Assert (In_Last = Stream_Element_Offset'Last - 1);
				pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
			end loop;
			-- Convert with continuing
			Convert (
				Converter,
				In_First_Zero_SEA,
				In_Last,
				Out_First_Zero_SEA,
				Out_Last,
				Status => Continuing_Status);
			pragma Assert (In_Last = Stream_Element_Offset'First);
			pragma Assert (Out_Last = Stream_Element_Offset'First);
			Convert (
				Converter,
				In_Last_Zero_SEA,
				In_Last,
				Out_Last_Zero_SEA,
				Out_Last,
				Status => Continuing_Status);
			pragma Assert (In_Last = Stream_Element_Offset'Last - 1);
			pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
			-- Convert with finishing
			Convert (
				Converter,
				Out_First_Zero_SEA,
				Out_Last,
				Finish => True,
				Status => Finishing_Status);
			pragma Assert (Out_Last = Stream_Element_Offset'First);
			Convert (
				Converter,
				Out_Last_Zero_SEA,
				Out_Last,
				Finish => True,
				Status => Finishing_Status);
			pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
			-- Convert all
			Convert (
				Converter,
				In_First_Zero_SEA,
				In_Last,
				Out_First_Zero_SEA,
				Out_Last,
				Finish => True,
				Status => Status);
			pragma Assert (In_Last = Stream_Element_Offset'First);
			pragma Assert (Out_Last = Stream_Element_Offset'First);
			Convert (
				Converter,
				In_Last_Zero_SEA,
				In_Last,
				Out_Last_Zero_SEA,
				Out_Last,
				Finish => True,
				Status => Status);
			pragma Assert (In_Last = Stream_Element_Offset'Last - 1);
			pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
			-- Convert all with substituting
			Convert (
				Converter,
				In_First_Zero_SEA,
				In_Last,
				Out_First_Zero_SEA,
				Out_Last,
				Finish => True,
				Status => Substituting_Status);
			pragma Assert (In_Last = Stream_Element_Offset'First);
			pragma Assert (Out_Last = Stream_Element_Offset'First);
			Convert (
				Converter,
				In_Last_Zero_SEA,
				In_Last,
				Out_Last_Zero_SEA,
				Out_Last,
				Finish => True,
				Status => Substituting_Status);
			pragma Assert (In_Last = Stream_Element_Offset'Last - 1);
			pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
		end Try_Conversions;
		Encodings : constant array (1 .. 5) of Encoding_Id := (
			Names.UTF_8,
			Names.UTF_16,
			Names.UTF_32,
			Names.Latin_1,
			Names.Windows_31J);
	begin
		for I in Encodings'Range loop
			declare
				Converter : Strings.Encoder := Strings.To (Encodings (I));
			begin
				Try_Conversions (
					Names.UTF_8,
					Encodings (I),
					Ada.Environment_Encoding.Converter (Converter));
			end;
			declare
				Converter : Wide_Strings.Encoder :=
					Wide_Strings.To (Encodings (I));
			begin
				Try_Conversions (
					Names.UTF_16,
					Encodings (I),
					Ada.Environment_Encoding.Converter (Converter));
			end;
			declare
				Converter : Wide_Wide_Strings.Encoder :=
					Wide_Wide_Strings.To (Encodings (I));
			begin
				Try_Conversions (
					Names.UTF_32,
					Encodings (I),
					Ada.Environment_Encoding.Converter (Converter));
			end;
			declare
				Converter : Strings.Decoder := Strings.From (Encodings (I));
			begin
				Try_Conversions (
					Encodings (I),
					Names.UTF_8,
					Ada.Environment_Encoding.Converter (Converter));
			end;
			declare
				Converter : Wide_Strings.Decoder :=
					Wide_Strings.From (Encodings (I));
			begin
				Try_Conversions (
					Encodings (I),
					Names.UTF_16,
					Ada.Environment_Encoding.Converter (Converter));
			end;
			declare
				Converter : Wide_Wide_Strings.Decoder :=
					Wide_Wide_Strings.From (Encodings (I));
			begin
				Try_Conversions (
					Encodings (I),
					Names.UTF_32,
					Ada.Environment_Encoding.Converter (Converter));
			end;
		end loop;
	end;
	-- Ada.Environment_Encodings.Generic_Strings
	declare
		use Ada.Environment_Encoding;
		generic
			with package Strings is new Generic_Strings (<>);
		procedure Generic_Try (Id : Encoding_Id);
		procedure Generic_Try (Id : Encoding_Id) is
			use type Strings.Character_Type;
		begin
			-- decoder
			declare
				Decoder : Strings.Decoder := Strings.From (Id);
			begin
				declare
					S : constant Strings.String_Type :=
						Strings.Decode (Decoder, In_First_Zero_SEA);
				begin
					pragma Assert (S'Length = 0);
					null;
				end;
				if Id /= Names.UTF_16 and then Id /= Names.UTF_32 then
					declare
						S : constant Strings.String_Type :=
							Strings.Decode (Decoder, In_First_One_SEA);
					begin
						pragma Assert (S'Length = 1);
						pragma Assert (S (S'First) =
							Strings.Character_Type'Val (Character'Pos ('A')));
						null;
					end;
				end if;
				declare
					S : constant Strings.String_Type :=
						Strings.Decode (Decoder, In_Last_Zero_SEA);
				begin
					pragma Assert (S'Length = 0);
					null;
				end;
				if Id /= Names.UTF_16 and then Id /= Names.UTF_32 then
					declare
						S : constant Strings.String_Type :=
							Strings.Decode (Decoder, In_Last_One_SEA);
					begin
						pragma Assert (S'Length = 1);
						pragma Assert (S (S'First) =
							Strings.Character_Type'Val (Character'Pos ('Z')));
						null;
					end;
				end if;
			end;
		end Generic_Try;
		procedure Try_Strings is new Generic_Try (Strings);
		procedure Try_Wide_Strings is new Generic_Try (Wide_Strings);
		procedure Try_Wide_Wide_Strings is new Generic_Try (Wide_Wide_Strings);
		Encodings : constant array (1 .. 5) of Encoding_Id := (
			Names.UTF_8,
			Names.UTF_16,
			Names.UTF_32,
			Names.Latin_1,
			Names.Windows_31J);
	begin
		for I in Encodings'Range loop
			Try_Strings (Encodings (I));
			Try_Wide_Strings (Encodings (I));
			Try_Wide_Wide_Strings (Encodings (I));
		end loop;
	end;
	-- Ada.Environment_Encodings.Streams
	declare
		use Ada.Environment_Encoding;
		Buffer : Unbounded_Storage_IO.Buffer_Type;
		Out_Last : Stream_Element_Offset;
	begin
		-- Do not reuse Inout_Converter because it will be finishing mode
		--   when it got End_Error once.
		declare
			Inout_Converter : aliased Encoding_Streams.Inout_Type :=
				Encoding_Streams.Open (
					Internal => Names.Latin_1,
					External => Names.Windows_31J,
					Stream => Unbounded_Storage_IO.Stream (Buffer));
		begin
			-- input (reach to EOF)
			Read (
				Encoding_Streams.Stream (Inout_Converter).all,
				Out_First_Zero_SEA,
				Out_Last);
			pragma Assert (Out_Last = Stream_Element_Offset'First);
			begin
				Read (
					Encoding_Streams.Stream (Inout_Converter).all,
					Out_First_One_SEA,
					Out_Last);
					raise Program_Error;
			exception
				when Constraint_Error => null;
			end;
		end;
		declare
			Inout_Converter : aliased Encoding_Streams.Inout_Type :=
				Encoding_Streams.Open (
					Internal => Names.Latin_1,
					External => Names.Windows_31J,
					Stream => Unbounded_Storage_IO.Stream (Buffer));
		begin
			Read (
				Encoding_Streams.Stream (Inout_Converter).all,
				Out_Last_Zero_SEA,
				Out_Last);
			pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
			Read (
				Encoding_Streams.Stream (Inout_Converter).all,
				Out_Last_One_SEA,
				Out_Last);
			pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
		end;
		declare
			Inout_Converter : aliased Encoding_Streams.Inout_Type :=
				Encoding_Streams.Open (
					Internal => Names.Latin_1,
					External => Names.Windows_31J,
					Stream => Unbounded_Storage_IO.Stream (Buffer));
		begin
			-- Set_Substitute before conversions
			Encoding_Streams.Set_Substitute (Inout_Converter, In_First_Zero_SEA);
			pragma Assert (
				Encoding_Streams.Substitute (Inout_Converter) = In_First_Zero_SEA);
			Encoding_Streams.Set_Substitute (Inout_Converter, In_First_One_SEA);
			pragma Assert (
				Encoding_Streams.Substitute (Inout_Converter) = In_First_One_SEA);
			Encoding_Streams.Set_Substitute (Inout_Converter, In_Last_Zero_SEA);
			pragma Assert (
				Encoding_Streams.Substitute (Inout_Converter) = In_Last_Zero_SEA);
			Encoding_Streams.Set_Substitute (Inout_Converter, In_Last_One_SEA);
			pragma Assert (
				Encoding_Streams.Substitute (Inout_Converter) = In_Last_One_SEA);
			-- reset the substitute
			Encoding_Streams.Set_Substitute (
				Inout_Converter,
				Default_Substitute (Names.Latin_1));
			-- output
			Write (
				Encoding_Streams.Stream (Inout_Converter).all,
				In_First_Zero_SEA);
			Write (
				Encoding_Streams.Stream (Inout_Converter).all,
				In_First_One_SEA);
			Write (
				Encoding_Streams.Stream (Inout_Converter).all,
				In_Last_Zero_SEA);
			Write (
				Encoding_Streams.Stream (Inout_Converter).all,
				In_Last_One_SEA);
			Encoding_Streams.Finish (Inout_Converter);
			-- reset the index to the first
			Unbounded_Storage_IO.Reset (Buffer);
			-- input
			Read (
				Encoding_Streams.Stream (Inout_Converter).all,
				Out_First_Zero_SEA,
				Out_Last);
			pragma Assert (Out_Last = Stream_Element_Offset'First);
			Out_First_One_SEA := (others => 0);
			Read (
				Encoding_Streams.Stream (Inout_Converter).all,
				Out_First_One_SEA,
				Out_Last);
			pragma Assert (Out_Last = Stream_Element_Offset'First);
			pragma Assert (Out_First_One_SEA = In_First_One_SEA);
			Read (
				Encoding_Streams.Stream (Inout_Converter).all,
				Out_Last_Zero_SEA,
				Out_Last);
			pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
			Out_Last_One_SEA := (others => 0);
			Read (
				Encoding_Streams.Stream (Inout_Converter).all,
				Out_Last_One_SEA,
				Out_Last);
			pragma Assert (Out_Last = Stream_Element_Offset'Last);
			pragma Assert (Out_Last_One_SEA = In_Last_One_SEA);
			-- Set_Substitute after conversions
			Encoding_Streams.Set_Substitute (Inout_Converter, In_First_Zero_SEA);
			pragma Assert (
				Encoding_Streams.Substitute (Inout_Converter) = In_First_Zero_SEA);
			Encoding_Streams.Set_Substitute (Inout_Converter, In_First_One_SEA);
			pragma Assert (
				Encoding_Streams.Substitute (Inout_Converter) = In_First_One_SEA);
			Encoding_Streams.Set_Substitute (Inout_Converter, In_Last_Zero_SEA);
			pragma Assert (
				Encoding_Streams.Substitute (Inout_Converter) = In_Last_Zero_SEA);
			Encoding_Streams.Set_Substitute (Inout_Converter, In_Last_One_SEA);
			pragma Assert (
				Encoding_Streams.Substitute (Inout_Converter) = In_Last_One_SEA);
		end;
	end;
	-- Ada.Streams.Overlaps_Storage_IO
	declare
		Out_Last : Stream_Element_Offset;
	begin
		declare -- Size => 0
			Overlay : Overlaps_Storage_IO.Overlay :=
				Overlaps_Storage_IO.Create (System.Null_Address, Size => 0);
		begin
			-- output (reach to EOF)
			Write (
				Overlaps_Storage_IO.Stream (Overlay).all,
				In_First_Zero_SEA);
			begin
				Write (
					Overlaps_Storage_IO.Stream (Overlay).all,
					In_First_One_SEA);
				raise Program_Error;
			exception
				when Storage_Error => null;
			end;
			Write (
				Overlaps_Storage_IO.Stream (Overlay).all,
				In_Last_Zero_SEA);
			begin
				Write (
					Overlaps_Storage_IO.Stream (Overlay).all,
					In_Last_One_SEA);
				raise Program_Error;
			exception
				when Storage_Error => null;
			end;
			-- input (reach to EOF)
			Read (
				Overlaps_Storage_IO.Stream (Overlay).all,
				Out_First_Zero_SEA,
				Out_Last);
			pragma Assert (Out_Last = Stream_Element_Offset'First);
			begin
				Read (
					Overlaps_Storage_IO.Stream (Overlay).all,
					Out_First_One_SEA,
					Out_Last);
				raise Program_Error;
			exception
				when Constraint_Error => null;
			end;
			Read (
				Overlaps_Storage_IO.Stream (Overlay).all,
				Out_Last_Zero_SEA,
				Out_Last);
			pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
			Read (
				Overlaps_Storage_IO.Stream (Overlay).all,
				Out_Last_One_SEA,
				Out_Last);
			pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
		end;
		declare -- Size => 1
			pragma Compile_Time_Error (
				Stream_Element'Size /= Standard'Storage_Unit,
				"Stream_Element'Size /= Storage_Unit");
			Storage : aliased System.Storage_Elements.Storage_Array (0 .. 0);
			Overlay : Overlaps_Storage_IO.Overlay :=
				Overlaps_Storage_IO.Create (Storage'Address, Size => 1);
			Out_Last : Stream_Element_Offset;
		begin
			-- output
			Write (
				Overlaps_Storage_IO.Stream (Overlay).all,
				In_First_Zero_SEA);
			Write (
				Overlaps_Storage_IO.Stream (Overlay).all,
				In_First_One_SEA);
			Overlaps_Storage_IO.Reset (Overlay);
			-- input
			Read (
				Overlaps_Storage_IO.Stream (Overlay).all,
				Out_First_Zero_SEA,
				Out_Last);
			pragma Assert (Out_Last = Stream_Element_Offset'First);
			Out_First_One_SEA := (others => 0);
			Read (
				Overlaps_Storage_IO.Stream (Overlay).all,
				Out_First_One_SEA,
				Out_Last);
			pragma Assert (Out_Last = Stream_Element_Offset'First);
			pragma Assert (Out_First_One_SEA = In_First_One_SEA);
			Overlaps_Storage_IO.Reset (Overlay);
			-- output
			Write (
				Overlaps_Storage_IO.Stream (Overlay).all,
				In_Last_Zero_SEA);
			Write (
				Overlaps_Storage_IO.Stream (Overlay).all,
				In_Last_One_SEA);
			Overlaps_Storage_IO.Reset (Overlay);
			-- input
			Read (
				Overlaps_Storage_IO.Stream (Overlay).all,
				Out_Last_Zero_SEA,
				Out_Last);
			pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
			Out_Last_One_SEA := (others => 0);
			Read (
				Overlaps_Storage_IO.Stream (Overlay).all,
				Out_Last_One_SEA,
				Out_Last);
			pragma Assert (Out_Last = Stream_Element_Offset'Last);
			pragma Assert (Out_Last_One_SEA = In_Last_One_SEA);
		end;
	end;
	-- Ada.Streams.Stream_IO
	declare
		File : Stream_IO.File_Type := Stream_IO.Create;
		Out_Last : Stream_Element_Offset;
	begin
		-- switch to input
		Stream_IO.Reset (File, Stream_IO.In_File);
		-- input (reach to EOF)
		Read (
			Stream_IO.Stream (File).all,
			Out_First_Zero_SEA,
			Out_Last);
		pragma Assert (Out_Last = Stream_Element_Offset'First);
		begin
			Read (
				Stream_IO.Stream (File).all,
				Out_First_One_SEA,
				Out_Last);
			raise Program_Error;
		exception
			when Constraint_Error => null;
		end;
		Read (
			Stream_IO.Stream (File).all,
			Out_Last_Zero_SEA,
			Out_Last);
		pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
		Read (
			Stream_IO.Stream (File).all,
			Out_Last_One_SEA,
			Out_Last);
		pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
		-- switch to output
		Stream_IO.Reset (File, Stream_IO.Out_File);
		-- output
		Write (
			Stream_IO.Stream (File).all,
			In_First_Zero_SEA);
		Write (
			Stream_IO.Stream (File).all,
			In_First_One_SEA);
		Write (
			Stream_IO.Stream (File).all,
			In_Last_Zero_SEA);
		Write (
			Stream_IO.Stream (File).all,
			In_Last_One_SEA);
		-- switch to input
		Stream_IO.Reset (File, Stream_IO.In_File);
		-- input
		Read (
			Stream_IO.Stream (File).all,
			Out_First_Zero_SEA,
			Out_Last);
		pragma Assert (Out_Last = Stream_Element_Offset'First);
		Out_First_One_SEA := (others => 0);
		Read (
			Stream_IO.Stream (File).all,
			Out_First_One_SEA,
			Out_Last);
		pragma Assert (Out_Last = Stream_Element_Offset'First);
		pragma Assert (Out_First_One_SEA = In_First_One_SEA);
		Read (
			Stream_IO.Stream (File).all,
			Out_Last_Zero_SEA,
			Out_Last);
		pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
		Out_Last_One_SEA := (others => 0);
		Read (
			Stream_IO.Stream (File).all,
			Out_Last_One_SEA,
			Out_Last);
		pragma Assert (Out_Last = Stream_Element_Offset'Last);
		pragma Assert (Out_Last_One_SEA = In_Last_One_SEA);
		-- cleanup
		Stream_IO.Delete (File);
	end;
	-- Ada.Streams.Unbounded_Storage_IO
	declare
		Buffer : Unbounded_Storage_IO.Buffer_Type;
		Out_Last : Stream_Element_Offset;
	begin
		-- input (reach to EOF)
		Read (
			Unbounded_Storage_IO.Stream (Buffer).all,
			Out_First_Zero_SEA,
			Out_Last);
		pragma Assert (Out_Last = Stream_Element_Offset'First);
		begin
			Read (
				Unbounded_Storage_IO.Stream (Buffer).all,
				Out_First_One_SEA,
				Out_Last);
			raise Program_Error;
		exception
			when Constraint_Error => null;
		end;
		Read (
			Unbounded_Storage_IO.Stream (Buffer).all,
			Out_Last_Zero_SEA,
			Out_Last);
		pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
		Read (
			Unbounded_Storage_IO.Stream (Buffer).all,
			Out_Last_One_SEA,
			Out_Last);
		pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
		-- output
		Write (
			Unbounded_Storage_IO.Stream (Buffer).all,
			In_First_Zero_SEA);
		Write (
			Unbounded_Storage_IO.Stream (Buffer).all,
			In_First_One_SEA);
		Write (
			Unbounded_Storage_IO.Stream (Buffer).all,
			In_Last_Zero_SEA);
		Write (
			Unbounded_Storage_IO.Stream (Buffer).all,
			In_Last_One_SEA);
		-- reset the index to the first
		Unbounded_Storage_IO.Reset (Buffer);
		-- input
		Read (
			Unbounded_Storage_IO.Stream (Buffer).all,
			Out_First_Zero_SEA,
			Out_Last);
		pragma Assert (Out_Last = Stream_Element_Offset'First);
		Out_First_One_SEA := (others => 0);
		Read (
			Unbounded_Storage_IO.Stream (Buffer).all,
			Out_First_One_SEA,
			Out_Last);
		pragma Assert (Out_Last = Stream_Element_Offset'First);
		pragma Assert (Out_First_One_SEA = In_First_One_SEA);
		Read (
			Unbounded_Storage_IO.Stream (Buffer).all,
			Out_Last_Zero_SEA,
			Out_Last);
		pragma Assert (Out_Last = Stream_Element_Offset'Last - 1);
		Out_Last_One_SEA := (others => 0);
		Read (
			Unbounded_Storage_IO.Stream (Buffer).all,
			Out_Last_One_SEA,
			Out_Last);
		pragma Assert (Out_Last = Stream_Element_Offset'Last);
		pragma Assert (Out_Last_One_SEA = In_Last_One_SEA);
	end;
	-- finished
	pragma Debug (Ada.Debug.Put ("OK"));
end streams_sea_thresholds;
