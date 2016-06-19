-- { dg-do run }
with Ada.Streams.Block_Transmission;
with Ada.Streams.Unbounded_Storage_IO;
with Interfaces.C.Generic_Strings;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;
with Interfaces.C.Wide_WStrings;
with Interfaces.C.Wide_Wide_WStrings;
procedure c_char_array_thresholds is
	use Interfaces.C;
	package char16_Pointers is
		new Pointers (
			Index => size_t,
			Element => char16_t,
			Element_Array => char16_array,
			Default_Terminator => char16_t'Val (0));
	package Wide_U16Strings is
		new Generic_Strings (
			Character_Type => Wide_Character,
			String_Type => Wide_String,
			Element => char16_t,
			Element_Array => char16_array,
			Pointers => char16_Pointers,
			To_C => To_C,
			To_Ada => To_Ada);
	package char32_Pointers is
		new Pointers (
			Index => size_t,
			Element => char32_t,
			Element_Array => char32_array,
			Default_Terminator => char32_t'Val (0));
	package Wide_Wide_U32Strings is
		new Generic_Strings (
			Character_Type => Wide_Wide_Character,
			String_Type => Wide_Wide_String,
			Element => char32_t,
			Element_Array => char32_array,
			Pointers => char32_Pointers,
			To_C => To_C,
			To_Ada => To_Ada);
	generic
		type char_t is (<>);
		type array_t is array (size_t range <>) of char_t;
	package Generic_Data is
		subtype First_Zero is array_t (size_t'First + 1 .. size_t'First);
		subtype First_One is array_t (size_t'First .. size_t'First);
		subtype Last_Zero is array_t (size_t'Last .. size_t'Last - 1);
		subtype Last_One is array_t (size_t'Last .. size_t'Last);
		In_First_Zero : constant First_Zero := (others => <>);
		In_First_One : constant First_One :=
			(size_t'First => char_t'Val (Character'Pos ('A')));
		Out_First_Zero : aliased array_t := First_Zero'(others => <>);
		Out_First_One : aliased array_t := First_One'(others => <>);
		In_Last_Zero : constant Last_Zero := (others => <>);
		In_Last_One : constant Last_One :=
			(size_t'Last => char_t'Val (Character'Pos ('Z')));
		Out_Last_Zero : aliased array_t := Last_Zero'(others => <>);
		Out_Last_One : aliased array_t := Last_One'(others => <>);
		First_Nul : constant First_One := (size_t'First => char_t'Val (0));
		Last_Nul : constant Last_One := (size_t'Last => char_t'Val (0));
	end Generic_Data;
	package char_array_Data is
		new Generic_Data (char, char_array);
	package wchar_array_Data is
		new Generic_Data (wchar_t, wchar_array);
	package char16_array_Data is
		new Generic_Data (char16_t, char16_array);
	package char32_array_Data is
		new Generic_Data (char32_t, char32_array);
begin
	-- streaming
	declare
		Buffer : Ada.Streams.Unbounded_Storage_IO.Buffer_Type;
	begin
		-- output
		declare
			generic
				with package Data is new Generic_Data (<>);
			procedure Generic_Write;
			procedure Generic_Write is
			begin
				Data.array_t'Write (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.In_First_Zero);
				Data.array_t'Output (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.In_First_Zero);
				Data.array_t'Write (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.In_First_One);
				Data.array_t'Output (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.In_First_One);
				Data.array_t'Write (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.In_Last_Zero);
				Data.array_t'Output (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.In_Last_Zero);
				Data.array_t'Write (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.In_Last_One);
				Data.array_t'Output (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.In_Last_One);
			end Generic_Write;
			procedure Write_char_array is
				new Generic_Write (char_array_Data);
			procedure Write_wchar_array is
				new Generic_Write (wchar_array_Data);
			procedure Write_char16_array is
				new Generic_Write (char16_array_Data);
			procedure Write_char32_array is
				new Generic_Write (char32_array_Data);
		begin
			Write_char_array;
			Write_wchar_array;
			Write_char16_array;
			Write_char32_array;
		end;
		-- reset the index to the first
		Ada.Streams.Unbounded_Storage_IO.Reset (Buffer);
		-- input
		declare
			generic
				with package Data is new Generic_Data (<>);
			procedure Generic_Read;
			procedure Generic_Read is
				use type Data.array_t;
			begin
				Data.Out_First_Zero := (others => Data.char_t'Val (0));
				Data.array_t'Read (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.Out_First_Zero);
				pragma Assert (Data.Out_First_Zero = Data.In_First_Zero);
				Data.Out_First_Zero := Data.array_t'Input (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer));
				pragma Assert (Data.Out_First_Zero = Data.In_First_Zero);
				Data.Out_First_One := (others => Data.char_t'Val (0));
				Data.array_t'Read (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.Out_First_One);
				pragma Assert (Data.Out_First_One = Data.In_First_One);
				Data.Out_First_One := Data.array_t'Input (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer));
				pragma Assert (Data.Out_First_One = Data.In_First_One);
				Data.Out_Last_Zero := (others => Data.char_t'Val (0));
				Data.array_t'Read (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.Out_Last_Zero);
				pragma Assert (Data.Out_Last_Zero = Data.In_Last_Zero);
				Data.Out_Last_Zero := Data.array_t'Input (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer));
				pragma Assert (Data.Out_Last_Zero = Data.In_Last_Zero);
				Data.Out_Last_One := (others => Data.char_t'Val (0));
				Data.array_t'Read (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.Out_Last_One);
				pragma Assert (Data.Out_Last_One = Data.In_Last_One);
				Data.Out_Last_One := Data.array_t'Input (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer));
				pragma Assert (Data.Out_Last_One = Data.In_Last_One);
			end Generic_Read;
			procedure Read_char_array is
				new Generic_Read (char_array_Data);
			procedure Read_wchar_array is
				new Generic_Read (wchar_array_Data);
			procedure Read_char16_array is
				new Generic_Read (char16_array_Data);
			procedure Read_char32_array is
				new Generic_Read (char32_array_Data);
		begin
			Read_char_array;
			Read_wchar_array;
			Read_char16_array;
			Read_char32_array;
		end;
	end;
	-- streaming with Ada.Streams.Block_Transmission
	declare
		Buffer : Ada.Streams.Unbounded_Storage_IO.Buffer_Type;
	begin
		-- output
		declare
			generic
				with package Data is new Generic_Data (<>);
			procedure Generic_Write;
			procedure Generic_Write is
				procedure Write is
					new Ada.Streams.Block_Transmission.Write (
						size_t,
						Data.char_t,
						Data.array_t);
				procedure Output is
					new Ada.Streams.Block_Transmission.Output (
						size_t,
						Data.char_t,
						Data.array_t,
						Write);
			begin
				Write (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.In_First_Zero);
				Output (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.In_First_Zero);
				Write (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.In_First_One);
				Output (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.In_First_One);
				Write (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.In_Last_Zero);
				Output (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.In_Last_Zero);
				Write (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.In_Last_One);
				Output (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.In_Last_One);
			end Generic_Write;
			procedure Write_char_array is
				new Generic_Write (char_array_Data);
			procedure Write_wchar_array is
				new Generic_Write (wchar_array_Data);
			procedure Write_char16_array is
				new Generic_Write (char16_array_Data);
			procedure Write_char32_array is
				new Generic_Write (char32_array_Data);
		begin
			Write_char_array;
			Write_wchar_array;
			Write_char16_array;
			Write_char32_array;
		end;
		-- reset the index to the first
		Ada.Streams.Unbounded_Storage_IO.Reset (Buffer);
		-- input
		declare
			generic
				with package Data is new Generic_Data (<>);
			procedure Generic_Read;
			procedure Generic_Read is
				use type Data.array_t;
				procedure Read is
					new Ada.Streams.Block_Transmission.Read (
						size_t,
						Data.char_t,
						Data.array_t);
				function Input is
					new Ada.Streams.Block_Transmission.Input (
						size_t,
						Data.char_t,
						Data.array_t,
						Read);
			begin
				Data.Out_First_Zero := (others => Data.char_t'Val (0));
				Read (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.Out_First_Zero);
				pragma Assert (Data.Out_First_Zero = Data.In_First_Zero);
				Data.Out_First_Zero := Input (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer));
				pragma Assert (Data.Out_First_Zero = Data.In_First_Zero);
				Data.Out_First_One := (others => Data.char_t'Val (0));
				Read (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.Out_First_One);
				pragma Assert (Data.Out_First_One = Data.In_First_One);
				Data.Out_First_One := Input (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer));
				pragma Assert (Data.Out_First_One = Data.In_First_One);
				Data.Out_Last_Zero := (others => Data.char_t'Val (0));
				Read (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.Out_Last_Zero);
				pragma Assert (Data.Out_Last_Zero = Data.In_Last_Zero);
				Data.Out_Last_Zero := Input (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer));
				pragma Assert (Data.Out_Last_Zero = Data.In_Last_Zero);
				Data.Out_Last_One := (others => Data.char_t'Val (0));
				Read (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer),
					Data.Out_Last_One);
				pragma Assert (Data.Out_Last_One = Data.In_Last_One);
				Data.Out_Last_One := Input (
					Ada.Streams.Unbounded_Storage_IO.Stream (Buffer));
				pragma Assert (Data.Out_Last_One = Data.In_Last_One);
			end Generic_Read;
			procedure Read_char_array is
				new Generic_Read (char_array_Data);
			procedure Read_wchar_array is
				new Generic_Read (wchar_array_Data);
			procedure Read_char16_array is
				new Generic_Read (char16_array_Data);
			procedure Read_char32_array is
				new Generic_Read (char32_array_Data);
		begin
			Read_char_array;
			Read_wchar_array;
			Read_char16_array;
			Read_char32_array;
		end;
	end;
	-- Is_Nul_Terminated
	declare
		generic
			with package Data is new Generic_Data (<>);
			with function Is_Nul_Terminated (Item : Data.array_t)
				return Boolean is <>;
		procedure Generic_Try_Is_Nul_Terminated;
		procedure Generic_Try_Is_Nul_Terminated is
		begin
			pragma Assert (not Is_Nul_Terminated (Data.In_First_Zero));
			pragma Assert (not Is_Nul_Terminated (Data.In_First_One));
			pragma Assert (not Is_Nul_Terminated (Data.In_Last_Zero));
			pragma Assert (not Is_Nul_Terminated (Data.In_Last_Zero));
			pragma Assert (Is_Nul_Terminated (Data.First_Nul));
			pragma Assert (Is_Nul_Terminated (Data.Last_Nul));
			null;
		end Generic_Try_Is_Nul_Terminated;
		procedure Try_char_array_Is_Nul_Terminated is
			new Generic_Try_Is_Nul_Terminated (char_array_Data);
		procedure Try_wchar_array_Is_Nul_Terminated is
			new Generic_Try_Is_Nul_Terminated (wchar_array_Data);
		procedure Try_char16_array_Is_Nul_Terminated is
			new Generic_Try_Is_Nul_Terminated (char16_array_Data);
		procedure Try_char32_array_Is_Nul_Terminated is
			new Generic_Try_Is_Nul_Terminated (char32_array_Data);
	begin
		Try_char_array_Is_Nul_Terminated;
		Try_wchar_array_Is_Nul_Terminated;
		Try_char16_array_Is_Nul_Terminated;
		Try_char32_array_Is_Nul_Terminated;
	end;
	-- Length
	declare
		generic
			with package Data is new Generic_Data (<>);
			with function Length (Item : Data.array_t) return size_t is <>;
		procedure Generic_Try_Length;
		procedure Generic_Try_Length is
			Dummy : size_t;
		begin
			begin
				Dummy := Length (Data.In_First_Zero);
				raise Program_Error;
			exception
				when Terminator_Error => null;
			end;
			begin
				Dummy := Length (Data.In_First_Zero);
				raise Program_Error;
			exception
				when Terminator_Error => null;
			end;
			begin
				Dummy := Length (Data.In_First_Zero);
				raise Program_Error;
			exception
				when Terminator_Error => null;
			end;
			begin
				Dummy := Length (Data.In_First_Zero);
				raise Program_Error;
			exception
				when Terminator_Error => null;
			end;
			pragma Assert (Length (Data.First_Nul) = 0);
			pragma Assert (Length (Data.Last_Nul) = 0);
		end Generic_Try_Length;
		procedure Try_char_array_Length is
			new Generic_Try_Length (char_array_Data);
		procedure Try_wchar_array_Length is
			new Generic_Try_Length (wchar_array_Data);
		procedure Try_char16_array_Length is
			new Generic_Try_Length (char16_array_Data);
		procedure Try_char32_array_Length is
			new Generic_Try_Length (char32_array_Data);
	begin
		Try_char_array_Length;
		Try_wchar_array_Length;
		Try_char16_array_Length;
		Try_char32_array_Length;
	end;
	-- To_C
	declare
		generic
			with package Data is new Generic_Data (<>);
			type Character_Type is (<>);
			type String_Type is array (Positive range <>) of Character_Type;
			with procedure To_array_t (
				Item : String_Type;
				Target : out Data.array_t;
				Count : out size_t;
				Append_Nul : Boolean;
				Substitute : Data.array_t);
		procedure Generic_Try_To_C;
		procedure Generic_Try_To_C is
			use type Data.array_t;
			In_First_Zero_S : constant String_Type (1 .. 0) := (others => <>);
			In_Last_Zero_S : constant
				String_Type (Integer'Last .. Integer'Last - 1) :=
				(others => <>);
			In_First_One_S : constant String_Type (1 .. 1) :=
				(1 => Character_Type'Val (Character'Pos ('A')));
			In_Last_One_S : constant String_Type (Integer'Last .. Integer'Last) :=
				(1 => Character_Type'Val (Character'Pos ('Z')));
			Out_Count : size_t;
		begin
			To_array_t (
				In_First_Zero_S,
				Data.Out_First_Zero,
				Out_Count,
				Append_Nul => False,
				Substitute => (0 => Data.char_t'Val (Character'Pos ('?'))));
			pragma Assert (Out_Count = 0);
			To_array_t (
				In_Last_Zero_S,
				Data.Out_Last_Zero,
				Out_Count,
				Append_Nul => False,
				Substitute =>
					(size_t'Last => Data.char_t'Val (Character'Pos ('?'))));
			pragma Assert (Out_Count = 0);
			Data.Out_First_One := (others => Data.char_t'Val (0));
			To_array_t (
				In_First_One_S,
				Data.Out_First_One,
				Out_Count,
				Append_Nul => False,
				Substitute => (0 => Data.char_t'Val (Character'Pos ('?'))));
			pragma Assert (Out_Count = 1);
			pragma Assert (Data.Out_First_One =
				(0 => Data.char_t'Val (Character'Pos ('A'))));
			Data.Out_Last_One := (others => Data.char_t'Val (0));
			To_array_t (
				In_Last_One_S,
				Data.Out_Last_One,
				Out_Count,
				Append_Nul => False,
				Substitute =>
					(size_t'Last => Data.char_t'Val (Character'Pos ('?'))));
			pragma Assert (Out_Count = 1);
			pragma Assert (Data.Out_Last_One =
				(0 => Data.char_t'Val (Character'Pos ('Z'))));
			To_array_t (
				In_First_Zero_S,
				Data.Out_First_One,
				Out_Count,
				Append_Nul => True,
				Substitute => (0 => Data.char_t'Val (Character'Pos ('?'))));
			pragma Assert (Out_Count = 1);
			pragma Assert (Data.Out_First_One = (0 => Data.char_t'Val (0)));
			To_array_t (
				In_Last_Zero_S,
				Data.Out_Last_One,
				Out_Count,
				Append_Nul => True,
				Substitute =>
					(size_t'Last => Data.char_t'Val (Character'Pos ('?'))));
			pragma Assert (Out_Count = 1);
			pragma Assert (Data.Out_Last_One = (0 => Data.char_t'Val (0)));
		end Generic_Try_To_C;
		procedure Try_String_To_char_array is
			new Generic_Try_To_C (
				char_array_Data,
				Character,
				String,
				To_char_array);
		procedure Try_Wide_String_To_wchar_array is
			new Generic_Try_To_C (
				wchar_array_Data,
				Wide_Character,
				Wide_String,
				To_wchar_array);
		procedure Try_Wide_Wide_String_To_wchar_array is
			new Generic_Try_To_C (
				wchar_array_Data,
				Wide_Wide_Character,
				Wide_Wide_String,
				To_wchar_array);
		procedure Try_Ada_To_char16_array is
			new Generic_Try_To_C (
				char16_array_Data,
				Wide_Character,
				Wide_String,
				To_C);
		procedure Try_Ada_To_char32_array is
			new Generic_Try_To_C (
				char32_array_Data,
				Wide_Wide_Character,
				Wide_Wide_String,
				To_C);
	begin
		Try_String_To_char_array;
		Try_Wide_String_To_wchar_array;
		Try_Wide_Wide_String_To_wchar_array;
		Try_Ada_To_char16_array;
		Try_Ada_To_char32_array;
	end;
	-- To_Ada
	declare
		generic
			with package Data is new Generic_Data (<>);
			type Character_Type is (<>);
			type String_Type is array (Positive range <>) of Character_Type;
			with procedure To_String_Type (
				Item : Data.array_t;
				Target : out String_Type;
				Count : out Natural;
				Trim_Nul : Boolean;
				Substitute : String_Type);
		procedure Generic_Try_To_Ada;
		procedure Generic_Try_To_Ada is
			Out_First_Zero_S : String_Type (1 .. 0);
			Out_Last_Zero_S : String_Type (Integer'Last .. Integer'Last - 1);
			Out_First_One_S : String_Type (1 .. 1);
			Out_Last_One_S : String_Type (Integer'Last .. Integer'Last);
			Out_Count : Natural;
		begin
			To_String_Type (
				Data.In_First_Zero,
				Out_First_Zero_S,
				Out_Count,
				Trim_Nul => False,
				Substitute => (1 => Character_Type'Val (Character'Pos ('?'))));
			pragma Assert (Out_Count = 0);
			To_String_Type (
				Data.In_Last_Zero,
				Out_Last_Zero_S,
				Out_Count,
				Trim_Nul => False,
				Substitute =>
					(Integer'Last => Character_Type'Val (Character'Pos ('?'))));
			pragma Assert (Out_Count = 0);
			Data.Out_First_One := (others => Data.char_t'Val (0));
			To_String_Type (
				Data.In_First_One,
				Out_First_One_S,
				Out_Count,
				Trim_Nul => False,
				Substitute => (1 => Character_Type'Val (Character'Pos ('?'))));
			pragma Assert (Out_Count = 1);
			pragma Assert (Out_First_One_S =
				(1 => Character_Type'Val (Character'Pos ('A'))));
			Out_Last_One_S := (others => Character_Type'Val (0));
			To_String_Type (
				Data.In_Last_One,
				Out_Last_One_S,
				Out_Count,
				Trim_Nul => False,
				Substitute =>
					(Integer'Last => Character_Type'Val (Character'Pos ('?'))));
			pragma Assert (Out_Count = 1);
			pragma Assert (Out_Last_One_S =
				(1 => Character_Type'Val (Character'Pos ('Z'))));
			To_String_Type (
				Data.First_Nul,
				Out_First_Zero_S,
				Out_Count,
				Trim_Nul => True,
				Substitute => (1 => Character_Type'Val (Character'Pos ('?'))));
			pragma Assert (Out_Count = 0);
			To_String_Type (
				Data.Last_Nul,
				Out_Last_Zero_S,
				Out_Count,
				Trim_Nul => True,
				Substitute =>
					(Integer'Last => Character_Type'Val (Character'Pos ('?'))));
			pragma Assert (Out_Count = 0);
		end Generic_Try_To_Ada;
		procedure Try_char_array_To_String is
			new Generic_Try_To_Ada (
				char_array_Data,
				Character,
				String,
				To_String);
		procedure Try_wchar_array_To_Wide_String is
			new Generic_Try_To_Ada (
				wchar_array_Data,
				Wide_Character,
				Wide_String,
				To_Wide_String);
		procedure Try_wchar_array_To_Wide_Wide_String is
			new Generic_Try_To_Ada (
				wchar_array_Data,
				Wide_Wide_Character,
				Wide_Wide_String,
				To_Wide_Wide_String);
		procedure Try_char16_array_To_Ada is
			new Generic_Try_To_Ada (
				char16_array_Data,
				Wide_Character,
				Wide_String,
				To_Ada);
		procedure Try_char32_array_To_Ada is
			new Generic_Try_To_Ada (
				char32_array_Data,
				Wide_Wide_Character,
				Wide_Wide_String,
				To_Ada);
	begin
		Try_char_array_To_String;
		Try_wchar_array_To_Wide_String;
		Try_wchar_array_To_Wide_Wide_String;
		Try_char16_array_To_Ada;
		Try_char32_array_To_Ada;
	end;
	declare
		generic
			with package Strings is new Generic_Strings (<>);
			with package Data is
				new Generic_Data (
					char_t => Strings.Element,
					array_t => Strings.Element_Array);
		procedure Generic_Try_Strings;
		procedure Generic_Try_Strings is
			use type Strings.chars_ptr;
			Dummy : Strings.chars_ptr;
		begin
			-- To_Chars_Ptr
			begin
				Dummy := Strings.To_Chars_Ptr (
					Data.Out_First_Zero'Access,
					Nul_Check => True);
				raise Program_Error;
			exception
				when Terminator_Error => null;
			end;
			Data.Out_First_One (size_t'First) := Strings.Element'Val (0);
			pragma Assert (
				Strings.To_Chars_Ptr (
					Data.Out_First_One'Access,
					Nul_Check => True) /= null);
			begin
				Dummy := Strings.To_Chars_Ptr (
					Data.Out_Last_Zero'Access,
					Nul_Check => True);
				raise Program_Error;
			exception
				when Terminator_Error => null;
			end;
			Data.Out_Last_One (size_t'Last) := Strings.Element'Val (0);
			pragma Assert (
				Strings.To_Chars_Ptr (
					Data.Out_Last_One'Access,
					Nul_Check => True) /= null);
		end Generic_Try_Strings;
		procedure Try_Strings is
			new Generic_Try_Strings (Strings, char_array_Data);
		procedure Try_Wide_WStrings is
			new Generic_Try_Strings (Wide_WStrings, wchar_array_Data);
		procedure Try_Wide_Wide_WStrings is
			new Generic_Try_Strings (Wide_Wide_WStrings, wchar_array_Data);
		procedure Try_Wide_U16Strings is
			new Generic_Try_Strings (Wide_U16Strings, char16_array_Data);
		procedure Try_Wide_Wide_U32Strings is
			new Generic_Try_Strings (Wide_Wide_U32Strings, char32_array_Data);
	begin
		Try_Strings;
		Try_Wide_WStrings;
		Try_Wide_Wide_WStrings;
		Try_Wide_U16Strings;
		Try_Wide_Wide_U32Strings;
	end;
	-- finished
	pragma Debug (Ada.Debug.Put ("OK"));
end c_char_array_thresholds;
