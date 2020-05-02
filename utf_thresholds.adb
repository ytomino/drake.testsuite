-- { dg-do run }
with Ada;
with System.UTF_Conversions;
procedure utf_thresholds is
	use type System.UTF_Conversions.UCS_4;
	use type System.UTF_Conversions.From_Status_Type;
	use type System.UTF_Conversions.To_Status_Type;
	generic
		type U is (<>);
		type U_String is array (Positive range <>) of U;
		with procedure From_UTF (
			Data : U_String;
			Last : out Natural;
			Result : out System.UTF_Conversions.UCS_4;
			Status : out System.UTF_Conversions.From_Status_Type);
		with procedure From_UTF_Reverse (
			Data : U_String;
			First : out Positive;
			Result : out System.UTF_Conversions.UCS_4;
			Status : out System.UTF_Conversions.From_Status_Type);
	procedure Generic_Try_From (
		S : U_String;
		Expected_Code : System.UTF_Conversions.UCS_4;
		Expected_Status : System.UTF_Conversions.From_Status_Type);
	procedure Generic_Try_From (
		S : U_String;
		Expected_Code : System.UTF_Conversions.UCS_4;
		Expected_Status : System.UTF_Conversions.From_Status_Type)
	is
		First : Positive;
		Last : Natural;
		Result : System.UTF_Conversions.UCS_4;
		From_Status : System.UTF_Conversions.From_Status_Type;
	begin
		From_UTF (S, Last, Result, From_Status);
		pragma Assert (Last = S'Last);
		pragma Assert (Result = Expected_Code);
		pragma Assert (From_Status = Expected_Status);
		From_UTF_Reverse (S, First, Result, From_Status);
		pragma Assert (First = S'First);
		pragma Assert (Result = Expected_Code);
		pragma Assert (From_Status = Expected_Status);
	end Generic_Try_From;
	generic
		type U is (<>);
		type U_String is array (Positive range <>) of U;
		with procedure To_UTF (
			Code : System.UTF_Conversions.UCS_4;
			Result : out U_String;
			Last : out Natural;
			Status : out System.UTF_Conversions.To_Status_Type);
	procedure Generic_Try_To (
		C : System.UTF_Conversions.UCS_4;
		Expected_String : U_String;
		Expected_Status : System.UTF_Conversions.To_Status_Type);
	procedure Generic_Try_To (
		C : System.UTF_Conversions.UCS_4;
		Expected_String : U_String;
		Expected_Status : System.UTF_Conversions.To_Status_Type)
	is
		Result : U_String (Expected_String'Range);
		Last : Natural;
		To_Status : System.UTF_Conversions.To_Status_Type;
	begin
		To_UTF (C, Result, Last, To_Status);
		pragma Assert (Result = Expected_String);
		pragma Assert (Last = Result'Last);
		pragma Assert (To_Status = Expected_Status);
	end Generic_Try_To;
	subtype U8 is Character;
	subtype U8_String is String;
	subtype U16 is Wide_Character;
	subtype U16_String is Wide_String;
	subtype U32 is Wide_Wide_Character;
	subtype U32_String is Wide_Wide_String;
begin
	-- UTF-8
	declare
		procedure Try_From_UTF_8 is
			new Generic_Try_From (
				U8,
				U8_String,
				System.UTF_Conversions.From_UTF_8,
				System.UTF_Conversions.From_UTF_8_Reverse);
		procedure Try_To_UTF_8 is
			new Generic_Try_To (
				U8,
				U8_String,
				System.UTF_Conversions.To_UTF_8);
		S6 : constant U8_String (Integer'Last - 5 .. Integer'Last) := (
			U8'Val (16#fd#), U8'Val (16#be#), U8'Val (16#bd#), U8'Val (16#bc#),
			U8'Val (16#bb#), U8'Val (16#ba#));
		S5 : constant U8_String (Integer'Last - 4 .. Integer'Last) := (
			U8'Val (16#fd#), U8'Val (16#be#), U8'Val (16#bd#), U8'Val (16#bc#),
			U8'Val (16#bb#));
		S4 : constant U8_String (Integer'Last - 3 .. Integer'Last) := (
			U8'Val (16#fd#), U8'Val (16#be#), U8'Val (16#bd#), U8'Val (16#bc#));
		S3 : constant U8_String (Integer'Last - 2 .. Integer'Last) := (
			U8'Val (16#fd#), U8'Val (16#be#), U8'Val (16#bd#));
		S2 : constant U8_String (Integer'Last - 1 .. Integer'Last) := (
			U8'Val (16#fd#), U8'Val (16#be#));
		S1 : constant U8_String (Integer'Last .. Integer'Last) :=
			(Integer'Last => U8'Val (16#fd#));
		T1 : constant U8_String (Integer'Last .. Integer'Last) :=
			(Integer'Last => U8'Val (16#7f#));
	begin
		Try_From_UTF_8 (S6, 16#7ef7_cefa#, System.UTF_Conversions.Success);
		Try_From_UTF_8 (S5, 16#7ef7_cec0#, System.UTF_Conversions.Truncated);
		Try_From_UTF_8 (S4, 16#7ef7_c000#, System.UTF_Conversions.Truncated);
		Try_From_UTF_8 (S3, 16#7ef4_0000#, System.UTF_Conversions.Truncated);
		Try_From_UTF_8 (S2, 16#7e00_0000#, System.UTF_Conversions.Truncated);
		Try_From_UTF_8 (S1, 16#4000_0000#, System.UTF_Conversions.Truncated);
		Try_From_UTF_8 (T1, 16#007f#, System.UTF_Conversions.Success);
		Try_To_UTF_8 (16#7ef7_cefa#, S6, System.UTF_Conversions.Success);
		Try_To_UTF_8 (16#7ef7_cec0#, S5, System.UTF_Conversions.Overflow);
		Try_To_UTF_8 (16#7ef7_c000#, S4, System.UTF_Conversions.Overflow);
		Try_To_UTF_8 (16#7ef4_0000#, S3, System.UTF_Conversions.Overflow);
		Try_To_UTF_8 (16#7e00_0000#, S2, System.UTF_Conversions.Overflow);
		Try_To_UTF_8 (16#4000_0000#, S1, System.UTF_Conversions.Overflow);
		Try_To_UTF_8 (16#007f#, T1, System.UTF_Conversions.Success);
	end;
	-- UTF-16
	declare
		procedure Try_From_UTF_16 is
			new Generic_Try_From (
				U16,
				U16_String,
				System.UTF_Conversions.From_UTF_16,
				System.UTF_Conversions.From_UTF_16_Reverse);
		procedure Try_To_UTF_16 is
			new Generic_Try_To (
				U16,
				U16_String,
				System.UTF_Conversions.To_UTF_16);
		S2 : constant U16_String (Integer'Last - 1 .. Integer'Last) := (
			U16'Val (16#dbff#), U16'Val (16#dffe#));
		S1 : constant U16_String (Integer'Last .. Integer'Last) :=
			(Integer'Last => U16'Val (16#dbff#));
		T1 : constant U16_String (Integer'Last .. Integer'Last) :=
			(Integer'Last => U16'Val (16#d7ff#));
	begin
		Try_From_UTF_16 (S2, 16#10_fffe#, System.UTF_Conversions.Success);
		Try_From_UTF_16 (S1, 16#10_fc00#, System.UTF_Conversions.Truncated);
		Try_From_UTF_16 (T1, 16#d7ff#, System.UTF_Conversions.Success);
		Try_To_UTF_16 (16#10_fffe#, S2, System.UTF_Conversions.Success);
		Try_To_UTF_16 (16#10_fc00#, S1, System.UTF_Conversions.Overflow);
		Try_To_UTF_16 (16#d7ff#, T1, System.UTF_Conversions.Success);
	end;
	-- UTF-32
	declare
		procedure Try_From_UTF_32 is
			new Generic_Try_From (
				U32,
				U32_String,
				System.UTF_Conversions.From_UTF_32,
				System.UTF_Conversions.From_UTF_32_Reverse);
		procedure Try_To_UTF_32 is
			new Generic_Try_To (
				U32,
				U32_String,
				System.UTF_Conversions.To_UTF_32);
		S1 : constant U32_String (Integer'Last .. Integer'Last) :=
			(Integer'Last => U32'Val (16#7fff_ffff#));
	begin
		Try_From_UTF_32 (S1, 16#7fff_ffff#, System.UTF_Conversions.Success);
		Try_To_UTF_32 (16#7fff_ffff#, S1, System.UTF_Conversions.Success);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end utf_thresholds;
