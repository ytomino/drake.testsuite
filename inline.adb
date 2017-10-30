-- { dg-do compile }
-- { dg-additional-options "-O2 -gnat-a -gnatp -gnatn2 -Winline -Werror" }
with Ada.Characters.Conversions;
with Ada.Colors;
with Ada.Command_Line;
with Ada.Directories.Temporary;
with Ada.Dispatching;
with Ada.Environment_Encoding;
with Ada.Environment_Variables;
-- with Ada.Interrupts;
-- with Ada.Locales;
with Ada.Naked_Text_IO;
with Ada.Processes;
with Ada.Streams.Naked_Stream_IO;
with Ada.Streams.Stream_IO.Naked;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
with Ada.Task_Identification;
with Ada.Text_IO.Naked;
with Ada.Text_IO.Terminal.Colors;
with System.Program;
procedure inline is
	procedure Ignore (X : Boolean) with Import;
	procedure Ignore (X : Integer) with Import;
	procedure Ignore (X : String) with Import;
	procedure Ignore (X : Wide_String) with Import;
	procedure Ignore (X : Wide_Wide_String) with Import;
	procedure Ignore (X : access Ada.Naked_Text_IO.Non_Controlled_File_Type)
		with Import;
	procedure Ignore (
		X : access Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type)
		with Import;
	procedure Ignore (X : Ada.Streams.Stream_Element_Array) with Import;
	procedure Ignore (X : Ada.Streams.Stream_Element_Offset) with Import;
	procedure Ignore (X : Ada.Strings.Maps.Character_Set) with Import;
	procedure Ignore (X : Ada.Strings.Maps.Character_Mapping) with Import;
	procedure Ignore (X : System.Address) with Import;
begin -- inline renamings
	-- private entities required by compiler
	delay 0.0; -- Ada.Calendar.Delays.Delay_For
	-- Ada.Characters.Conversions
	declare
		use Ada.Characters.Conversions;
	begin
		Ignore (To_Wide_String (String'(""), Substitute => ""));
		Ignore (To_Wide_Wide_String (String'(""), Substitute => ""));
		Ignore (To_Wide_Wide_String (Wide_String'(""), Substitute => ""));
--		Ignore (To_String (Wide_String'(""), Substitute => ' '));
		Ignore (To_String (Wide_String'(""), Substitute => ""));
--		Ignore (To_String (Wide_Wide_String'(""), Substitute => ' '));
		Ignore (To_String (Wide_Wide_String'(""), Substitute => ""));
--		Ignore (To_Wide_String (Wide_Wide_String'(""), Substitute => ' '));
		Ignore (To_Wide_String (Wide_Wide_String'(""), Substitute => ""));
	end;
	-- Ada.Command_Line
	declare
		use Ada.Command_Line;
	begin
		Ignore (Argument_Count);
	end;
	-- Ada.Directories
	declare
		use Ada.Directories;
		No_Existing_Name : constant String := "$$$";
	begin
		Ignore (Current_Directory);
		Set_Directory (No_Existing_Name);
		Delete_Directory (No_Existing_Name);
		Delete_File (No_Existing_Name);
		Rename (No_Existing_Name, No_Existing_Name, Overwrite => False);
--		Copy_File (No_Existing_Name, No_Existing_Name,
--			Form => "overwrite=false");
		Copy_File (No_Existing_Name, No_Existing_Name, Overwrite => False);
		Replace_File (No_Existing_Name, No_Existing_Name);
		Symbolic_Link (No_Existing_Name, No_Existing_Name, Overwrite => False);
		Ignore (Full_Name (No_Existing_Name));
		Ignore (Exists (No_Existing_Name));
	end;
	-- Ada.Directories.Temporary
	declare
		use Ada.Directories.Temporary;
		No_Existing_Name : constant String := "$$$";
	begin
		Ignore (Temporary_Directory);
		Set_Temporary_Directory (No_Existing_Name);
		Ignore (Create_Temporary_File (Directory => No_Existing_Name));
		Ignore (Create_Temporary_Directory (Directory => No_Existing_Name));
	end;
	-- Ada.Dispatching
	declare
		use Ada.Dispatching;
	begin
		Ada.Dispatching.Yield;
	end;
	-- Ada.Environment_Encoding
	declare
		use Ada.Environment_Encoding;
		procedure Ignore (X : Encoding_Id) with Import;
		Encoding : constant Encoding_Id := UTF_8;
		Conv : Converter;
		pragma Unmodified (Conv);
	begin
		Ignore (Image (Encoding));
		Ignore (Default_Substitute (Encoding));
		Ignore (Min_Size_In_Stream_Elements (Encoding));
		Ignore (Current_Encoding);
		Ignore (Is_Open (Conv));
	end;
	-- Ada.Environment_Variables
	declare
		use Ada.Environment_Variables;
		No_Existing_Name : constant String := "$$$";
	begin
		Ignore (Value (No_Existing_Name));
		Ignore (Value (No_Existing_Name, Default => ""));
		Ignore (Exists (No_Existing_Name));
		Set (No_Existing_Name, "1");
		Clear (No_Existing_Name);
		Clear;
	end;
	-- Ada.Interrupts
--	declare
--		use Ada.Interrupts;
--		Interrupt : constant Interrupt_Id := Interrupt_Id'First;
--		Old_Handler : Parameterless_Handler;
--	begin
--		Ignore (Is_Reserved (Interrupt));
--		Unchecked_Exchange_Handler (Old_Handler, null, Interrupt);
--		Raise_Interrupt (Interrupt);
--	end;
	-- Ada.Locales
--	declare
--		use Ada.Locales;
--		procedure Ignore (X : ISO_639_Alpha_2) with Import;
--		procedure Ignore (X : ISO_639_Alpha_3) with Import;
--	begin
--		Ignore (ISO_639_Alpha_2'(Language));
--		Ignore (ISO_639_Alpha_3'(Language));
--	end;
	-- Ada.Processes
	declare
		use Ada.Processes;
		Command_Line : String (1 .. 10);
		Last : Natural := Command_Line'First - 1;
		Argument : constant String := "-";
		Child : Process;
--		Command : Command_Type;
		Status : Ada.Command_Line.Exit_Status;
	begin
		Append_Argument (Command_Line, Last, Argument);
		Ignore (Is_Open (Child));
--		Shell (Command, Status);
		Shell (Command_Line, Status);
--		Shell (Command);
--		Shell (Command_Line);
	end;
	-- Ada.Streams.Stream_IO.Naked
	declare
		use Ada.Streams.Stream_IO.Naked;
		File : Ada.Streams.Stream_IO.File_Type;
	begin
		Ignore (Non_Controlled (File));
	end;
	-- Ada.Strings.Maps.Constants
	declare
		use Ada.Strings.Maps.Constants;
	begin
		Ignore (Unassigned_Set);
		Ignore (Uppercase_Letter_Set);
		Ignore (Lowercase_Letter_Set);
		Ignore (Titlecase_Letter_Set);
		Ignore (Modifier_Letter_Set);
		Ignore (Other_Letter_Set);
		Ignore (Decimal_Number_Set);
		Ignore (Letter_Number_Set);
		Ignore (Other_Number_Set);
		Ignore (Line_Separator_Set);
		Ignore (Paragraph_Separator_Set);
		Ignore (Control_Set);
		Ignore (Format_Set);
		Ignore (Private_Use_Set);
		Ignore (Surrogate_Set);
		Ignore (Base_Set);
		Ignore (Graphic_Set);
		Ignore (Letter_Set);
		Ignore (Basic_Set);
		Ignore (Decimal_Digit_Set);
		Ignore (Hexadecimal_Digit_Set);
		Ignore (Alphanumeric_Set);
		Ignore (Special_Set);
		Ignore (ISO_646_Set);
		Ignore (Lower_Case_Map);
		Ignore (Upper_Case_Map);
		Ignore (Case_Folding_Map);
		Ignore (Base_Map);
		Ignore (Basic_Map);
	end;
	-- Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants
	declare
		use Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
	begin
		Ignore (Wide_Character_Set);
	end;
	-- Ada.Task_Identification
	declare
		use Ada.Task_Identification;
		procedure Ignore (X : Task_Id) with Import;
	begin
		Ignore (Current_Task);
		Ignore (Environment_Task);
		Abort_Task (Null_Task_Id);
		Ignore (Is_Terminated (Null_Task_Id));
		Ignore (Is_Callable (Null_Task_Id));
		Ignore (Activation_Is_Complete (Null_Task_Id));
	end;
	-- Ada.Text_IO.Naked
	declare
		use Ada.Text_IO.Naked;
		File : Ada.Text_IO.File_Type;
	begin
		Ignore (Non_Controlled (File));
	end;
	-- Ada.Text_IO.Terminal.Colors
	declare
		use Ada.Text_IO.Terminal.Colors;
		procedure Ignore (X : Color) with Import;
		Black : constant Ada.Colors.RGB := (0.0, 0.0, 0.0);
	begin
		Ignore (To_Color (Black));
	end;
	-- System.Program
	declare
		use System.Program;
	begin
		Ignore (System.Program.Load_Address);
	end;
end inline;
