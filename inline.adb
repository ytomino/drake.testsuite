-- { dg-do compile }
-- { dg-additional-options "-O2 -gnat-a -gnatp -gnatn2 -Winline -Werror" }
with Ada.Characters.Conversions;
with Ada.Colors;
with Ada.Command_Line;
with Ada.Directories.Temporary;
with Ada.Directories.Volumes;
with Ada.Dispatching;
with Ada.Environment_Encoding;
with Ada.Environment_Variables;
with Ada.Interrupts;
with Ada.Locales;
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
	procedure Ignore (X : Ada.Directories.Volumes.File_System) with Import;
	procedure Ignore (X : Ada.Environment_Encoding.Encoding_Id) with Import;
	procedure Ignore (X : Ada.Locales.ISO_639_Alpha_2) with Import;
	procedure Ignore (X : Ada.Locales.ISO_639_Alpha_3) with Import;
	procedure Ignore (X : Ada.Locales.ISO_3166_1_Alpha_2) with Import;
	procedure Ignore (X : Ada.Processes.Process) with Import;
	procedure Ignore (X : access Ada.Naked_Text_IO.Non_Controlled_File_Type)
		with Import;
	procedure Ignore (
		X : access Ada.Streams.Naked_Stream_IO.Non_Controlled_File_Type)
		with Import;
	procedure Ignore (X : Ada.Streams.Stream_Element_Array) with Import;
	procedure Ignore (X : Ada.Streams.Stream_Element_Offset) with Import;
	procedure Ignore (X : Ada.Strings.Maps.Character_Set) with Import;
	procedure Ignore (X : Ada.Strings.Maps.Character_Mapping) with Import;
	procedure Ignore (X : Ada.Task_Identification.Task_Id) with Import;
	procedure Ignore (X : Ada.Text_IO.Terminal.Colors.Color) with Import;
	procedure Ignore (X : System.Address) with Import;
begin -- inline renamings
	-- private entities required by compiler
	delay 0.0; -- Ada.Calendar.Delays.Delay_For
	-- Ada.Characters.Conversions
	declare
		use Ada.Characters.Conversions;
	begin
		Ignore (To_Wide_String (String'(""), Substitute => "")); -- renamed
		Ignore (To_Wide_Wide_String (String'(""), Substitute => "")); -- renamed
		Ignore (To_Wide_Wide_String (Wide_String'(""), Substitute => ""));
			-- renamed
--		Ignore (To_String (Wide_String'(""), Substitute => ' '));
		Ignore (To_String (Wide_String'(""), Substitute => "")); -- renamed
--		Ignore (To_String (Wide_Wide_String'(""), Substitute => ' '));
		Ignore (To_String (Wide_Wide_String'(""), Substitute => "")); -- renamed
--		Ignore (To_Wide_String (Wide_Wide_String'(""), Substitute => ' '));
		Ignore (To_Wide_String (Wide_Wide_String'(""), Substitute => ""));
			-- renamed
	end;
	-- Ada.Command_Line
	declare
		use Ada.Command_Line;
	begin
		Ignore (Argument_Count); -- renamed
	end;
	-- Ada.Directories
	declare
		use Ada.Directories;
		No_Existing_Name : constant String := "$$$";
	begin
		Ignore (Current_Directory); -- renamed
		Set_Directory (No_Existing_Name); -- renamed
		Delete_Directory (No_Existing_Name); -- renamed
		Delete_File (No_Existing_Name); -- renamed
		Rename (No_Existing_Name, No_Existing_Name, Overwrite => False);
			-- renamed
--		Copy_File (No_Existing_Name, No_Existing_Name,
--			Form => "overwrite=false");
		Copy_File (No_Existing_Name, No_Existing_Name, Overwrite => False);
			-- renamed
		Replace_File (No_Existing_Name, No_Existing_Name); -- renamed
		Symbolic_Link (No_Existing_Name, No_Existing_Name, Overwrite => False);
			-- renamed
		Ignore (Full_Name (No_Existing_Name)); -- renamed
		Ignore (Exists (No_Existing_Name)); -- renamed
		-- Ada.Directories.Temporary
		declare
			use Temporary;
		begin
			Ignore (Temporary_Directory); -- renamed
			Set_Temporary_Directory (No_Existing_Name); -- renamed
			Ignore (Create_Temporary_File (Directory => No_Existing_Name));
				-- renamed
			Ignore (Create_Temporary_Directory (Directory => No_Existing_Name));
				-- renamed
		end;
		-- Ada.Directories.Volumes
		declare
			use Volumes;
		begin
			Ignore (Where (No_Existing_Name)); -- renamed
		end;
	end;
	-- Ada.Dispatching
	declare
		use Ada.Dispatching;
	begin
		Ada.Dispatching.Yield; -- renamed
	end;
	-- Ada.Environment_Encoding
	declare
		use Ada.Environment_Encoding;
		Encoding : constant Encoding_Id := UTF_8;
--		Conv : Converter;
	begin
		Ignore (Image (Encoding)); -- renamed
		Ignore (Default_Substitute (Encoding)); -- renamed
		Ignore (Min_Size_In_Stream_Elements (Encoding)); -- renamed
		Ignore (Current_Encoding); -- renamed
--		Ignore (Is_Open (Conv));
	end;
	-- Ada.Environment_Variables
	declare
		use Ada.Environment_Variables;
		No_Existing_Name : constant String := "$$$";
	begin
		Ignore (Value (No_Existing_Name)); -- renamed
		Ignore (Value (No_Existing_Name, Default => "")); -- renamed
		Ignore (Exists (No_Existing_Name)); -- renamed
		Set (No_Existing_Name, "1"); -- renamed
		Clear (No_Existing_Name); -- renamed
		Clear; -- renamed
	end;
	-- Ada.Interrupts
	declare
		use Ada.Interrupts;
		Interrupt : constant Interrupt_Id := Interrupt_Id'First;
		Old_Handler : Parameterless_Handler;
	begin
		Ignore (Is_Reserved (Interrupt)); -- renamed
		Unchecked_Exchange_Handler (Old_Handler, null, Interrupt); -- renamed
		Raise_Interrupt (Interrupt); -- renamed
	end;
	-- Ada.Locales
	declare
		use Ada.Locales;
	begin
		Ignore (ISO_639_Alpha_2'(Language)); -- renamed
		Ignore (ISO_639_Alpha_3'(Language)); -- renamed
		Ignore (Country); -- renamed
	end;
	-- Ada.Processes
	declare
		use Ada.Processes;
		Command_Line : String (1 .. 10);
		Last : Natural := Command_Line'First - 1;
		Argument : constant String := "-";
--		Child : Process;
		Command : Command_Type;
--		Status : Ada.Command_Line.Exit_Status;
	begin
		Append_Argument (Command_Line, Last, Argument); -- renamed
--		Ignore (Is_Open (Child));
--		Create (Child, Command);
--		Create (Child, Command_Line);
		Ignore (Create (Command)); -- renamed
		Ignore (Create (Command_Line)); -- renamed
--		Shell (Command, Status);
--		Shell (Command_Line, Status);
--		Shell (Command);
--		Shell (Command_Line);
	end;
	-- Ada.Streams.Stream_IO.Naked
	declare
		use Ada.Streams.Stream_IO.Naked;
		File : Ada.Streams.Stream_IO.File_Type;
	begin
		Ignore (Non_Controlled (File)); -- renamed
	end;
	-- Ada.Strings.Maps.Constants
	declare
		use Ada.Strings.Maps.Constants;
	begin
		Ignore (Unassigned_Set); -- renamed
		Ignore (Uppercase_Letter_Set); -- renamed
		Ignore (Lowercase_Letter_Set); -- renamed
		Ignore (Titlecase_Letter_Set); -- renamed
		Ignore (Modifier_Letter_Set); -- renamed
		Ignore (Other_Letter_Set); -- renamed
		Ignore (Decimal_Number_Set); -- renamed
		Ignore (Letter_Number_Set); -- renamed
		Ignore (Other_Number_Set); -- renamed
		Ignore (Line_Separator_Set); -- renamed
		Ignore (Paragraph_Separator_Set); -- renamed
		Ignore (Control_Set); -- renamed
		Ignore (Format_Set); -- renamed
		Ignore (Private_Use_Set); -- renamed
		Ignore (Surrogate_Set); -- renamed
		Ignore (Base_Set); -- renamed
		Ignore (Graphic_Set); -- renamed
		Ignore (Letter_Set); -- renamed
		Ignore (Basic_Set); -- renamed
		Ignore (Decimal_Digit_Set); -- renamed
		Ignore (Hexadecimal_Digit_Set); -- renamed
		Ignore (Alphanumeric_Set); -- renamed
		Ignore (Special_Set); -- renamed
		Ignore (ISO_646_Set); -- renamed
		Ignore (Lower_Case_Map); -- renamed
		Ignore (Upper_Case_Map); -- renamed
		Ignore (Case_Folding_Map); -- renamed
		Ignore (Base_Map); -- renamed
		Ignore (Basic_Map); -- renamed
	end;
	-- Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants
	declare
		use Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
	begin
		Ignore (Wide_Character_Set); -- renamed
	end;
	-- Ada.Task_Identification
	declare
		use Ada.Task_Identification;
	begin
		Ignore (Current_Task); -- renamed
		Ignore (Environment_Task); -- renamed
		Abort_Task (Null_Task_Id); -- renamed
		Ignore (Is_Terminated (Null_Task_Id)); -- renamed
		Ignore (Is_Callable (Null_Task_Id)); -- renamed
		Ignore (Activation_Is_Complete (Null_Task_Id)); -- renamed
	end;
	-- Ada.Text_IO.Naked
	declare
		use Ada.Text_IO.Naked;
		File : Ada.Text_IO.File_Type;
	begin
		Ignore (Non_Controlled (File)); -- renamed
	end;
	-- Ada.Text_IO.Terminal.Colors
	declare
		use Ada.Text_IO.Terminal.Colors;
		Black : constant Ada.Colors.RGB := (0.0, 0.0, 0.0);
	begin
		Ignore (To_Color (Black)); -- renamed
	end;
	-- System.Program
	declare
		use System.Program;
	begin
		Ignore (Load_Address); -- renamed
	end;
end inline;
