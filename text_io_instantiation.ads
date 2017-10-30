-- { dg-do compile }
-- { dg-additional-options "-O -ftrampolines" }
-- { dg-final { scan-assembler-not "__elab\[sb\]" } }
-- { dg-final { scan-assembler-not "__enable_execute_stack" } }
pragma Restrictions (No_Elaboration_Code);
pragma Restrictions (No_Implicit_Dynamic_Code);
with Ada.Numerics.Complex_Types;
with Ada.Strings.Bounded;
with Ada.Text_IO.Bounded_IO;
with Ada.Text_IO.Complex_IO;
with Ada.Wide_Text_IO.Complex_IO;
with Ada.Wide_Wide_Text_IO.Complex_IO;
package text_io_instantiation is
	
	type Unsigned is mod 2 ** Integer'Size;
	subtype Enumeration_1 is Boolean;
	subtype Enumeration_2 is Character;
	type Fixed_1 is delta 0.25 range -128.0 .. 128.0;
	type Fixed_2 is delta 4.0 range -128.0 .. 128.0;
	type Decimal_1 is delta 0.1 digits 3;
	type Decimal_2 is delta 10.0 digits 3;
	
	package Complex_Types renames Ada.Numerics.Complex_Types;
	
	package Bounded_Length is
		new Ada.Strings.Bounded.Generic_Bounded_Length (10);
	
	-- Text_IO
	
	package Integer_Text_IO is new Ada.Text_IO.Integer_IO (Integer);
	package Unsigned_Text_IO is new Ada.Text_IO.Modular_IO (Unsigned);
	package Enumeration_1_Text_IO is
		new Ada.Text_IO.Enumeration_IO (Enumeration_1);
	package Enumeration_2_Text_IO is
		new Ada.Text_IO.Enumeration_IO (Enumeration_2);
	package Float_Text_IO is new Ada.Text_IO.Float_IO (Float);
	package Fixed_1_Text_IO is new Ada.Text_IO.Fixed_IO (Fixed_1);
	package Fixed_2_Text_IO is new Ada.Text_IO.Fixed_IO (Fixed_2);
	package Decimal_1_Text_IO is new Ada.Text_IO.Decimal_IO (Decimal_1);
	package Decimal_2_Text_IO is new Ada.Text_IO.Decimal_IO (Decimal_2);
	
	package Complex_Text_IO is new Ada.Text_IO.Complex_IO (Complex_Types);
	
	package Bounded_Text_IO is new Ada.Text_IO.Bounded_IO (Bounded_Length);
	
	-- Wide_Text_IO
	
	package Integer_Wide_Text_IO is new Ada.Wide_Text_IO.Integer_IO (Integer);
	package Unsigned_Wide_Text_IO is new Ada.Wide_Text_IO.Modular_IO (Unsigned);
	package Enumeration_1_Wide_Text_IO is
		new Ada.Wide_Text_IO.Enumeration_IO (Enumeration_1);
	package Enumeration_2_Wide_Text_IO is
		new Ada.Wide_Text_IO.Enumeration_IO (Enumeration_2);
	package Float_Wide_Text_IO is new Ada.Wide_Text_IO.Float_IO (Float);
	package Fixed_1_Wide_Text_IO is new Ada.Wide_Text_IO.Fixed_IO (Fixed_1);
	package Fixed_2_Wide_Text_IO is new Ada.Wide_Text_IO.Fixed_IO (Fixed_2);
	package Decimal_1_Wide_Text_IO is
		new Ada.Wide_Text_IO.Decimal_IO (Decimal_1);
	package Decimal_2_Wide_Text_IO is
		new Ada.Wide_Text_IO.Decimal_IO (Decimal_2);
	
	package Complex_Wide_Text_IO is
		new Ada.Wide_Text_IO.Complex_IO (Complex_Types);
	
	-- Wide_Wide_Text_IO
	
	package Integer_Wide_Wide_Text_IO is
		new Ada.Wide_Wide_Text_IO.Integer_IO (Integer);
	package Unsigned_Wide_Wide_Text_IO is
		new Ada.Wide_Wide_Text_IO.Modular_IO (Unsigned);
	package Enumeration_1_Wide_Wide_Text_IO is
		new Ada.Wide_Wide_Text_IO.Enumeration_IO (Enumeration_1);
	package Enumeration_2_Wide_Wide_Text_IO is
		new Ada.Wide_Wide_Text_IO.Enumeration_IO (Enumeration_2);
	package Float_Wide_Wide_Text_IO is
		new Ada.Wide_Wide_Text_IO.Float_IO (Float);
	package Fixed_1_Wide_Wide_Text_IO is
		new Ada.Wide_Wide_Text_IO.Fixed_IO (Fixed_1);
	package Fixed_2_Wide_Wide_Text_IO is
		new Ada.Wide_Wide_Text_IO.Fixed_IO (Fixed_2);
	package Decimal_1_Wide_Wide_Text_IO is
		new Ada.Wide_Wide_Text_IO.Decimal_IO (Decimal_1);
	package Decimal_2_Wide_Wide_Text_IO is
		new Ada.Wide_Wide_Text_IO.Decimal_IO (Decimal_2);
	
	package Complex_Wide_Wide_Text_IO is
		new Ada.Wide_Wide_Text_IO.Complex_IO (Complex_Types);
	
end text_io_instantiation;
