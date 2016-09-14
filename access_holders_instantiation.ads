-- { dg-do assemble }
-- { dg-additional-options -O }
-- { dg-final { scan-assembler-not "__elab[sb]$" } }
-- { dg-final { scan-assembler-not "\tcall\t_*__enable_execute_stack$" } }
pragma Restrictions (No_Elaboration_Code);
pragma Restrictions (No_Implicit_Dynamic_Code);
with Ada.Containers.Access_Holders;
with Ada.Unchecked_Deallocation;
package access_holders_instantiation is
	
	type Character_Access is access Character;
	procedure Free is
		new Ada.Unchecked_Deallocation (Character, Character_Access);
	
	package The_Holders is new Ada.Containers.Access_Holders (Character_Access);
	
end access_holders_instantiation;
