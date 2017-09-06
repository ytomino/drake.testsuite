-- { dg-do assemble }
-- { dg-additional-options -O }
-- { dg-final { scan-assembler-not "__elab[sb](@PLT)?$" } }
-- { dg-final { scan-assembler-not "\tcall\t_*__enable_execute_stack(@PLT)?$" } }
pragma Restrictions (No_Elaboration_Code);
pragma Restrictions (No_Implicit_Dynamic_Code);
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Limited_Doubly_Linked_Lists;
with Ada.Containers.Limited_Hashed_Maps;
with Ada.Containers.Limited_Hashed_Sets;
with Ada.Containers.Limited_Ordered_Maps;
with Ada.Containers.Limited_Ordered_Sets;
with Ada.Containers.Limited_Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
package containers_instantiation is
	
	package The_Vectors is new Ada.Containers.Vectors (Positive, Character);
	
	package The_Lists is new Ada.Containers.Doubly_Linked_Lists (Character);
	
	function Hash (Item : Positive) return Ada.Containers.Hash_Type is
		(Ada.Containers.Hash_Type (Item));
	
	package The_H_Maps is
		new Ada.Containers.Hashed_Maps (Positive, Character,
			Hash => Hash, Equivalent_Keys => "=");
	
	package The_O_Maps is new Ada.Containers.Ordered_Maps (Positive, Character);
	
	function Hash (Item : Character) return Ada.Containers.Hash_Type is
		(Character'Pos (Item));
	
	package The_H_Sets is
		new Ada.Containers.Hashed_Sets (Character,
			Hash => Hash, Equivalent_Elements => "=");
	
	package The_O_Sets is new Ada.Containers.Ordered_Sets (Character);
	
	-- indefinite containers
	
	package The_I_Vectors is
		new Ada.Containers.Indefinite_Vectors (Positive, Character);
	
	package The_I_Lists is
		new Ada.Containers.Indefinite_Doubly_Linked_Lists (Character);
	
	package The_I_H_Maps is
		new Ada.Containers.Indefinite_Hashed_Maps (Positive, Character,
			Hash => Hash, Equivalent_Keys => "=");
	
	package The_I_O_Maps is
		new Ada.Containers.Indefinite_Ordered_Maps (Character, Character);
	
	package The_I_H_Sets is
		new Ada.Containers.Indefinite_Hashed_Sets (Character,
			Hash => Hash, Equivalent_Elements => "=");
	
	package The_I_O_Sets is
		new Ada.Containers.Indefinite_Ordered_Sets (Character);
	
	package The_I_Holders is new Ada.Containers.Indefinite_Holders (Character);
	
	-- limited containers
	
	package The_L_Vectors is
		new Ada.Containers.Limited_Vectors (Positive, Character);
	
	package The_L_Lists is
		new Ada.Containers.Limited_Doubly_Linked_Lists (Character);
	
	package The_L_H_Maps is
		new Ada.Containers.Limited_Hashed_Maps (Positive, Character,
			Hash => Hash, Equivalent_Keys => "=");
	
	package The_L_O_Maps is
		new Ada.Containers.Limited_Ordered_Maps (Positive, Character);
	
	package The_L_H_Sets is
		new Ada.Containers.Limited_Hashed_Sets (Character,
			Hash => Hash, Equivalent_Elements => "=");
	
	package The_L_O_Sets is new Ada.Containers.Limited_Ordered_Sets (Character);
	
end containers_instantiation;
