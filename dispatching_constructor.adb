-- { dg-do run }
with Ada.Tags.Generic_Dispatching_Constructor;
procedure dispatching_constructor is
	use type Ada.Tags.Tag;
	package Root is
		type T is abstract tagged limited null record;
		function Create (Params : not null access Integer) return T is abstract;
	end Root;
	Derived_Creation_Count : Natural := 0;
	package Derived is
		type T is new Root.T with null record;
		overriding function Create (Params : not null access Integer) return T;
	end Derived;
	package body Derived is
		overriding function Create (Params : not null access Integer) return T is
			pragma Unreferenced (Params);
		begin
			Derived_Creation_Count := Derived_Creation_Count + 1;
			return (Root.T with null record);
		end Create;
	end Derived;
	function Virtual_Create is new Ada.Tags.Generic_Dispatching_Constructor (
		Root.T,
		Integer,
		Root.Create);
	Params : aliased Integer := 10;
	Obj : Root.T'Class := Virtual_Create (Derived.T'Tag, Params'Access);
begin
	pragma Assert (Obj'Tag = Derived.T'Tag);
	pragma Assert (Derived_Creation_Count = 1);
	pragma Debug (Ada.Debug.Put ("OK"));
	null;
end dispatching_constructor;
