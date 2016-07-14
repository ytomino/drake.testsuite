-- { dg-do run }
with Ada.Tags.Generic_Dispatching_Constructor;
procedure dispatching_constructor is
	use type Ada.Tags.Tag;
begin
	-- tagged
	declare
		package Root is
			type T is abstract tagged limited null record;
			function Create (Params : not null access Integer) return T is
				abstract;
		end Root;
		Derived_Creation_Count : Natural := 0;
		package Derived is
			type T is new Root.T with null record;
			overriding function Create (Params : not null access Integer)
				return T;
		end Derived;
		package body Derived is
			overriding function Create (Params : not null access Integer)
				return T
			is
				pragma Unreferenced (Params);
			begin
				Derived_Creation_Count := Derived_Creation_Count + 1;
				return (Root.T with null record);
			end Create;
		end Derived;
		function Virtual_Create is
			new Ada.Tags.Generic_Dispatching_Constructor (
				Root.T,
				Integer,
				Root.Create);
		Params : aliased Integer := 10;
		Obj : Root.T'Class := Virtual_Create (Derived.T'Tag, Params'Access);
	begin
		pragma Assert (Obj'Tag = Derived.T'Tag);
		pragma Assert (Derived_Creation_Count = 1);
		null;
	end;
	-- interface
	declare
		package Root is
			type I is limited interface;
			function Create (Params : not null access Integer) return I is
				abstract;
		end Root;
		Derived_Creation_Count : Natural := 0;
		package Derived is
			type T is new Root.I with null record;
			overriding function Create (Params : not null access Integer)
				return T;
		end Derived;
		package body Derived is
			overriding function Create (Params : not null access Integer)
				return T
			is
				pragma Unreferenced (Params);
			begin
				Derived_Creation_Count := Derived_Creation_Count + 1;
				return (Root.I with null record);
			end Create;
		end Derived;
		function Virtual_Create is
			new Ada.Tags.Generic_Dispatching_Constructor (
				Root.I,
				Integer,
				Root.Create);
		Params : aliased Integer := 10;
		Obj : Root.I'Class := Virtual_Create (Derived.T'Tag, Params'Access);
	begin
		pragma Assert (Obj'Tag = Derived.T'Tag);
		pragma Assert (Derived_Creation_Count = 1);
		begin
			declare
				Bad : Root.I'Class := Virtual_Create (Root.I'Tag, Params'Access);
				pragma Unreferenced (Bad);
			begin
				raise Program_Error; -- unreachable
			end;
		exception
			when Ada.Tags.Tag_Error => null;
		end;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end dispatching_constructor;
