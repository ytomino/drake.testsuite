-- { dg-do run }
with Ada.Finalization;
procedure finalization is
	C_Count : Natural := 0;
	type C is new Ada.Finalization.Controlled with null record;
	overriding procedure Initialize (Object : in out C);
	overriding procedure Finalize (Object : in out C);
	overriding procedure Initialize (Object : in out C) is
		pragma Unreferenced (Object);
	begin
		pragma Assert (C_Count = 0);
		C_Count := C_Count + 1;
	end Initialize;
	overriding procedure Finalize (Object : in out C) is
		pragma Unreferenced (Object);
	begin
		pragma Assert (C_Count = 1);
		C_Count := C_Count - 1;
	end Finalize;
	LC_Count : Natural := 0;
	type LC is new Ada.Finalization.Limited_Controlled with null record;
	overriding procedure Initialize (Object : in out LC);
	overriding procedure Finalize (Object : in out LC);
	overriding procedure Initialize (Object : in out LC) is
		pragma Unreferenced (Object);
	begin
		pragma Assert (LC_Count = 0);
		LC_Count := LC_Count + 1;
	end Initialize;
	overriding procedure Finalize (Object : in out LC) is
		pragma Unreferenced (Object);
	begin
		pragma Assert (LC_Count = 1);
		LC_Count := LC_Count - 1;
	end Finalize;
begin
	declare
		A : C;
		pragma Unreferenced (A);
		B : LC;
		pragma Unreferenced (B);
	begin
		pragma Assert (C_Count = 1);
		pragma Assert (LC_Count = 1);
		null;
	end;
	pragma Assert (C_Count = 0);
	pragma Assert (LC_Count = 0);
	pragma Debug (Ada.Debug.Put ("OK"));
end finalization;
