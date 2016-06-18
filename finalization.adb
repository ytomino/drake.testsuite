-- { dg-do run }
with Ada.Finalization;
procedure finalization is
	type C is new Ada.Finalization.Controlled with null record;
	overriding procedure Initialize (Object : in out C);
	overriding procedure Finalize (Object : in out C);
	overriding procedure Initialize (Object : in out C) is
	begin
		Ada.Debug.Put ("init");
	end Initialize;
	overriding procedure Finalize (Object : in out C) is
	begin
		Ada.Debug.Put ("done");
	end Finalize;
	type LC is new Ada.Finalization.Limited_Controlled with null record;
	overriding procedure Initialize (Object : in out LC);
	overriding procedure Finalize (Object : in out LC);
	overriding procedure Initialize (Object : in out LC) is
	begin
		Ada.Debug.Put ("init");
	end Initialize;
	overriding procedure Finalize (Object : in out LC) is
	begin
		Ada.Debug.Put ("done");
	end Finalize;
begin
	declare
		A : C;
		B : LC;
	begin
		null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end finalization;
