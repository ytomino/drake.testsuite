-- { dg-do run }
with Ada.Numerics.MT19937;
procedure mt19937_image is
	package MT19937 renames Ada.Numerics.MT19937;
	use type MT19937.State;
	procedure Check (Initiator : MT19937.Unsigned_32) is
		State : constant MT19937.State := MT19937.Initialize (Initiator);
		S : constant String := MT19937.Image (State);
		State_2 : constant MT19937.State := MT19937.Value (S);
	begin
--		Ada.Debug.Put ('"' & S & '"');
		for I in S'Range loop
			pragma Assert (
				if I rem 9 = 0 then
					S (I) = ':'
				else
					S (I) in '0' .. '9' | 'A' .. 'F');
			null;
		end loop;
		pragma Assert (State = State_2);
	end Check;
begin
	Check (0);
	Check (MT19937.Unsigned_32'Last);
	Check (MT19937.Default_Initiator);
	pragma Debug (Ada.Debug.Put ("OK"));
end mt19937_image;
