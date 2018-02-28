-- { dg-do run }
with Ada.Numerics.SFMT_19937;
procedure sfmt_image is
	package SFMT renames Ada.Numerics.SFMT_19937;
	use type SFMT.State;
	procedure Check (Initiator : SFMT.Unsigned_32) is
		State : constant SFMT.State := SFMT.Initialize (Initiator);
		S : constant String := SFMT.Image (State);
		State_2 : constant SFMT.State := SFMT.Value (S);
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
	Check (SFMT.Unsigned_32'Last);
	Check (SFMT.Default_Initiator);
	pragma Debug (Ada.Debug.Put ("OK"));
end sfmt_image;
