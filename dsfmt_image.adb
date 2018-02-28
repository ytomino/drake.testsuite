-- { dg-do run }
with Ada.Numerics.dSFMT_19937;
procedure dsfmt_image is
	package dSFMT renames Ada.Numerics.dSFMT_19937;
	use type dSFMT.State;
	procedure Check (Initiator : dSFMT.Unsigned_32) is
		State : constant dSFMT.State := dSFMT.Initialize (Initiator);
		S : constant String := dSFMT.Image (State);
		State_2 : constant dSFMT.State := dSFMT.Value (S);
	begin
--		Ada.Debug.Put ('"' & S & '"');
		for I in S'Range loop
			pragma Assert (
				if I rem 17 = 0 then
					S (I) = ':'
				else
					S (I) in '0' .. '9' | 'A' .. 'F');
			null;
		end loop;
		pragma Assert (State = State_2);
	end Check;
begin
	Check (0);
	Check (dSFMT.Unsigned_32'Last);
	Check (dSFMT.Default_Initiator);
	pragma Debug (Ada.Debug.Put ("OK"));
end dsfmt_image;
