-- { dg-do run }
with Ada;
with Interfaces.COBOL;
procedure cobol_alphanumeric_thresholds is
	use Interfaces.COBOL;
begin
	-- To_COBOL
	declare
		In_First_Zero : constant String (1 .. 0) := "";
		In_Last_Zero : constant String (Integer'Last .. Integer'Last - 1) := "";
		Out_First_Zero : Alphanumeric (1 .. 0);
		Out_Last_Zero : Alphanumeric (Integer'Last .. Integer'Last - 1);
		Last : Natural;
	begin
		To_COBOL (In_First_Zero, Out_First_Zero, Last);
		pragma Assert (Last = Out_First_Zero'First - 1);
		To_COBOL (In_First_Zero, Out_Last_Zero, Last);
		pragma Assert (Last = Out_Last_Zero'First - 1);
		To_COBOL (In_Last_Zero, Out_First_Zero, Last);
		pragma Assert (Last = Out_First_Zero'First - 1);
		To_COBOL (In_Last_Zero, Out_Last_Zero, Last);
		pragma Assert (Last = Out_Last_Zero'First - 1);
	end;
	-- To_Ada
	declare
		In_First_Zero : constant Alphanumeric (1 .. 0) := "";
		In_Last_Zero : constant Alphanumeric (Integer'Last .. Integer'Last - 1) := "";
		Out_First_Zero : String (1 .. 0);
		Out_Last_Zero : String (Integer'Last .. Integer'Last - 1);
		Last : Natural;
	begin
		To_Ada (In_First_Zero, Out_First_Zero, Last);
		pragma Assert (Last = Out_First_Zero'First - 1);
		To_Ada (In_First_Zero, Out_Last_Zero, Last);
		pragma Assert (Last = Out_Last_Zero'First - 1);
		To_Ada (In_Last_Zero, Out_First_Zero, Last);
		pragma Assert (Last = Out_First_Zero'First - 1);
		To_Ada (In_Last_Zero, Out_Last_Zero, Last);
		pragma Assert (Last = Out_Last_Zero'First - 1);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end cobol_alphanumeric_thresholds;
