-- { dg-do run }
with Ada.Execution_Time;
with Ada.Real_Time;
with Ada.Unchecked_Conversion;
with System.Arith_64; -- why is __gnat_mulv64 required ???
pragma Unreferenced (System.Arith_64);
procedure delay_1 is
	use type Ada.Execution_Time.CPU_Time;
	use type Ada.Real_Time.Time;
	use type Ada.Real_Time.Time_Span;
	First_RT : Ada.Real_Time.Time := Ada.Real_Time.Clock;
	First_CT : Ada.Execution_Time.CPU_Time := Ada.Execution_Time.Clock;
	Now_RT : Ada.Real_Time.Time;
	Now_CT : Ada.Execution_Time.CPU_Time;
begin
	delay 1.0;
	Now_RT := Ada.Real_Time.Clock;
	Now_CT := Ada.Execution_Time.Clock;
	declare
		RD : constant Duration := Ada.Real_Time.To_Duration (Now_RT - First_RT);
		CD : constant Duration := Ada.Real_Time.To_Duration (Now_CT - First_CT);
	begin
		Ada.Debug.Put ("Real Time:" & Duration'Image (RD));
		Ada.Debug.Put ("CPU Time: " & Duration'Image (CD));
		pragma Assert (RD >= 1.0);
		pragma Assert (CD < 0.5);
	end;
	-- Time_Span
	pragma Assert (Ada.Real_Time.To_Time_Span (1.0) + Ada.Real_Time.To_Time_Span (2.0) = Ada.Real_Time.To_Time_Span (3.0));
	pragma Assert (Ada.Real_Time.To_Time_Span (3.0) - Ada.Real_Time.To_Time_Span (2.0) = Ada.Real_Time.To_Time_Span (1.0));
	pragma Assert (-Ada.Real_Time.To_Time_Span (1.0) = Ada.Real_Time.To_Time_Span (-1.0));
	pragma Assert (Ada.Real_Time.To_Time_Span (2.0) * 3 = Ada.Real_Time.To_Time_Span (6.0));
	pragma Assert (Ada.Real_Time.To_Time_Span (6.0) / Ada.Real_Time.To_Time_Span (2.0) = 3);
	pragma Assert (Ada.Real_Time.To_Time_Span (6.0) / 2 = Ada.Real_Time.To_Time_Span (3.0));
	pragma Assert (abs Ada.Real_Time.To_Time_Span (1.0) = Ada.Real_Time.To_Time_Span (1.0));
	pragma Assert (abs Ada.Real_Time.To_Time_Span (-1.0) = Ada.Real_Time.To_Time_Span (1.0));
	-- Time and Time_Span
	pragma Assert (Now_RT + Ada.Real_Time.To_Time_Span (2.0) - Now_RT = Ada.Real_Time.To_Time_Span (2.0));
	pragma Assert (Ada.Real_Time.To_Time_Span (2.0) + Now_RT = Now_RT + Ada.Real_Time.To_Time_Span (2.0));
	pragma Assert (Now_RT - Ada.Real_Time.To_Time_Span (2.0) = Now_RT + Ada.Real_Time.To_Time_Span (-2.0));
	-- Execution_Time.Time is started from 0
	declare
		function To_Duration is new Ada.Unchecked_Conversion (Ada.Execution_Time.CPU_Time, Duration);
	begin
		pragma Assert (To_Duration (First_CT) in 0.0 .. 0.5);
		pragma Assert (To_Duration (Now_CT) in 0.0 .. 0.5); -- "delay" do not use CPU time
		null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end delay_1;
