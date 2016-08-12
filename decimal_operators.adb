-- { dg-do run }
with Ada;
procedure decimal_operators is
begin
	-- deciaml fixed (power of 10)
	declare
		type T1 is delta 0.1 digits 10;
		pragma Assert (T1'Aft = 1);
	begin
		pragma Assert (T1'Scale = 1);
		pragma Assert (T1'Delta = 0.1);
		pragma Assert (T1'Small = 0.1);
		pragma Assert (T1'Image (-1.2) = "-1.2");
		pragma Assert (T1'Digits = 10);
		pragma Assert (T1'Mantissa = 34); -- log 1e10 / log 2 = 33.2...
		declare
			subtype S1 is T1 range 0.0 .. T1'Value ("1.0");
		begin
			pragma Assert (S1'Digits = T1'Digits); -- do not reflect the range
			pragma Assert (S1'Mantissa = 4); -- log 10 / log 2 = 3.32...
			null;
		end;
	end;
	declare
		type T2 is delta 1.0 digits 10;
	begin
		pragma Assert (T2'Scale = 0);
		pragma Assert (T2'Delta = 1.0);
		pragma Assert (T2'Small = 1.0);
		pragma Assert (T2'Image (-12.0) = "-12.0");
		pragma Assert (T2'Digits = 10);
		pragma Assert (T2'Mantissa = 34);
		null;
	end;
	declare
		type T3 is delta 10.0 digits 10;
	begin
		pragma Assert (T3'Scale = -1);
		pragma Assert (T3'Delta = 10.0);
		pragma Assert (T3'Small = 10.0);
		pragma Assert (T3'Image (-120.0) = "-120.0");
		pragma Assert (T3'Digits = 10);
		pragma Assert (T3'Mantissa = 34);
		null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end decimal_operators;
