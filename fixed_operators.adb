-- { dg-do run }
with Ada;
procedure fixed_operators is
begin
	-- ordinary fixed (power of 2)
	declare
		type T4 is delta 0.01 range 0.00 .. 1.99;
		pragma Assert (T4'Aft = 2);
	begin
		-- ordinary fixed type does not have 'Scale attribute.
		pragma Assert (T4'Delta = T4'(0.01));
		pragma Assert (T4'Small = T4'(0.01));
		pragma Assert (T4'Image (1.45) = " 1.45");
		pragma Assert (T4'Mantissa = 8); -- log 199 / log 2 = 7.6...
	end;
	declare
		type T5 is delta 100.0 range -10000.0 .. 10000.0;
		pragma Assert (T5'Aft = 1);
	begin
		-- ordinary fixed type does not have 'Scale attribute.
		pragma Assert (T5'Delta = T5'(100.0));
		pragma Assert (T5'Small = T5'(100.0));
		pragma Assert (T5'Image (-128.0) = "-128.0");
		pragma Assert (T5'Mantissa in 7 .. 8); -- log 100 / log 2 = 6.6...
		-- but T5'Mantissa = 8, not 7. errors in calculation?
	end;
	-- just 64bit
	declare
		type T6 is delta 0.25 range 0.0 .. 16#0fff_ffff_ffff_ffff.c#;
		X : T6 := T6'Last;
		Y : T6 := X / T6'Value (T6'Image (T6'Last));
	begin
		pragma Assert (Y = 1.0);
		pragma Assert (T6'Mantissa = 62);
		null;
	end;
	-- over 64bit
	pragma Assert (Duration'Mantissa = Duration'Size - 1);
	declare -- X > Y
		X : Duration := 16#1_ffff_ffff.0#;
		Y : Duration := X / Duration'Value ("2.0");
	begin
		pragma Assert (Y = 16#0_ffff_ffff.8#);
		null;
	end;
	declare -- X < X
		X : Duration := 16#1_0000_0000.0#;
		Y : Duration := X / Duration'Value ("16#2_0000_0000.0#");
	begin
		pragma Assert (Y = 0.5);
		null;
	end;
	-- dynamic range
	declare
		type T is delta 1.0 range -256.0 .. 255.0;
		subtype S0 is T range 0.0 .. T'Value ("0.0");
		subtype S1 is T range 0.0 .. T'Value ("1.0");
		subtype S6 is T range 0.0 .. T'Value ("63.0");
		subtype S7 is T range 0.0 .. T'Value ("64.0");
		subtype M6 is T range T'Value ("-64.0") .. 0.0;
		subtype M7 is T range T'Value ("-65.0") .. 0.0;
	begin
		pragma Assert (T'Mantissa = 8);
		pragma Assert (S0'Mantissa = 0);
		pragma Assert (S1'Mantissa = 1);
		pragma Assert (S6'Mantissa = 6);
		pragma Assert (S7'Mantissa = 7);
		pragma Assert (M6'Mantissa = 6);
		pragma Assert (M7'Mantissa = 7);
		null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end fixed_operators;
