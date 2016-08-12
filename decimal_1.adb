-- { dg-do run }
with Ada.Decimal;
procedure decimal_1 is
begin
	declare
		type T1 is delta 0.1 digits 10;
		procedure Divide is new Ada.Decimal.Divide (T1, T1, T1, T1);
		Q, R : T1;
	begin
		Divide (5.0, 0.5, Q, R);
		pragma Assert (Q = 10.0 and then R = 0.0);
		Divide (0.5, 1.2, Q, R);
		pragma Assert (Q = 0.4 and then R = 0.0); -- R = 0.02
	end;
	declare
		type T2 is delta 1.0 digits 10;
		procedure Divide is new Ada.Decimal.Divide (T2, T2, T2, T2);
		Q2, R2 : T2;
	begin
		Divide (50.0, 12.0, Q2, R2);
		pragma Assert (Q2 = 4.0 and then R2 = 2.0);
	end;
	declare
		type T3 is delta 10.0 digits 10;
		procedure Divide is new Ada.Decimal.Divide (T3, T3, T3, T3);
		Q3, R3 : T3;
	begin
		Divide (5000.0, 120.0, Q3, R3);
		pragma Assert (Q3 = 40.0 and then R3 = 200.0);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end decimal_1;
