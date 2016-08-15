-- { dg-do run }
with Ada.Fixed;
procedure fixed_1 is
begin
	-- Dividend_Type'Small = Divisor_Type'Small
	declare
		type X_Type is delta 0.500 range 0.0 .. 9999.0;
		type Q_Type is delta 0.250 range 0.0 .. 9999.0;
		type R_Type is delta 0.125 range 0.0 .. 9999.0;
		procedure Divide is
			new Ada.Fixed.Divide (X_Type, X_Type, Q_Type, R_Type);
		Q : Q_Type;
		R : R_Type;
	begin
		Divide (10.0, 3.0, Q, R);
		pragma Assert (Q = 3.0 and then R = 1.0);
	end;
	declare
		type X_Type is delta 0.125 range 0.0 .. 9999.0;
		type Q_Type is delta 0.250 range 0.0 .. 9999.0;
		type R_Type is delta 0.500 range 0.0 .. 9999.0;
		procedure Divide is
			new Ada.Fixed.Divide (X_Type, X_Type, Q_Type, R_Type);
		Q : Q_Type;
		R : R_Type;
	begin
		Divide (10.0, 3.0, Q, R);
		pragma Assert (Q = 3.0 and then R = 1.0);
	end;
	declare
		type X_Type is delta 2.0 range 0.0 .. 9999.0;
		type Q_Type is delta 1.0 range 0.0 .. 9999.0;
		type R_Type is delta 0.5 range 0.0 .. 9999.0;
		procedure Divide is
			new Ada.Fixed.Divide (X_Type, X_Type, Q_Type, R_Type);
		Q : Q_Type;
		R : R_Type;
	begin
		Divide (10.0, 4.0, Q, R);
		pragma Assert (Q = 2.0 and then R = 2.0);
	end;
	declare
		type X_Type is delta 0.5 range 0.0 .. 9999.0;
		type Q_Type is delta 1.0 range 0.0 .. 9999.0;
		type R_Type is delta 2.0 range 0.0 .. 9999.0;
		procedure Divide is
			new Ada.Fixed.Divide (X_Type, X_Type, Q_Type, R_Type);
		Q : Q_Type;
		R : R_Type;
	begin
		Divide (11.0, 3.0, Q, R);
		pragma Assert (Q = 3.0 and then R = 2.0);
	end;
	declare
		type X_Type is delta 8.0 range 0.0 .. 9999.0;
		type Q_Type is delta 4.0 range 0.0 .. 9999.0;
		type R_Type is delta 2.0 range 0.0 .. 9999.0;
		procedure Divide is
			new Ada.Fixed.Divide (X_Type, X_Type, Q_Type, R_Type);
		Q : Q_Type;
		R : R_Type;
	begin
		Divide (32.0, 8.0, Q, R);
		pragma Assert (Q = 4.0 and then R = 0.0);
	end;
	declare
		type X_Type is delta 2.0 range 0.0 .. 9999.0;
		type Q_Type is delta 4.0 range 0.0 .. 9999.0;
		type R_Type is delta 8.0 range 0.0 .. 9999.0;
		procedure Divide is
			new Ada.Fixed.Divide (X_Type, X_Type, Q_Type, R_Type);
		Q : Q_Type;
		R : R_Type;
	begin
		Divide (176.0, 20.0, Q, R);
		pragma Assert (Q = 8.0 and then R = 16.0);
	end;
	-- Dividend_Type'Small /= Divisor_Type'Small
	declare
		type N_Type is delta 0.5000 range 0.0 .. 9999.0;
		type D_Type is delta 0.2500 range 0.0 .. 9999.0;
		type Q_Type is delta 0.1250 range 0.0 .. 9999.0;
		type R_Type is delta 0.0625 range 0.0 .. 9999.0;
		procedure Divide is
			new Ada.Fixed.Divide (N_Type, D_Type, Q_Type, R_Type);
		Q : Q_Type;
		R : R_Type;
	begin
		Divide (10.0, 3.0, Q, R);
		pragma Assert (Q = 3.0 and then R = 1.0);
	end;
	declare
		type N_Type is delta 0.0625 range 0.0 .. 9999.0;
		type D_Type is delta 0.1250 range 0.0 .. 9999.0;
		type Q_Type is delta 0.2500 range 0.0 .. 9999.0;
		type R_Type is delta 0.5000 range 0.0 .. 9999.0;
		procedure Divide is
			new Ada.Fixed.Divide (N_Type, D_Type, Q_Type, R_Type);
		Q : Q_Type;
		R : R_Type;
	begin
		Divide (10.0, 3.0, Q, R);
		pragma Assert (Q = 3.0 and then R = 1.0);
	end;
	declare
		type N_Type is delta 2.00 range 0.0 .. 9999.0;
		type D_Type is delta 1.00 range 0.0 .. 9999.0;
		type Q_Type is delta 0.50 range 0.0 .. 9999.0;
		type R_Type is delta 0.25 range 0.0 .. 9999.0;
		procedure Divide is
			new Ada.Fixed.Divide (N_Type, D_Type, Q_Type, R_Type);
		Q : Q_Type;
		R : R_Type;
	begin
		Divide (10.0, 4.0, Q, R);
		pragma Assert (Q = 2.0 and then R = 2.0);
	end;
	declare
		type N_Type is delta 0.25 range 0.0 .. 9999.0;
		type D_Type is delta 0.50 range 0.0 .. 9999.0;
		type Q_Type is delta 1.00 range 0.0 .. 9999.0;
		type R_Type is delta 2.00 range 0.0 .. 9999.0;
		procedure Divide is
			new Ada.Fixed.Divide (N_Type, D_Type, Q_Type, R_Type);
		Q : Q_Type;
		R : R_Type;
	begin
		Divide (11.0, 3.0, Q, R);
		pragma Assert (Q = 3.0 and then R = 2.0);
	end;
	declare
		type N_Type is delta 16.0 range 0.0 .. 9999.0;
		type D_Type is delta  8.0 range 0.0 .. 9999.0;
		type Q_Type is delta  4.0 range 0.0 .. 9999.0;
		type R_Type is delta  2.0 range 0.0 .. 9999.0;
		procedure Divide is
			new Ada.Fixed.Divide (N_Type, D_Type, Q_Type, R_Type);
		Q : Q_Type;
		R : R_Type;
	begin
		Divide (32.0, 8.0, Q, R);
		pragma Assert (Q = 4.0 and then R = 0.0);
	end;
	declare
		type N_Type is delta  2.0 range 0.0 .. 9999.0;
		type D_Type is delta  4.0 range 0.0 .. 9999.0;
		type Q_Type is delta  8.0 range 0.0 .. 9999.0;
		type R_Type is delta 16.0 range 0.0 .. 9999.0;
		procedure Divide is
			new Ada.Fixed.Divide (N_Type, D_Type, Q_Type, R_Type);
		Q : Q_Type;
		R : R_Type;
	begin
		Divide (176.0, 20.0, Q, R);
		pragma Assert (Q = 8.0 and then R = 16.0);
	end;
	-- minus values
	declare
		type T is delta 0.5 range -9999.0 .. 9999.0;
		procedure Divide is new Ada.Fixed.Divide (T, T, T, T);
		Q : T;
		R : T;
	begin
		Divide (-10.0, 3.0, Q, R);
		pragma Assert (Q = -3.0 and then R = -1.0);
		Divide (10.0, -3.0, Q, R);
		pragma Assert (Q = -3.0 and then R = 1.0);
		Divide (-10.0, -3.0, Q, R);
		pragma Assert (Q = 3.0 and then R = -1.0);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end fixed_1;
