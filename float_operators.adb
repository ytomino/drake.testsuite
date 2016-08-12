-- { dg-do run }
with Ada.Float;
-- FreeBSD 7.x does not have nan, nanf, nal, but 8.x have.
procedure float_operators is
	generic
		type T is digits <>;
	procedure Test;
	procedure Test is
		function Infinity is new Ada.Float.Infinity (T);
		X : T := T'Value ("2.0");
		type Unaligned is record
			Padding : Character;
			Value : T;
		end record;
		pragma Pack (Unaligned);
		Y : Unaligned := (ASCII.NUL, T'Value ("3.5"));
	begin
		pragma Assert (T'Adjacent (X, Infinity) > X);
		pragma Assert (T'Adjacent (X, -Infinity) < X);
		pragma Assert (T'Floor (T'Adjacent (X, Infinity)) = X);
		pragma Assert (T'Ceiling (T'Adjacent (X, -Infinity)) = X);
		pragma Assert (T'Ceiling (X + 0.1) = 3.0);
		pragma Assert (T'Compose (Y.Value, -1) = 0.4375);
		pragma Assert (T'Copy_Sign (X, -1.0) = -2.0);
		pragma Assert (T'Exponent (X) = 2);
		pragma Assert (T'Floor (X - 0.1) = 1.0);
		pragma Assert (T'Fraction (X) = 0.5);
		pragma Assert (T'Leading_Part (X + 0.75, 3) = 2.5);
		pragma Assert (T'Machine (X) = X);
		pragma Assert (T'Machine_Rounding (X - 0.1) = X);
		pragma Assert (T'Model (X) = X);
		pragma Assert (T'Pred (X) < X);
		pragma Assert (T'Ceiling (T'Pred (X)) = X);
		pragma Assert (T'Remainder (Y.Value, 2.0) = -0.5);
		pragma Assert (T'Remainder (-Y.Value, 2.0) = 0.5);
		pragma Assert (T'Remainder (Y.Value, -2.0) = -0.5);
		pragma Assert (T'Remainder (-Y.Value, -2.0) = 0.5);
		pragma Assert (T'Rounding (X + 0.9) = 3.0);
		pragma Assert (T'Succ (X) > X);
		pragma Assert (T'Floor (T'Succ (X)) = X);
		pragma Assert (T'Scaling (X, 1) = 4.0);
		pragma Assert (T'Scaling (X, -1) = 1.0);
		pragma Assert (T'Truncation (X + 0.9) = 2.0);
		pragma Assert (T'Unbiased_Rounding (X + 1.5) = 4.0);
		pragma Assert (T'Unbiased_Rounding (X + 1.4) = 3.0);
		pragma Assert (T'Unbiased_Rounding (X + 0.6) = 3.0);
		pragma Assert (T'Unbiased_Rounding (X + 0.5) = 2.0);
		pragma Assert (T'Unbiased_Rounding (X - 0.5) = 2.0);
		pragma Assert (T'Unbiased_Rounding (X - 0.6) = 1.0);
		pragma Assert (T'Unbiased_Rounding (X - 1.4) = 1.0);
		pragma Assert (T'Unbiased_Rounding (X - 1.5) = 0.0);
		pragma Assert (T'Unbiased_Rounding (X - 2.5) = 0.0);
		pragma Assert (T'Unbiased_Rounding (X - 2.6) = -1.0);
		pragma Assert (T'Unbiased_Rounding (X - 3.4) = -1.0);
		pragma Assert (T'Unbiased_Rounding (X - 3.5) = -2.0);
		pragma Assert (X'Valid);
		pragma Assert (Y.Value'Valid); -- Unaligned_Valid
		null;
	end Test;
begin
	declare
		procedure Short_Float_Test is new Test (Short_Float);
	begin
		Short_Float_Test;
	end;
	declare
		procedure Float_Test is new Test (Float);
	begin
		Float_Test;
	end;
	declare
		procedure Long_Float_Test is new Test (Long_Float);
	begin
		Long_Float_Test;
	end;
	declare
		procedure Long_Long_Float_Test is new Test (Long_Long_Float);
	begin
		Long_Long_Float_Test;
	end;
	declare
		type Custom_Float is digits 12;
		procedure Custom_Float_Test is new Test (Custom_Float);
	begin
		Custom_Float_Test;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end float_operators;
