-- { dg-do run }
with Ada;
with Interfaces; use Interfaces;
with System.Arith_64; use System.Arith_64;
procedure required_arith_64 is
	r, q : Integer_64;
begin
	-- Scaled_Divide (x * y / z)
	Scaled_Divide (Integer_64'First, Integer_64'First, Integer_64'First,
		q => q, r => r, Round => False);
	pragma Assert (q = Integer_64'First);
	pragma Assert (r = 0);
	begin -- -8000h * -8000h / 7FFFh = 8001h (remainder is 1)
		Scaled_Divide (Integer_64'First, Integer_64'First, Integer_64'Last,
			q => q, r => r, Round => False);
		raise Program_Error; -- unreachable
	exception
		when Constraint_Error => null;
	end;
	Scaled_Divide (Integer_64'First, Integer_64'Last, Integer_64'First,
		q => q, r => r, Round => False);
	pragma Assert (q = Integer_64'Last);
	pragma Assert (r = 0);
	Scaled_Divide (Integer_64'First, Integer_64'Last, Integer_64'Last,
		q => q, r => r, Round => False);
	pragma Assert (q = Integer_64'First);
	pragma Assert (r = 0);
	Scaled_Divide (Integer_64'Last, Integer_64'First, Integer_64'First,
		q => q, r => r, Round => False);
	pragma Assert (q = Integer_64'Last);
	pragma Assert (r = 0);
	Scaled_Divide (Integer_64'Last, Integer_64'First, Integer_64'Last,
		q => q, r => r, Round => False);
	pragma Assert (q = Integer_64'First);
	pragma Assert (r = 0);
	Scaled_Divide (Integer_64'Last, Integer_64'Last, Integer_64'First,
		q => q, r => r, Round => False);
	pragma Assert (q = Integer_64'First + 2); -- -16#7FFF_FFFF_FFFF_FFFE#
	pragma Assert (r = 1);
	Scaled_Divide (Integer_64'Last, Integer_64'Last, Integer_64'Last,
		q => q, r => r, Round => False);
	pragma Assert (q = Integer_64'Last);
	pragma Assert (r = 0);
	Scaled_Divide (Integer_64'First, 16#3FFF_FFFF_FFFF_FFFF#, Integer_64'First,
		q => q, r => r, Round => False);
	pragma Assert (q = 16#3FFF_FFFF_FFFF_FFFF#);
	pragma Assert (r = 0);
	begin -- -8000h * -1 / 1 = 8000h
		Scaled_Divide (Integer_64'First, -1, 1,
			q => q, r => r, Round => False);
		raise Program_Error; -- unreachable
	exception
		when Constraint_Error => null;
	end;
	-- Double_Divide (x / (y * z))
	Double_Divide (Integer_64'First, Integer_64'First, Integer_64'First,
		q => q, r => r, Round => False);
	pragma Assert (q = 0);
	pragma Assert (r = Integer_64'First);
	Double_Divide (Integer_64'Last, Integer_64'First, Integer_64'First,
		q => q, r => r, Round => False);
	pragma Assert (q = 0);
	pragma Assert (r = Integer_64'Last);
	Double_Divide (Integer_64'First, Integer_64'First, -1,
		q => q, r => r, Round => False);
	pragma Assert (q = -1);
	pragma Assert (r = 0);
	Double_Divide (Integer_64'First, -1, Integer_64'First,
		q => q, r => r, Round => False);
	pragma Assert (q = -1);
	pragma Assert (r = 0);
	Double_Divide (Integer_64'First, -1, -1,
		q => q, r => r, Round => False);
	pragma Assert (q = Integer_64'First);
	pragma Assert (r = 0);
	begin -- -8000h / -1 / 1
		Double_Divide (Integer_64'First, -1, 1,
			q => q, r => r, Round => False);
		raise Program_Error; -- unreachable
	exception
		when Constraint_Error => null;
	end;
	Double_Divide (Integer_64'First, -1, 2,
		q => q, r => r, Round => False);
	pragma Assert (q = Integer_64'Last / 2 + 1); -- 16#4000_0000_0000_0000#
	pragma Assert (r = 0);
	Double_Divide (Integer_64'First, 1, -2,
		q => q, r => r, Round => False);
	pragma Assert (q = Integer_64'Last / 2 + 1); -- 16#4000_0000_0000_0000#
	pragma Assert (r = 0);
	Double_Divide (-Integer_64'Last, 1, -1,
		q => q, r => r, Round => False);
	pragma Assert (q = Integer_64'Last);
	pragma Assert (r = 0);
	Double_Divide (16#80#, 16#80#, 16#80#,
		q => q, r => r, Round => False);
	pragma Assert (q = 0);
	pragma Assert (r = 16#80#);
	-- finished
	pragma Debug (Ada.Debug.Put ("OK"));
end required_arith_64;
