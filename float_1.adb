-- { dg-do run }
with Ada.Float;
procedure float_1 is
begin
	declare -- one type
		generic
			type T is digits <>;
		procedure Generic_Try;
		procedure Generic_Try is
			function Infinity is new Ada.Float.Infinity (T);
			function NaN is new Ada.Float.NaN (T);
			function Is_Infinity is new Ada.Float.Is_Infinity (T);
			function Is_NaN is new Ada.Float.Is_NaN (T);
			function Is_Negative is new Ada.Float.Is_Negative (T);
			procedure Divide is new Ada.Float.Divide (T, T, T, T);
			Q, R : T;
		begin
			pragma Assert (T'Image (Infinity) = " INF");
			pragma Assert (T'Image (-Infinity) = "-INF");
			pragma Assert (Is_Infinity (Infinity));
			pragma Assert (T'Image (NaN) = " NAN");
			pragma Assert (T'Image (-NaN) = "-NAN");
			pragma Assert (not (-NaN < 0.0)); -- comparison NaN is always False
			pragma Assert (Is_NaN (NaN));
			pragma Assert (Is_Negative (-1.0));
			pragma Assert (not Is_Negative (0.0));
			pragma Assert (not Is_Negative (+1.0));
			Divide (4.5, 2.0, Q, R);
			pragma Assert (Q = 2.0 and then R = 0.5);
			Divide (5.0, 0.5, Q, R);
			pragma Assert (Q = 10.0 and then R = 0.0);
			Divide (0.9, 1.0, Q, R);
			pragma Assert (Q = 0.0 and then R = 0.9);
			Divide (-0.9, 1.0, Q, R);
			pragma Assert (Q = 0.0 and then R = -0.9);
		end Generic_Try;
		procedure Try_Short_Float is new Generic_Try (Short_Float);
		procedure Try_Float is new Generic_Try (Float);
		procedure Try_Long_Float is new Generic_Try (Long_Float);
		procedure Try_Long_Long_Float is new Generic_Try (Long_Long_Float);
		type Custom_Float is digits 12;
		procedure Try_Custom_Float is new Generic_Try (Custom_Float);
	begin
		Try_Short_Float;
		Try_Float;
		Try_Long_Float;
		Try_Long_Long_Float;
		Try_Custom_Float;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end float_1;
