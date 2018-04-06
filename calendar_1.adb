-- { dg-do run }
with Ada.Calendar.Formatting;
with Ada.Calendar.Naked;
with Ada.Calendar.Time_Zones;
with Ada.Formatting;
with System.Native_Time;
procedure calendar_1 is
	function Image_4 is
		new Ada.Formatting.Integer_Image (Integer,
			Signs => Ada.Formatting.Triming_Sign_Marks, Digits_Width => 4);
	function Image_2 is
		new Ada.Formatting.Integer_Image (Integer,
			Signs => Ada.Formatting.Triming_Sign_Marks, Digits_Width => 2);
	use type Ada.Calendar.Time;
	use type Ada.Calendar.Time_Zones.Time_Offset;
	Now : Ada.Calendar.Time := Ada.Calendar.Clock;
	Year : Ada.Calendar.Year_Number;
	Month : Ada.Calendar.Month_Number;
	Day : Ada.Calendar.Day_Number;
	Seconds : Ada.Calendar.Day_Duration;
	H : Ada.Calendar.Formatting.Hour_Number;
	M : Ada.Calendar.Formatting.Minute_Number;
	S : Ada.Calendar.Formatting.Second_Number;
	SS : Ada.Calendar.Formatting.Second_Duration;
	LS : Boolean;
	Day_Name : Ada.Calendar.Formatting.Day_Name;
	Remaked : Ada.Calendar.Time;
begin
	Ada.Debug.Put (
		System.Native_Time.Nanosecond_Number'Image (
			System.Native_Time.Nanosecond_Number'Integer_Value (
				Ada.Calendar.Naked.Seconds_From_2150 (Now))));
	Ada.Calendar.Split (Now, Year, Month, Day, Seconds);
	Ada.Calendar.Formatting.Split (Seconds, H, M, S, SS);
	Ada.Debug.Put (
		"GM "
		& Image_4 (Year) & '-'
		& Image_2 (Month) & '-'
		& Image_2 (Day)
		& Duration'Image (Seconds));
	Ada.Debug.Put (
		"GM "
		& Image_4 (Year) & '-'
		& Image_2 (Month) & '-'
		& Image_2 (Day) & ' '
		& Image_2 (H) & ':'
		& Image_2 (M) & ':'
		& Image_2 (S)
		& Duration'Image (SS));
	pragma Assert (Ada.Calendar.Seconds (Now) = Seconds);
	Remaked := Ada.Calendar.Time_Of (Year, Month, Day, Seconds);
	pragma Assert (Remaked = Now);
	Ada.Debug.Put (
		"TZ ="
		& Ada.Calendar.Time_Zones.Time_Offset'Image (
			Ada.Calendar.Time_Zones.UTC_Time_Offset));
	pragma Assert (Ada.Calendar.Formatting.Value (
		Ada.Calendar.Formatting.Image (Ada.Calendar.Time_Zones.UTC_Time_Offset)) =
		Ada.Calendar.Time_Zones.UTC_Time_Offset);
	-- Time_Offset = 540 in Japan
	Ada.Calendar.Formatting.Split (Now, Year, Month, Day, H, M, S, SS, LS,
		Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset);
	Ada.Debug.Put (
		"LT "
		& Image_4 (Year) & '-'
		& Image_2 (Month) & '-'
		& Image_2 (Day) & ' '
		& Image_2 (H) & ':'
		& Image_2 (M) & ':'
		& Image_2 (S)
		& Duration'Image (SS));
	Remaked := Ada.Calendar.Formatting.Time_Of (Year, Month, Day, H, M, S, SS, LS,
		Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset);
	pragma Assert (Remaked = Now);
	Day_Name := Ada.Calendar.Formatting.Day_of_Week (
		Now,
		Time_Zone => Ada.Calendar.Time_Zones.UTC_Time_Offset);
	Ada.Debug.Put (Ada.Calendar.Formatting.Day_Name'Image (Day_Name));
	declare
		Img : String := Ada.Calendar.Formatting.Image (Now, Include_Time_Fraction => True);
	begin
		Ada.Debug.Put (Img);
		Remaked := Ada.Calendar.Formatting.Value (Img);
		pragma Assert (
			Ada.Calendar.Formatting.Image (Remaked,
				Include_Time_Fraction => True) =
			Img);
	end;
	declare
		X : Duration := Duration'(((15.0 * 60.0) + 25.0) * 60.0 + 35.45);
		Img : String := Ada.Calendar.Formatting.Image (X, Include_Time_Fraction => True);
		RX : Duration;
	begin
		pragma Assert (Img = "15:25:35.45");
		RX := Ada.Calendar.Formatting.Value (Img);
		pragma Assert (
			Ada.Calendar.Formatting.Image (RX, Include_Time_Fraction => False) =
			"15:25:35");
		pragma Assert (RX = X);
	end;
	declare -- elapsed time
		Max : constant Duration := (23.0 * 60.0 + 59.0) * 60.0 + 59.99;
		Max_S : constant String := Ada.Calendar.Formatting.Image (Max, Include_Time_Fraction => True);
		Neg : constant Duration := -((23.0 * 60.0 + 30.0) * 60.0 + 30.0);
		Neg_S : constant String := Ada.Calendar.Formatting.Image (Neg, Include_Time_Fraction => False);
	begin
		pragma Assert (Max_S = "23:59:59.99");
		pragma Assert (Ada.Calendar.Formatting.Value (Max_S) = Max);
		pragma Assert (Neg_S = "-23:30:30");
		pragma Assert (Ada.Calendar.Formatting.Value (Neg_S) = Neg);
	end;
	declare -- first / last
		EF : Ada.Calendar.Time;
		EL : Ada.Calendar.Time;
	begin
		EF := Ada.Calendar.Time_Of (1901, 1, 1);
		Ada.Calendar.Split (EF, Year, Month, Day, Seconds);
		pragma Assert (
			Year = 1901 and then Month = 1 and then Day = 1
			and then Seconds = 0.0);
		EL := Ada.Calendar.Time_Of (2099, 12, 31);
		Ada.Calendar.Split (EL, Year, Month, Day, Seconds);
		pragma Assert (
			Year = 2099 and then Month = 12 and then Day = 31
			and then Seconds = 0.0);
	exception
		when Ada.Calendar.Time_Error => Ada.Debug.Put ("Time_Error");
	end;
	declare -- 2150-01-01
		Z : constant Ada.Calendar.Time := Ada.Calendar.Time_Of (2150, 1, 1);
	begin
		pragma Assert (Ada.Calendar.Naked.Seconds_From_2150 (Z) = 0.0);
		null;
	end;
	declare -- overflow
		D : Ada.Calendar.Time;
	begin
		D := Now - Duration'First + Duration'Last; -- out of range
		Ada.Debug.Put ("plase compile with -gnato");
	exception
		when Ada.Calendar.Time_Error => null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end calendar_1;
