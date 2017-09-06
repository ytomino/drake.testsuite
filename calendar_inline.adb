-- { dg-do assemble }
-- { dg-additional-options -O2 -gnatn }
-- { dg-final { scan-assembler-times "^\tcall\t_*ada__calendar__packed_split(@PLT)?$" 1 } }
-- { dg-final { scan-assembler-times "^\tcall\t_*ada__calendar__formatting__packed_split(@PLT)?$" 1 } }
with ada.text_io; use ada.text_io;
with ada.calendar;
with ada.calendar.formatting;
procedure calendar_inline is
	t : Ada.Calendar.time := Ada.Calendar.clock;
begin
	declare
		use ada.calendar;
	begin
		Put (Year (t)'img);
		Put (month (t)'img);
		Put (day (t)'img);
		new_line;
	end;
	declare
		use ada.calendar.formatting;
	begin
		Put (Year (t)'img);
		Put (month (t)'img);
		Put (day (t)'img);
		Put (hour (t)'img);
		Put (minute (t)'img);
		Put (second (t)'img);
		new_line;
	end;
end calendar_inline;
