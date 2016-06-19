-- { dg-do run }
-- Please run with some command line arguments.
with ada.command_line;
procedure command_line_iterate is
begin
	declare -- invalid ranges
		use ada.command_line;
	begin
		for I in iterate (1, 0) loop
			raise Program_Error;
		end loop;
		for I in reverse iterate (1, 0) loop
			raise Program_Error;
		end loop;
		for I in iterate (Argument_Count + 1, Argument_Count) loop
			raise Program_Error;
		end loop;
		for I in reverse iterate (Argument_Count + 1, Argument_Count) loop
			raise Program_Error;
		end loop;
		begin
			for I in iterate (1, Argument_Count + 1) loop
				raise Program_Error;
			end loop;
			raise Program_Error;
		exception
			when Constraint_Error => null;
		end;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end command_line_iterate;
