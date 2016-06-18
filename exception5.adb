with Ada.Exceptions;
with Ada.Strings.Unbounded;
procedure exception5 is
	
	procedure Magic is
		S : aliased Ada.Strings.Unbounded.Unbounded_String;
	begin
		Ada.Strings.Unbounded.Append (S, "12345678");
		Ada.Strings.Unbounded.Append (S, "12345678");
		Ada.Strings.Unbounded.Append (S, "1234567812345678");
		Ada.Strings.Unbounded.Append (S, "9");
		Ada.Debug.Put (S.Reference.Element.all);
	end Magic;
	
	procedure Test is
	begin
		begin
			Magic;
			raise Program_Error; -- reuse memory used by Magic.S
		exception
			when E : Program_Error =>
				Ada.Debug.Put (Ada.Exceptions.Exception_Information (E));
		end;
	end Test;

begin
	Test;
	pragma Debug (Ada.Debug.Put ("OK"));
end exception5;
