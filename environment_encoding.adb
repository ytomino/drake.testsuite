-- { dg-do run }
-- { dg-additional-options "-largs -licucore -margs" { target "*-*-darwin*" } }
with Ada.Environment_Encoding;
procedure environment_encoding is
begin
	declare
		Converter : Ada.Environment_Encoding.Converter;
		pragma Unmodified (Converter);
	begin
		pragma Assert (not Ada.Environment_Encoding.Is_Open (Converter));
		null;
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end environment_encoding;
