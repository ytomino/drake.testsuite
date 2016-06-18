-- { dg-do run }
with Ada.Tags;
with Ada.Finalization;
procedure tags_external_tag is
	use type Ada.Tags.Tag;
	type T is tagged null record;
begin
	Ada.Debug.Put (T'External_Tag);
	pragma Assert (Ada.Tags.Internal_Tag (T'External_Tag) = T'Tag);
	pragma Assert (
		Ada.Finalization.Controlled'External_Tag =
		"ADA.FINALIZATION.CONTROLLED");
	pragma Assert (Ada.Tags.Internal_Tag (Ada.Finalization.Controlled'External_Tag) = Ada.Finalization.Controlled'Tag);
	pragma Assert (
		Ada.Finalization.Limited_Controlled'External_Tag =
		"ADA.FINALIZATION.LIMITED_CONTROLLED");
	pragma Assert (Ada.Tags.Internal_Tag (Ada.Finalization.Limited_Controlled'External_Tag) = Ada.Finalization.Limited_Controlled'Tag);
	pragma Debug (Ada.Debug.Put ("OK"));
end tags_external_tag;
