-- { dg-do run }
with Ada.Tags;
with Ada.Unchecked_Conversion;
with System.Storage_Elements.Formatting;
procedure tags_1 is
	package SSEF renames System.Storage_Elements.Formatting;
	use type Ada.Tags.Tag;
	use type Ada.Tags.Tag_Array;
	use type System.Address;
	type T is tagged null record;
	type D is new T with null record;
	type I is interface;
	type DI is new T and I with null record;
	type L is tagged limited null record;
	type LD is new L with null record;
	type LI is limited interface;
	type LDI is new L and LI with null record;
begin
	pragma Assert (Ada.Tags.Expanded_Name (LDI'Tag) = "TAGS_1.LDI");
	pragma Assert (Ada.Tags.Expanded_Name (LI'Tag) = "TAGS_1.LI");
	Ada.Debug.Put (Ada.Tags.External_Tag (LDI'Tag));
	Ada.Debug.Put (Ada.Tags.External_Tag (LI'Tag));
	pragma Assert (Ada.Tags.Parent_Tag (T'Tag) = Ada.Tags.No_Tag);
	pragma Assert (Ada.Tags.Parent_Tag (D'Tag) = T'Tag);
	pragma Assert (Ada.Tags.Parent_Tag (LD'Tag) = L'Tag);
	pragma Assert (Ada.Tags.Parent_Tag (LDI'Tag) = L'Tag);
	pragma Assert (Ada.Tags.Parent_Tag (LI'Tag) = Ada.Tags.No_Tag);
	pragma Assert (Ada.Tags.Interface_Ancestor_Tags (D'Tag)'Length = 0);
	pragma Assert (Ada.Tags.Interface_Ancestor_Tags (DI'Tag) = (1 => I'Tag));
	pragma Assert (Ada.Tags.Interface_Ancestor_Tags (LI'Tag)'Length = 0);
	pragma Assert (Ada.Tags.Is_Descendant_At_Same_Level (D'Tag, T'Tag));
	pragma Assert (Ada.Tags.Is_Descendant_At_Same_Level (D'Tag, D'Tag));
	pragma Assert (Ada.Tags.Is_Descendant_At_Same_Level (DI'Tag, I'Tag));
	pragma Assert (not Ada.Tags.Is_Descendant_At_Same_Level (LI'Tag, I'Tag));
	pragma Assert (not Ada.Tags.Is_Abstract (T'Tag));
	pragma Assert (Ada.Tags.Is_Abstract (I'Tag));
	declare
		type T_Access is access all T'Class;
		function T_Cast is
			new Ada.Unchecked_Conversion (T_Access, System.Address);
		Obj : aliased DI;
		Ref : access T'Class := Obj'Access;
		IRef : access I'Class := Obj'Access;
	begin
		pragma Assert (Ref.all in DI'Class); -- resolved inline (no runtime)
		pragma Assert (Ref.all in I'Class); -- use IW_Membership
		pragma Assert (IRef'Tag = DI'Tag);
		pragma Assert (IRef.all in DI'Class);
		pragma Assert (T_Access (IRef) = Ref);
		declare
			type IA is access all I'Class;
			function I_Cast is new Ada.Unchecked_Conversion (IA, System.Address);
			IRef2 : access I'Class := IA (Ref);
		begin
			pragma Assert (T_Cast (Ref) /= I_Cast (IRef2));
			null;
		end;
	end;
	declare
		type L_Access is access all L'Class;
		type LI_Access is access all LI'Class;
		function L_Cast is new Ada.Unchecked_Conversion (L_Access, System.Address);
		function LI_Cast is new Ada.Unchecked_Conversion (LI_Access, System.Address);
		procedure X (ObjRef : L_Access) is
			LIRef : LI_Access := LI'Class (ObjRef.all)'Access;
		begin
			pragma Assert (L_Cast (ObjRef) /= LI_Cast (LIRef));
			Ada.Debug.Put (SSEF.Image (L_Cast (ObjRef)));
			Ada.Debug.Put (SSEF.Image (LI_Cast (LIRef)));
		end X;
		Obj : aliased LDI;
		LIRef : LI_Access := Obj'Access;
	begin
		pragma Assert (L_Cast (Obj'Access) /= LI_Cast (LIRef));
		Ada.Debug.Put (SSEF.Image (L_Cast (Obj'Access)));
		Ada.Debug.Put (SSEF.Image (LI_Cast (LIRef)));
		X (Obj'Access);
	end;
	pragma Debug (Ada.Debug.Put ("OK"));
end tags_1;
