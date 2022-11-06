--  Advanced Resource Embedder 1.2.0
with System.Storage_Elements;
package Embedded is

   type Content_Access is access constant System.Storage_Elements.Storage_Array;

   type Name_Access is access constant String;

   type Name_Array is array (Natural range <>) of Name_Access;

   Names : constant Name_Array;

   --  Returns the data stream with the given name or null.
   function Get_Content (Name : String) return
      access constant System.Storage_Elements.Storage_Array;

private


   K_0             : aliased constant String := "D410-b-12.bdf";
   K_1             : aliased constant String := "DGlogoOrange.ico";

   Names : constant Name_Array := (
      K_0'Access, K_1'Access);
end Embedded;
