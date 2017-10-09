with Samples.Petstore.Clients;
with Samples.Petstore.Models;
with Swagger;
with Util.Http.Clients.Curl;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Calendar.Formatting;
with Ada.Exceptions;
procedure Test is

   use Ada.Text_IO;

   procedure Usage;
   procedure Print_Pet (Pet : in Samples.Petstore.Models.Pet_Type);
   procedure Print_Order (Order : in Samples.Petstore.Models.Order_Type);
   procedure Get_User (C : in out Samples.Petstore.Clients.Client_Type);
   procedure Get_Pet (C : in out Samples.Petstore.Clients.Client_Type);
   procedure Get_Order (C : in out Samples.Petstore.Clients.Client_Type);
   procedure List_Inventory (C : in out Samples.Petstore.Clients.Client_Type);
   procedure List_Pet (C : in out Samples.Petstore.Clients.Client_Type);
   procedure Delete_Pet (C : in out Samples.Petstore.Clients.Client_Type);
   procedure Delete_User (C : in out Samples.Petstore.Clients.Client_Type);
   procedure Delete_Order (C : in out Samples.Petstore.Clients.Client_Type);
   procedure Add_Pet (C : in out Samples.Petstore.Clients.Client_Type);
   procedure Login (C        : in out Samples.Petstore.Clients.Client_Type;
                    Username : in String;
                    Password : in String);

   Server    : constant Swagger.UString := Swagger.To_UString ("http://petstore.swagger.io/v2");
   Arg_Count : constant Natural := Ada.Command_Line.Argument_Count;
   Arg       : Positive := 1;

   procedure Usage is
   begin
      Put_Line ("Usage: petstore {list|add|rm|update} {user|order|pet} {params}...");
      Put_Line ("  get pet <id>...                Print pet given its id");
      Put_Line ("  get user <name>...             Print user given its name");
      Put_Line ("  get order <id>...              Print order given its id");
      Put_Line ("  list pet <status>...           List the pets with the given status");
      Put_Line ("  list inventory                 List the inventory");
      Put_Line ("  add pet <id> <name> <status> <category-id> <category-name");
      Put_Line ("                                 Add a pet");
      Put_Line ("  rm user <name>...              Remove user with given name");
      Put_Line ("  rm order <id>...               Remove order with given id");
      Put_Line ("  login <username> <password>    Use login operation to get a session");
   end Usage;

   procedure Print_Pet (Pet : in Samples.Petstore.Models.Pet_Type) is
      Need_Indent : Boolean := False;
   begin
      Put_Line ("Id      : " & Swagger.Long'Image (Pet.Id));
      Put_Line ("Name    : " & Swagger.To_String (Pet.Name));
      Put_Line ("Status  : " & Swagger.To_String (Pet.Status));
      if not Pet.Tags.Is_Empty then
         Put ("Tags    : ");
         for Tag of Pet.Tags loop
            Put_Line ((if Need_Indent then "          " else "")
                      & Swagger.To_String (Tag.Name));
            Need_Indent := True;
         end loop;
      end if;
      if not Pet.Photo_Urls.Is_Empty then
         Need_Indent := False;
         Put ("URLs    : ");
         for Url of Pet.Photo_Urls loop
            Put_Line ((if Need_Indent then "          " else "") & Url);
            Need_Indent := True;
         end loop;
      end if;
   end Print_Pet;

   procedure Print_Order (Order : in Samples.Petstore.Models.Order_Type) is
   begin
      Put_Line ("Id          : " & Swagger.Long'Image (Order.Id));
      Put_Line ("Pet id      : " & Swagger.Long'Image (Order.Pet_Id));
      Put_Line ("Quantity    : " & Integer'Image (Order.Quantity));
      Put_Line ("Status      : " & Swagger.To_String (Order.Status));
      Put_Line ("Ship date   : " & Ada.Calendar.Formatting.Image (Order.Ship_Date));
      Put_Line ("Complete    : " & Boolean'Image (Order.Complete));
   end Print_Order;

   procedure Get_User (C : in out Samples.Petstore.Clients.Client_Type) is

      User : Samples.Petstore.Models.User_Type;
   begin
      for I in Arg .. Arg_Count loop
         C.Get_User_By_Name (Swagger.To_UString (Ada.Command_Line.Argument (I)), User);
         Put_Line ("Id       : " & Swagger.Long'Image (User.Id));
         Put_Line ("Username : " & Swagger.To_String (User.Username));
         Put_Line ("Firstname: " & Swagger.To_String (User.First_Name));
         Put_Line ("Lastname : " & Swagger.To_String (User.Last_Name));
         Put_Line ("Email    : " & Swagger.To_String (User.Email));
         Put_Line ("Password : " & Swagger.To_String (User.Password));
         Put_Line ("Phone    : " & Swagger.To_String (User.Phone));
      end loop;
   end Get_User;

   procedure Get_Pet (C : in out Samples.Petstore.Clients.Client_Type) is
      Pet  : Samples.Petstore.Models.Pet_Type;
   begin
      C.Set_Server (Server);
      for I in Arg .. Arg_Count loop
         declare
            P : constant String := Ada.Command_Line.Argument (I);
         begin
            C.Get_Pet_By_Id (Swagger.Long'Value (P), Pet);
            Print_Pet (Pet);
         end;
      end loop;
   end Get_Pet;

   procedure Get_Order (C : in out Samples.Petstore.Clients.Client_Type) is
      Order  : Samples.Petstore.Models.Order_Type;
   begin
      C.Set_Server (Server);
      for I in Arg .. Arg_Count loop
         declare
            P : constant String := Ada.Command_Line.Argument (I);
         begin
            C.Get_Order_By_Id (Swagger.Long'Value (P), Order);
            Print_Order (Order);
         end;
      end loop;
   end Get_Order;

   procedure List_Pet (C : in out Samples.Petstore.Clients.Client_Type) is
      Pets  : Samples.Petstore.Models.Pet_Type_Vectors.Vector;
   begin
      for I in Arg .. Arg_Count loop
         declare
            Status  : Swagger.UString_Vectors.Vector;
            P : constant String := Ada.Command_Line.Argument (I);
         begin
            Status.Append (P);
            C.Find_Pets_By_Status (Status, Pets);
            for Pet of Pets loop
               Print_Pet (Pet);
            end loop;
         end;
      end loop;
   end List_Pet;

   procedure List_Inventory (C : in out Samples.Petstore.Clients.Client_Type) is
      List : Swagger.Integer_Map;
      Iter : Swagger.Integer_Maps.Cursor;
   begin
      C.Get_Inventory (List);
      Ada.Text_IO.Put_Line ("Inventory size " & Natural'Image (Natural (List.Length)));
      Iter := List.First;
      while Swagger.Integer_Maps.Has_Element (Iter) loop
         Put (Swagger.Integer_Maps.Key (Iter));
         Set_Col (70);
         Put_Line (Natural'Image (Swagger.Integer_Maps.Element (Iter)));
         Swagger.Integer_Maps.Next (Iter);
      end loop;
   end List_Inventory;

   procedure Login (C        : in out Samples.Petstore.Clients.Client_Type;
                    Username : in String;
                    Password : in String) is
      Session : Swagger.UString;
   begin
      C.Login_User (Swagger.To_UString (Username),
                    Swagger.To_UString (Password),
                    Session);
      Put_Line ("New session : " & Swagger.To_String (Session));
   end Login;

   procedure Add_Pet (C : in out Samples.Petstore.Clients.Client_Type) is
      Pet : Samples.Petstore.Models.Pet_Type;
   begin
      if Arg_Count /= 7 then
         Put_Line ("Missing some arguments for add pet command");
         Usage;
         return;
      end if;
      Pet.Id := Swagger.Long'Value (Ada.Command_Line.Argument (Arg));
      Pet.Name := Swagger.To_UString (Ada.Command_Line.Argument (Arg + 1));
      Pet.Status := Swagger.To_UString (Ada.Command_Line.Argument (Arg + 2));
      Pet.Category.Id := Swagger.Long'Value (Ada.Command_Line.Argument (Arg + 3));
      Pet.Category.Name := Swagger.To_UString (Ada.Command_Line.Argument (Arg + 4));
      C.Add_Pet (Pet);
   end Add_Pet;

   procedure Delete_User (C : in out Samples.Petstore.Clients.Client_Type) is
   begin
      for I in Arg .. Arg_Count loop
         C.Delete_User (Username => Swagger.To_UString (Ada.Command_Line.Argument (I)));
      end loop;
   end Delete_User;

   procedure Delete_Order (C : in out Samples.Petstore.Clients.Client_Type) is
   begin
      for I in Arg .. Arg_Count loop
         C.Delete_Order (Swagger.To_UString (Ada.Command_Line.Argument (I)));
      end loop;
   end Delete_Order;

   procedure Delete_Pet (C : in out Samples.Petstore.Clients.Client_Type) is
      Key : constant Swagger.UString := Swagger.To_UString (Ada.Command_Line.Argument (Arg));
   begin
      Arg := Arg + 1;
      for I in Arg .. Arg_Count loop
         C.Delete_Pet (Swagger.Long'Value (Ada.Command_Line.Argument (I)), Key);
      end loop;
   end Delete_Pet;

begin
   if Arg_Count <= 1 then
      Usage;
      return;
   end if;
   Util.Http.Clients.Curl.Register;
   declare
      Command : constant String := Ada.Command_Line.Argument (Arg);
      Item    : constant String := Ada.Command_Line.Argument (Arg + 1);
      C       : Samples.Petstore.Clients.Client_Type;
   begin
      C.Set_Server (Server);
      Arg := Arg + 2;
      if Command = "login" then
         Login (C, Item, Ada.Command_Line.Argument (Arg));
      elsif Command = "get" then
         if Item = "user" then
            Get_User (C);
         elsif Item = "pet" then
            Get_Pet (C);
         elsif Item = "order" then
            Get_Order (C);
         else
            Usage;
         end if;
      elsif Command = "list" then
         if Item = "pet" then
            List_Pet (C);
         elsif Item = "inventory" then
            List_Inventory (C);
         else
            Usage;
         end if;
      elsif Command = "add" then
         if Item = "pet" then
            Add_Pet (C);
         else
            Usage;
         end if;
      elsif Command = "rm" then
         if Item = "user" then
            Delete_User (C);
         elsif Item = "order" then
            Delete_Order (C);
         elsif Item = "pet" then
            Delete_Pet (C);
         else
            Usage;
         end if;
      elsif Command = "update" then
         Usage;
      else
         Usage;
      end if;

   exception
      when E : Constraint_Error =>
         Put_Line ("Constraint error raised: " & Ada.Exceptions.Exception_Message (E));

   end;
end Test;
