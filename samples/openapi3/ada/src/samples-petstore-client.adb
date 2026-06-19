--  ------------ EDIT NOTE ------------
--  OpenAPI Petstore
--  This is a sample server Petstore server. For this sample, you can use the api key `special_key` to test the authorization filters.
--  This file was generated with openapi-generator.  You can modify it to implement
--  the client.  After you modify this file, you should add the following line
--  to the .openapi-generator-ignore file:
--
--  src/samples-petstore.ads
--
--  Then, you can drop this edit note comment.
--  ------------ EDIT NOTE ------------
with Samples.Petstore.Clients;
with Samples.Petstore.Models;
with Swagger;
with Swagger.Credentials.OAuth;
with Util.Http.Clients.Curl;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Calendar.Formatting;
with Ada.Exceptions;
procedure Samples.Petstore.Client is

   use Ada.Text_IO;

   procedure Usage;

   Server    : constant Swagger.UString := Swagger.To_UString ("http://localhost:8080/v2");
   Arg_Count : constant Natural := Ada.Command_Line.Argument_Count;
   Arg       : Positive := 1;

   procedure Usage is
   begin
      Put_Line ("Usage: Petstore {params}...");
   end Usage;

begin
   if Arg_Count <= 1 then
      Usage;
      return;
   end if;
   Util.Http.Clients.Curl.Register;
   declare
      Command : constant String := Ada.Command_Line.Argument (Arg);
      Item    : constant String := Ada.Command_Line.Argument (Arg + 1);
      Cred    : aliased Swagger.Credentials.OAuth.OAuth2_Credential_Type;
      C       : Samples.Petstore.Clients.Client_Type;
   begin
      C.Set_Server (Server);
      C.Set_Credentials (Cred'Unchecked_Access);
      Arg := Arg + 2;

   exception
      when E : Constraint_Error =>
         Put_Line ("Constraint error raised: " & Ada.Exceptions.Exception_Message (E));

   end;
end Samples.Petstore.Client;
