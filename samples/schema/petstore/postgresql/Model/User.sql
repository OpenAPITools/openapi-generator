--
-- "OpenAPI Petstore"
-- Prepared SQL queries for 'User' definition.
-- Created using 'openapi-generator' ('postgresql-schema' generator)
-- (https://openapi-generator.tech/docs/generators/postgresql-schema)
--


--
-- SELECT template for table 'user'
--
SELECT "id", username, first_name, last_name, email, "password", phone, user_status FROM "user" WHERE 1=1;

--
-- INSERT template for table 'user'
--
INSERT INTO "user" ("id", username, first_name, last_name, email, "password", phone, user_status) VALUES (?, ?, ?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table 'user'
--
UPDATE "user" SET "id" = ?, username = ?, first_name = ?, last_name = ?, email = ?, "password" = ?, phone = ?, user_status = ? WHERE 1=2;

--
-- DELETE template for table 'user'
--
DELETE FROM "user" WHERE 1=2;

