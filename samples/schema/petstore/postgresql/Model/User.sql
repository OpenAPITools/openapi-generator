--
-- "OpenAPI Petstore"
-- Prepared SQL queries for 'User' definition.
-- Created using 'openapi-generator' ('postgresql-schema' generator)
-- (https://openapi-generator.tech/docs/generators/postgresql-schema)
--
-- Created: 04.12.2024 22:27:11
--


--
-- SELECT template for table 'User'
--
SELECT id, username, firstName, lastName, email, "password", phone, userStatus FROM "User" WHERE 1=1;

--
-- INSERT template for table 'User'
--
INSERT INTO "User" (id, username, firstName, lastName, email, "password", phone, userStatus) VALUES (?, ?, ?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table 'User'
--
UPDATE "User" SET id = ?, username = ?, firstName = ?, lastName = ?, email = ?, "password" = ?, phone = ?, userStatus = ? WHERE 1=2;

--
-- DELETE template for table 'User'
--
DELETE FROM "User" WHERE 1=2;

