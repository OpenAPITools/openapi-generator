--
-- "OpenAPI Petstore"
-- Prepared SQL queries for 'Category' definition.
-- Created using 'openapi-generator' ('postgresql-schema' generator)
-- (https://openapi-generator.tech/docs/generators/postgresql-schema)
--
-- Created: 04.12.2024 22:27:11
--


--
-- SELECT template for table 'Category'
--
SELECT id, "name" FROM Category WHERE 1=1;

--
-- INSERT template for table 'Category'
--
INSERT INTO Category (id, "name") VALUES (?, ?);

--
-- UPDATE template for table 'Category'
--
UPDATE Category SET id = ?, "name" = ? WHERE 1=2;

--
-- DELETE template for table 'Category'
--
DELETE FROM Category WHERE 1=2;

