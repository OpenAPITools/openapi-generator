--
-- "OpenAPI Petstore"
-- Prepared SQL queries for 'Category' definition.
-- Created using 'openapi-generator' ('postgresql-schema' generator)
-- (https://openapi-generator.tech/docs/generators/postgresql-schema)
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

