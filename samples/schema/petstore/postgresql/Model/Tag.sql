--
-- "OpenAPI Petstore"
-- Prepared SQL queries for 'Tag' definition.
-- Created using 'openapi-generator' ('postgresql-schema' generator)
-- (https://openapi-generator.tech/docs/generators/postgresql-schema)
--
-- Created: 04.12.2024 22:27:11
--


--
-- SELECT template for table 'Tag'
--
SELECT id, "name" FROM Tag WHERE 1=1;

--
-- INSERT template for table 'Tag'
--
INSERT INTO Tag (id, "name") VALUES (?, ?);

--
-- UPDATE template for table 'Tag'
--
UPDATE Tag SET id = ?, "name" = ? WHERE 1=2;

--
-- DELETE template for table 'Tag'
--
DELETE FROM Tag WHERE 1=2;

