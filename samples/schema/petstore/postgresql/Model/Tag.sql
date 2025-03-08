--
-- "OpenAPI Petstore"
-- Prepared SQL queries for 'Tag' definition.
-- Created using 'openapi-generator' ('postgresql-schema' generator)
-- (https://openapi-generator.tech/docs/generators/postgresql-schema)
--


--
-- SELECT template for table 'tag'
--
SELECT "id", "name" FROM tag WHERE 1=1;

--
-- INSERT template for table 'tag'
--
INSERT INTO tag ("id", "name") VALUES (?, ?);

--
-- UPDATE template for table 'tag'
--
UPDATE tag SET "id" = ?, "name" = ? WHERE 1=2;

--
-- DELETE template for table 'tag'
--
DELETE FROM tag WHERE 1=2;

