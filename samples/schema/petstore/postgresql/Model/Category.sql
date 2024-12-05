--
-- "OpenAPI Petstore"
-- Prepared SQL queries for 'Category' definition.
-- Created using 'openapi-generator' ('postgresql-schema' generator)
-- (https://openapi-generator.tech/docs/generators/postgresql-schema)
--


--
-- SELECT template for table 'category'
--
SELECT "id", "name" FROM category WHERE 1=1;

--
-- INSERT template for table 'category'
--
INSERT INTO category ("id", "name") VALUES (?, ?);

--
-- UPDATE template for table 'category'
--
UPDATE category SET "id" = ?, "name" = ? WHERE 1=2;

--
-- DELETE template for table 'category'
--
DELETE FROM category WHERE 1=2;

