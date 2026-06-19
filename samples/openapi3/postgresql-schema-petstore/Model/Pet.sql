--
-- "OpenAPI Petstore"
-- Prepared SQL queries for 'Pet' definition.
-- Created using 'openapi-generator' ('postgresql-schema' generator)
-- (https://openapi-generator.tech/docs/generators/postgresql-schema)
--


--
-- SELECT template for table 'pet'
--
SELECT "id", category, "name", photo_urls, tags, status FROM pet WHERE 1=1;

--
-- INSERT template for table 'pet'
--
INSERT INTO pet ("id", category, "name", photo_urls, tags, status) VALUES (?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table 'pet'
--
UPDATE pet SET "id" = ?, category = ?, "name" = ?, photo_urls = ?, tags = ?, status = ? WHERE 1=2;

--
-- DELETE template for table 'pet'
--
DELETE FROM pet WHERE 1=2;

