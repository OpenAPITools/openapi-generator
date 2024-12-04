--
-- "OpenAPI Petstore"
-- Prepared SQL queries for 'ApiResponse' definition.
-- Created using 'openapi-generator' ('postgresql-schema' generator)
-- (https://openapi-generator.tech/docs/generators/postgresql-schema)
--
-- Created: 04.12.2024 22:27:11
--


--
-- SELECT template for table 'ApiResponse'
--
SELECT code, "type", message FROM ApiResponse WHERE 1=1;

--
-- INSERT template for table 'ApiResponse'
--
INSERT INTO ApiResponse (code, "type", message) VALUES (?, ?, ?);

--
-- UPDATE template for table 'ApiResponse'
--
UPDATE ApiResponse SET code = ?, "type" = ?, message = ? WHERE 1=2;

--
-- DELETE template for table 'ApiResponse'
--
DELETE FROM ApiResponse WHERE 1=2;

