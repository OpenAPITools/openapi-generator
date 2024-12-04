--
-- "OpenAPI Petstore"
-- Prepared SQL queries for 'ApiResponse' definition.
-- Created using 'openapi-generator' ('postgresql-schema' generator)
-- (https://openapi-generator.tech/docs/generators/postgresql-schema)
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

