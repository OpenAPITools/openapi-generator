--
-- "OpenAPI Petstore"
-- Prepared SQL queries for 'ApiResponse' definition.
-- Created using 'openapi-generator' ('postgresql-schema' generator)
-- (https://openapi-generator.tech/docs/generators/postgresql-schema)
--


--
-- SELECT template for table 'api_response'
--
SELECT code, "type", message FROM api_response WHERE 1=1;

--
-- INSERT template for table 'api_response'
--
INSERT INTO api_response (code, "type", message) VALUES (?, ?, ?);

--
-- UPDATE template for table 'api_response'
--
UPDATE api_response SET code = ?, "type" = ?, message = ? WHERE 1=2;

--
-- DELETE template for table 'api_response'
--
DELETE FROM api_response WHERE 1=2;

