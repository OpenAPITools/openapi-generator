--
-- "OpenAPI Petstore"
-- Prepared SQL queries for 'Order' definition.
-- Created using 'openapi-generator' ('postgresql-schema' generator)
-- (https://openapi-generator.tech/docs/generators/postgresql-schema)
--
-- Created: 04.12.2024 22:27:11
--


--
-- SELECT template for table 'Order'
--
SELECT id, petId, quantity, shipDate, status, complete FROM "Order" WHERE 1=1;

--
-- INSERT template for table 'Order'
--
INSERT INTO "Order" (id, petId, quantity, shipDate, status, complete) VALUES (?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table 'Order'
--
UPDATE "Order" SET id = ?, petId = ?, quantity = ?, shipDate = ?, status = ?, complete = ? WHERE 1=2;

--
-- DELETE template for table 'Order'
--
DELETE FROM "Order" WHERE 1=2;

