--
-- "OpenAPI Petstore"
-- Prepared SQL queries for 'Order' definition.
-- Created using 'openapi-generator' ('postgresql-schema' generator)
-- (https://openapi-generator.tech/docs/generators/postgresql-schema)
--


--
-- SELECT template for table 'order'
--
SELECT "id", pet_id, quantity, ship_date, status, complete FROM "order" WHERE 1=1;

--
-- INSERT template for table 'order'
--
INSERT INTO "order" ("id", pet_id, quantity, ship_date, status, complete) VALUES (?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table 'order'
--
UPDATE "order" SET "id" = ?, pet_id = ?, quantity = ?, ship_date = ?, status = ?, complete = ? WHERE 1=2;

--
-- DELETE template for table 'order'
--
DELETE FROM "order" WHERE 1=2;

