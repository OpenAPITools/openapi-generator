--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'Order' definition.
--


--
-- SELECT template for table `Order`
--
SELECT `id`, `petId`, `quantity`, `shipDate`, `status`, `complete` FROM `Order` WHERE 1;

--
-- INSERT template for table `Order`
--
INSERT INTO `Order`(`id`, `petId`, `quantity`, `shipDate`, `status`, `complete`) VALUES (?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table `Order`
--
UPDATE `Order` SET `id` = ?, `petId` = ?, `quantity` = ?, `shipDate` = ?, `status` = ?, `complete` = ? WHERE 1;

--
-- DELETE template for table `Order`
--
DELETE FROM `Order` WHERE 0;

