--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'TypeHolderDefault' definition.
--


--
-- SELECT template for table `TypeHolderDefault`
--
SELECT `string_item`, `number_item`, `integer_item`, `bool_item`, `array_item` FROM `TypeHolderDefault` WHERE 1;

--
-- INSERT template for table `TypeHolderDefault`
--
INSERT INTO `TypeHolderDefault`(`string_item`, `number_item`, `integer_item`, `bool_item`, `array_item`) VALUES (?, ?, ?, ?, ?);

--
-- UPDATE template for table `TypeHolderDefault`
--
UPDATE `TypeHolderDefault` SET `string_item` = ?, `number_item` = ?, `integer_item` = ?, `bool_item` = ?, `array_item` = ? WHERE 1;

--
-- DELETE template for table `TypeHolderDefault`
--
DELETE FROM `TypeHolderDefault` WHERE 0;

