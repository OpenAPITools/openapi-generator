--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'TypeHolderExample' definition.
--


--
-- SELECT template for table `TypeHolderExample`
--
SELECT `string_item`, `number_item`, `float_item`, `integer_item`, `bool_item`, `array_item` FROM `TypeHolderExample` WHERE 1;

--
-- INSERT template for table `TypeHolderExample`
--
INSERT INTO `TypeHolderExample`(`string_item`, `number_item`, `float_item`, `integer_item`, `bool_item`, `array_item`) VALUES (?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table `TypeHolderExample`
--
UPDATE `TypeHolderExample` SET `string_item` = ?, `number_item` = ?, `float_item` = ?, `integer_item` = ?, `bool_item` = ?, `array_item` = ? WHERE 1;

--
-- DELETE template for table `TypeHolderExample`
--
DELETE FROM `TypeHolderExample` WHERE 0;

