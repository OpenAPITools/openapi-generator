--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'NullableClass' definition.
--


--
-- SELECT template for table `NullableClass`
--
SELECT `integer_prop`, `number_prop`, `boolean_prop`, `string_prop`, `date_prop`, `datetime_prop`, `array_nullable_prop`, `array_and_items_nullable_prop`, `array_items_nullable`, `object_nullable_prop`, `object_and_items_nullable_prop`, `object_items_nullable` FROM `NullableClass` WHERE 1;

--
-- INSERT template for table `NullableClass`
--
INSERT INTO `NullableClass`(`integer_prop`, `number_prop`, `boolean_prop`, `string_prop`, `date_prop`, `datetime_prop`, `array_nullable_prop`, `array_and_items_nullable_prop`, `array_items_nullable`, `object_nullable_prop`, `object_and_items_nullable_prop`, `object_items_nullable`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table `NullableClass`
--
UPDATE `NullableClass` SET `integer_prop` = ?, `number_prop` = ?, `boolean_prop` = ?, `string_prop` = ?, `date_prop` = ?, `datetime_prop` = ?, `array_nullable_prop` = ?, `array_and_items_nullable_prop` = ?, `array_items_nullable` = ?, `object_nullable_prop` = ?, `object_and_items_nullable_prop` = ?, `object_items_nullable` = ? WHERE 1;

--
-- DELETE template for table `NullableClass`
--
DELETE FROM `NullableClass` WHERE 0;

