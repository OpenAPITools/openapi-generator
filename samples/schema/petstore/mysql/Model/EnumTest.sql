--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'Enum_Test' definition.
--


--
-- SELECT template for table `Enum_Test`
--
SELECT `enum_string`, `enum_string_required`, `enum_integer`, `enum_number`, `outerEnum`, `outerEnumInteger`, `outerEnumDefaultValue`, `outerEnumIntegerDefaultValue` FROM `Enum_Test` WHERE 1;

--
-- INSERT template for table `Enum_Test`
--
INSERT INTO `Enum_Test`(`enum_string`, `enum_string_required`, `enum_integer`, `enum_number`, `outerEnum`, `outerEnumInteger`, `outerEnumDefaultValue`, `outerEnumIntegerDefaultValue`) VALUES (?, ?, ?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table `Enum_Test`
--
UPDATE `Enum_Test` SET `enum_string` = ?, `enum_string_required` = ?, `enum_integer` = ?, `enum_number` = ?, `outerEnum` = ?, `outerEnumInteger` = ?, `outerEnumDefaultValue` = ?, `outerEnumIntegerDefaultValue` = ? WHERE 1;

--
-- DELETE template for table `Enum_Test`
--
DELETE FROM `Enum_Test` WHERE 0;

