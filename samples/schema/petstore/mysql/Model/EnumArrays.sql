--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'EnumArrays' definition.
--


--
-- SELECT template for table `EnumArrays`
--
SELECT `just_symbol`, `array_enum` FROM `EnumArrays` WHERE 1;

--
-- INSERT template for table `EnumArrays`
--
INSERT INTO `EnumArrays`(`just_symbol`, `array_enum`) VALUES (?, ?);

--
-- UPDATE template for table `EnumArrays`
--
UPDATE `EnumArrays` SET `just_symbol` = ?, `array_enum` = ? WHERE 1;

--
-- DELETE template for table `EnumArrays`
--
DELETE FROM `EnumArrays` WHERE 0;

