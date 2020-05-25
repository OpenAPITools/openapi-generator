--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'AdditionalPropertiesClass' definition.
--


--
-- SELECT template for table `AdditionalPropertiesClass`
--
SELECT `map_string`, `map_number`, `map_integer`, `map_boolean`, `map_array_integer`, `map_array_anytype`, `map_map_string`, `map_map_anytype`, `anytype_1`, `anytype_2`, `anytype_3` FROM `AdditionalPropertiesClass` WHERE 1;

--
-- INSERT template for table `AdditionalPropertiesClass`
--
INSERT INTO `AdditionalPropertiesClass`(`map_string`, `map_number`, `map_integer`, `map_boolean`, `map_array_integer`, `map_array_anytype`, `map_map_string`, `map_map_anytype`, `anytype_1`, `anytype_2`, `anytype_3`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table `AdditionalPropertiesClass`
--
UPDATE `AdditionalPropertiesClass` SET `map_string` = ?, `map_number` = ?, `map_integer` = ?, `map_boolean` = ?, `map_array_integer` = ?, `map_array_anytype` = ?, `map_map_string` = ?, `map_map_anytype` = ?, `anytype_1` = ?, `anytype_2` = ?, `anytype_3` = ? WHERE 1;

--
-- DELETE template for table `AdditionalPropertiesClass`
--
DELETE FROM `AdditionalPropertiesClass` WHERE 0;

