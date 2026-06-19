--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'AdditionalPropertiesClass' definition.
--


--
-- SELECT template for table `AdditionalPropertiesClass`
--
SELECT `map_property`, `map_of_map_property` FROM `AdditionalPropertiesClass` WHERE 1;

--
-- INSERT template for table `AdditionalPropertiesClass`
--
INSERT INTO `AdditionalPropertiesClass`(`map_property`, `map_of_map_property`) VALUES (?, ?);

--
-- UPDATE template for table `AdditionalPropertiesClass`
--
UPDATE `AdditionalPropertiesClass` SET `map_property` = ?, `map_of_map_property` = ? WHERE 1;

--
-- DELETE template for table `AdditionalPropertiesClass`
--
DELETE FROM `AdditionalPropertiesClass` WHERE 0;

