--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'MixedPropertiesAndAdditionalPropertiesClass' definition.
--


--
-- SELECT template for table `MixedPropertiesAndAdditionalPropertiesClass`
--
SELECT `uuid`, `dateTime`, `map` FROM `MixedPropertiesAndAdditionalPropertiesClass` WHERE 1;

--
-- INSERT template for table `MixedPropertiesAndAdditionalPropertiesClass`
--
INSERT INTO `MixedPropertiesAndAdditionalPropertiesClass`(`uuid`, `dateTime`, `map`) VALUES (?, ?, ?);

--
-- UPDATE template for table `MixedPropertiesAndAdditionalPropertiesClass`
--
UPDATE `MixedPropertiesAndAdditionalPropertiesClass` SET `uuid` = ?, `dateTime` = ?, `map` = ? WHERE 1;

--
-- DELETE template for table `MixedPropertiesAndAdditionalPropertiesClass`
--
DELETE FROM `MixedPropertiesAndAdditionalPropertiesClass` WHERE 0;

