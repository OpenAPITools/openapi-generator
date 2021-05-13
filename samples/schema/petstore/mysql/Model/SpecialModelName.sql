--
-- OpenAPI Petstore.
-- Prepared SQL queries for '_special_model.name_' definition.
--


--
-- SELECT template for table `_special_model.name_`
--
SELECT `$special[property.name]` FROM `_special_model.name_` WHERE 1;

--
-- INSERT template for table `_special_model.name_`
--
INSERT INTO `_special_model.name_`(`$special[property.name]`) VALUES (?);

--
-- UPDATE template for table `_special_model.name_`
--
UPDATE `_special_model.name_` SET `$special[property.name]` = ? WHERE 1;

--
-- DELETE template for table `_special_model.name_`
--
DELETE FROM `_special_model.name_` WHERE 0;

