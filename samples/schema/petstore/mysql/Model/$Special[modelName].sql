--
-- OpenAPI Petstore.
-- Prepared SQL queries for '$special[model.name]' definition.
--


--
-- SELECT template for table `$special[model.name]`
--
SELECT `$special[property.name]` FROM `$special[model.name]` WHERE 1;

--
-- INSERT template for table `$special[model.name]`
--
INSERT INTO `$special[model.name]`(`$special[property.name]`) VALUES (?);

--
-- UPDATE template for table `$special[model.name]`
--
UPDATE `$special[model.name]` SET `$special[property.name]` = ? WHERE 1;

--
-- DELETE template for table `$special[model.name]`
--
DELETE FROM `$special[model.name]` WHERE 0;

