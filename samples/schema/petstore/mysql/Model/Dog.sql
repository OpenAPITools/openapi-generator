--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'Dog' definition.
--


--
-- SELECT template for table `Dog`
--
SELECT `type`, `color`, `breed` FROM `Dog` WHERE 1;

--
-- INSERT template for table `Dog`
--
INSERT INTO `Dog`(`type`, `color`, `breed`) VALUES (?, ?, ?);

--
-- UPDATE template for table `Dog`
--
UPDATE `Dog` SET `type` = ?, `color` = ?, `breed` = ? WHERE 1;

--
-- DELETE template for table `Dog`
--
DELETE FROM `Dog` WHERE 0;

