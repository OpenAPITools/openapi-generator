--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'Dog' definition.
--


--
-- SELECT template for table `Dog`
--
SELECT `species`, `color`, `breed` FROM `Dog` WHERE 1;

--
-- INSERT template for table `Dog`
--
INSERT INTO `Dog`(`species`, `color`, `breed`) VALUES (?, ?, ?);

--
-- UPDATE template for table `Dog`
--
UPDATE `Dog` SET `species` = ?, `color` = ?, `breed` = ? WHERE 1;

--
-- DELETE template for table `Dog`
--
DELETE FROM `Dog` WHERE 0;

