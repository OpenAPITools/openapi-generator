--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'Dog' definition.
--


--
-- SELECT template for table `Dog`
--
SELECT `className`, `color`, `breed` FROM `Dog` WHERE 1;

--
-- INSERT template for table `Dog`
--
INSERT INTO `Dog`(`className`, `color`, `breed`) VALUES (?, ?, ?);

--
-- UPDATE template for table `Dog`
--
UPDATE `Dog` SET `className` = ?, `color` = ?, `breed` = ? WHERE 1;

--
-- DELETE template for table `Dog`
--
DELETE FROM `Dog` WHERE 0;

