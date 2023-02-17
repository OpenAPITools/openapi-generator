--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'Cat' definition.
--


--
-- SELECT template for table `Cat`
--
SELECT `type`, `color`, `declawed` FROM `Cat` WHERE 1;

--
-- INSERT template for table `Cat`
--
INSERT INTO `Cat`(`type`, `color`, `declawed`) VALUES (?, ?, ?);

--
-- UPDATE template for table `Cat`
--
UPDATE `Cat` SET `type` = ?, `color` = ?, `declawed` = ? WHERE 1;

--
-- DELETE template for table `Cat`
--
DELETE FROM `Cat` WHERE 0;

