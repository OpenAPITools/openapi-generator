--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'Cat' definition.
--


--
-- SELECT template for table `Cat`
--
SELECT `className`, `color`, `declawed` FROM `Cat` WHERE 1;

--
-- INSERT template for table `Cat`
--
INSERT INTO `Cat`(`className`, `color`, `declawed`) VALUES (?, ?, ?);

--
-- UPDATE template for table `Cat`
--
UPDATE `Cat` SET `className` = ?, `color` = ?, `declawed` = ? WHERE 1;

--
-- DELETE template for table `Cat`
--
DELETE FROM `Cat` WHERE 0;

