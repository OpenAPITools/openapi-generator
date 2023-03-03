--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'Special-Cat' definition.
--


--
-- SELECT template for table `Special-Cat`
--
SELECT `className`, `color`, `declawed`, `kind` FROM `Special-Cat` WHERE 1;

--
-- INSERT template for table `Special-Cat`
--
INSERT INTO `Special-Cat`(`className`, `color`, `declawed`, `kind`) VALUES (?, ?, ?, ?);

--
-- UPDATE template for table `Special-Cat`
--
UPDATE `Special-Cat` SET `className` = ?, `color` = ?, `declawed` = ?, `kind` = ? WHERE 1;

--
-- DELETE template for table `Special-Cat`
--
DELETE FROM `Special-Cat` WHERE 0;

