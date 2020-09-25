--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'BigCat' definition.
--


--
-- SELECT template for table `BigCat`
--
SELECT `className`, `color`, `declawed`, `kind` FROM `BigCat` WHERE 1;

--
-- INSERT template for table `BigCat`
--
INSERT INTO `BigCat`(`className`, `color`, `declawed`, `kind`) VALUES (?, ?, ?, ?);

--
-- UPDATE template for table `BigCat`
--
UPDATE `BigCat` SET `className` = ?, `color` = ?, `declawed` = ?, `kind` = ? WHERE 1;

--
-- DELETE template for table `BigCat`
--
DELETE FROM `BigCat` WHERE 0;

