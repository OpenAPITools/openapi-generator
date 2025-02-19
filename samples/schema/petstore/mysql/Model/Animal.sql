--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'Animal' definition.
--


--
-- SELECT template for table `Animal`
--
SELECT `className`, `color` FROM `Animal` WHERE 1;

--
-- INSERT template for table `Animal`
--
INSERT INTO `Animal`(`className`, `color`) VALUES (?, ?);

--
-- UPDATE template for table `Animal`
--
UPDATE `Animal` SET `className` = ?, `color` = ? WHERE 1;

--
-- DELETE template for table `Animal`
--
DELETE FROM `Animal` WHERE 0;

