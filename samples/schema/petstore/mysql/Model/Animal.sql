--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'Animal' definition.
--


--
-- SELECT template for table `Animal`
--
SELECT `species`, `color` FROM `Animal` WHERE 1;

--
-- INSERT template for table `Animal`
--
INSERT INTO `Animal`(`species`, `color`) VALUES (?, ?);

--
-- UPDATE template for table `Animal`
--
UPDATE `Animal` SET `species` = ?, `color` = ? WHERE 1;

--
-- DELETE template for table `Animal`
--
DELETE FROM `Animal` WHERE 0;

