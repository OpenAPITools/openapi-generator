--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'Animal' definition.
--


--
-- SELECT template for table `Animal`
--
SELECT `type`, `color` FROM `Animal` WHERE 1;

--
-- INSERT template for table `Animal`
--
INSERT INTO `Animal`(`type`, `color`) VALUES (?, ?);

--
-- UPDATE template for table `Animal`
--
UPDATE `Animal` SET `type` = ?, `color` = ? WHERE 1;

--
-- DELETE template for table `Animal`
--
DELETE FROM `Animal` WHERE 0;

