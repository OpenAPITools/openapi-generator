--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'OuterComposite' definition.
--


--
-- SELECT template for table `OuterComposite`
--
SELECT `my_number`, `my_string`, `my_boolean` FROM `OuterComposite` WHERE 1;

--
-- INSERT template for table `OuterComposite`
--
INSERT INTO `OuterComposite`(`my_number`, `my_string`, `my_boolean`) VALUES (?, ?, ?);

--
-- UPDATE template for table `OuterComposite`
--
UPDATE `OuterComposite` SET `my_number` = ?, `my_string` = ?, `my_boolean` = ? WHERE 1;

--
-- DELETE template for table `OuterComposite`
--
DELETE FROM `OuterComposite` WHERE 0;

