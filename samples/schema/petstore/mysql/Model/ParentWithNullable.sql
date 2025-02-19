--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'ParentWithNullable' definition.
--


--
-- SELECT template for table `ParentWithNullable`
--
SELECT `type`, `nullableProperty` FROM `ParentWithNullable` WHERE 1;

--
-- INSERT template for table `ParentWithNullable`
--
INSERT INTO `ParentWithNullable`(`type`, `nullableProperty`) VALUES (?, ?);

--
-- UPDATE template for table `ParentWithNullable`
--
UPDATE `ParentWithNullable` SET `type` = ?, `nullableProperty` = ? WHERE 1;

--
-- DELETE template for table `ParentWithNullable`
--
DELETE FROM `ParentWithNullable` WHERE 0;

