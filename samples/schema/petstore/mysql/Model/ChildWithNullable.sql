--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'ChildWithNullable' definition.
--


--
-- SELECT template for table `ChildWithNullable`
--
SELECT `type`, `nullableProperty`, `otherProperty` FROM `ChildWithNullable` WHERE 1;

--
-- INSERT template for table `ChildWithNullable`
--
INSERT INTO `ChildWithNullable`(`type`, `nullableProperty`, `otherProperty`) VALUES (?, ?, ?);

--
-- UPDATE template for table `ChildWithNullable`
--
UPDATE `ChildWithNullable` SET `type` = ?, `nullableProperty` = ?, `otherProperty` = ? WHERE 1;

--
-- DELETE template for table `ChildWithNullable`
--
DELETE FROM `ChildWithNullable` WHERE 0;

