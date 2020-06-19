--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'inline_object_3' definition.
--


--
-- SELECT template for table `inline_object_3`
--
SELECT `integer`, `int32`, `int64`, `number`, `float`, `double`, `string`, `pattern_without_delimiter`, `byte`, `binary`, `date`, `dateTime`, `password`, `callback` FROM `inline_object_3` WHERE 1;

--
-- INSERT template for table `inline_object_3`
--
INSERT INTO `inline_object_3`(`integer`, `int32`, `int64`, `number`, `float`, `double`, `string`, `pattern_without_delimiter`, `byte`, `binary`, `date`, `dateTime`, `password`, `callback`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table `inline_object_3`
--
UPDATE `inline_object_3` SET `integer` = ?, `int32` = ?, `int64` = ?, `number` = ?, `float` = ?, `double` = ?, `string` = ?, `pattern_without_delimiter` = ?, `byte` = ?, `binary` = ?, `date` = ?, `dateTime` = ?, `password` = ?, `callback` = ? WHERE 1;

--
-- DELETE template for table `inline_object_3`
--
DELETE FROM `inline_object_3` WHERE 0;

