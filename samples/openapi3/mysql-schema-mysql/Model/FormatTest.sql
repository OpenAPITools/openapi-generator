--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'format_test' definition.
--


--
-- SELECT template for table `format_test`
--
SELECT `integer`, `int32`, `int64`, `number`, `float`, `double`, `decimal`, `string`, `byte`, `binary`, `date`, `dateTime`, `uuid`, `password`, `pattern_with_digits`, `pattern_with_digits_and_delimiter` FROM `format_test` WHERE 1;

--
-- INSERT template for table `format_test`
--
INSERT INTO `format_test`(`integer`, `int32`, `int64`, `number`, `float`, `double`, `decimal`, `string`, `byte`, `binary`, `date`, `dateTime`, `uuid`, `password`, `pattern_with_digits`, `pattern_with_digits_and_delimiter`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table `format_test`
--
UPDATE `format_test` SET `integer` = ?, `int32` = ?, `int64` = ?, `number` = ?, `float` = ?, `double` = ?, `decimal` = ?, `string` = ?, `byte` = ?, `binary` = ?, `date` = ?, `dateTime` = ?, `uuid` = ?, `password` = ?, `pattern_with_digits` = ?, `pattern_with_digits_and_delimiter` = ? WHERE 1;

--
-- DELETE template for table `format_test`
--
DELETE FROM `format_test` WHERE 0;

