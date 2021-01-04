--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'testEndpointParametersBody' definition.
--


--
-- SELECT template for table `testEndpointParametersBody`
--
SELECT `integer`, `int32`, `int64`, `number`, `float`, `double`, `string`, `pattern_without_delimiter`, `byte`, `binary`, `date`, `dateTime`, `password`, `callback` FROM `testEndpointParametersBody` WHERE 1;

--
-- INSERT template for table `testEndpointParametersBody`
--
INSERT INTO `testEndpointParametersBody`(`integer`, `int32`, `int64`, `number`, `float`, `double`, `string`, `pattern_without_delimiter`, `byte`, `binary`, `date`, `dateTime`, `password`, `callback`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table `testEndpointParametersBody`
--
UPDATE `testEndpointParametersBody` SET `integer` = ?, `int32` = ?, `int64` = ?, `number` = ?, `float` = ?, `double` = ?, `string` = ?, `pattern_without_delimiter` = ?, `byte` = ?, `binary` = ?, `date` = ?, `dateTime` = ?, `password` = ?, `callback` = ? WHERE 1;

--
-- DELETE template for table `testEndpointParametersBody`
--
DELETE FROM `testEndpointParametersBody` WHERE 0;

