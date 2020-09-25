--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'ArrayTest' definition.
--


--
-- SELECT template for table `ArrayTest`
--
SELECT `array_of_string`, `array_array_of_integer`, `array_array_of_model` FROM `ArrayTest` WHERE 1;

--
-- INSERT template for table `ArrayTest`
--
INSERT INTO `ArrayTest`(`array_of_string`, `array_array_of_integer`, `array_array_of_model`) VALUES (?, ?, ?);

--
-- UPDATE template for table `ArrayTest`
--
UPDATE `ArrayTest` SET `array_of_string` = ?, `array_array_of_integer` = ?, `array_array_of_model` = ? WHERE 1;

--
-- DELETE template for table `ArrayTest`
--
DELETE FROM `ArrayTest` WHERE 0;

