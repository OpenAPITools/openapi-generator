--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'testEnumParametersBody' definition.
--


--
-- SELECT template for table `testEnumParametersBody`
--
SELECT `enum_form_string_array`, `enum_form_string` FROM `testEnumParametersBody` WHERE 1;

--
-- INSERT template for table `testEnumParametersBody`
--
INSERT INTO `testEnumParametersBody`(`enum_form_string_array`, `enum_form_string`) VALUES (?, ?);

--
-- UPDATE template for table `testEnumParametersBody`
--
UPDATE `testEnumParametersBody` SET `enum_form_string_array` = ?, `enum_form_string` = ? WHERE 1;

--
-- DELETE template for table `testEnumParametersBody`
--
DELETE FROM `testEnumParametersBody` WHERE 0;

