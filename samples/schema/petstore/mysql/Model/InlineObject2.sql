--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'inline_object_2' definition.
--


--
-- SELECT template for table `inline_object_2`
--
SELECT `enum_form_string_array`, `enum_form_string` FROM `inline_object_2` WHERE 1;

--
-- INSERT template for table `inline_object_2`
--
INSERT INTO `inline_object_2`(`enum_form_string_array`, `enum_form_string`) VALUES (?, ?);

--
-- UPDATE template for table `inline_object_2`
--
UPDATE `inline_object_2` SET `enum_form_string_array` = ?, `enum_form_string` = ? WHERE 1;

--
-- DELETE template for table `inline_object_2`
--
DELETE FROM `inline_object_2` WHERE 0;

