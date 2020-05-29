--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'inline_object_1' definition.
--


--
-- SELECT template for table `inline_object_1`
--
SELECT `additionalMetadata`, `file` FROM `inline_object_1` WHERE 1;

--
-- INSERT template for table `inline_object_1`
--
INSERT INTO `inline_object_1`(`additionalMetadata`, `file`) VALUES (?, ?);

--
-- UPDATE template for table `inline_object_1`
--
UPDATE `inline_object_1` SET `additionalMetadata` = ?, `file` = ? WHERE 1;

--
-- DELETE template for table `inline_object_1`
--
DELETE FROM `inline_object_1` WHERE 0;

