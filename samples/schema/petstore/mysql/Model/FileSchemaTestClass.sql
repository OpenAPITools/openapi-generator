--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'FileSchemaTestClass' definition.
--


--
-- SELECT template for table `FileSchemaTestClass`
--
SELECT `file`, `files` FROM `FileSchemaTestClass` WHERE 1;

--
-- INSERT template for table `FileSchemaTestClass`
--
INSERT INTO `FileSchemaTestClass`(`file`, `files`) VALUES (?, ?);

--
-- UPDATE template for table `FileSchemaTestClass`
--
UPDATE `FileSchemaTestClass` SET `file` = ?, `files` = ? WHERE 1;

--
-- DELETE template for table `FileSchemaTestClass`
--
DELETE FROM `FileSchemaTestClass` WHERE 0;

