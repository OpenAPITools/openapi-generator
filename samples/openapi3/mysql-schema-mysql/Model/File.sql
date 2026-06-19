--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'File' definition.
--


--
-- SELECT template for table `File`
--
SELECT `sourceURI` FROM `File` WHERE 1;

--
-- INSERT template for table `File`
--
INSERT INTO `File`(`sourceURI`) VALUES (?);

--
-- UPDATE template for table `File`
--
UPDATE `File` SET `sourceURI` = ? WHERE 1;

--
-- DELETE template for table `File`
--
DELETE FROM `File` WHERE 0;

