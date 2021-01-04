--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'uploadFileBody' definition.
--


--
-- SELECT template for table `uploadFileBody`
--
SELECT `additionalMetadata`, `file` FROM `uploadFileBody` WHERE 1;

--
-- INSERT template for table `uploadFileBody`
--
INSERT INTO `uploadFileBody`(`additionalMetadata`, `file`) VALUES (?, ?);

--
-- UPDATE template for table `uploadFileBody`
--
UPDATE `uploadFileBody` SET `additionalMetadata` = ?, `file` = ? WHERE 1;

--
-- DELETE template for table `uploadFileBody`
--
DELETE FROM `uploadFileBody` WHERE 0;

