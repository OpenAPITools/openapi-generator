--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'uploadFileWithRequiredFileBody' definition.
--


--
-- SELECT template for table `uploadFileWithRequiredFileBody`
--
SELECT `additionalMetadata`, `requiredFile` FROM `uploadFileWithRequiredFileBody` WHERE 1;

--
-- INSERT template for table `uploadFileWithRequiredFileBody`
--
INSERT INTO `uploadFileWithRequiredFileBody`(`additionalMetadata`, `requiredFile`) VALUES (?, ?);

--
-- UPDATE template for table `uploadFileWithRequiredFileBody`
--
UPDATE `uploadFileWithRequiredFileBody` SET `additionalMetadata` = ?, `requiredFile` = ? WHERE 1;

--
-- DELETE template for table `uploadFileWithRequiredFileBody`
--
DELETE FROM `uploadFileWithRequiredFileBody` WHERE 0;

