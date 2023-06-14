--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'fakeBigDecimalMap_200_response' definition.
--


--
-- SELECT template for table `fakeBigDecimalMap_200_response`
--
SELECT `someId`, `someMap` FROM `fakeBigDecimalMap_200_response` WHERE 1;

--
-- INSERT template for table `fakeBigDecimalMap_200_response`
--
INSERT INTO `fakeBigDecimalMap_200_response`(`someId`, `someMap`) VALUES (?, ?);

--
-- UPDATE template for table `fakeBigDecimalMap_200_response`
--
UPDATE `fakeBigDecimalMap_200_response` SET `someId` = ?, `someMap` = ? WHERE 1;

--
-- DELETE template for table `fakeBigDecimalMap_200_response`
--
DELETE FROM `fakeBigDecimalMap_200_response` WHERE 0;

