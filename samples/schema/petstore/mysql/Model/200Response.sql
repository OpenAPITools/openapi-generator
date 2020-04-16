--
-- OpenAPI Petstore.
-- Prepared SQL queries for '200_response' definition.
--


--
-- SELECT template for table `200_response`
--
SELECT `name`, `class` FROM `200_response` WHERE 1;

--
-- INSERT template for table `200_response`
--
INSERT INTO `200_response`(`name`, `class`) VALUES (?, ?);

--
-- UPDATE template for table `200_response`
--
UPDATE `200_response` SET `name` = ?, `class` = ? WHERE 1;

--
-- DELETE template for table `200_response`
--
DELETE FROM `200_response` WHERE 0;

