--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'updatePetWithFormBody' definition.
--


--
-- SELECT template for table `updatePetWithFormBody`
--
SELECT `name`, `status` FROM `updatePetWithFormBody` WHERE 1;

--
-- INSERT template for table `updatePetWithFormBody`
--
INSERT INTO `updatePetWithFormBody`(`name`, `status`) VALUES (?, ?);

--
-- UPDATE template for table `updatePetWithFormBody`
--
UPDATE `updatePetWithFormBody` SET `name` = ?, `status` = ? WHERE 1;

--
-- DELETE template for table `updatePetWithFormBody`
--
DELETE FROM `updatePetWithFormBody` WHERE 0;

