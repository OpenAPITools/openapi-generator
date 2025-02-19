--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'inline_object' definition.
--


--
-- SELECT template for table `inline_object`
--
SELECT `name`, `status` FROM `inline_object` WHERE 1;

--
-- INSERT template for table `inline_object`
--
INSERT INTO `inline_object`(`name`, `status`) VALUES (?, ?);

--
-- UPDATE template for table `inline_object`
--
UPDATE `inline_object` SET `name` = ?, `status` = ? WHERE 1;

--
-- DELETE template for table `inline_object`
--
DELETE FROM `inline_object` WHERE 0;

