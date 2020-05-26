--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'Tag' definition.
--


--
-- SELECT template for table `Tag`
--
SELECT `id`, `name` FROM `Tag` WHERE 1;

--
-- INSERT template for table `Tag`
--
INSERT INTO `Tag`(`id`, `name`) VALUES (?, ?);

--
-- UPDATE template for table `Tag`
--
UPDATE `Tag` SET `id` = ?, `name` = ? WHERE 1;

--
-- DELETE template for table `Tag`
--
DELETE FROM `Tag` WHERE 0;

