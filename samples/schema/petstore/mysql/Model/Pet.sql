--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'Pet' definition.
--


--
-- SELECT template for table `Pet`
--
SELECT `id`, `category`, `name`, `photoUrls`, `tags`, `status` FROM `Pet` WHERE 1;

--
-- INSERT template for table `Pet`
--
INSERT INTO `Pet`(`id`, `category`, `name`, `photoUrls`, `tags`, `status`) VALUES (?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table `Pet`
--
UPDATE `Pet` SET `id` = ?, `category` = ?, `name` = ?, `photoUrls` = ?, `tags` = ?, `status` = ? WHERE 1;

--
-- DELETE template for table `Pet`
--
DELETE FROM `Pet` WHERE 0;

