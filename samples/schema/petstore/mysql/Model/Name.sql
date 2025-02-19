--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'Name' definition.
--


--
-- SELECT template for table `Name`
--
SELECT `name`, `snake_case`, `property`, `123Number` FROM `Name` WHERE 1;

--
-- INSERT template for table `Name`
--
INSERT INTO `Name`(`name`, `snake_case`, `property`, `123Number`) VALUES (?, ?, ?, ?);

--
-- UPDATE template for table `Name`
--
UPDATE `Name` SET `name` = ?, `snake_case` = ?, `property` = ?, `123Number` = ? WHERE 1;

--
-- DELETE template for table `Name`
--
DELETE FROM `Name` WHERE 0;

