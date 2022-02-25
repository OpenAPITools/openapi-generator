--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'NumberOnly' definition.
--


--
-- SELECT template for table `NumberOnly`
--
SELECT `JustNumber` FROM `NumberOnly` WHERE 1;

--
-- INSERT template for table `NumberOnly`
--
INSERT INTO `NumberOnly`(`JustNumber`) VALUES (?);

--
-- UPDATE template for table `NumberOnly`
--
UPDATE `NumberOnly` SET `JustNumber` = ? WHERE 1;

--
-- DELETE template for table `NumberOnly`
--
DELETE FROM `NumberOnly` WHERE 0;

