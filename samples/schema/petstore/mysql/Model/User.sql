--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'User' definition.
--


--
-- SELECT template for table `User`
--
SELECT `id`, `username`, `firstName`, `lastName`, `email`, `password`, `phone`, `userStatus` FROM `User` WHERE 1;

--
-- INSERT template for table `User`
--
INSERT INTO `User`(`id`, `username`, `firstName`, `lastName`, `email`, `password`, `phone`, `userStatus`) VALUES (?, ?, ?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table `User`
--
UPDATE `User` SET `id` = ?, `username` = ?, `firstName` = ?, `lastName` = ?, `email` = ?, `password` = ?, `phone` = ?, `userStatus` = ? WHERE 1;

--
-- DELETE template for table `User`
--
DELETE FROM `User` WHERE 0;

