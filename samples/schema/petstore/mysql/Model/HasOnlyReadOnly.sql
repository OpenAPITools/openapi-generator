--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'hasOnlyReadOnly' definition.
--


--
-- SELECT template for table `hasOnlyReadOnly`
--
SELECT `bar`, `foo` FROM `hasOnlyReadOnly` WHERE 1;

--
-- INSERT template for table `hasOnlyReadOnly`
--
INSERT INTO `hasOnlyReadOnly`(`bar`, `foo`) VALUES (?, ?);

--
-- UPDATE template for table `hasOnlyReadOnly`
--
UPDATE `hasOnlyReadOnly` SET `bar` = ?, `foo` = ? WHERE 1;

--
-- DELETE template for table `hasOnlyReadOnly`
--
DELETE FROM `hasOnlyReadOnly` WHERE 0;

