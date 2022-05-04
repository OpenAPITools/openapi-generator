--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'AllOfWithSingleRef' definition.
--


--
-- SELECT template for table `AllOfWithSingleRef`
--
SELECT `username`, `SingleRefType` FROM `AllOfWithSingleRef` WHERE 1;

--
-- INSERT template for table `AllOfWithSingleRef`
--
INSERT INTO `AllOfWithSingleRef`(`username`, `SingleRefType`) VALUES (?, ?);

--
-- UPDATE template for table `AllOfWithSingleRef`
--
UPDATE `AllOfWithSingleRef` SET `username` = ?, `SingleRefType` = ? WHERE 1;

--
-- DELETE template for table `AllOfWithSingleRef`
--
DELETE FROM `AllOfWithSingleRef` WHERE 0;

