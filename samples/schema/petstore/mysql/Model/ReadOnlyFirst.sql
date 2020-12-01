--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'ReadOnlyFirst' definition.
--


--
-- SELECT template for table `ReadOnlyFirst`
--
SELECT `bar`, `baz` FROM `ReadOnlyFirst` WHERE 1;

--
-- INSERT template for table `ReadOnlyFirst`
--
INSERT INTO `ReadOnlyFirst`(`bar`, `baz`) VALUES (?, ?);

--
-- UPDATE template for table `ReadOnlyFirst`
--
UPDATE `ReadOnlyFirst` SET `bar` = ?, `baz` = ? WHERE 1;

--
-- DELETE template for table `ReadOnlyFirst`
--
DELETE FROM `ReadOnlyFirst` WHERE 0;

