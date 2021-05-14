--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'ObjectWithDeprecatedFields' definition.
--


--
-- SELECT template for table `ObjectWithDeprecatedFields`
--
SELECT `uuid`, `id`, `deprecatedRef`, `bars` FROM `ObjectWithDeprecatedFields` WHERE 1;

--
-- INSERT template for table `ObjectWithDeprecatedFields`
--
INSERT INTO `ObjectWithDeprecatedFields`(`uuid`, `id`, `deprecatedRef`, `bars`) VALUES (?, ?, ?, ?);

--
-- UPDATE template for table `ObjectWithDeprecatedFields`
--
UPDATE `ObjectWithDeprecatedFields` SET `uuid` = ?, `id` = ?, `deprecatedRef` = ?, `bars` = ? WHERE 1;

--
-- DELETE template for table `ObjectWithDeprecatedFields`
--
DELETE FROM `ObjectWithDeprecatedFields` WHERE 0;

