--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'MapTest' definition.
--


--
-- SELECT template for table `MapTest`
--
SELECT `map_map_of_string`, `map_of_enum_string`, `direct_map`, `indirect_map` FROM `MapTest` WHERE 1;

--
-- INSERT template for table `MapTest`
--
INSERT INTO `MapTest`(`map_map_of_string`, `map_of_enum_string`, `direct_map`, `indirect_map`) VALUES (?, ?, ?, ?);

--
-- UPDATE template for table `MapTest`
--
UPDATE `MapTest` SET `map_map_of_string` = ?, `map_of_enum_string` = ?, `direct_map` = ?, `indirect_map` = ? WHERE 1;

--
-- DELETE template for table `MapTest`
--
DELETE FROM `MapTest` WHERE 0;

