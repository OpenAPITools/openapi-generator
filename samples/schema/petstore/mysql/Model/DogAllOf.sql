--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'DogAllOf' definition.
--


--
-- SELECT template for table `DogAllOf`
--
SELECT `breed` FROM `DogAllOf` WHERE 1;

--
-- INSERT template for table `DogAllOf`
--
INSERT INTO `DogAllOf`(`breed`) VALUES (?);

--
-- UPDATE template for table `DogAllOf`
--
UPDATE `DogAllOf` SET `breed` = ? WHERE 1;

--
-- DELETE template for table `DogAllOf`
--
DELETE FROM `DogAllOf` WHERE 0;

