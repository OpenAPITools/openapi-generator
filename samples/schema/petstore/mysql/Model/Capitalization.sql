--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'Capitalization' definition.
--


--
-- SELECT template for table `Capitalization`
--
SELECT `smallCamel`, `CapitalCamel`, `small_Snake`, `Capital_Snake`, `SCA_ETH_Flow_Points`, `ATT_NAME` FROM `Capitalization` WHERE 1;

--
-- INSERT template for table `Capitalization`
--
INSERT INTO `Capitalization`(`smallCamel`, `CapitalCamel`, `small_Snake`, `Capital_Snake`, `SCA_ETH_Flow_Points`, `ATT_NAME`) VALUES (?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table `Capitalization`
--
UPDATE `Capitalization` SET `smallCamel` = ?, `CapitalCamel` = ?, `small_Snake` = ?, `Capital_Snake` = ?, `SCA_ETH_Flow_Points` = ?, `ATT_NAME` = ? WHERE 1;

--
-- DELETE template for table `Capitalization`
--
DELETE FROM `Capitalization` WHERE 0;

