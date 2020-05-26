--
-- OpenAPI Petstore.
-- Prepared SQL queries for 'XmlItem' definition.
--


--
-- SELECT template for table `XmlItem`
--
SELECT `attribute_string`, `attribute_number`, `attribute_integer`, `attribute_boolean`, `wrapped_array`, `name_string`, `name_number`, `name_integer`, `name_boolean`, `name_array`, `name_wrapped_array`, `prefix_string`, `prefix_number`, `prefix_integer`, `prefix_boolean`, `prefix_array`, `prefix_wrapped_array`, `namespace_string`, `namespace_number`, `namespace_integer`, `namespace_boolean`, `namespace_array`, `namespace_wrapped_array`, `prefix_ns_string`, `prefix_ns_number`, `prefix_ns_integer`, `prefix_ns_boolean`, `prefix_ns_array`, `prefix_ns_wrapped_array` FROM `XmlItem` WHERE 1;

--
-- INSERT template for table `XmlItem`
--
INSERT INTO `XmlItem`(`attribute_string`, `attribute_number`, `attribute_integer`, `attribute_boolean`, `wrapped_array`, `name_string`, `name_number`, `name_integer`, `name_boolean`, `name_array`, `name_wrapped_array`, `prefix_string`, `prefix_number`, `prefix_integer`, `prefix_boolean`, `prefix_array`, `prefix_wrapped_array`, `namespace_string`, `namespace_number`, `namespace_integer`, `namespace_boolean`, `namespace_array`, `namespace_wrapped_array`, `prefix_ns_string`, `prefix_ns_number`, `prefix_ns_integer`, `prefix_ns_boolean`, `prefix_ns_array`, `prefix_ns_wrapped_array`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);

--
-- UPDATE template for table `XmlItem`
--
UPDATE `XmlItem` SET `attribute_string` = ?, `attribute_number` = ?, `attribute_integer` = ?, `attribute_boolean` = ?, `wrapped_array` = ?, `name_string` = ?, `name_number` = ?, `name_integer` = ?, `name_boolean` = ?, `name_array` = ?, `name_wrapped_array` = ?, `prefix_string` = ?, `prefix_number` = ?, `prefix_integer` = ?, `prefix_boolean` = ?, `prefix_array` = ?, `prefix_wrapped_array` = ?, `namespace_string` = ?, `namespace_number` = ?, `namespace_integer` = ?, `namespace_boolean` = ?, `namespace_array` = ?, `namespace_wrapped_array` = ?, `prefix_ns_string` = ?, `prefix_ns_number` = ?, `prefix_ns_integer` = ?, `prefix_ns_boolean` = ?, `prefix_ns_array` = ?, `prefix_ns_wrapped_array` = ? WHERE 1;

--
-- DELETE template for table `XmlItem`
--
DELETE FROM `XmlItem` WHERE 0;

