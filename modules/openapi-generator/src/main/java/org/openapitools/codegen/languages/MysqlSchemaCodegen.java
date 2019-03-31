/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class MysqlSchemaCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(MysqlSchemaCodegen.class);

    public static final String CODEGEN_VENDOR_EXTENSION_KEY = "x-mysqlSchema";
    public static final String DEFAULT_DATABASE_NAME = "defaultDatabaseName";
    public static final String JSON_DATA_TYPE_ENABLED = "jsonDataTypeEnabled";
    public static final Integer ENUM_MAX_ELEMENTS = 65535;
    public static final Integer IDENTIFIER_MAX_LENGTH = 64;

    protected Vector<String> mysqlNumericTypes = new Vector<String>(Arrays.asList(
            "BIGINT", "BIT", "BOOL", "BOOLEAN", "DEC", "DECIMAL", "DOUBLE", "DOUBLE PRECISION", "FIXED", "FLOAT", "INT", "INTEGER", "MEDIUMINT", "NUMERIC", "REAL", "SMALLINT", "TINYINT"
    ));

    protected Vector<String> mysqlDateAndTimeTypes = new Vector<String>(Arrays.asList(
            "DATE", "DATETIME", "TIME", "TIMESTAMP", "YEAR"
    ));

    protected Vector<String> mysqlStringTypes = new Vector<String>(Arrays.asList(
            "BINARY", "BLOB", "CHAR", "CHAR BYTE", "CHARACTER", "ENUM", "LONGBLOB", "LONGTEXT", "MEDIUMBLOB", "MEDIUMTEXT", "SET", "TEXT", "TINYBLOB", "TINYTEXT", "VARBINARY", "VARCHAR"
    ));

    protected Vector<String> mysqlSpatialTypes = new Vector<String>(Arrays.asList(
            "GEOMETRY", "GEOMETRYCOLLECTION", "LINESTRING", "MULTILINESTRING", "MULTIPOINT", "MULTIPOLYGON", "POINT", "POLYGON"
    ));

    protected String defaultDatabaseName = "", databaseNamePrefix = "", databaseNameSuffix = "_db";
    protected String tableNamePrefix = "tbl_", tableNameSuffix = "";
    protected String columnNamePrefix = "col_", columnNameSuffix = "";
    protected Boolean jsonDataTypeEnabled = true;

    public MysqlSchemaCodegen() {
        super();

        // clear import mapping (from default generator) as mysql does not use import directives
        importMapping.clear();

        //modelTestTemplateFiles.put("model_test.mustache", ".php");
        // no doc files
        // modelDocTemplateFiles.clear();
        // apiDocTemplateFiles.clear();

        // https://dev.mysql.com/doc/refman/8.0/en/keywords.html
        setReservedWordsLowerCase(
                Arrays.asList(
                        // SQL reserved words
                        "ACCESSIBLE", "ADD", "ALL", "ALTER", "ANALYZE", "AND", "AS", "ASC", "ASENSITIVE",
                        "BEFORE", "BETWEEN", "BIGINT", "BINARY", "BLOB", "BOTH", "BY",
                        "CALL", "CASCADE", "CASE", "CHANGE", "CHAR", "CHARACTER", "CHECK", "COLLATE", "COLUMN", "CONDITION", "CONSTRAINT", "CONTINUE", "CONVERT", "CREATE", "CROSS", "CUBE", "CUME_DIST", "CURRENT_DATE", "CURRENT_TIME", "CURRENT_TIMESTAMP", "CURRENT_USER", "CURSOR",
                        "DATABASE", "DATABASES", "DAY_HOUR", "DAY_MICROSECOND", "DAY_MINUTE", "DAY_SECOND", "DEC", "DECIMAL", "DECLARE", "DEFAULT", "DELAYED", "DELETE", "DENSE_RANK", "DESC", "DESCRIBE", "DETERMINISTIC", "DISTINCT", "DISTINCTROW", "DIV", "DOUBLE", "DROP", "DUAL",
                        "EACH", "ELSE", "ELSEIF", "EMPTY", "ENCLOSED", "ESCAPED", "EXCEPT", "EXISTS", "EXIT", "EXPLAIN",
                        "FALSE", "FETCH", "FIRST_VALUE", "FLOAT", "FLOAT4", "FLOAT8", "FOR", "FORCE", "FOREIGN", "FROM", "FULLTEXT", "FUNCTION",
                        "GENERATED", "GET", "GRANT", "GROUP", "GROUPING", "GROUPS",
                        "HAVING", "HIGH_PRIORITY", "HOUR_MICROSECOND", "HOUR_MINUTE", "HOUR_SECOND",
                        "IF", "IGNORE", "IN", "INDEX", "INFILE", "INNER", "INOUT", "INSENSITIVE", "INSERT", "INT", "INT1", "INT2", "INT3", "INT4", "INT8", "INTEGER", "INTERVAL", "INTO", "IO_AFTER_GTIDS", "IO_BEFORE_GTIDS", "IS", "ITERATE",
                        "JOIN", "JSON_TABLE",
                        "KEY", "KEYS", "KILL",
                        "LAG", "LAST_VALUE", "LEAD", "LEADING", "LEAVE", "LEFT", "LIKE", "LIMIT", "LINEAR", "LINES", "LOAD", "LOCALTIME", "LOCALTIMESTAMP", "LOCK", "LONG", "LONGBLOB", "LONGTEXT", "LOOP", "LOW_PRIORITY",
                        "MASTER_BIND", "MASTER_SSL_VERIFY_SERVER_CERT", "MATCH", "MAXVALUE", "MEDIUMBLOB", "MEDIUMINT", "MEDIUMTEXT", "MIDDLEINT", "MINUTE_MICROSECOND", "MINUTE_SECOND", "MOD", "MODIFIES",
                        "NATURAL", "NOT", "NO_WRITE_TO_BINLOG", "NTH_VALUE", "NTILE", "NULL", "NUMERIC",
                        "OF", "ON", "OPTIMIZE", "OPTIMIZER_COSTS", "OPTION", "OPTIONALLY", "OR", "ORDER", "OUT", "OUTER", "OUTFILE", "OVER",
                        "PARTITION", "PERCENT_RANK", "PERSIST", "PERSIST_ONLY", "PRECISION", "PRIMARY", "PROCEDURE", "PURGE",
                        "RANGE", "RANK", "READ", "READS", "READ_WRITE", "REAL", "RECURSIVE", "REFERENCES", "REGEXP", "RELEASE", "RENAME", "REPEAT", "REPLACE", "REQUIRE", "RESIGNAL", "RESTRICT", "RETURN", "REVOKE", "RIGHT", "RLIKE", "ROLE", "ROW", "ROWS", "ROW_NUMBER",
                        "SCHEMA", "SCHEMAS", "SECOND_MICROSECOND", "SELECT", "SENSITIVE", "SEPARATOR", "SET", "SHOW", "SIGNAL", "SMALLINT", "SPATIAL", "SPECIFIC", "SQL", "SQLEXCEPTION", "SQLSTATE", "SQLWARNING", "SQL_BIG_RESULT", "SQL_CALC_FOUND_ROWS", "SQL_SMALL_RESULT", "SSL", "STARTING", "STORED", "STRAIGHT_JOIN", "SYSTEM",
                        "TABLE", "TERMINATED", "THEN", "TINYBLOB", "TINYINT", "TINYTEXT", "TO", "TRAILING", "TRIGGER", "TRUE",
                        "UNDO", "UNION", "UNIQUE", "UNLOCK", "UNSIGNED", "UPDATE", "USAGE", "USE", "USING", "UTC_DATE", "UTC_TIME", "UTC_TIMESTAMP",
                        "VALUES", "VARBINARY", "VARCHAR", "VARCHARACTER", "VARYING", "VIRTUAL",
                        "WHEN", "WHERE", "WHILE", "WINDOW", "WITH", "WRITE",
                        "XOR",
                        "YEAR_MONTH",
                        "ZEROFILL"
                )
        );

        // all types can be threaded as primitives except array, object and refs
        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "bool",
                        "boolean",
                        "int",
                        "integer",
                        "double",
                        "float",
                        "string",
                        "date",
                        "Date",
                        "DateTime",
                        "long",
                        "short",
                        "char",
                        "ByteArray",
                        "binary",
                        "file",
                        "UUID",
                        "BigDecimal",
                        "mixed",
                        "number",
                        "void",
                        "byte"
                )
        );

        // https://dev.mysql.com/doc/refman/8.0/en/data-types.html
        typeMapping.put("array", "JSON");
        typeMapping.put("map", "JSON");
        typeMapping.put("List", "JSON");
        typeMapping.put("boolean", "BOOL");
        typeMapping.put("string", "TEXT");
        typeMapping.put("int", "INT");
        typeMapping.put("byte", "TEXT");
        typeMapping.put("float", "DECIMAL");
        typeMapping.put("number", "DECIMAL");
        typeMapping.put("date", "DATE");
        typeMapping.put("Date", "DATETIME");
        typeMapping.put("DateTime", "DATETIME");
        typeMapping.put("long", "BIGINT");
        typeMapping.put("short", "SMALLINT");
        typeMapping.put("char", "TEXT");
        typeMapping.put("double", "DECIMAL");
        typeMapping.put("object", "JSON");
        typeMapping.put("integer", "INT");
        typeMapping.put("ByteArray", "MEDIUMBLOB");
        typeMapping.put("binary", "MEDIUMBLOB");
        typeMapping.put("file", "MEDIUMBLOB");
        typeMapping.put("UUID", "TEXT");
        typeMapping.put("BigDecimal", "DECIMAL");

        embeddedTemplateDir = templateDir = "mysql-schema";

        // it seems that cli options from DefaultCodegen are useless here
        cliOptions.clear();
        addOption(DEFAULT_DATABASE_NAME, "Default database name for all MySQL queries", defaultDatabaseName);
        addSwitch(JSON_DATA_TYPE_ENABLED, "Use special JSON MySQL data type for complex model properties. Requires MySQL version 5.7.8. Generates TEXT data type when disabled", jsonDataTypeEnabled);
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SCHEMA;
    }

    @Override
    public String getName() {
        return "mysql-schema";
    }

    @Override
    public String getHelp() {
        return "Generates a MySQL schema based on the model or schema defined in the OpenAPI specification (v2, v3).";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(DEFAULT_DATABASE_NAME)) {
            if (additionalProperties.get(DEFAULT_DATABASE_NAME).equals("")) {
                additionalProperties.remove(DEFAULT_DATABASE_NAME);
            } else {
                this.setDefaultDatabaseName((String) additionalProperties.get(DEFAULT_DATABASE_NAME));
                // default database name may be escaped, need to overwrite additional prop
                additionalProperties.put(DEFAULT_DATABASE_NAME, getDefaultDatabaseName());
            }
        }

        if (additionalProperties.containsKey(JSON_DATA_TYPE_ENABLED)) {
            this.setJsonDataTypeEnabled(Boolean.valueOf(additionalProperties.get(JSON_DATA_TYPE_ENABLED).toString()));
        } else {
            additionalProperties.put(JSON_DATA_TYPE_ENABLED, getJsonDataTypeEnabled());
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("mysql_schema.mustache", "", "mysql_schema.sql"));
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        objs = super.postProcessModels(objs);

        List<Object> models = (List<Object>) objs.get("models");
        for (Object _mo : models) {
            Map<String, Object> mo = (Map<String, Object>) _mo;
            CodegenModel model = (CodegenModel) mo.get("model");
            String modelName = model.getName();
            String modelDescription = model.getDescription();
            Map<String, Object> modelVendorExtensions = model.getVendorExtensions();
            Map<String, Object> mysqlSchema = new HashMap<String, Object>();
            Map<String, Object> tableDefinition = new HashMap<String, Object>();

            if (modelVendorExtensions.containsKey(CODEGEN_VENDOR_EXTENSION_KEY)) {
                // user already specified schema values
                LOGGER.info("Found vendor extension in '" + modelName + "' model, autogeneration skipped");
                continue;
            } else {
                modelVendorExtensions.put(CODEGEN_VENDOR_EXTENSION_KEY, mysqlSchema);
                mysqlSchema.put("tableDefinition", tableDefinition);
                tableDefinition.put("tblName", toTableName(modelName));
                tableDefinition.put("tblComment", modelDescription);
            }
        }

        return objs;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        switch (property.getDataType().toUpperCase(Locale.ROOT)) {
            case "BOOL":
                processBooleanTypeProperty(model, property);
                break;
            case "TINYINT":
            case "SMALLINT":
            case "INT":
            case "BIGINT":
                processIntegerTypeProperty(model, property);
                break;
            case "DECIMAL":
                processDecimalTypeProperty(model, property);
                break;
            case "MEDIUMBLOB":
            case "TEXT":
                processStringTypeProperty(model, property);
                break;
            case "DATE":
            case "DATETIME":
                processDateTypeProperty(model, property);
                break;
            case "JSON":
                processJsonTypeProperty(model, property);
                break;
            default:
                processUnknownTypeProperty(model, property);
        }
    }

    /**
     * Processes each model's property mapped to integer type and adds related vendor extensions
     *
     * @param model    model
     * @param property model's property
     */
    public void processIntegerTypeProperty(CodegenModel model, CodegenProperty property) {
        Map<String, Object> vendorExtensions = property.getVendorExtensions();
        Map<String, Object> mysqlSchema = new HashMap<String, Object>();
        Map<String, Object> columnDefinition = new HashMap<String, Object>();
        ArrayList columnDataTypeArguments = new ArrayList();
        String baseName = property.getBaseName();
        String dataType = property.getDataType();
        String dataFormat = property.getDataFormat();
        String description = property.getDescription();
        String minimum = property.getMinimum();
        String maximum = property.getMaximum();
        Boolean exclusiveMinimum = property.getExclusiveMinimum();
        Boolean exclusiveMaximum = property.getIExclusiveMaximum();
        String defaultValue = property.getDefaultValue();
        Boolean required = property.getRequired();
        Boolean unsigned = false;
        Boolean isUuid = property.isUuid;
        Boolean isEnum = property.isEnum;

        if (vendorExtensions.containsKey(CODEGEN_VENDOR_EXTENSION_KEY)) {
            // user already specified schema values
            LOGGER.info("Found vendor extension in '" + baseName + "' property, autogeneration skipped");
            return;
        }

        vendorExtensions.put(CODEGEN_VENDOR_EXTENSION_KEY, mysqlSchema);
        mysqlSchema.put("columnDefinition", columnDefinition);
        columnDefinition.put("colName", toColumnName(baseName));

        if (Boolean.TRUE.equals(isEnum)) {
            Map<String, Object> allowableValues = property.getAllowableValues();
            List<Object> enumValues = (List<Object>) allowableValues.get("values");
            for (Integer i = 0; i < enumValues.size(); i++) {
                if (i > ENUM_MAX_ELEMENTS - 1) {
                    LOGGER.warn("ENUM column can have maximum of " + ENUM_MAX_ELEMENTS.toString() + " distinct elements, following value will be skipped: " + (String) enumValues.get(i));
                    break;
                }
                String value = String.valueOf(enumValues.get(i));
                columnDataTypeArguments.add(toCodegenMysqlDataTypeArgument(value, (Boolean) (i + 1 < enumValues.size())));
            }
            columnDefinition.put("colDataType", "ENUM");
            columnDefinition.put("colDataTypeArguments", columnDataTypeArguments);
        } else {
            if ("int64".equals(dataFormat)) {
                columnDefinition.put("colDataType", "BIGINT");
            } else {
                Long min = (minimum != null) ? Long.parseLong(minimum) : null;
                Long max = (maximum != null) ? Long.parseLong(maximum) : null;
                if (exclusiveMinimum == true && min != null) min += 1;
                if (exclusiveMaximum == true && max != null) max -= 1;
                if (min != null && min >= 0) {
                    unsigned = true;
                }
                columnDefinition.put("colUnsigned", unsigned);
                columnDefinition.put("colDataType", getMysqlMatchedIntegerDataType(min, max, unsigned));
            }
        }

        if (Boolean.TRUE.equals(required)) {
            columnDefinition.put("colNotNull", true);
        } else {
            columnDefinition.put("colNotNull", false);
            try {
                columnDefinition.put("colDefault", toCodegenMysqlDataTypeDefault(defaultValue, (String) columnDefinition.get("colDataType")));
            } catch (RuntimeException exception) {
                LOGGER.warn("Property '" + baseName + "' of model '" + model.getName() + "' mapped to MySQL data type which doesn't support default value");
                columnDefinition.put("colDefault", null);
            }
        }

        if (description != null) {
            columnDefinition.put("colComment", description);
        }
    }

    /**
     * Processes each model's property mapped to decimal type and adds related vendor extensions
     *
     * @param model    model
     * @param property model's property
     */
    public void processDecimalTypeProperty(CodegenModel model, CodegenProperty property) {
        Map<String, Object> vendorExtensions = property.getVendorExtensions();
        Map<String, Object> mysqlSchema = new HashMap<String, Object>();
        Map<String, Object> columnDefinition = new HashMap<String, Object>();
        ArrayList columnDataTypeArguments = new ArrayList();
        String baseName = property.getBaseName();
        String dataType = property.getDataType();
        String dataFormat = property.getDataFormat();
        String description = property.getDescription();
        String minimum = property.getMinimum();
        String maximum = property.getMaximum();
        Boolean exclusiveMinimum = property.getExclusiveMinimum();
        Boolean exclusiveMaximum = property.getIExclusiveMaximum();
        String defaultValue = property.getDefaultValue();
        Boolean required = property.getRequired();
        Boolean unsigned = false;
        Boolean isEnum = property.isEnum;

        if (vendorExtensions.containsKey(CODEGEN_VENDOR_EXTENSION_KEY)) {
            // user already specified schema values
            LOGGER.info("Found vendor extension in '" + baseName + "' property, autogeneration skipped");
            return;
        }

        vendorExtensions.put(CODEGEN_VENDOR_EXTENSION_KEY, mysqlSchema);
        mysqlSchema.put("columnDefinition", columnDefinition);
        columnDefinition.put("colName", toColumnName(baseName));

        if (Boolean.TRUE.equals(isEnum)) {
            Map<String, Object> allowableValues = property.getAllowableValues();
            List<Object> enumValues = (List<Object>) allowableValues.get("values");
            for (Integer i = 0; i < enumValues.size(); i++) {
                if (i > ENUM_MAX_ELEMENTS - 1) {
                    LOGGER.warn("ENUM column can have maximum of " + ENUM_MAX_ELEMENTS.toString() + " distinct elements, following value will be skipped: " + (String) enumValues.get(i));
                    break;
                }
                String value = String.valueOf(enumValues.get(i));
                columnDataTypeArguments.add(toCodegenMysqlDataTypeArgument(value, (Boolean) (i + 1 < enumValues.size())));
            }
            columnDefinition.put("colDataType", "ENUM");
            columnDefinition.put("colDataTypeArguments", columnDataTypeArguments);
        } else {
            Float min = (minimum != null) ? Float.valueOf(minimum) : null;
            Float max = (maximum != null) ? Float.valueOf(maximum) : null;
            if (exclusiveMinimum == true && min != null) min += 1;
            if (exclusiveMaximum == true && max != null) max -= 1;
            if (min != null && min >= 0) {
                unsigned = true;
            }
            columnDefinition.put("colDataType", "DECIMAL");
            columnDefinition.put("colUnsigned", unsigned);
            columnDefinition.put("colDataTypeArguments", columnDataTypeArguments);
            columnDataTypeArguments.add(toCodegenMysqlDataTypeArgument(20, true));
            columnDataTypeArguments.add(toCodegenMysqlDataTypeArgument(9, false));
        }

        if (Boolean.TRUE.equals(required)) {
            columnDefinition.put("colNotNull", true);
        } else {
            columnDefinition.put("colNotNull", false);
            try {
                columnDefinition.put("colDefault", toCodegenMysqlDataTypeDefault(defaultValue, (String) columnDefinition.get("colDataType")));
            } catch (RuntimeException exception) {
                LOGGER.warn("Property '" + baseName + "' of model '" + model.getName() + "' mapped to MySQL data type which doesn't support default value");
                columnDefinition.put("colDefault", null);
            }
        }

        if (description != null) {
            columnDefinition.put("colComment", description);
        }
    }

    /**
     * Processes each model's property mapped to boolean type and adds related vendor extensions
     *
     * @param model    model
     * @param property model's property
     */
    public void processBooleanTypeProperty(CodegenModel model, CodegenProperty property) {
        Map<String, Object> vendorExtensions = property.getVendorExtensions();
        Map<String, Object> mysqlSchema = new HashMap<String, Object>();
        Map<String, Object> columnDefinition = new HashMap<String, Object>();
        ArrayList columnDataTypeArguments = new ArrayList();
        String baseName = property.getBaseName();
        String description = property.getDescription();
        String defaultValue = property.getDefaultValue();
        Boolean required = property.getRequired();

        if (vendorExtensions.containsKey(CODEGEN_VENDOR_EXTENSION_KEY)) {
            // user already specified schema values
            LOGGER.info("Found vendor extension in '" + baseName + "' property, autogeneration skipped");
            return;
        }

        vendorExtensions.put(CODEGEN_VENDOR_EXTENSION_KEY, mysqlSchema);
        mysqlSchema.put("columnDefinition", columnDefinition);
        columnDefinition.put("colName", toColumnName(baseName));
        columnDefinition.put("colDataType", "TINYINT");
        columnDefinition.put("colDataTypeArguments", columnDataTypeArguments);
        columnDataTypeArguments.add(toCodegenMysqlDataTypeArgument(1, false));

        if (Boolean.TRUE.equals(required)) {
            columnDefinition.put("colNotNull", true);
        } else {
            columnDefinition.put("colNotNull", false);
            try {
                columnDefinition.put("colDefault", toCodegenMysqlDataTypeDefault(defaultValue, (String) columnDefinition.get("colDataType")));
            } catch (RuntimeException exception) {
                LOGGER.warn("Property '" + baseName + "' of model '" + model.getName() + "' mapped to MySQL data type which doesn't support default value");
                columnDefinition.put("colDefault", null);
            }
        }

        if (description != null) {
            columnDefinition.put("colComment", description);
        }
    }

    /**
     * Processes each model's property mapped to string type and adds related vendor extensions
     *
     * @param model    model
     * @param property model's property
     */
    public void processStringTypeProperty(CodegenModel model, CodegenProperty property) {
        Map<String, Object> vendorExtensions = property.getVendorExtensions();
        Map<String, Object> mysqlSchema = new HashMap<String, Object>();
        Map<String, Object> columnDefinition = new HashMap<String, Object>();
        ArrayList columnDataTypeArguments = new ArrayList();
        String baseName = property.getBaseName();
        String dataType = property.getDataType();
        String dataFormat = property.getDataFormat();
        String description = property.getDescription();
        Integer minLength = property.getMinLength();
        Integer maxLength = property.getMaxLength();
        String defaultValue = property.getDefaultValue();
        Boolean required = property.getRequired();
        Boolean isEnum = property.isEnum;

        if (vendorExtensions.containsKey(CODEGEN_VENDOR_EXTENSION_KEY)) {
            // user already specified schema values
            LOGGER.info("Found vendor extension in '" + baseName + "' property, autogeneration skipped");
            return;
        }

        vendorExtensions.put(CODEGEN_VENDOR_EXTENSION_KEY, mysqlSchema);
        mysqlSchema.put("columnDefinition", columnDefinition);
        columnDefinition.put("colName", toColumnName(baseName));

        if (Boolean.TRUE.equals(isEnum)) {
            Map<String, Object> allowableValues = property.getAllowableValues();
            List<Object> enumValues = (List<Object>) allowableValues.get("values");
            columnDefinition.put("colDataType", "ENUM");
            columnDefinition.put("colDataTypeArguments", columnDataTypeArguments);
            for (Integer i = 0; i < enumValues.size(); i++) {
                if (i > ENUM_MAX_ELEMENTS - 1) {
                    LOGGER.warn("ENUM column can have maximum of " + ENUM_MAX_ELEMENTS.toString() + " distinct elements, following value will be skipped: " + (String) enumValues.get(i));
                    break;
                }
                String value = String.valueOf(enumValues.get(i));
                columnDataTypeArguments.add(toCodegenMysqlDataTypeArgument(value, (Boolean) (i + 1 < enumValues.size())));
            }
        } else if (dataType.equals("MEDIUMBLOB")) {
            columnDefinition.put("colDataType", "MEDIUMBLOB");
        } else {
            String matchedStringType = getMysqlMatchedStringDataType(minLength, maxLength);
            columnDefinition.put("colDataType", matchedStringType);
            if (matchedStringType.equals("CHAR") || matchedStringType.equals("VARCHAR")) {
                columnDefinition.put("colDataTypeArguments", columnDataTypeArguments);
                columnDataTypeArguments.add(toCodegenMysqlDataTypeArgument((maxLength != null) ? maxLength : 255, false));
            }
        }

        if (Boolean.TRUE.equals(required)) {
            columnDefinition.put("colNotNull", true);
        } else {
            columnDefinition.put("colNotNull", false);
            try {
                columnDefinition.put("colDefault", toCodegenMysqlDataTypeDefault(defaultValue, (String) columnDefinition.get("colDataType")));
            } catch (RuntimeException exception) {
                LOGGER.warn("Property '" + baseName + "' of model '" + model.getName() + "' mapped to MySQL data type which doesn't support default value");
                columnDefinition.put("colDefault", null);
            }
        }

        if (description != null) {
            columnDefinition.put("colComment", description);
        }
    }

    /**
     * Processes each model's property mapped to date type and adds related vendor extensions
     *
     * @param model    model
     * @param property model's property
     */
    public void processDateTypeProperty(CodegenModel model, CodegenProperty property) {
        Map<String, Object> vendorExtensions = property.getVendorExtensions();
        Map<String, Object> mysqlSchema = new HashMap<String, Object>();
        Map<String, Object> columnDefinition = new HashMap<String, Object>();
        String baseName = property.getBaseName();
        String dataType = property.getDataType();
        Boolean required = property.getRequired();
        String description = property.getDescription();
        String defaultValue = property.getDefaultValue();

        if (vendorExtensions.containsKey(CODEGEN_VENDOR_EXTENSION_KEY)) {
            // user already specified schema values
            LOGGER.info("Found vendor extension in '" + baseName + "' property, autogeneration skipped");
            return;
        }

        vendorExtensions.put(CODEGEN_VENDOR_EXTENSION_KEY, mysqlSchema);
        mysqlSchema.put("columnDefinition", columnDefinition);
        columnDefinition.put("colName", toColumnName(baseName));
        columnDefinition.put("colDataType", dataType);

        if (Boolean.TRUE.equals(required)) {
            columnDefinition.put("colNotNull", true);
        } else {
            columnDefinition.put("colNotNull", false);
            try {
                columnDefinition.put("colDefault", toCodegenMysqlDataTypeDefault(defaultValue, (String) columnDefinition.get("colDataType")));
            } catch (RuntimeException exception) {
                LOGGER.warn("Property '" + baseName + "' of model '" + model.getName() + "' mapped to MySQL data type which doesn't support default value");
                columnDefinition.put("colDefault", null);
            }
        }

        if (description != null) {
            columnDefinition.put("colComment", description);
        }
    }

    /**
     * Processes each model's property mapped to JSON type and adds related vendor extensions
     *
     * @param model    model
     * @param property model's property
     */
    public void processJsonTypeProperty(CodegenModel model, CodegenProperty property) {
        Map<String, Object> vendorExtensions = property.getVendorExtensions();
        Map<String, Object> mysqlSchema = new HashMap<String, Object>();
        Map<String, Object> columnDefinition = new HashMap<String, Object>();
        String baseName = property.getBaseName();
        String dataType = property.getDataType();
        Boolean required = property.getRequired();
        String description = property.getDescription();
        String defaultValue = property.getDefaultValue();

        if (vendorExtensions.containsKey(CODEGEN_VENDOR_EXTENSION_KEY)) {
            // user already specified schema values
            LOGGER.info("Found vendor extension in '" + baseName + "' property, autogeneration skipped");
            return;
        }

        vendorExtensions.put(CODEGEN_VENDOR_EXTENSION_KEY, mysqlSchema);
        mysqlSchema.put("columnDefinition", columnDefinition);
        columnDefinition.put("colName", toColumnName(baseName));
        columnDefinition.put("colDataType", dataType);
        if (Boolean.FALSE.equals(getJsonDataTypeEnabled())) {
            columnDefinition.put("colDataType", "TEXT");
        }

        if (Boolean.TRUE.equals(required)) {
            columnDefinition.put("colNotNull", true);
        } else {
            columnDefinition.put("colNotNull", false);
            try {
                columnDefinition.put("colDefault", toCodegenMysqlDataTypeDefault(defaultValue, (String) columnDefinition.get("colDataType")));
            } catch (RuntimeException exception) {
                LOGGER.warn("Property '" + baseName + "' of model '" + model.getName() + "' mapped to MySQL data type which doesn't support default value");
                columnDefinition.put("colDefault", null);
            }
        }

        if (description != null) {
            columnDefinition.put("colComment", description);
        }
    }

    /**
     * Processes each model's property not mapped to any type and adds related vendor extensions
     * Most of time it's related to referenced properties eg. \Model\User
     *
     * @param model    model
     * @param property model's property
     */
    public void processUnknownTypeProperty(CodegenModel model, CodegenProperty property) {
        Map<String, Object> vendorExtensions = property.getVendorExtensions();
        Map<String, Object> mysqlSchema = new HashMap<String, Object>();
        Map<String, Object> columnDefinition = new HashMap<String, Object>();
        String baseName = property.getBaseName();
        Boolean required = property.getRequired();
        String description = property.getDescription();
        String defaultValue = property.getDefaultValue();

        if (vendorExtensions.containsKey(CODEGEN_VENDOR_EXTENSION_KEY)) {
            // user already specified schema values
            LOGGER.info("Found vendor extension in '" + baseName + "' property, autogeneration skipped");
            return;
        }

        vendorExtensions.put(CODEGEN_VENDOR_EXTENSION_KEY, mysqlSchema);
        mysqlSchema.put("columnDefinition", columnDefinition);
        columnDefinition.put("colName", toColumnName(baseName));
        columnDefinition.put("colDataType", "TEXT");

        if (Boolean.TRUE.equals(required)) {
            columnDefinition.put("colNotNull", true);
        } else {
            columnDefinition.put("colNotNull", false);
            try {
                columnDefinition.put("colDefault", toCodegenMysqlDataTypeDefault(defaultValue, (String) columnDefinition.get("colDataType")));
            } catch (RuntimeException exception) {
                LOGGER.warn("Property '" + baseName + "' of model '" + model.getName() + "' mapped to MySQL data type which doesn't support default value");
                columnDefinition.put("colDefault", null);
            }
        }

        if (description != null) {
            columnDefinition.put("colComment", description);
        }
    }

    /**
     * Generates codegen property for MySQL data type argument
     *
     * @param value   argument value
     * @param hasMore shows whether codegen has more arguments or not
     * @return generated codegen property
     */
    public HashMap<String, Object> toCodegenMysqlDataTypeArgument(Object value, Boolean hasMore) {
        HashMap<String, Object> arg = new HashMap<String, Object>();
        if (value instanceof String) {
            arg.put("isString", true);
            arg.put("isFloat", false);
            arg.put("isInteger", false);
            arg.put("isNumeric", false);
        } else if (value instanceof Integer || value instanceof Long) {
            arg.put("isString", false);
            arg.put("isFloat", false);
            arg.put("isInteger", true);
            arg.put("isNumeric", true);
        } else if (value instanceof Number) {
            arg.put("isString", false);
            arg.put("isFloat", true);
            arg.put("isInteger", false);
            arg.put("isNumeric", true);
        } else {
            LOGGER.warn("MySQL data type argument can be primitive type only. Class '" + value.getClass() + "' is provided");
        }
        arg.put("argumentValue", value);
        arg.put("hasMore", hasMore);
        return arg;
    }

    /**
     * Generates default codegen property for MySQL column definition
     * Ref: https://dev.mysql.com/doc/refman/5.7/en/data-type-defaults.html
     *
     * @param defaultValue  value
     * @param mysqlDataType MySQL data type
     * @return generated codegen property
     */
    public HashMap<String, Object> toCodegenMysqlDataTypeDefault(String defaultValue, String mysqlDataType) {
        HashMap<String, Object> defaultMap = new HashMap<String, Object>();
        if (defaultValue == null || defaultValue.toUpperCase(Locale.ROOT).equals("NULL")) {
            defaultMap.put("defaultValue", "NULL");
            defaultMap.put("isString", false);
            defaultMap.put("isNumeric", false);
            defaultMap.put("isKeyword", true);
            return defaultMap;
        }

        switch (mysqlDataType.toUpperCase(Locale.ROOT)) {
            case "TINYINT":
            case "SMALLINT":
            case "MEDIUMINT":
            case "INT":
            case "BIGINT":
                // SERIAL DEFAULT VALUE is a special case. In the definition of an integer column, it is an alias for NOT NULL AUTO_INCREMENT UNIQUE
                if (defaultValue.equals("SERIAL DEFAULT VALUE")) {
                    defaultMap.put("defaultValue", defaultValue);
                    defaultMap.put("isString", false);
                    defaultMap.put("isNumeric", false);
                    defaultMap.put("isKeyword", true);
                } else {
                    defaultMap.put("defaultValue", defaultValue);
                    defaultMap.put("isString", false);
                    defaultMap.put("isNumeric", true);
                    defaultMap.put("isKeyword", false);
                }
                return defaultMap;
            case "TIMESTAMP":
            case "DATETIME":
                // The exception is that, for TIMESTAMP and DATETIME columns, you can specify CURRENT_TIMESTAMP as the default
                if (defaultValue.equals("CURRENT_TIMESTAMP")) {
                    defaultMap.put("defaultValue", defaultValue);
                    defaultMap.put("isString", false);
                    defaultMap.put("isNumeric", false);
                    defaultMap.put("isKeyword", true);
                } else {
                    defaultMap.put("defaultValue", defaultValue);
                    defaultMap.put("isString", true);
                    defaultMap.put("isNumeric", false);
                    defaultMap.put("isKeyword", false);
                }
                return defaultMap;
            case "TINYBLOB":
            case "BLOB":
            case "MEDIUMBLOB":
            case "LONGBLOB":
            case "TINYTEXT":
            case "TEXT":
            case "MEDIUMTEXT":
            case "LONGTEXT":
            case "GEOMETRY":
            case "JSON":
                // The BLOB, TEXT, GEOMETRY, and JSON data types cannot be assigned a default value.
                throw new RuntimeException("The BLOB, TEXT, GEOMETRY, and JSON data types cannot be assigned a default value");
            default:
                defaultMap.put("defaultValue", defaultValue);
                defaultMap.put("isString", true);
                defaultMap.put("isNumeric", false);
                defaultMap.put("isKeyword", false);
                return defaultMap;
        }
    }

    /**
     * Finds best fitted MySQL data type for integer variable based on minimum and maximum properties
     *
     * @param minimum  (optional) codegen property
     * @param maximum  (optional) codegen property
     * @param unsigned (optional) whether variable is unsigned or not
     * @return MySQL integer data type
     */
    public String getMysqlMatchedIntegerDataType(Long minimum, Long maximum, Boolean unsigned) {
        // we can choose fit mysql data type
        // ref: https://dev.mysql.com/doc/refman/8.0/en/integer-types.html
        Long min = (minimum != null) ? minimum : -2147483648L;
        Long max = (maximum != null) ? maximum : 2147483647L;
        Long actualMin = Math.min(min, max); // sometimes min and max values can be mixed up
        Long actualMax = Math.max(min, max); // sometimes only minimum specified and it can be pretty high
        if (minimum != null && maximum != null && minimum > maximum) {
            LOGGER.warn("Codegen property 'minimum' cannot be greater than 'maximum'");
        }
        if (Boolean.TRUE.equals(unsigned) && actualMin >= 0) {
            if (actualMax <= 255) {
                return "TINYINT";
            } else if (actualMax <= 65535) {
                return "SMALLINT";
            } else if (actualMax <= 16777215) {
                return "MEDIUMINT";
            } else if (actualMax <= 4294967295L) {
                return "INT";
            } else if (actualMax > 4294967295L) {
                return "BIGINT";
            }
        } else {
            if (actualMin >= -128 && actualMax <= 127) {
                return "TINYINT";
            } else if (actualMin >= -32768 && actualMax <= 32767) {
                return "SMALLINT";
            } else if (actualMin >= -8388608 && actualMax <= 8388607) {
                return "MEDIUMINT";
            } else if (actualMin >= -2147483648 && actualMax <= 2147483647) {
                return "INT";
            } else if (actualMin < -2147483648 || actualMax > 2147483647) {
                return "BIGINT";
            }
        }

        return "INT";
    }

    /**
     * Finds best fitted MySQL data type for string variable based on minLength and maxLength properties
     *
     * @param minLength (optional) codegen property
     * @param maxLength (optional) codegen property
     * @return MySQL string data type
     */
    public String getMysqlMatchedStringDataType(Integer minLength, Integer maxLength) {
        // we can choose fit mysql data type
        // ref: https://dev.mysql.com/doc/refman/8.0/en/string-type-overview.html
        Integer min = (minLength != null && minLength >= 0) ? minLength : 0;
        Integer max = (maxLength != null && maxLength >= 0) ? maxLength : 65535;
        Integer actualMin = Math.min(min, max); // sometimes minLength and maxLength values can be mixed up
        Integer actualMax = Math.max(min, max); // sometimes only minLength specified and it can be pretty high
        if (minLength != null && maxLength != null && minLength > maxLength) {
            LOGGER.warn("Codegen property 'minLength' cannot be greater than 'maxLength'");
        }
        if (actualMax.equals(actualMin) && actualMax <= 255) {
            return "CHAR";
        } else if (actualMax <= 255) {
            return "VARCHAR";
        } else if (actualMax > 255 && actualMax <= 65535) {
            return "TEXT";
        } else if (actualMax > 65535 && actualMax <= 16777215) {
            return "MEDIUMTEXT";
        } else if (actualMax > 16777215) {
            return "LONGTEXT";
        }
        return "TEXT";
    }

    /**
     * Checks whether string is one of MySQL Data Types
     * Ref: https://dev.mysql.com/doc/refman/8.0/en/data-type-overview.html
     *
     * @param dataType which needs to check
     * @return true if value is correct MySQL data type, otherwise false
     */
    public Boolean isMysqlDataType(String dataType) {
        return (
                mysqlNumericTypes.contains(dataType.toUpperCase(Locale.ROOT)) ||
                        mysqlDateAndTimeTypes.contains(dataType.toUpperCase(Locale.ROOT)) ||
                        mysqlStringTypes.contains(dataType.toUpperCase(Locale.ROOT)) ||
                        mysqlSpatialTypes.contains(dataType.toUpperCase(Locale.ROOT)) ||
                        dataType.toUpperCase(Locale.ROOT).equals("JSON")
        );
    }

    /**
     * Converts name to valid MySQL database name
     * Produced name must be used with backticks only, eg. `database_name`
     *
     * @param name source name
     * @return database name
     */
    public String toDatabaseName(String name) {
        String identifier = toMysqlIdentifier(name, databaseNamePrefix, databaseNameSuffix);
        if (identifier.length() > IDENTIFIER_MAX_LENGTH) {
            LOGGER.warn("Database name cannot exceed 64 chars. Name '" + name + "' will be truncated");
            identifier = identifier.substring(0, IDENTIFIER_MAX_LENGTH);
        }
        return identifier;
    }

    /**
     * Converts name to valid MySQL column name
     * Produced name must be used with backticks only, eg. `table_name`
     *
     * @param name source name
     * @return table name
     */
    public String toTableName(String name) {
        String identifier = toMysqlIdentifier(name, tableNamePrefix, tableNameSuffix);
        if (identifier.length() > IDENTIFIER_MAX_LENGTH) {
            LOGGER.warn("Table name cannot exceed 64 chars. Name '" + name + "' will be truncated");
            identifier = identifier.substring(0, IDENTIFIER_MAX_LENGTH);
        }
        return identifier;
    }

    /**
     * Converts name to valid MySQL column name
     * Produced name must be used with backticks only, eg. `column_name`
     *
     * @param name source name
     * @return column name
     */
    public String toColumnName(String name) {
        String identifier = toMysqlIdentifier(name, columnNamePrefix, columnNameSuffix);
        if (identifier.length() > IDENTIFIER_MAX_LENGTH) {
            LOGGER.warn("Column name cannot exceed 64 chars. Name '" + name + "' will be truncated");
            identifier = identifier.substring(0, IDENTIFIER_MAX_LENGTH);
        }
        return identifier;
    }

    /**
     * Converts name to valid MySQL identifier which can be used as database, table, column name
     * Produced name must be used with backticks only, eg. `column_name`
     *
     * @param name   source name
     * @param prefix when escaped name is digits only, prefix will be prepended
     * @param suffix when escaped name is digits only, suffix will be appended
     * @return identifier name
     */
    public String toMysqlIdentifier(String name, String prefix, String suffix) {
        String escapedName = escapeMysqlQuotedIdentifier(name);
        // Database, table, and column names cannot end with space characters.
        if (escapedName.matches(".*\\s$")) {
            LOGGER.warn("Database, table, and column names cannot end with space characters. Check '" + name + "' name");
            escapedName = escapedName.replaceAll("\\s+$", "");
        }

        // Identifiers may begin with a digit but unless quoted may not consist solely of digits.
        if (escapedName.matches("^\\d+$")) {
            LOGGER.warn("Database, table, and column names cannot consist solely of digits. Check '" + name + "' name");
            escapedName = prefix + escapedName + suffix;
        }

        // identifier name cannot be empty
        if (escapedName.isEmpty()) {
            throw new RuntimeException("Empty database/table/column name for property '" + name.toString() + "' not allowed");
        }
        return escapedName;
    }

    /**
     * Escapes MySQL identifier to use it in SQL statements without backticks, eg. SELECT identifier FROM
     * Ref: https://dev.mysql.com/doc/refman/8.0/en/identifiers.html
     *
     * @param identifier source identifier
     * @return escaped identifier
     */
    public String escapeMysqlUnquotedIdentifier(String identifier) {
        // ASCII: [0-9,a-z,A-Z$_] (basic Latin letters, digits 0-9, dollar, underscore) Extended: U+0080 .. U+FFFF
        Pattern regexp = Pattern.compile("[^0-9a-zA-z$_\\u0080-\\uFFFF]");
        Matcher matcher = regexp.matcher(identifier);
        if (matcher.find()) {
            LOGGER.warn("Identifier '" + identifier + "' contains unsafe characters out of [0-9,a-z,A-Z$_] and U+0080..U+FFFF range");
            identifier = identifier.replaceAll("[^0-9a-zA-z$_\\u0080-\\uFFFF]", "");
        }

        // ASCII NUL (U+0000) and supplementary characters (U+10000 and higher) are not permitted in quoted or unquoted identifiers.
        // Don't know how to match these characters, hope that first regexp already strip them
        // Pattern regexp2 = Pattern.compile("[\0\uD800\uDC00-\uDBFF\uDFFF]");
        return identifier;
    }

    /**
     * Escapes MySQL identifier to use it in SQL statements with backticks, eg. SELECT `identifier` FROM
     * Ref: https://dev.mysql.com/doc/refman/8.0/en/identifiers.html
     *
     * @param identifier source identifier
     * @return escaped identifier
     */
    public String escapeMysqlQuotedIdentifier(String identifier) {
        // ASCII: U+0001 .. U+007F Extended: U+0080 .. U+FFFF
        Pattern regexp = Pattern.compile("[^\\u0001-\\u007F\\u0080-\\uFFFF]");
        Matcher matcher = regexp.matcher(identifier);
        if (matcher.find()) {
            LOGGER.warn("Identifier '" + identifier + "' contains unsafe characters out of U+0001..U+007F and U+0080..U+FFFF range");
            identifier = identifier.replaceAll("[^\\u0001-\\u007F\\u0080-\\uFFFF]", "");
        }

        // ASCII NUL (U+0000) and supplementary characters (U+10000 and higher) are not permitted in quoted or unquoted identifiers.
        // Don't know how to match these characters, hope that first regexp already strip them
        // Pattern regexp2 = Pattern.compile("[\0\uD800\uDC00-\uDBFF\uDFFF]");
        return identifier;
    }

    @Override
    public String escapeReservedWord(String name) {
        LOGGER.warn("'" + name + "' is MySQL reserved word. Do not use that word or properly escape it with backticks in mustache template");
        return name;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ' to avoid code injection
        return input.replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    /**
     * Sets default database name for all MySQL queries
     * Provided value will be escaped when necessary
     *
     * @param databaseName source name
     */
    public void setDefaultDatabaseName(String databaseName) {
        String escapedName = toDatabaseName(databaseName);
        if (escapedName.equals(databaseName) == false) {
            LOGGER.error("Invalid database name. '" + databaseName + "' cannot be used as MySQL identifier. Escaped value '" + escapedName + "' will be used instead.");
        }
        this.defaultDatabaseName = escapedName;
    }

    /**
     * Returns default database name for all MySQL queries
     * This value must be used with backticks only, eg. `database_name`
     *
     * @return default database name
     */
    public String getDefaultDatabaseName() {
        return this.defaultDatabaseName;
    }

    /**
     * Enables special JSON data type in all MySQL queries
     * JSON data type requires MySQL version 5.7.8
     *
     * @param enabled true to enable, otherwise false
     */
    public void setJsonDataTypeEnabled(Boolean enabled) {
        this.jsonDataTypeEnabled = enabled;
    }

    /**
     * Whether JSON data type enabled or disabled in all MySQL queries
     *
     * @return true if enabled otherwise false
     */
    public Boolean getJsonDataTypeEnabled() {
        return this.jsonDataTypeEnabled;
    }

}
