/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.openapitools.codegen.utils.StringUtils.underscore;

@SuppressWarnings("unchecked")
public class PostgresqlSchemaCodegen extends DefaultCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(PostgresqlSchemaCodegen.class);

    public static final String VENDOR_EXTENSION_POSTGRESQL_SCHEMA = "x-postgresql-schema";
    public static final String DEFAULT_DATABASE_NAME = "defaultDatabaseName";
    public static final String JSON_DATA_TYPE = "jsonDataType";
    public static final String IDENTIFIER_NAMING_CONVENTION = "identifierNamingConvention";
    public static final String NAMED_PARAMETERS_ENABLED = "namedParametersEnabled";
    public static final String ID_AUTOINC_ENABLED = "idAutoIncEnabled";
    public static final Integer ENUM_MAX_ELEMENTS = 65535;
    public static final Integer IDENTIFIER_MAX_LENGTH = 63;

    protected Vector<String> postgresqlNumericTypes = new Vector<>(Arrays.asList(
            "SMALLINT", "INTEGER", "INT", "BIGINT", "DECIMAL", "NUMERIC", "REAL", "DOUBLE PRECISION", "SMALLSERIAL",
            "SERIAL",
            "BIGSERIAL"));

    protected Vector<String> postgresqlDateAndTimeTypes = new Vector<>(Arrays.asList(
            "DATE", "TIME", "TIME WITH TIME ZONE", "TIMESTAMP", "TIMESTAMP WITH TIME ZONE", "INTERVAL"));

    protected Vector<String> postgresqlStringTypes = new Vector<>(Arrays.asList(
            "CHAR", "VARCHAR", "TEXT"));

    protected Vector<String> postgresqlSpatialTypes = new Vector<>(Arrays.asList(
            "POINT", "LINE", "LSEG", "BOX", "PATH", "POLYGON", "CIRCLE"));

    /**
     * Returns default database name for all PostgreSQL queries
     * This value must be used with backticks only, e.g. `database_name`
     */
    @Getter
    protected String defaultDatabaseName = "", databaseNamePrefix = "", databaseNameSuffix = "_db";
    protected String tableNamePrefix = "tbl_", tableNameSuffix = "";
    protected String columnNamePrefix = "col_", columnNameSuffix = "";
    /**
     * Which type of JSON data types will be used.
     * JSON data type requires PostgreSQL version 9.4 or newer
     */
    @Getter
    @Setter
    protected String jsonDataType = "json";
    /**
     * Whether named parameters enabled or disabled in prepared SQLs
     */
    @Getter
    @Setter
    protected Boolean namedParametersEnabled = false;
    /**
     * Returns identifier naming convention for table names and column names.
     */
    @Getter
    protected String identifierNamingConvention = "snake_case";
    /**
     * Whether autoincrement feature enabled for integer 'id' fields
     */
    @Getter
    @Setter
    protected Boolean idAutoIncEnabled = false;

    public PostgresqlSchemaCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.noneOf(WireFormatFeature.class))
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling)
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism)
                .clientModificationFeatures(EnumSet.noneOf(ClientModificationFeature.class)));
        // clear import mapping (from default generator) as postgresql does not use
        // import directives
        importMapping.clear();

        setModelPackage("Model");
        modelTemplateFiles.put("query_examples.mustache", ".sql");

        // https://www.postgresql.org/docs/17/sql-keywords-appendix.html
        setReservedWordsLowerCase(
                Arrays.asList(
                        // SQL reserved words
                        "A", "ABORT", "ABS", "ABSENT", "ABSOLUTE", "ACCESS", "ACCORDING", "ACOS", "ACTION", "ADA",
                        "ADD", "ADMIN", "AFTER", "AGGREGATE", "ALL", "ALLOCATE", "ALSO", "ALTER", "ALWAYS", "ANALYSE",
                        "ANALYZE", "AND", "ANY", "ANY_VALUE", "ARE", "ARRAY", "ARRAY_AGG", "ARRAY_MAX_CARDINALITY",
                        "AS", "ASC", "ASENSITIVE", "ASIN", "ASSERTION", "ASSIGNMENT", "ASYMMETRIC", "AT", "ATAN",
                        "ATOMIC", "ATTACH", "ATTRIBUTE", "ATTRIBUTES", "AUTHORIZATION", "AVG", "BACKWARD", "BASE64",
                        "BEFORE", "BEGIN", "BEGIN_FRAME", "BEGIN_PARTITION", "BERNOULLI", "BETWEEN", "BIGINT", "BINARY",
                        "BIT", "BIT_LENGTH", "BLOB", "BLOCKED", "BOM", "BOOLEAN", "BOTH", "BREADTH", "BTRIM", "BY", "C",
                        "CACHE", "CALL", "CALLED", "CARDINALITY", "CASCADE", "CASCADED", "CASE", "CAST", "CATALOG",
                        "CATALOG_NAME", "CEIL", "CEILING", "CHAIN", "CHAINING", "CHAR", "CHARACTER", "CHARACTERISTICS",
                        "CHARACTERS", "CHARACTER_LENGTH", "CHARACTER_SET_CATALOG", "CHARACTER_SET_NAME",
                        "CHARACTER_SET_SCHEMA", "CHAR_LENGTH", "CHECK", "CHECKPOINT", "CLASS", "CLASSIFIER",
                        "CLASS_ORIGIN", "CLOB", "CLOSE", "CLUSTER", "COALESCE", "COBOL", "COLLATE", "COLLATION",
                        "COLLATION_CATALOG", "COLLATION_NAME", "COLLATION_SCHEMA", "COLLECT", "COLUMN", "COLUMNS",
                        "COLUMN_NAME", "COMMAND_FUNCTION", "COMMAND_FUNCTION_CODE", "COMMENT", "COMMENTS", "COMMIT",
                        "COMMITTED", "COMPRESSION", "CONCURRENTLY", "CONDITION", "CONDITIONAL", "CONDITION_NUMBER",
                        "CONFIGURATION", "CONFLICT", "CONNECT", "CONNECTION", "CONNECTION_NAME", "CONSTRAINT",
                        "CONSTRAINTS", "CONSTRAINT_CATALOG", "CONSTRAINT_NAME", "CONSTRAINT_SCHEMA", "CONSTRUCTOR",
                        "CONTAINS", "CONTENT", "CONTINUE", "CONTROL", "CONVERSION", "CONVERT", "COPARTITION", "COPY",
                        "CORR", "CORRESPONDING", "COS", "COSH", "COST", "COUNT", "COVAR_POP", "COVAR_SAMP", "CREATE",
                        "CROSS", "CSV", "CUBE", "CUME_DIST", "CURRENT", "CURRENT_CATALOG", "CURRENT_DATE",
                        "CURRENT_DEFAULT_TRANSFORM_GROUP", "CURRENT_PATH", "CURRENT_ROLE", "CURRENT_ROW",
                        "CURRENT_SCHEMA", "CURRENT_TIME", "CURRENT_TIMESTAMP", "CURRENT_TRANSFORM_GROUP_FOR_TYPE",
                        "CURRENT_USER", "CURSOR", "CURSOR_NAME", "CYCLE", "DATA", "DATABASE", "DATALINK", "DATE",
                        "DATETIME_INTERVAL_CODE", "DATETIME_INTERVAL_PRECISION", "DAY", "DB", "DEALLOCATE", "DEC",
                        "DECFLOAT", "DECIMAL", "DECLARE", "DEFAULT", "DEFAULTS", "DEFERRABLE", "DEFERRED", "DEFINE",
                        "DEFINED", "DEFINER", "DEGREE", "DELETE", "DELIMITER", "DELIMITERS", "DENSE_RANK", "DEPENDS",
                        "DEPTH", "DEREF", "DERIVED", "DESC", "DESCRIBE", "DESCRIPTOR", "DETACH", "DETERMINISTIC",
                        "DIAGNOSTICS", "DICTIONARY", "DISABLE", "DISCARD", "DISCONNECT", "DISPATCH", "DISTINCT",
                        "DLNEWCOPY", "DLPREVIOUSCOPY", "DLURLCOMPLETE", "DLURLCOMPLETEONLY", "DLURLCOMPLETEWRITE",
                        "DLURLPATH", "DLURLPATHONLY", "DLURLPATHWRITE", "DLURLSCHEME", "DLURLSERVER", "DLVALUE", "DO",
                        "DOCUMENT", "DOMAIN", "DOUBLE", "DROP", "DYNAMIC", "DYNAMIC_FUNCTION", "DYNAMIC_FUNCTION_CODE",
                        "EACH", "ELEMENT", "ELSE", "EMPTY", "ENABLE", "ENCODING", "ENCRYPTED", "END", "END-EXEC",
                        "END_FRAME", "END_PARTITION", "ENFORCED", "ENUM", "EQUALS", "ERROR", "ESCAPE", "EVENT", "EVERY",
                        "EXCEPT", "EXCEPTION", "EXCLUDE", "EXCLUDING", "EXCLUSIVE", "EXEC", "EXECUTE", "EXISTS", "EXP",
                        "EXPLAIN", "EXPRESSION", "EXTENSION", "EXTERNAL", "EXTRACT", "FALSE", "FAMILY", "FETCH", "FILE",
                        "FILTER", "FINAL", "FINALIZE", "FINISH", "FIRST", "FIRST_VALUE", "FLAG", "FLOAT", "FLOOR",
                        "FOLLOWING", "FOR", "FORCE", "FOREIGN", "FORMAT", "FORTRAN", "FORWARD", "FOUND", "FRAME_ROW",
                        "FREE", "FREEZE", "FROM", "FS", "FULFILL", "FULL", "FUNCTION", "FUNCTIONS", "FUSION", "G",
                        "GENERAL", "GENERATED", "GET", "GLOBAL", "GO", "GOTO", "GRANT", "GRANTED", "GREATEST", "GROUP",
                        "GROUPING", "GROUPS", "HANDLER", "HAVING", "HEADER", "HEX", "HIERARCHY", "HOLD", "HOUR", "ID",
                        "IDENTITY", "IF", "IGNORE", "ILIKE", "IMMEDIATE", "IMMEDIATELY", "IMMUTABLE", "IMPLEMENTATION",
                        "IMPLICIT", "IMPORT", "IN", "INCLUDE", "INCLUDING", "INCREMENT", "INDENT", "INDEX", "INDEXES",
                        "INDICATOR", "INHERIT", "INHERITS", "INITIAL", "INITIALLY", "INLINE", "INNER", "INOUT", "INPUT",
                        "INSENSITIVE", "INSERT", "INSTANCE", "INSTANTIABLE", "INSTEAD", "INT", "INTEGER", "INTEGRITY",
                        "INTERSECT", "INTERSECTION", "INTERVAL", "INTO", "INVOKER", "IS", "ISNULL", "ISOLATION", "JOIN",
                        "JSON", "JSON_ARRAY", "JSON_ARRAYAGG", "JSON_EXISTS", "JSON_OBJECT", "JSON_OBJECTAGG",
                        "JSON_QUERY", "JSON_SCALAR", "JSON_SERIALIZE", "JSON_TABLE", "JSON_TABLE_PRIMITIVE",
                        "JSON_VALUE", "K", "KEEP", "KEY", "KEYS", "KEY_MEMBER", "KEY_TYPE", "LABEL", "LAG", "LANGUAGE",
                        "LARGE", "LAST", "LAST_VALUE", "LATERAL", "LEAD", "LEADING", "LEAKPROOF", "LEAST", "LEFT",
                        "LENGTH", "LEVEL", "LIBRARY", "LIKE", "LIKE_REGEX", "LIMIT", "LINK", "LISTAGG", "LISTEN", "LN",
                        "LOAD", "LOCAL", "LOCALTIME", "LOCALTIMESTAMP", "LOCATION", "LOCATOR", "LOCK", "LOCKED", "LOG",
                        "LOG10", "LOGGED", "LOWER", "LPAD", "LTRIM", "M", "MAP", "MAPPING", "MATCH", "MATCHED",
                        "MATCHES", "MATCH_NUMBER", "MATCH_RECOGNIZE", "MATERIALIZED", "MAX", "MAXVALUE", "MEASURES",
                        "MEMBER", "MERGE", "MERGE_ACTION", "MESSAGE_LENGTH", "MESSAGE_OCTET_LENGTH", "MESSAGE_TEXT",
                        "METHOD", "MIN", "MINUTE", "MINVALUE", "MOD", "MODE", "MODIFIES", "MODULE", "MONTH", "MORE",
                        "MOVE", "MULTISET", "MUMPS", "NAME", "NAMES", "NAMESPACE", "NATIONAL", "NATURAL", "NCHAR",
                        "NCLOB", "NESTED", "NESTING", "NEW", "NEXT", "NFC", "NFD", "NFKC", "NFKD", "NIL", "NO", "NONE",
                        "NORMALIZE", "NORMALIZED", "NOT", "NOTHING", "NOTIFY", "NOTNULL", "NOWAIT", "NTH_VALUE",
                        "NTILE", "NULL", "NULLABLE", "NULLIF", "NULLS", "NULL_ORDERING", "NUMBER", "NUMERIC", "OBJECT",
                        "OCCURRENCE", "OCCURRENCES_REGEX", "OCTETS", "OCTET_LENGTH", "OF", "OFF", "OFFSET", "OIDS",
                        "OLD", "OMIT", "ON", "ONE", "ONLY", "OPEN", "OPERATOR", "OPTION", "OPTIONS", "OR", "ORDER",
                        "ORDERING", "ORDINALITY", "OTHERS", "OUT", "OUTER", "OUTPUT", "OVER", "OVERFLOW", "OVERLAPS",
                        "OVERLAY", "OVERRIDING", "OWNED", "OWNER", "P", "PAD", "PARALLEL", "PARAMETER",
                        "PARAMETER_MODE", "PARAMETER_NAME", "PARAMETER_ORDINAL_POSITION", "PARAMETER_SPECIFIC_CATALOG",
                        "PARAMETER_SPECIFIC_NAME", "PARAMETER_SPECIFIC_SCHEMA", "PARSER", "PARTIAL", "PARTITION",
                        "PASCAL", "PASS", "PASSING", "PASSTHROUGH", "PASSWORD", "PAST", "PATH", "PATTERN", "PER",
                        "PERCENT", "PERCENTILE_CONT", "PERCENTILE_DISC", "PERCENT_RANK", "PERIOD", "PERMISSION",
                        "PERMUTE", "PIPE", "PLACING", "PLAN", "PLANS", "PLI", "POLICY", "PORTION", "POSITION",
                        "POSITION_REGEX", "POWER", "PRECEDES", "PRECEDING", "PRECISION", "PREPARE", "PREPARED",
                        "PRESERVE", "PREV", "PRIMARY", "PRIOR", "PRIVATE", "PRIVILEGES", "PROCEDURAL", "PROCEDURE",
                        "PROCEDURES", "PROGRAM", "PRUNE", "PTF", "PUBLIC", "PUBLICATION", "QUOTE", "QUOTES", "RANGE",
                        "RANK", "READ", "READS", "REAL", "REASSIGN", "RECHECK", "RECOVERY", "RECURSIVE", "REF",
                        "REFERENCES", "REFERENCING", "REFRESH", "REGR_AVGX", "REGR_AVGY", "REGR_COUNT",
                        "REGR_INTERCEPT", "REGR_R2", "REGR_SLOPE", "REGR_SXX", "REGR_SXY", "REGR_SYY", "REINDEX",
                        "RELATIVE", "RELEASE", "RENAME", "REPEATABLE", "REPLACE", "REPLICA", "REQUIRING", "RESET",
                        "RESPECT", "RESTART", "RESTORE", "RESTRICT", "RESULT", "RETURN", "RETURNED_CARDINALITY",
                        "RETURNED_LENGTH", "RETURNED_OCTET_LENGTH", "RETURNED_SQLSTATE", "RETURNING", "RETURNS",
                        "REVOKE", "RIGHT", "ROLE", "ROLLBACK", "ROLLUP", "ROUTINE", "ROUTINES", "ROUTINE_CATALOG",
                        "ROUTINE_NAME", "ROUTINE_SCHEMA", "ROW", "ROWS", "ROW_COUNT", "ROW_NUMBER", "RPAD", "RTRIM",
                        "RULE", "RUNNING", "SAVEPOINT", "SCALAR", "SCALE", "SCHEMA", "SCHEMAS", "SCHEMA_NAME", "SCOPE",
                        "SCOPE_CATALOG", "SCOPE_NAME", "SCOPE_SCHEMA", "SCROLL", "SEARCH", "SECOND", "SECTION",
                        "SECURITY", "SEEK", "SELECT", "SELECTIVE", "SELF", "SEMANTICS", "SENSITIVE", "SEQUENCE",
                        "SEQUENCES", "SERIALIZABLE", "SERVER", "SERVER_NAME", "SESSION", "SESSION_USER", "SET", "SETOF",
                        "SETS", "SHARE", "SHOW", "SIMILAR", "SIMPLE", "SIN", "SINH", "SIZE", "SKIP", "SMALLINT",
                        "SNAPSHOT", "SOME", "SORT_DIRECTION", "SOURCE", "SPACE", "SPECIFIC", "SPECIFICTYPE",
                        "SPECIFIC_NAME", "SQL", "SQLCODE", "SQLERROR", "SQLEXCEPTION", "SQLSTATE", "SQLWARNING", "SQRT",
                        "STABLE", "STANDALONE", "START", "STATE", "STATEMENT", "STATIC", "STATISTICS", "STDDEV_POP",
                        "STDDEV_SAMP", "STDIN", "STDOUT", "STORAGE", "STORED", "STRICT", "STRING", "STRIP", "STRUCTURE",
                        "STYLE", "SUBCLASS_ORIGIN", "SUBMULTISET", "SUBSCRIPTION", "SUBSET", "SUBSTRING",
                        "SUBSTRING_REGEX", "SUCCEEDS", "SUM", "SUPPORT", "SYMMETRIC", "SYSID", "SYSTEM", "SYSTEM_TIME",
                        "SYSTEM_USER", "T", "TABLE", "TABLES", "TABLESAMPLE", "TABLESPACE", "TABLE_NAME", "TAN", "TANH",
                        "TARGET", "TEMP", "TEMPLATE", "TEMPORARY", "TEXT", "THEN", "THROUGH", "TIES", "TIME",
                        "TIMESTAMP", "TIMEZONE_HOUR", "TIMEZONE_MINUTE", "TO", "TOKEN", "TOP_LEVEL_COUNT", "TRAILING",
                        "TRANSACTION", "TRANSACTIONS_COMMITTED", "TRANSACTIONS_ROLLED_BACK", "TRANSACTION_ACTIVE",
                        "TRANSFORM", "TRANSFORMS", "TRANSLATE", "TRANSLATE_REGEX", "TRANSLATION", "TREAT", "TRIGGER",
                        "TRIGGER_CATALOG", "TRIGGER_NAME", "TRIGGER_SCHEMA", "TRIM", "TRIM_ARRAY", "TRUE", "TRUNCATE",
                        "TRUSTED", "TYPE", "TYPES", "UESCAPE", "UNBOUNDED", "UNCOMMITTED", "UNCONDITIONAL", "UNDER",
                        "UNENCRYPTED", "UNION", "UNIQUE", "UNKNOWN", "UNLINK", "UNLISTEN", "UNLOGGED", "UNMATCHED",
                        "UNNAMED", "UNNEST", "UNTIL", "UNTYPED", "UPDATE", "UPPER", "URI", "USAGE", "USER",
                        "USER_DEFINED_TYPE_CATALOG", "USER_DEFINED_TYPE_CODE", "USER_DEFINED_TYPE_NAME",
                        "USER_DEFINED_TYPE_SCHEMA", "USING", "UTF16", "UTF32", "UTF8", "VACUUM", "VALID", "VALIDATE",
                        "VALIDATOR", "VALUE", "VALUES", "VALUE_OF", "VARBINARY", "VARCHAR", "VARIADIC", "VARYING",
                        "VAR_POP", "VAR_SAMP", "VERBOSE", "VERSION", "VERSIONING", "VIEW", "VIEWS", "VOLATILE", "WHEN",
                        "WHENEVER", "WHERE", "WHITESPACE", "WIDTH_BUCKET", "WINDOW", "WITH", "WITHIN", "WITHOUT",
                        "WORK", "WRAPPER", "WRITE", "XML", "XMLAGG", "XMLATTRIBUTES", "XMLBINARY", "XMLCAST",
                        "XMLCOMMENT", "XMLCONCAT", "XMLDECLARATION", "XMLDOCUMENT", "XMLELEMENT", "XMLEXISTS",
                        "XMLFOREST", "XMLITERATE", "XMLNAMESPACES", "XMLPARSE", "XMLPI", "XMLQUERY", "XMLROOT",
                        "XMLSCHEMA", "XMLSERIALIZE", "XMLTABLE", "XMLTEXT", "XMLVALIDATE", "YEAR", "YES", "ZONE"));

        // primitive data types
        languageSpecificPrimitives = new HashSet<>(
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
                        "file",
                        "UUID",
                        "URI",
                        "BigDecimal",
                        "mixed",
                        "number",
                        "void",
                        "byte"));

        // https://www.postgresql.org/docs/17/datatype.html
        typeMapping.put("array", "JSON");
        typeMapping.put("set", "JSON");
        typeMapping.put("map", "JSON");
        typeMapping.put("List", "JSON");
        typeMapping.put("boolean", "BOOLEAN");
        typeMapping.put("string", "TEXT");
        typeMapping.put("int", "INTEGER");
        typeMapping.put("byte", "TEXT");
        typeMapping.put("float", "DECIMAL");
        typeMapping.put("number", "DECIMAL");
        typeMapping.put("date", "DATE");
        typeMapping.put("DateTime", "TIMESTAMP");
        typeMapping.put("long", "BIGINT");
        typeMapping.put("short", "SMALLINT");
        typeMapping.put("char", "TEXT");
        typeMapping.put("double", "DECIMAL");
        typeMapping.put("object", "JSON");
        typeMapping.put("integer", "INTEGER");
        typeMapping.put("ByteArray", "BYTEA");
        typeMapping.put("file", "BYTEA");
        typeMapping.put("UUID", "TEXT");
        typeMapping.put("URI", "TEXT");
        typeMapping.put("BigDecimal", "DECIMAL");

        embeddedTemplateDir = templateDir = "postgresql-schema";

        // it seems that cli options from DefaultCodegen are useless here
        cliOptions.clear();

        addOption(DEFAULT_DATABASE_NAME,
                "Database name that will be used for all generated PostgreSQL DDL and DML statements.", defaultDatabaseName);

        addSwitch(NAMED_PARAMETERS_ENABLED,
                "Generates query examples with named variables in value placeholders (eg.`:name`,`:quantity`) if `true`. Otherwise, generates question marks `?` in value placeholders.",
                namedParametersEnabled);

        addSwitch(ID_AUTOINC_ENABLED,
                "If `true`, generates autoincrement PostgreSQL types `SERIAL` and `BIGSERIAL` for `int32` and `int64` respectively for integer fields with name 'id'.",
                idAutoIncEnabled);

        // we used to snake_case table/column names, let's add this option
        CliOption identifierNamingOpt = new CliOption(IDENTIFIER_NAMING_CONVENTION,
                "Naming convention of PostgreSQL idebntifiers (table names and column names).");
        identifierNamingOpt.addEnum("snake_case", "Transform named to 'snake_case'.")
                .addEnum("original", "Leave original names as in `YAML` file.")
                .setDefault("snake_case");
        cliOptions.add(identifierNamingOpt);

        CliOption jsonDataTypeOpt = new CliOption(JSON_DATA_TYPE,
                "Use of PostgreSQL data types for complex model properties.");
        jsonDataTypeOpt.addEnum("json", "Generate `JSON` fields. Value is stored in `JSON` data type field as human-readable text. Value compliance with JSON standard is checked.")
                .addEnum("jsonb",
                        "Generate `JSONB` fields. Value is stored in `JSONB` data type field in binary format. `JSONB` data type is generally more efficient than `JSON` but it is not human-readable. Value compliance with JSON standard is checked.")
                .addEnum("off", "Generate `TEXT` fields. Just store the value as plain text. Value compliance with JSON standard is not checked.")
                .setDefault("json");
        cliOptions.add(jsonDataTypeOpt);
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SCHEMA;
    }

    @Override
    public String getName() {
        return "postgresql-schema";
    }

    @Override
    public String getHelp() {
        return "Generates a PostgreSQL schema based on the schema defined in the OpenAPI specification (v2, v3)";
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

        if (additionalProperties.containsKey(NAMED_PARAMETERS_ENABLED)) {
            this.setNamedParametersEnabled(
                    Boolean.valueOf(additionalProperties.get(NAMED_PARAMETERS_ENABLED).toString()));
        }
        additionalProperties.put(NAMED_PARAMETERS_ENABLED, getNamedParametersEnabled());

        if (additionalProperties.containsKey(ID_AUTOINC_ENABLED)) {
            this.setIdAutoIncEnabled(
                    Boolean.valueOf(additionalProperties.get(ID_AUTOINC_ENABLED).toString()));
        }
        additionalProperties.put(ID_AUTOINC_ENABLED, getIdAutoIncEnabled());

        if (additionalProperties.containsKey(IDENTIFIER_NAMING_CONVENTION)) {
            this.setIdentifierNamingConvention((String) additionalProperties.get(IDENTIFIER_NAMING_CONVENTION));
        }

        if (additionalProperties.containsKey(JSON_DATA_TYPE)) {
            this.setJsonDataType((String) additionalProperties.get(JSON_DATA_TYPE));
        }

        // make model src path available in mustache template
        additionalProperties.put("modelSrcPath", "./" + toSrcPath(modelPackage));

        supportingFiles.add(new SupportingFile(
                "README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile(
                "postgresql_schema.mustache", "", "postgresql_schema.sql"));
        supportingFiles.add(new SupportingFile(
                "postgresql_schema_oauth2.mustache", "", "postgresql_schema_oauth2.sql"));
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs = super.postProcessModels(objs);

        for (ModelMap mo : objs.getModels()) {
            CodegenModel model = mo.getModel();
            String modelName = model.getName();
            String tableName = this.toTableName(modelName);
            String modelDescription = model.getDescription();
            Map<String, Object> modelVendorExtensions = model.getVendorExtensions();
            Map<String, Object> postgresqlSchema = new HashMap<>();
            Map<String, Object> tableDefinition = new HashMap<>();

            if (this.getIdentifierNamingConvention().equals("snake_case") && !modelName.equals(tableName)) {
                // add original name in table comment
                String commentExtra = "Original model name - " + modelName + ".";
                modelDescription = (modelDescription == null || modelDescription.isEmpty()) ? commentExtra
                        : modelDescription + ". " + commentExtra;
            }

            if (modelVendorExtensions.containsKey(VENDOR_EXTENSION_POSTGRESQL_SCHEMA)) {
                // user already specified schema values
                LOGGER.info("Found vendor extension in '{}' model, autogeneration skipped", modelName);
            } else {
                modelVendorExtensions.put(VENDOR_EXTENSION_POSTGRESQL_SCHEMA, postgresqlSchema);
                postgresqlSchema.put("tableDefinition", tableDefinition);
                tableDefinition.put("tblName", tableName);
                tableDefinition.put("tblComment", modelDescription);
                if (isReservedWord(tableName)) { // Output table name in double quotes if it is a reserved word
                    tableDefinition.put("tblNameQuoted", true);
                }
            }
        }
        return objs;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        switch (property.getDataType().toUpperCase(Locale.ROOT)) {
            case "BOOLEAN":
                processBooleanTypeProperty(model, property);
                break;
            case "SMALLINT":
            case "INTEGER":
            case "BIGINT":
                processIntegerTypeProperty(model, property);
                break;
            case "DECIMAL":
                processDecimalTypeProperty(model, property);
                break;
            case "TEXT":
            case "BYTEA":
                processStringTypeProperty(model, property);
                break;
            case "DATE":
            case "TIMESTAMP":
                processDateTypeProperty(model, property);
                break;
            case "JSON":
            case "JSONB":
                processJsonTypeProperty(model, property);
                break;
            default:
                processUnknownTypeProperty(model, property);
        }
    }

    /**
     * Processes each model's property mapped to integer type and adds related
     * vendor extensions
     *
     * @param model    model
     * @param property model's property
     */
    public void processIntegerTypeProperty(CodegenModel model, CodegenProperty property) {
        Map<String, Object> vendorExtensions = property.getVendorExtensions();
        Map<String, Object> postgresqlSchema = new HashMap<>();
        Map<String, Object> columnDefinition = new HashMap<>();
        Map<String, Object> typeDefinition = new HashMap<>();
        ArrayList columnDataTypeArguments = new ArrayList();
        String baseName = property.getBaseName();
        String colName = this.toColumnName(baseName);
        String dataType = property.getDataType();
        String dataFormat = property.getDataFormat();
        String description = property.getDescription();
        String minimum = property.getMinimum();
        String maximum = property.getMaximum();
        boolean exclusiveMinimum = property.getExclusiveMinimum();
        boolean exclusiveMaximum = property.getIExclusiveMaximum();
        String defaultValue = property.getDefaultValue();
        Boolean required = property.getRequired();
        Boolean isUuid = property.isUuid;
        Boolean isEnum = property.isEnum;
        String tableName = this.toTableName(model.getName());

        if (vendorExtensions.containsKey(VENDOR_EXTENSION_POSTGRESQL_SCHEMA)) {
            // user already specified schema values
            LOGGER.info("Found vendor extension in '{}' property, autogeneration skipped", baseName);
            return;
        }

        if (this.getIdentifierNamingConvention().equals("snake_case") && !baseName.equals(colName)) {
            // add original name in column comment
            String commentExtra = "Original param name - " + baseName + ".";
            description = (description == null || description.isEmpty()) ? commentExtra
                    : description + ". " + commentExtra;
        }

        vendorExtensions.put(VENDOR_EXTENSION_POSTGRESQL_SCHEMA, postgresqlSchema);
        postgresqlSchema.put("columnDefinition", columnDefinition);
        columnDefinition.put("colName", colName);
        if (isReservedWord(colName)) { // Output column name in double quotes if it is a reserved word
            columnDefinition.put("colNameQuoted", true);
        } else {
            columnDefinition.put("colNameQuoted", false);
        }
        columnDefinition.put("tblName", tableName);
        if (isReservedWord(model.getName())) { // Output table name (for column comment) in double quotes if it is a
            // reserved word
            columnDefinition.put("tblNameQuoted", true);
        } else {
            columnDefinition.put("tblNameQuoted", false);
        }

        if (Boolean.TRUE.equals(isEnum)) {
            Map<String, Object> allowableValues = property.getAllowableValues();
            List<Object> enumValues = (List<Object>) allowableValues.get("values");
            String typeName = this.toTableName(model.getName())
                    + "_" + this.toColumnName(property.getName());
            postgresqlSchema.put("typeDefinition", typeDefinition);
            columnDefinition.put("colDataType", typeName);
            typeDefinition.put("typeName", typeName);
            typeDefinition.put("typeArguments", columnDataTypeArguments);
            for (int i = 0; i < enumValues.size(); i++) {
                if (i > ENUM_MAX_ELEMENTS - 1) {
                    LOGGER.warn(
                            "ENUM column can have maximum of {} distinct elements, following value will be skipped: {}",
                            ENUM_MAX_ELEMENTS, (String) enumValues.get(i));
                    break;
                }
                String value = String.valueOf(enumValues.get(i));
                columnDataTypeArguments.add(toCodegenPostgresqlDataTypeArgument(value));
            }
        } else {
            if ("int64".equals(dataFormat)) {
                if (colName.equals("id") && Boolean.valueOf(additionalProperties.get(ID_AUTOINC_ENABLED).toString())) {
                    columnDefinition.put("colDataType", "BIGSERIAL");
                } else {
                    columnDefinition.put("colDataType", "BIGINT");
                }
            } else {
                Long min = (minimum != null) ? Long.parseLong(minimum) : null;
                Long max = (maximum != null) ? Long.parseLong(maximum) : null;
                if (exclusiveMinimum && min != null)
                    min += 1;
                String colDataType = getPostgresqlMatchedIntegerDataType(min, max, false);
                if (colName.equals("id") && Boolean.valueOf(additionalProperties.get(ID_AUTOINC_ENABLED).toString())) {
                    if (colDataType.equals("BIGINT")) {
                        colDataType = "BIGSERIAL";
                    } else {
                        colDataType = "SERIAL";
                    }
                }
                columnDefinition.put("colDataType", colDataType);
            }
        }

        if (!columnDefinition.get("colDataType").equals("SERIAL")
                && !columnDefinition.get("colDataType").equals("BIGSERIAL")) { // No default value for autoincremented
            // IDs
            if (Boolean.TRUE.equals(required)) {
                columnDefinition.put("colNotNull", true);
            } else {
                columnDefinition.put("colNotNull", false);
                try {
                    columnDefinition.put("colDefault",
                            toCodegenPostgresqlDataTypeDefault(defaultValue,
                                    (String) columnDefinition.get("colDataType")));
                } catch (RuntimeException exception) {
                    LOGGER.warn(
                            "Property '{}' of model '{}' mapped to PostgreSQL data type which doesn't support default value",
                            baseName, model.getName());
                    columnDefinition.put("colDefault", null);
                }
            }
        }

        if (description != null) {
            columnDefinition.put("colComment", description);
        }
    }

    /**
     * Processes each model's property mapped to decimal type and adds related
     * vendor extensions
     *
     * @param model    model
     * @param property model's property
     */
    public void processDecimalTypeProperty(CodegenModel model, CodegenProperty property) {
        Map<String, Object> vendorExtensions = property.getVendorExtensions();
        Map<String, Object> postgresqlSchema = new HashMap<>();
        Map<String, Object> columnDefinition = new HashMap<>();
        Map<String, Object> typeDefinition = new HashMap<>();
        ArrayList columnDataTypeArguments = new ArrayList();
        String baseName = property.getBaseName();
        String colName = this.toColumnName(baseName);
        String dataType = property.getDataType();
        String dataFormat = property.getDataFormat();
        String description = property.getDescription();
        String minimum = property.getMinimum();
        String maximum = property.getMaximum();
        boolean exclusiveMinimum = property.getExclusiveMinimum();
        boolean exclusiveMaximum = property.getIExclusiveMaximum();
        String defaultValue = property.getDefaultValue();
        Boolean required = property.getRequired();
        Boolean isEnum = property.isEnum;
        String tableName = this.toTableName(model.getName());

        if (vendorExtensions.containsKey(VENDOR_EXTENSION_POSTGRESQL_SCHEMA)) {
            // user already specified schema values
            LOGGER.info("Found vendor extension in '{}' property, autogeneration skipped", baseName);
            return;
        }

        if (this.getIdentifierNamingConvention().equals("snake_case") && !baseName.equals(colName)) {
            // add original name in column comment
            String commentExtra = "Original param name - " + baseName + ".";
            description = (description == null || description.isEmpty()) ? commentExtra
                    : description + ". " + commentExtra;
        }

        vendorExtensions.put(VENDOR_EXTENSION_POSTGRESQL_SCHEMA, postgresqlSchema);
        postgresqlSchema.put("columnDefinition", columnDefinition);
        columnDefinition.put("colName", colName);
        if (isReservedWord(colName)) { // Output column name in double quotes if it is a reserved word
            columnDefinition.put("colNameQuoted", true);
        } else {
            columnDefinition.put("colNameQuoted", false);
        }
        columnDefinition.put("tblName", tableName);
        if (isReservedWord(model.getName())) { // Output table name (for column comment) in double quotes if it is a
            // reserved word
            columnDefinition.put("tblNameQuoted", true);
        } else {
            columnDefinition.put("tblNameQuoted", false);
        }

        if (Boolean.TRUE.equals(isEnum)) {
            Map<String, Object> allowableValues = property.getAllowableValues();
            List<Object> enumValues = (List<Object>) allowableValues.get("values");
            String typeName = this.toTableName(model.getName())
                    + "_" + this.toColumnName(property.getName());
            postgresqlSchema.put("typeDefinition", typeDefinition);
            columnDefinition.put("colDataType", typeName);
            typeDefinition.put("typeName", typeName);
            typeDefinition.put("typeArguments", columnDataTypeArguments);
            for (int i = 0; i < enumValues.size(); i++) {
                if (i > ENUM_MAX_ELEMENTS - 1) {
                    LOGGER.warn(
                            "ENUM column can have maximum of {} distinct elements, following value will be skipped: {}",
                            ENUM_MAX_ELEMENTS, (String) enumValues.get(i));
                    break;
                }
                String value = String.valueOf(enumValues.get(i));
                columnDataTypeArguments.add(toCodegenPostgresqlDataTypeArgument(value));
            }
        } else {
            Float min = (minimum != null) ? Float.valueOf(minimum) : null;
            Float max = (maximum != null) ? Float.valueOf(maximum) : null;
            if (exclusiveMinimum && min != null)
                min += 1;
            if (exclusiveMaximum && max != null)
                max -= 1;
            columnDefinition.put("colDataType", "DECIMAL");
            columnDefinition.put("colDataTypeArguments", columnDataTypeArguments);
            columnDataTypeArguments.add(toCodegenPostgresqlDataTypeArgument(20));
            columnDataTypeArguments.add(toCodegenPostgresqlDataTypeArgument(9));
        }

        if (Boolean.TRUE.equals(required)) {
            columnDefinition.put("colNotNull", true);
        } else {
            columnDefinition.put("colNotNull", false);
            try {
                columnDefinition.put("colDefault",
                        toCodegenPostgresqlDataTypeDefault(defaultValue, (String) columnDefinition.get("colDataType")));
            } catch (RuntimeException exception) {
                LOGGER.warn(
                        "Property '{}' of model '{}' mapped to PostgreSQL data type which doesn't support default value",
                        baseName, model.getName());
                columnDefinition.put("colDefault", null);
            }
        }

        if (description != null) {
            columnDefinition.put("colComment", description);
        }
    }

    /**
     * Processes each model's property mapped to string type and adds related vendor
     * extensions
     *
     * @param model    model
     * @param property model's property
     */
    public void processStringTypeProperty(CodegenModel model, CodegenProperty property) {
        Map<String, Object> vendorExtensions = property.getVendorExtensions();
        Map<String, Object> postgresqlSchema = new HashMap<>();
        Map<String, Object> columnDefinition = new HashMap<>();
        Map<String, Object> typeDefinition = new HashMap<>();
        ArrayList columnDataTypeArguments = new ArrayList();
        String baseName = property.getBaseName();
        String colName = this.toColumnName(baseName);
        String dataType = property.getDataType();
        String dataFormat = property.getDataFormat();
        String description = property.getDescription();
        Integer minLength = property.getMinLength();
        Integer maxLength = property.getMaxLength();
        String defaultValue = property.getDefaultValue();
        Boolean required = property.getRequired();
        Boolean isEnum = property.isEnum;
        String tableName = this.toTableName(model.getName());

        if (vendorExtensions.containsKey(VENDOR_EXTENSION_POSTGRESQL_SCHEMA)) {
            // user already specified schema values
            LOGGER.info("Found vendor extension in '{}' property, autogeneration skipped", baseName);
            return;
        }

        if (this.getIdentifierNamingConvention().equals("snake_case") && !baseName.equals(colName)) {
            // add original name in column comment
            String commentExtra = "Original param name - " + baseName + ".";
            description = (description == null || description.isEmpty()) ? commentExtra
                    : description + ". " + commentExtra;
        }

        vendorExtensions.put(VENDOR_EXTENSION_POSTGRESQL_SCHEMA, postgresqlSchema);
        postgresqlSchema.put("columnDefinition", columnDefinition);
        columnDefinition.put("colName", colName);
        if (isReservedWord(colName)) { // Output column name in double quotes if it is a reserved word
            columnDefinition.put("colNameQuoted", true);
        } else {
            columnDefinition.put("colNameQuoted", false);
        }
        columnDefinition.put("tblName", tableName);
        if (isReservedWord(model.getName())) { // Output table name (for column comment) in double quotes if it is a
            // reserved word
            columnDefinition.put("tblNameQuoted", true);
        } else {
            columnDefinition.put("tblNameQuoted", false);
        }

        if (Boolean.TRUE.equals(isEnum)) {
            Map<String, Object> allowableValues = property.getAllowableValues();
            List<Object> enumValues = (List<Object>) allowableValues.get("values");
            String typeName = this.toTableName(model.getName())
                    + "_" + this.toColumnName(property.getName());
            postgresqlSchema.put("typeDefinition", typeDefinition);
            columnDefinition.put("colDataType", typeName);
            typeDefinition.put("typeName", typeName);
            typeDefinition.put("typeArguments", columnDataTypeArguments);
            for (int i = 0; i < enumValues.size(); i++) {
                if (i > ENUM_MAX_ELEMENTS - 1) {
                    LOGGER.warn(
                            "ENUM column can have maximum of {} distinct elements, following value will be skipped: {}",
                            ENUM_MAX_ELEMENTS, (String) enumValues.get(i));
                    break;
                }
                String value = String.valueOf(enumValues.get(i));
                columnDataTypeArguments.add(toCodegenPostgresqlDataTypeArgument(value));
            }
        } else if (dataType.equals("BYTEA")) {
            columnDefinition.put("colDataType", "BYTEA");
        } else {
            String matchedStringType = getPostgresqlMatchedStringDataType(minLength, maxLength);
            columnDefinition.put("colDataType", matchedStringType);
            if (matchedStringType.equals("VARCHAR")) {
                columnDefinition.put("colDataTypeArguments", columnDataTypeArguments);
                columnDataTypeArguments.add(toCodegenPostgresqlDataTypeArgument((maxLength != null) ? maxLength : 255));
            }
        }

        if (Boolean.TRUE.equals(required)) {
            columnDefinition.put("colNotNull", true);
        } else {
            columnDefinition.put("colNotNull", false);
            try {
                columnDefinition.put("colDefault",
                        toCodegenPostgresqlDataTypeDefault(defaultValue, (String) columnDefinition.get("colDataType")));
            } catch (RuntimeException exception) {
                LOGGER.warn(
                        "Property '{}' of model '{}' mapped to PostgreSQL data type which doesn't support default value",
                        baseName, model.getName());
                columnDefinition.put("colDefault", null);
            }
        }

        if (description != null) {
            columnDefinition.put("colComment", description);
        }

    }

    /**
     * Processes each model's property mapped to boolean type and adds related
     * vendor extensions
     *
     * @param model    model
     * @param property model's property
     */
    public void processBooleanTypeProperty(CodegenModel model, CodegenProperty property) {
        Map<String, Object> vendorExtensions = property.getVendorExtensions();
        Map<String, Object> postgresqlSchema = new HashMap<>();
        Map<String, Object> columnDefinition = new HashMap<>();
        ArrayList columnDataTypeArguments = new ArrayList();
        String baseName = property.getBaseName();
        String colName = this.toColumnName(baseName);
        String description = property.getDescription();
        String defaultValue = property.getDefaultValue();
        Boolean required = property.getRequired();
        String tableName = this.toTableName(model.getName());

        if (vendorExtensions.containsKey(VENDOR_EXTENSION_POSTGRESQL_SCHEMA)) {
            // user already specified schema values
            LOGGER.info("Found vendor extension in '{}' property, autogeneration skipped", baseName);
            return;
        }

        if (this.getIdentifierNamingConvention().equals("snake_case") && !baseName.equals(colName)) {
            // add original name in column comment
            String commentExtra = "Original param name - " + baseName + ".";
            description = (description == null || description.isEmpty()) ? commentExtra
                    : description + ". " + commentExtra;
        }

        vendorExtensions.put(VENDOR_EXTENSION_POSTGRESQL_SCHEMA, postgresqlSchema);
        postgresqlSchema.put("columnDefinition", columnDefinition);
        columnDefinition.put("colName", colName);
        if (isReservedWord(colName)) { // Output column name in double quotes if it is a reserved word
            columnDefinition.put("colNameQuoted", true);
        } else {
            columnDefinition.put("colNameQuoted", false);
        }
        columnDefinition.put("tblName", tableName);
        if (isReservedWord(model.getName())) { // Output table name (for column comment) in double quotes if it is a
            // reserved word
            columnDefinition.put("tblNameQuoted", true);
        } else {
            columnDefinition.put("tblNameQuoted", false);
        }

        columnDefinition.put("colDataType", "BOOLEAN");

        if (Boolean.TRUE.equals(required)) {
            columnDefinition.put("colNotNull", true);
        } else {
            columnDefinition.put("colNotNull", false);
            try {
                columnDefinition.put("colDefault",
                        toCodegenPostgresqlDataTypeDefault(defaultValue, (String) columnDefinition.get("colDataType")));
            } catch (RuntimeException exception) {
                LOGGER.warn(
                        "Property '{}' of model '{}' mapped to PostgreSQL data type which doesn't support default value",
                        baseName, model.getName());
                columnDefinition.put("colDefault", null);
            }
        }

        if (description != null) {
            columnDefinition.put("colComment", description);
        }
    }

    /**
     * Processes each model's property mapped to date type and adds related vendor
     * extensions
     *
     * @param model    model
     * @param property model's property
     */
    public void processDateTypeProperty(CodegenModel model, CodegenProperty property) {
        Map<String, Object> vendorExtensions = property.getVendorExtensions();
        Map<String, Object> postgresqlSchema = new HashMap<>();
        Map<String, Object> columnDefinition = new HashMap<>();
        String baseName = property.getBaseName();
        String colName = this.toColumnName(baseName);
        String dataType = property.getDataType();
        Boolean required = property.getRequired();
        String description = property.getDescription();
        String defaultValue = property.getDefaultValue();
        String tableName = this.toTableName(model.getName());

        if (vendorExtensions.containsKey(VENDOR_EXTENSION_POSTGRESQL_SCHEMA)) {
            // user already specified schema values
            LOGGER.info("Found vendor extension in '{}' property, autogeneration skipped", baseName);
            return;
        }

        if (this.getIdentifierNamingConvention().equals("snake_case") && !baseName.equals(colName)) {
            // add original name in column comment
            String commentExtra = "Original param name - " + baseName + ".";
            description = (description == null || description.isEmpty()) ? commentExtra
                    : description + ". " + commentExtra;
        }

        vendorExtensions.put(VENDOR_EXTENSION_POSTGRESQL_SCHEMA, postgresqlSchema);
        postgresqlSchema.put("columnDefinition", columnDefinition);
        columnDefinition.put("colName", colName);
        if (isReservedWord(colName)) { // Output column name in double quotes if it is a reserved word
            columnDefinition.put("colNameQuoted", true);
        } else {
            columnDefinition.put("colNameQuoted", false);
        }
        columnDefinition.put("tblName", tableName);
        if (isReservedWord(model.getName())) { // Output table name (for column comment) in double quotes if it is a
            // reserved word
            columnDefinition.put("tblNameQuoted", true);
        } else {
            columnDefinition.put("tblNameQuoted", false);
        }

        columnDefinition.put("colDataType", dataType);

        if (Boolean.TRUE.equals(required)) {
            columnDefinition.put("colNotNull", true);
        } else {
            columnDefinition.put("colNotNull", false);
            try {
                columnDefinition.put("colDefault",
                        toCodegenPostgresqlDataTypeDefault(defaultValue, (String) columnDefinition.get("colDataType")));
            } catch (RuntimeException exception) {
                LOGGER.warn(
                        "Property '{}' of model '{}' mapped to PostgreSQL data type which doesn't support default value",
                        baseName, model.getName());
                columnDefinition.put("colDefault", null);
            }
        }

        if (description != null) {
            columnDefinition.put("colComment", description);
        }
    }

    /**
     * Processes each model's property mapped to JSON type and adds related vendor
     * extensions
     *
     * @param model    model
     * @param property model's property
     */
    public void processJsonTypeProperty(CodegenModel model, CodegenProperty property) {
        Map<String, Object> vendorExtensions = property.getVendorExtensions();
        Map<String, Object> postgresqlSchema = new HashMap<>();
        Map<String, Object> columnDefinition = new HashMap<>();
        String baseName = property.getBaseName();
        String colName = this.toColumnName(baseName);
        String dataType = property.getDataType();
        Boolean required = property.getRequired();
        String description = property.getDescription();
        String defaultValue = property.getDefaultValue();
        String tableName = this.toTableName(model.getName());

        if (vendorExtensions.containsKey(VENDOR_EXTENSION_POSTGRESQL_SCHEMA)) {
            // user already specified schema values
            LOGGER.info("Found vendor extension in '{}' property, autogeneration skipped", baseName);
            return;
        }

        if (this.getIdentifierNamingConvention().equals("snake_case") && !baseName.equals(colName)) {
            // add original name in column comment
            String commentExtra = "Original param name - " + baseName + ".";
            description = (description == null || description.isEmpty()) ? commentExtra
                    : description + ". " + commentExtra;
        }

        vendorExtensions.put(VENDOR_EXTENSION_POSTGRESQL_SCHEMA, postgresqlSchema);
        postgresqlSchema.put("columnDefinition", columnDefinition);
        columnDefinition.put("colName", colName);
        if (isReservedWord(colName)) { // Output column name in double quotes if it is a reserved word
            columnDefinition.put("colNameQuoted", true);
        } else {
            columnDefinition.put("colNameQuoted", false);
        }
        columnDefinition.put("tblName", tableName);
        if (isReservedWord(model.getName())) { // Output table name (for column comment) in double quotes if it is a
            // reserved word
            columnDefinition.put("tblNameQuoted", true);
        } else {
            columnDefinition.put("tblNameQuoted", false);
        }

        columnDefinition.put("colDataType", dataType);

        if (getJsonDataType().equals("off")) {
            columnDefinition.put("colDataType", "TEXT");
        } else if (getJsonDataType().equals("json")) {
            columnDefinition.put("colDataType", "JSON");
        } else if (getJsonDataType().equals("jsonb")) {
            columnDefinition.put("colDataType", "JSONB");
        }

        if (Boolean.TRUE.equals(required)) {
            columnDefinition.put("colNotNull", true);
        } else {
            columnDefinition.put("colNotNull", false);
            try {
                columnDefinition.put("colDefault",
                        toCodegenPostgresqlDataTypeDefault(defaultValue, (String) columnDefinition.get("colDataType")));
            } catch (RuntimeException exception) {
                LOGGER.warn(
                        "Property '{}' of model '{}' mapped to PostgreSQL data type which doesn't support default value",
                        baseName, model.getName());
                columnDefinition.put("colDefault", null);
            }
        }

        if (description != null) {
            columnDefinition.put("colComment", description);
        }
    }

    /**
     * Processes each model's property not mapped to any type and adds related
     * vendor extensions
     * Most of time it's related to referenced properties eg. \Model\User
     *
     * @param model    model
     * @param property model's property
     */
    public void processUnknownTypeProperty(CodegenModel model, CodegenProperty property) {
        Map<String, Object> vendorExtensions = property.getVendorExtensions();
        Map<String, Object> postgresqlSchema = new HashMap<>();
        Map<String, Object> columnDefinition = new HashMap<>();
        String baseName = property.getBaseName();
        String colName = this.toColumnName(baseName);
        Boolean required = property.getRequired();
        String description = property.getDescription();
        String defaultValue = property.getDefaultValue();
        String tableName = this.toTableName(model.getName());

        if (vendorExtensions.containsKey(VENDOR_EXTENSION_POSTGRESQL_SCHEMA)) {
            // user already specified schema values
            LOGGER.info("Found vendor extension in '{}' property, autogeneration skipped", baseName);
            return;
        }

        if (this.getIdentifierNamingConvention().equals("snake_case") && !baseName.equals(colName)) {
            // add original name in column comment
            String commentExtra = "Original param name - " + baseName + ".";
            description = (description == null || description.isEmpty()) ? commentExtra
                    : description + ". " + commentExtra;
        }

        vendorExtensions.put(VENDOR_EXTENSION_POSTGRESQL_SCHEMA, postgresqlSchema);
        postgresqlSchema.put("columnDefinition", columnDefinition);
        columnDefinition.put("colName", colName);
        if (isReservedWord(colName)) { // Output column name in double quotes if it is a reserved word
            columnDefinition.put("colNameQuoted", true);
        } else {
            columnDefinition.put("colNameQuoted", false);
        }
        columnDefinition.put("tblName", tableName);
        if (isReservedWord(model.getName())) { // Output table name (for column comment) in double quotes if it is a
            // reserved word
            columnDefinition.put("tblNameQuoted", true);
        } else {
            columnDefinition.put("tblNameQuoted", false);
        }

        columnDefinition.put("colDataType", "TEXT");

        if (Boolean.TRUE.equals(required)) {
            columnDefinition.put("colNotNull", true);
        } else {
            columnDefinition.put("colNotNull", false);
            try {
                columnDefinition.put("colDefault",
                        toCodegenPostgresqlDataTypeDefault(defaultValue, (String) columnDefinition.get("colDataType")));
            } catch (RuntimeException exception) {
                LOGGER.warn(
                        "Property '{}' of model '{}' mapped to PostgreSQL data type which doesn't support default value",
                        baseName, model.getName());
                columnDefinition.put("colDefault", null);
            }
        }

        if (description != null) {
            columnDefinition.put("colComment", description);
        }
    }

    /**
     * Generates codegen property for PostgreSQL data type argument
     *
     * @param value argument value
     * @return generated codegen property
     */
    public HashMap<String, Object> toCodegenPostgresqlDataTypeArgument(Object value) {
        HashMap<String, Object> arg = new HashMap<>();
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
            LOGGER.warn("PostgreSQL data type argument can be primitive type only. Class '{}' is provided",
                    value.getClass());
        }
        arg.put("argumentValue", value);
        return arg;
    }

    /**
     * Generates default value codegen property for PostgreSQL column definition
     * Ref: https://www.postgresql.org/docs/17/datatype.html
     *
     * @param defaultValue       value
     * @param postgresqlDataType PostgreSQL data type
     * @return generated codegen property
     */
    public HashMap<String, Object> toCodegenPostgresqlDataTypeDefault(String defaultValue, String postgresqlDataType) {
        HashMap<String, Object> defaultMap = new HashMap<>();
        if (defaultValue == null || defaultValue.toUpperCase(Locale.ROOT).equals("NULL")) {
            defaultMap.put("defaultValue", "NULL");
            defaultMap.put("isString", false);
            defaultMap.put("isNumeric", false);
            defaultMap.put("isKeyword", true);
            return defaultMap;
        }

        switch (postgresqlDataType.toUpperCase(Locale.ROOT)) {
            case "SMALLINT":
            case "INTEGER":
            case "BIGINT":
                // SERIAL DEFAULT VALUE is a special case. In the definition of an integer
                // column, it is an alias for NOT NULL AUTO_INCREMENT UNIQUE
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
            case "DATE":
                // The exception is that, for TIMESTAMP,DATE columns, you can specify
                // CURRENT_TIMESTAMP,CURRENT_DATE as the default
                if (defaultValue.equals("CURRENT_TIMESTAMP")) {
                    defaultMap.put("defaultValue", defaultValue);
                    defaultMap.put("isString", false);
                    defaultMap.put("isNumeric", false);
                    defaultMap.put("isKeyword", true);

                } else if (defaultValue.equals("CURRENT_DATE")) {
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
            case "BYTEA":
            case "TEXT":
            case "GEOMETRY":
            case "JSON":
            case "JSONB":
                // The BLOB, TEXT, GEOMETRY, and JSON data types cannot be assigned a default
                // value.
                throw new RuntimeException(
                        "The BLOB, TEXT, GEOMETRY, and JSON data types cannot be assigned a default value");
            default:
                defaultMap.put("defaultValue", defaultValue);
                defaultMap.put("isString", true);
                defaultMap.put("isNumeric", false);
                defaultMap.put("isKeyword", false);

                return defaultMap;
        }
    }

    /**
     * Finds best fitted PostgreSQL data type for integer variable based on minimum
     * and maximum properties
     *
     * @param minimum  (optional) codegen property
     * @param maximum  (optional) codegen property
     * @param unsigned (optional) whether variable is unsigned or not
     * @return PostgreSQL integer data type
     */
    public String getPostgresqlMatchedIntegerDataType(Long minimum, Long maximum, Boolean unsigned) {
        // we can choose fit postgresql data type
        // ref: https://www.postgresql.org/docs/17/datatype-numeric.html
        long min = (minimum != null) ? minimum : -2147483648L;
        long max = (maximum != null) ? maximum : 2147483647L;
        long actualMin = Math.min(min, max); // sometimes min and max values can be mixed up
        long actualMax = Math.max(min, max); // sometimes only minimum specified and it can be pretty high
        if (minimum != null && maximum != null && minimum > maximum) {
            LOGGER.warn("Codegen property 'minimum' cannot be greater than 'maximum'");
        }
        if (actualMin >= -32768 && actualMax <= 32767) {
            return "SMALLINT";
        } else if (actualMin >= -2147483648 && actualMax <= 2147483647) {
            return "INTEGER";
        } else if (actualMin < -2147483648 || actualMax > 2147483647) {
            return "BIGINT";
        }
        return "INTEGER";
    }

    /**
     * Finds best fitted PostgreSQL data type for string variable based on minLength
     * and maxLength properties
     *
     * @param minLength (optional) codegen property
     * @param maxLength (optional) codegen property
     * @return PostgreSQL string data type
     */
    public String getPostgresqlMatchedStringDataType(Integer minLength, Integer maxLength) {
        // we can choose fit postgresql data type
        // ref: https://www.postgresql.org/docs/17/datatype-character.html
        int min = (minLength != null && minLength >= 0) ? minLength : 0;
        int max = (maxLength != null && maxLength >= 0) ? maxLength : 65536;
        Integer actualMin = Math.min(min, max); // sometimes minLength and maxLength values can be mixed up
        Integer actualMax = Math.max(min, max); // sometimes only minLength specified and it can be pretty high
        if (minLength != null && maxLength != null && minLength > maxLength) {
            LOGGER.warn("Codegen property 'minLength' cannot be greater than 'maxLength'");
        }
        if (actualMax <= 65535) {
            return "VARCHAR";
        }
        return "TEXT";
    }

    /**
     * Checks whether string is one of PostgreSQL Data Types
     * Ref: https://dev.postgresql.com/doc/refman/8.0/en/data-type-overview.html
     *
     * @param dataType which needs to check
     * @return true if value is correct PostgreSQL data type, otherwise false
     */
    public Boolean isPostgresqlDataType(String dataType) {
        return (postgresqlNumericTypes.contains(dataType.toUpperCase(Locale.ROOT)) ||
                postgresqlDateAndTimeTypes.contains(dataType.toUpperCase(Locale.ROOT)) ||
                postgresqlStringTypes.contains(dataType.toUpperCase(Locale.ROOT)) ||
                postgresqlSpatialTypes.contains(dataType.toUpperCase(Locale.ROOT))) ||
                dataType.toUpperCase(Locale.ROOT).equals("JSON") ||
                dataType.toUpperCase(Locale.ROOT).equals("JSONB");
    }

    /**
     * Converts name to valid PostgreSQL database name
     *
     * @param name source name
     * @return database name
     */
    public String toDatabaseName(String name) {
        String identifier = toPostgresqlIdentifier(name, databaseNamePrefix, databaseNameSuffix);
        if (identifier.length() > IDENTIFIER_MAX_LENGTH) {
            LOGGER.warn("Database name cannot exceed {} chars. Name '{}' will be truncated", IDENTIFIER_MAX_LENGTH,
                    name);
            identifier = identifier.substring(0, IDENTIFIER_MAX_LENGTH);
        }
        return identifier;
    }

    /**
     * Converts name to valid PostgreSQL column name
     *
     * @param name source name
     * @return table name
     */
    public String toTableName(String name) {
        String identifier = toPostgresqlIdentifier(name, tableNamePrefix, tableNameSuffix);
        if (identifierNamingConvention.equals("snake_case")) {
            identifier = underscore(identifier);
        }
        if (identifier.length() > IDENTIFIER_MAX_LENGTH) {
            LOGGER.warn("Table name cannot exceed {} chars. Name '{}' will be truncated", IDENTIFIER_MAX_LENGTH, name);
            identifier = identifier.substring(0, IDENTIFIER_MAX_LENGTH);
        }
        return identifier;
    }

    /**
     * Converts name to valid PostgreSQL column name
     *
     * @param name source name
     * @return column name
     */
    public String toColumnName(String name) {
        String identifier = toPostgresqlIdentifier(name, columnNamePrefix, columnNameSuffix);
        if (identifierNamingConvention.equals("snake_case")) {
            identifier = underscore(identifier);
        }
        if (identifier.length() > IDENTIFIER_MAX_LENGTH) {
            LOGGER.warn("Column name cannot exceed {} chars. Name '{}' will be truncated", IDENTIFIER_MAX_LENGTH, name);
            identifier = identifier.substring(0, IDENTIFIER_MAX_LENGTH);
        }
        return identifier;
    }

    /**
     * Converts name to valid PostgreSQL identifier which can be used as database,
     * table, column name
     *
     * @param name   source name
     * @param prefix when escaped name is digits only, prefix will be prepended
     * @param suffix when escaped name is digits only, suffix will be appended
     * @return identifier name
     */
    public String toPostgresqlIdentifier(String name, String prefix, String suffix) {
        String escapedName = escapePostgresqlQuotedIdentifier(name);
        // Database, table, and column names cannot end with space characters.
        if (escapedName.matches(".*\\s$")) {
            LOGGER.warn("Database, table, and column names cannot end with space characters. Check '{}' name", name);
            escapedName = escapedName.replaceAll("\\s+$", "");
        }

        // Identifiers may begin with a digit but unless quoted may not consist solely
        // of digits.
        if (escapedName.matches("^\\d+$")) {
            LOGGER.warn("Database, table, and column names cannot consist solely of digits. Check '{}' name", name);
            escapedName = prefix + escapedName + suffix;
        }

        // identifier name cannot be empty
        if (escapedName.isEmpty()) {
            throw new RuntimeException("Empty database/table/column name for property '" + name + "' not allowed");
        }
        return escapedName;
    }

    /**
     * Escapes PostgreSQL identifier to use it in SQL statements without backticks,
     * eg. SELECT identifier FROM
     *
     * @param identifier source identifier
     * @return escaped identifier
     */
    public String escapePostgresqlUnquotedIdentifier(String identifier) {
        // ASCII: [0-9,a-z,A-Z$_] (basic Latin letters, digits 0-9, dollar, underscore)
        // Extended: U+0080 .. U+FFFF
        Pattern regexp = Pattern.compile("[^0-9a-zA-z$_\\u0080-\\uFFFF]");
        Matcher matcher = regexp.matcher(identifier);
        if (matcher.find()) {
            LOGGER.warn("Identifier '{}' contains unsafe characters out of [0-9,a-z,A-Z$_] and U+0080..U+FFFF range",
                    identifier);
            identifier = identifier.replaceAll("[^0-9a-zA-z$_\\u0080-\\uFFFF]", "");
        }

        // ASCII NUL (U+0000) and supplementary characters (U+10000 and higher) are not
        // permitted in quoted or unquoted identifiers.
        // Don't know how to match these characters, hope that first regexp already
        // strip them
        // Pattern regexp2 = Pattern.compile("[\0\uD800\uDC00-\uDBFF\uDFFF]");
        return identifier;
    }

    /**
     * Escapes PostgreSQL identifier to use it in SQL statements with backticks, eg.
     * SELECT `identifier` FROM
     *
     * @param identifier source identifier
     * @return escaped identifier
     */
    public String escapePostgresqlQuotedIdentifier(String identifier) {
        // ASCII: U+0001 .. U+007F Extended: U+0080 .. U+FFFF
        Pattern regexp = Pattern.compile("[^\\u0001-\\u007F\\u0080-\\uFFFF]");
        Matcher matcher = regexp.matcher(identifier);
        if (matcher.find()) {
            LOGGER.warn("Identifier '{}' contains unsafe characters out of U+0001..U+007F and U+0080..U+FFFF range",
                    identifier);
            identifier = identifier.replaceAll("[^\\u0001-\\u007F\\u0080-\\uFFFF]", "");
        }

        // ASCII NUL (U+0000) and supplementary characters (U+10000 and higher) are not
        // permitted in quoted or unquoted identifiers.
        // Don't know how to match these characters, hope that first regexp already
        // strip them
        // Pattern regexp2 = Pattern.compile("[\0\uD800\uDC00-\uDBFF\uDFFF]");
        return identifier;
    }

    @Override
    public String escapeReservedWord(String name) {
        // *** For PostgreSQL:
        // *** If table name or column name is a reserved word,
        // *** it could be still used in double quotes
        // *** (this is done in template by adding attributes 'tblNameQuoted' and
        // 'colNameQuoted' when necessary)

        // LOGGER.warn(
        // "'{}' is PostgreSQL reserved word. Do not use that word or properly escape it
        // with backticks in mustache template",
        // name);
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
     * Sets default database name for all PostgreSQL queries
     * Provided value will be escaped when necessary
     *
     * @param databaseName source name
     */
    public void setDefaultDatabaseName(String databaseName) {
        String escapedName = toDatabaseName(databaseName);
        if (!escapedName.equals(databaseName)) {
            LOGGER.error(
                    "Invalid database name. '{}' cannot be used as PostgreSQL identifier. Escaped value '{}' will be used instead.",
                    databaseName, escapedName);
        }
        this.defaultDatabaseName = escapedName;
    }

    /**
     * Sets identifier naming convention for table names and column names.
     * This is not related to database name which is defined by defaultDatabaseName
     * option.
     *
     * @param naming identifier naming convention (snake_case|original)
     */
    public void setIdentifierNamingConvention(String naming) {
        switch (naming) {
            case "snake_case":
            case "original":
                this.identifierNamingConvention = naming;
                break;
            default:
                LOGGER.warn("\"{}\" is invalid \"identifierNamingConvention\" argument. Current \"{}\" used instead.",
                        naming, this.identifierNamingConvention);
        }
    }

    /**
     * Slightly modified version of AbstractPhpCodegen.toSrcPath method.
     *
     * @param packageName package name
     * @return path
     */
    public String toSrcPath(String packageName) {
        // Trim prefix file separators from package path
        String packagePath = StringUtils.removeStart(
                // Replace period, backslash, forward slash with file separator in package name
                packageName.replaceAll("[\\.\\\\/]", Matcher.quoteReplacement("/")),
                File.separator);

        // Trim trailing file separators from the overall path
        return StringUtils.removeEnd(packagePath, File.separator);
    }

    @Override
    public GeneratorLanguage generatorLanguage() {
        return GeneratorLanguage.POSTGRESQL;
    }
}
