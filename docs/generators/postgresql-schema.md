---
title: Documentation for the postgresql-schema Generator
---

## METADATA

| Property | Value | Notes |
| -------- | ----- | ----- |
| generator name | postgresql-schema | pass this to the generate command after -g |
| generator stability | BETA | |
| generator type | SCHEMA | |
| generator language | PostgreSQL | |
| generator default templating engine | mustache | |
| helpTxt | Generates a PostgreSQL schema based on the schema defined in the OpenAPI specification (v2, v3) | |

## CONFIG OPTIONS
These options may be applied as additional-properties (cli) or configOptions (plugins). Refer to [configuration docs](https://openapi-generator.tech/docs/configuration) for more details.

| Option | Description | Values | Default |
| ------ | ----------- | ------ | ------- |
|defaultDatabaseName|Default database name for all PostgreSQL queries| ||
|identifierNamingConvention|Naming convention of PostgreSQL identifiers(table names and column names). This is not related to database name which is defined by defaultDatabaseName option|<dl><dt>**original**</dt><dd>Do not transform original names</dd><dt>**snake_case**</dt><dd>Use snake_case names</dd></dl>|original|
|jsonDataType|Use special JSON data types for complex model properties <br><br>*Note: JSON data type requires PostgreSQL version 9.4 or newer*|<dl><dt>**json**</dt><dd>Stores data as a text string.<br>- Suitable if you need to preserve the data in its original form in human-readable format.<br>- Slower in performing operations because the data has to be parsed each time.</dd><dt>**jsonb**</dt><dd>- Stores data in a binary format.<br>- Supports indexing, which makes search and data retrieval operations faster.<br>- Does not preserve the order of keys and may change formatting (e.g., removing extra spaces)</dd><dt>**off**</dt><dd>JSON data types disabled. TEXT type will be used to store JSON data</dd></dl>|json|
|namedParametersEnabled|Generates model prepared SQLs with named parameters, eg. :petName. Question mark placeholder used when option disabled.| |false|
|idAutoIncEnabled|Add an auto-increment feature for 'id' fields of integer type.|<dl><dt>**true**</dt><dd>This generates SERIAL/BIGSERIAL type instead of INT/BIGINT, which automatically handle 'id' field value increments when inserting data into table.<br>The choice between SERIAL and BIGSERIAL is based on the format of the OpenAPI schema object 'id' property (int32 to SERIAL and int64 to BIGSERIAL respectively)</dd><dt>**false**</dt><dd>- Leave integer 'id' field type as is (INT/BIGINT)</dd></dl>|false|

## IMPORT MAPPING

| Type/Alias | Imports |
| ---------- | ------- |


## INSTANTIATION TYPES

| Type/Alias | Instantiated By |
| ---------- | --------------- |


## LANGUAGE PRIMITIVES

<ul class="column-ul">
<li>BigDecimal</li>
<li>ByteArray</li>
<li>URI</li>
<li>UUID</li>
<li>binary</li>
<li>bool</li>
<li>boolean</li>
<li>byte</li>
<li>char</li>
<li>date</li>
<li>DateTime</li>
<li>double</li>
<li>file</li>
<li>float</li>
<li>int</li>
<li>integer</li>
<li>long</li>
<li>mixed</li>
<li>number</li>
<li>short</li>
<li>string</li>
<li>void</li>
</ul>

## RESERVED WORDS

| ABORT      | ABSOLUTE  | ACCESS     | ACTION    | ADD       |
|------------|-----------|------------|-----------|-----------|
| ADMIN      | AFTER     | AGGREGATE  | ALL       | ALSO      |
| ALTER      | ALWAYS    | ANALYSE    | ANALYZE   | AND       |
| ANY        | ARRAY     | AS         | ASC       | ASSERTION |
| ASSIGNMENT | ASYMMETRIC| AT         | ATTACH    | ATTRIBUTE |
| AUTHORIZATION | BACKWARD | BEFORE   | BEGIN     | BETWEEN   |
| BIGINT     | BINARY    | BIT        | BOOLEAN   | BOTH      |
| BY         | CACHE     | CALL       | CALLED    | CASCADE   |
| CASCADED   | CASE      | CAST       | CATALOG   | CHAIN     |
| CHAR       | CHARACTER | CHARACTERISTICS | CHECK | CHECKPOINT|
| CLASS      | CLOSE     | CLUSTER    | COALESCE  | COLLATE   |
| COLLATION  | COLUMN    | COLUMNS    | COMMENT   | COMMENTS  |
| COMMIT     | COMMITTED | CONCURRENTLY | CONFIGURATION | CONFLICT |
| CONNECTION | CONSTRAINT| CONSTRAINTS| CONTENT   | CONTINUE  |
| CONVERSION | COPY      | COST       | CREATE    | CROSS     |
| CSV        | CUBE      | CURRENT    | CURRENT_CATALOG | CURRENT_DATE |
| CURRENT_ROLE | CURRENT_SCHEMA | CURRENT_TIME | CURRENT_TIMESTAMP | CURRENT_USER |
| CURSOR     | CYCLE     | DATA       | DATABASE  | DAY       |
| DEALLOCATE | DEC       | DECIMAL    | DECLARE   | DEFAULT   |
| DEFAULTS   | DEFERRABLE| DEFERRED   | DEFINE    | DEFINER   |
| DELETE     | DELIMITER | DELIMITERS | DEPENDS   | DEPTH     |
| DESC       | DESCRIBE  | DETACH     | DICTIONARY| DISABLE   |
| DISCARD    | DISTINCT  | DO         | DOCUMENT  | DOMAIN    |
| DOUBLE     | DROP      | EACH       | ELSE      | ENABLE    |
| ENCODING   | ENCRYPTED | END        | ENUM      | ESCAPE    |
| EVENT      | EXCEPT    | EXCLUDE    | EXCLUDING | EXCLUSIVE |
| EXECUTE    | EXISTS    | EXPLAIN    | EXTENSION | EXTERNAL  |
| EXTRACT    | FALSE     | FAMILY     | FETCH     | FILTER    |
| FINALIZE   | FIRST     | FLOAT      | FOLLOWING | FOR       |
| FORCE      | FOREIGN   | FORWARD    | FREEZE    | FROM      |
| FULL       | FUNCTION  | FUNCTIONS  | GENERATED | GLOBAL    |
| GRANT      | GRANTED   | GREATEST   | GROUP     | GROUPING  |
| HANDLER    | HAVING    | HEADER     | HOLD      | HOUR      |
| IDENTITY   | IF        | ILIKE      | IMMEDIATE | IMMUTABLE |
| IMPLICIT   | IMPORT    | IN         | INCLUDING | INCREMENT |
| INDEX      | INDEXES   | INHERIT    | INHERITS  | INITIALLY |
| INLINE     | INNER     | INOUT      | INPUT     | INSENSITIVE |
| INSERT     | INSTEAD   | INT        | INTEGER   | INTERSECT |
| INTERVAL   | INTO      | INVOKER    | IS        | ISNULL    |
| ISOLATION  | JOIN      | KEY        | LABEL     | LANGUAGE  |
| LARGE      | LAST      | LATERAL    | LEADING   | LEAKPROOF |
| LEAST      | LEFT      | LEVEL      | LIKE      | LIMIT     |
| LISTEN     | LOAD      | LOCAL      | LOCALTIME | LOCALTIMESTAMP |
| LOCATION   | LOCK      | LOCKED     | LOGGED    | MAPPING   |
| MATCH      | MATERIALIZED | MAXVALUE | METHOD    | MINUTE    |
| MINVALUE   | MODE      | MODIFY     | MONTH     | MOVE      |
| NAME       | NAMES     | NATIONAL   | NATURAL   | NCHAR     |
| NEW        | NEXT      | NO         | NONE      | NORMALIZE |
| NORMALIZED | NOT       | NOTHING    | NOTIFY    | NOTNULL   |
| NOWAIT     | NULL      | NULLABLE   | NULLIF    | NUMERIC   |
| OBJECT     | OF        | OFF        | OFFSET    | OIDS      |
| OLD        | ON        | ONLY       | OPERATOR  | OPTION    |
| OPTIONS    | OR        | ORDER      | ORDINALITY | OTHERS   |
| OUT        | OUTER      | OVER       | OVERLAPS   | OVERLAY    |
| OVERRIDING | OWNED      | OWNER      | PARALLEL   | PARAMETER  |
| PARSER     | PARTIAL    | PARTITION  | PASSING    | PASSWORD   |
| PLACING    | PLANS      | POLICY     | POSITION   | PRECEDING  |
| PRECISION  | PREPARE    | PREPARED   | PRESERVE   | PRIMARY    |
| PRIOR      | PRIVILEGES | PROCEDURAL | PROCEDURE  | PROGRAM    |
| PUBLICATION| QUOTE      | RANGE      | READ       | REAL       |
| REASSIGN   | RECHECK    | RECURSIVE  | REF        | REFERENCES |
| REFERENCING| REFRESH    | REINDEX    | RELATIVE   | RELEASE    |
| RENAME     | REPEATABLE | REPLACE    | REPLICA    | RESET      |
| RESTART    | RESTRICT   | RETURNING  | RETURNS    | REVOKE     |
| RIGHT      | ROLE       | ROLLBACK   | ROLLUP     | ROUTINE    |
| ROUTINES   | ROW        | ROWS       | RULE       | SAVEPOINT  |
| SCHEMA     | SCHEMAS    | SCROLL     | SEARCH     | SECOND     |
| SECURITY   | SELECT     | SEQUENCE   | SEQUENCES  | SERIALIZABLE|
| SERVER     | SESSION    | SESSION_USER| SET       | SETOF      |
| SETS       | SHARE      | SHOW       | SIMILAR    | SIMPLE     |
| SKIP       | SMALLINT   | SNAPSHOT   | SOME       | SQL        |
| STABLE     | STANDALONE | START      | STATEMENT  | STATISTICS |
| STDIN      | STDOUT     | STORAGE    | STRICT     | STRIP      |
| SUBSCRIPTION| SUBSTRING | SYMMETRIC  | SYSID      | SYSTEM     |
| TABLE      | TABLES     | TABLESAMPLE| TABLESPACE | TEMP       |
| TEMPLATE   | TEMPORARY  | TEXT       | THEN       | TIES       |
| TIME       | TIMESTAMP  | TO         | TRAILING   | TRANSACTION|
| TRANSFORM  | TREAT      | TRIGGER    | TRIM       | TRUE       |
| TRUNCATE   | TRUSTED    | TYPE       | TYPES      | UNBOUNDED  |
| UNCOMMITTED| UNENCRYPTED| UNION      | UNIQUE     | UNKNOWN    |
| UNLISTEN   | UNLOGGED   | UNTIL      | UPDATE     | USER       |
| USING      | VACUUM     | VALID      | VALIDATE   | VALIDATOR  |
| VALUE      | VALUES     | VARCHAR    | VARIADIC   | VARYING    |
| VERBOSE    | VERSION    | VIEW       | VIEWS      | VOLATILE   |
| WHEN       | WHERE      | WHITESPACE | WINDOW     | WITH       |
| WITHIN     | WITHOUT    | WORK       | WRAPPER    | WRITE      |
| XML        | XMLATTRIBUTES| XMLCONCAT | XMLELEMENT | XMLEXISTS  |
| XMLFOREST  | XMLNAMESPACES| XMLPARSE | XMLPI      | XMLROOT    |
| XMLSERIALIZE| XMLTABLE  | YEAR       | YES        | ZONE       |

Source: [Full list of keywords in the PostgreSQL 17](https://www.postgresql.org/docs/17/sql-keywords-appendix.html)


## FEATURE SET

### Client Modification Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|BasePath|✗|ToolingExtension
|Authorizations|✗|ToolingExtension
|UserAgent|✗|ToolingExtension
|MockServer|✗|ToolingExtension

### Data Type Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |s
|Custom|✗|OAS2,OAS3
|Int32|✓|OAS2,OAS3
|Int64|✓|OAS2,OAS3
|Float|✓|OAS2,OAS3
|Double|✓|OAS2,OAS3
|Decimal|✓|ToolingExtension
|String|✓|OAS2,OAS3
|Byte|✓|OAS2,OAS3
|Binary|✓|OAS2,OAS3
|Boolean|✓|OAS2,OAS3
|Date|✓|OAS2,OAS3
|DateTime|✓|OAS2,OAS3
|Password|✓|OAS2,OAS3
|File|✓|OAS2
|Uuid|✗|
|Array|✓|OAS2,OAS3
|Null|✗|OAS3
|AnyType|✗|OAS2,OAS3
|Object|✓|OAS2,OAS3
|Maps|✓|ToolingExtension
|CollectionFormat|✓|OAS2
|CollectionFormatMulti|✓|OAS2
|Enum|✓|OAS2,OAS3
|ArrayOfEnum|✓|ToolingExtension
|ArrayOfModel|✓|ToolingExtension
|ArrayOfCollectionOfPrimitives|✓|ToolingExtension
|ArrayOfCollectionOfModel|✓|ToolingExtension
|ArrayOfCollectionOfEnum|✓|ToolingExtension
|MapOfEnum|✓|ToolingExtension
|MapOfModel|✓|ToolingExtension
|MapOfCollectionOfPrimitives|✓|ToolingExtension
|MapOfCollectionOfModel|✓|ToolingExtension
|MapOfCollectionOfEnum|✓|ToolingExtension

### Documentation Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Readme|✓|ToolingExtension
|Model|✓|ToolingExtension
|Api|✓|ToolingExtension

### Global Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Host|✓|OAS2,OAS3
|BasePath|✓|OAS2,OAS3
|Info|✓|OAS2,OAS3
|Schemes|✗|OAS2,OAS3
|PartialSchemes|✓|OAS2,OAS3
|Consumes|✓|OAS2
|Produces|✓|OAS2
|ExternalDocumentation|✓|OAS2,OAS3
|Examples|✓|OAS2,OAS3
|XMLStructureDefinitions|✗|OAS2,OAS3
|MultiServer|✗|OAS3
|ParameterizedServer|✗|OAS3
|ParameterStyling|✗|OAS3
|Callbacks|✗|OAS3
|LinkObjects|✗|OAS3

### Parameter Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Path|✓|OAS2,OAS3
|Query|✓|OAS2,OAS3
|Header|✓|OAS2,OAS3
|Body|✓|OAS2
|FormUnencoded|✓|OAS2
|FormMultipart|✓|OAS2
|Cookie|✓|OAS3

### Schema Support Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|Simple|✓|OAS2,OAS3
|Composite|✓|OAS2,OAS3
|Polymorphism|✗|OAS2,OAS3
|Union|✗|OAS3
|allOf|✗|OAS2,OAS3
|anyOf|✗|OAS3
|oneOf|✗|OAS3
|not|✗|OAS3

### Security Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|BasicAuth|✗|OAS2,OAS3
|ApiKey|✗|OAS2,OAS3
|OpenIDConnect|✗|OAS3
|BearerToken|✗|OAS3
|OAuth2_Implicit|✗|OAS2,OAS3
|OAuth2_Password|✗|OAS2,OAS3
|OAuth2_ClientCredentials|✗|OAS2,OAS3
|OAuth2_AuthorizationCode|✗|OAS2,OAS3
|SignatureAuth|✗|OAS3
|AWSV4Signature|✗|ToolingExtension

### Wire Format Feature
| Name | Supported | Defined By |
| ---- | --------- | ---------- |
|JSON|✗|OAS2,OAS3
|XML|✗|OAS2,OAS3
|PROTOBUF|✗|ToolingExtension
|Custom|✗|OAS2,OAS3
