# MySQL Schema Codegen

Main goal of this generator is to provide database structure file almost identical you usually generate with:
- PHPMyAdmin (Export structure only, SQL syntax)
- Adminer
- `mysqldump` function

[MySQL documentation](https://dev.mysql.com/doc/)

## Requirements
- MySQL Server ^5.7.8 (`JSON` column type added)

## Openapi Data Type to MySQL Data Type mapping

| Openapi Data Type | Openapi Data Format | Dependent properties | MySQL Data Types | Default MySQL Data Type |
| --- | --- | --- | --- | --- |
| `integer` | `int32` | `minimum` / `maximum` / `minimumExclusive` / `maximumExclusive` | `TINYINT` / `SMALLINT` / `MEDIUMINT`/ `INT` / `BIGINT` | `INT` |
| `integer` | `int64` | `minimum` / `maximum` / `minimumExclusive` / `maximumExclusive` | `TINYINT` / `SMALLINT` / `MEDIUMINT` / `INT` / `BIGINT` | `BIGINT` |
| `boolean` | | | `TINYINT` | `TINYINT` |
| `number` | `float` | | `DECIMAL` | `DECIMAL` |
| `number` | `double` | | `DECIMAL` | `DECIMAL` |
| `string` | | `minLength` / `maxLength` | `CHAR` / `VARCHAR` / `TEXT` / `MEDIUMTEXT` / `LONGTEXT` | `TEXT` |
| `string` | `byte` |  | `TEXT` | `TEXT` |
| `string` | `binary` |  | `MEDIUMBLOB` | `MEDIUMBLOB` |
| `file` | |  | `MEDIUMBLOB` | `MEDIUMBLOB` |
| `string` | `date` | | `DATE` | `DATE` |
| `string` | `date-time` | | `DATETIME` | `DATETIME` |
| `string` | `enum` | | `ENUM` | `ENUM` |
| `array` | | | `JSON` | `JSON` |
| `object` | | | `JSON` | `JSON` |
| `\Model\User` (referenced definition) | | | `TEXT` | `TEXT` |

## How to use

Produced file(`mysql_schema.sql`) contains every table definition. Current implementation doesn't drop or modify existed tables, if you want rewrite whole schema make sure they're not presented.

### PHPMyAdmin

1. Choose **Import** tab from the home screen
2. In section **File to import** click to **Choose File** and find generated `mysql_schema.sql`
3. Make sure **Format** selector set to **SQL**
4. Push **Go** button

### Adminer

1. Click **Import** link in left sidebar
2. In **File upload** fieldset click to **Choose Files** and find generated `mysql_schema.sql`
3. Push **Execute** button
