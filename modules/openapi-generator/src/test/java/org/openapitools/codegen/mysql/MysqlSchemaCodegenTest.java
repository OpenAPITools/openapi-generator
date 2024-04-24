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

package org.openapitools.codegen.mysql;

import org.openapitools.codegen.languages.MysqlSchemaCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Set;

public class MysqlSchemaCodegenTest {

    @Test
    public void testGetMysqlMatchedIntegerDataType() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        Assertions.assertSame(codegen.getMysqlMatchedIntegerDataType(null, null, null), "INT");

        Assertions.assertSame(codegen.getMysqlMatchedIntegerDataType(-128L, 127L, false), "TINYINT");
        Assertions.assertSame(codegen.getMysqlMatchedIntegerDataType(0L, 255L, true), "TINYINT");

        Assertions.assertSame(codegen.getMysqlMatchedIntegerDataType(500L, 100L, null), "SMALLINT");
        Assertions.assertSame(codegen.getMysqlMatchedIntegerDataType(500L, 100L, true), "SMALLINT");
        Assertions.assertSame(codegen.getMysqlMatchedIntegerDataType(500L, 100L, false), "SMALLINT");
        Assertions.assertSame(codegen.getMysqlMatchedIntegerDataType(-32768L, 32767L, false), "SMALLINT");
        Assertions.assertSame(codegen.getMysqlMatchedIntegerDataType(0L, 65535L, true), "SMALLINT");

        Assertions.assertSame(codegen.getMysqlMatchedIntegerDataType(-8388608L, 8388607L, false), "MEDIUMINT");
        Assertions.assertSame(codegen.getMysqlMatchedIntegerDataType(0L, 16777215L, true), "MEDIUMINT");

        Assertions.assertSame(codegen.getMysqlMatchedIntegerDataType(-2147483648L, 2147483647L, false), "INT");
        Assertions.assertSame(codegen.getMysqlMatchedIntegerDataType(Long.parseLong(String.valueOf(Integer.MIN_VALUE)), Long.parseLong(String.valueOf(Integer.MAX_VALUE)), false), "INT");
        Assertions.assertSame(codegen.getMysqlMatchedIntegerDataType(0L, 4294967295L, true), "INT");

        Assertions.assertSame(codegen.getMysqlMatchedIntegerDataType(-2147483649L, 2147483648L, false), "BIGINT");
        Assertions.assertSame(codegen.getMysqlMatchedIntegerDataType(0L, 4294967296L, true), "BIGINT");
    }

    @Test
    public void testGetMysqlMatchedStringDataType() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(6, 6), "CHAR");
        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(0, 0), "CHAR");
        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(255, 255), "CHAR");

        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(null, 100), "VARCHAR");
        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(null, 255), "VARCHAR");
        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(50, 255), "VARCHAR");
        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(100, 20), "VARCHAR");

        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(null, null), "TEXT");
        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(100, null), "TEXT");
        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(255, null), "TEXT");
        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(null, 256), "TEXT");

        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(16777215, null), "MEDIUMTEXT");
        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(16777215, 100), "MEDIUMTEXT");
        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(null, 16777215), "MEDIUMTEXT");
        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(100, 16777215), "MEDIUMTEXT");

        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(16777216, null), "LONGTEXT");
        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(null, 16777216), "LONGTEXT");
        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(16777216, 16777216), "LONGTEXT");
        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(100, 16777216), "LONGTEXT");
        Assertions.assertSame(codegen.getMysqlMatchedStringDataType(100, Integer.MAX_VALUE), "LONGTEXT");
    }

    @Test
    public void testToCodegenMysqlDataTypeArgument() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        String strArgument = "HelloWorld";
        HashMap<String, Object> strProp = codegen.toCodegenMysqlDataTypeArgument(strArgument);
        Assertions.assertTrue((Boolean) strProp.get("isString"));
        Assertions.assertFalse((Boolean) strProp.get("isFloat"));
        Assertions.assertFalse((Boolean) strProp.get("isInteger"));
        Assertions.assertFalse((Boolean) strProp.get("isNumeric"));
        Assertions.assertSame(strProp.get("argumentValue"), strArgument);

        Integer intArgument = 10;
        HashMap<String, Object> intProp = codegen.toCodegenMysqlDataTypeArgument(intArgument);
        Assertions.assertFalse((Boolean) intProp.get("isString"));
        Assertions.assertFalse((Boolean) intProp.get("isFloat"));
        Assertions.assertTrue((Boolean) intProp.get("isInteger"));
        Assertions.assertTrue((Boolean) intProp.get("isNumeric"));
        Assertions.assertSame(intProp.get("argumentValue"), intArgument);

        Double floatArgument = 3.14;
        HashMap<String, Object> floatProp = codegen.toCodegenMysqlDataTypeArgument(floatArgument);
        Assertions.assertFalse((Boolean) floatProp.get("isString"));
        Assertions.assertTrue((Boolean) floatProp.get("isFloat"));
        Assertions.assertFalse((Boolean) floatProp.get("isInteger"));
        Assertions.assertTrue((Boolean) floatProp.get("isNumeric"));
        Assertions.assertSame(floatProp.get("argumentValue"), floatArgument);
    }

    @Test
    public void testToCodegenMysqlDataTypeDefault() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        HashMap<String, Object> defaultMap = null;
        ArrayList<String> intFixture = new ArrayList<String>(Arrays.asList(
            "TINYINT", "SmallInt", "Mediumint", "INT", "bigint"
        ));
        for(String intType : intFixture) {
            defaultMap = codegen.toCodegenMysqlDataTypeDefault("150", intType);
            Assertions.assertTrue((Boolean) defaultMap.get("isNumeric"));
            Assertions.assertFalse((Boolean) defaultMap.get("isString"));
            Assertions.assertFalse((Boolean) defaultMap.get("isKeyword"));
            Assertions.assertSame(defaultMap.get("defaultValue"), "150");
        }
        defaultMap = codegen.toCodegenMysqlDataTypeDefault("SERIAL DEFAULT VALUE", "TINYINT");
        Assertions.assertFalse((Boolean) defaultMap.get("isNumeric"));
        Assertions.assertFalse((Boolean) defaultMap.get("isString"));
        Assertions.assertTrue((Boolean) defaultMap.get("isKeyword"));
        Assertions.assertSame(defaultMap.get("defaultValue"), "SERIAL DEFAULT VALUE");

        ArrayList<String> dateFixture = new ArrayList<String>(Arrays.asList(
            "Timestamp", "DateTime"
        ));
        for(String dateType : dateFixture) {
            defaultMap = codegen.toCodegenMysqlDataTypeDefault("2018-08-12", dateType);
            Assertions.assertFalse((Boolean) defaultMap.get("isNumeric"));
            Assertions.assertTrue((Boolean) defaultMap.get("isString"));
            Assertions.assertFalse((Boolean) defaultMap.get("isKeyword"));
            Assertions.assertSame(defaultMap.get("defaultValue"), "2018-08-12");
        }
        defaultMap = codegen.toCodegenMysqlDataTypeDefault("CURRENT_TIMESTAMP", "Timestamp");
        Assertions.assertFalse((Boolean) defaultMap.get("isNumeric"));
        Assertions.assertFalse((Boolean) defaultMap.get("isString"));
        Assertions.assertTrue((Boolean) defaultMap.get("isKeyword"));
        Assertions.assertSame(defaultMap.get("defaultValue"), "CURRENT_TIMESTAMP");

        ArrayList<String> restFixture = new ArrayList<String>(Arrays.asList(
            "VARCHAR", "CHAR", "ENUM", "UNKNOWN"
        ));
        for(String restType : restFixture) {
            defaultMap = codegen.toCodegenMysqlDataTypeDefault("sometext", restType);
            Assertions.assertFalse((Boolean) defaultMap.get("isNumeric"));
            Assertions.assertTrue((Boolean) defaultMap.get("isString"));
            Assertions.assertFalse((Boolean) defaultMap.get("isKeyword"));
            Assertions.assertSame(defaultMap.get("defaultValue"), "sometext");
        }
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void testToCodegenMysqlDataTypeDefaultWithExceptionalColumnType() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        HashMap<String, Object> defaultMap = null;
        ArrayList<String> specialFixture = new ArrayList<String>(Arrays.asList(
            "TINYBLOB", "Blob", "MEDIUMBLOB", "LONGBLOB", "TINYTEXT", "TEXT", "MEDIUMTEXT", "LONGTEXT", "GEOMETRY", "JSON"
        ));
        for(String specialType : specialFixture) {
            defaultMap = codegen.toCodegenMysqlDataTypeDefault("2018-08-12", specialType);
            Assertions.assertNull(defaultMap);
        }
    }

    @Test
    public void testIsMysqlDataType() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        ArrayList<String> trueFixture = new ArrayList<String>(Arrays.asList(
            "INTEGER", "integer", "Integer", "DATETIME", "datetime", "DateTime", "VARCHAR", "varchar", "VarChar", "POINT", "Point", "point", "JSON", "json", "Json"
        ));
        ArrayList<String> falseFixture = new ArrayList<String>(Arrays.asList(
            "unknown", "HashMap", "HASHMAP", "hashmap"
        ));
        for(String trueValue : trueFixture) {
            Assertions.assertTrue(codegen.isMysqlDataType(trueValue), "'" + trueValue + "' isn't MySQL data type");
        }
        for(String falseValue : falseFixture) {
            Assertions.assertFalse(codegen.isMysqlDataType(falseValue), "'" + falseValue + "' is MySQL data type");
        }
    }

    @Test
    public void testToMysqlIdentifier() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        Assertions.assertEquals(codegen.toMysqlIdentifier("table_name", "tbl_", ""), "table_name");
        Assertions.assertEquals(codegen.toMysqlIdentifier("table_name   ", "tbl_", ""), "table_name");
        Assertions.assertEquals(codegen.toMysqlIdentifier("12345678", "tbl_", ""), "tbl_12345678");
    }

    @Test(expectedExceptions = RuntimeException.class)
    public void testToMysqlIdentifierWithEmptyString() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        codegen.toMysqlIdentifier("   ", "tbl_", "");
    }

    @Test
    public void testEscapeMysqlUnquotedIdentifier() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        Assertions.assertEquals(codegen.escapeMysqlUnquotedIdentifier("table1Z$_"), "table1Z$_");
        Assertions.assertEquals(codegen.escapeMysqlUnquotedIdentifier("table1Z$_!#%~&?()*+-./"), "table1Z$_");
        Assertions.assertEquals(codegen.escapeMysqlUnquotedIdentifier("table1Z$_русскийтекст"), "table1Z$_русскийтекст");
        Assertions.assertEquals(codegen.escapeMysqlQuotedIdentifier("table𐀀"), "table");
        Assertions.assertEquals(codegen.escapeMysqlQuotedIdentifier("table_name!'()�"), "table_name!'()�");
        Assertions.assertEquals(codegen.escapeMysqlQuotedIdentifier("table_name𐌅𐌌 "), "table_name");
    }

    @Test
    public void testEscapeMysqlQuotedIdentifier() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        Assertions.assertEquals(codegen.escapeMysqlQuotedIdentifier("table"), "table");
        Assertions.assertEquals(codegen.escapeMysqlQuotedIdentifier("table𐀀"), "table");
        Assertions.assertEquals(codegen.escapeMysqlQuotedIdentifier("table_name!'()�"), "table_name!'()�");
        Assertions.assertEquals(codegen.escapeMysqlQuotedIdentifier("table_name𐌅𐌌 "), "table_name");
    }

    @Test
    public void testIsReservedWord() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        Set<String> reservedWords = codegen.reservedWords();
        ArrayList<String> trueFixture = new ArrayList<String>(Arrays.asList(
            "accessible", "asc", "between", "blob", "change", "column", "day_hour", "distinct", "enclosed", "except", "explain", "float", "for", "function", "grant", "grouping", "high_priority", "groups", "hour_minute", "insensitive", "interval", "json_table", "keys", "kill", "leave", "left", "mediumblob", "modifies", "not", "null", "numeric", "optimize", "outer", "precision", "primary", "references", "replace", "select", "sql", "then", "tinytext", "unique", "unlock", "varchar", "virtual", "when", "where", "xor", "year_month", "zerofill"
        ));
        ArrayList<String> falseFixture = new ArrayList<String>(Arrays.asList(
            "after", "boolean", "charset", "cpu", "current", "delay_key_write", "end", "format", "global", "host", "install", "json", "key_block_size", "local", "max_size", "none", "offset", "partial", "quarter", "relay", "second", "status", "timestamp", "until", "variables", "without", "xml", "year"
        ));
        for(String trueValue : trueFixture) {
            Assertions.assertTrue(reservedWords.contains(trueValue), "'" + trueValue + "' isn't MySQL reserved word");
        }
        for(String falseValue : falseFixture) {
            Assertions.assertFalse(reservedWords.contains(falseValue), "'" + falseValue + "' is MySQL reserved word");
        }
    }

    @Test
    public void testSetDefaultDatabaseName() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        codegen.setDefaultDatabaseName("valid_db_name");
        Assertions.assertSame(codegen.getDefaultDatabaseName(), "valid_db_name");
        codegen.setDefaultDatabaseName("12345");
        Assertions.assertNotSame(codegen.getDefaultDatabaseName(), "12345");
    }

    @Test
    public void testGetDefaultDatabaseName() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        Assertions.assertSame(codegen.getDefaultDatabaseName(), "");
    }

    @Test
    public void testSetJsonDataTypeEnabled() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        codegen.setJsonDataTypeEnabled(true);
        Assertions.assertTrue(codegen.getJsonDataTypeEnabled());
        codegen.setJsonDataTypeEnabled(false);
        Assertions.assertFalse(codegen.getJsonDataTypeEnabled());
    }

    @Test
    public void testGetJsonDataTypeEnabled() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        Assertions.assertTrue(codegen.getJsonDataTypeEnabled());
        codegen.setJsonDataTypeEnabled(false);
        Assertions.assertFalse(codegen.getJsonDataTypeEnabled());
    }

    @Test
    public void testSetNamedParametersEnabled() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        codegen.setNamedParametersEnabled(true);
        Assertions.assertTrue(codegen.getNamedParametersEnabled());
        codegen.setNamedParametersEnabled(false);
        Assertions.assertFalse(codegen.getNamedParametersEnabled());
    }

    @Test
    public void testGetNamedParametersEnabled() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        Assertions.assertFalse(codegen.getNamedParametersEnabled());
        codegen.setNamedParametersEnabled(true);
        Assertions.assertTrue(codegen.getNamedParametersEnabled());
    }

    @Test
    public void testSetIdentifierNamingConvention() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        Assertions.assertSame("original", codegen.getIdentifierNamingConvention());
        codegen.setIdentifierNamingConvention("invalidValue");
        Assertions.assertSame("original", codegen.getIdentifierNamingConvention());
        codegen.setIdentifierNamingConvention("snake_case");
        Assertions.assertSame("snake_case", codegen.getIdentifierNamingConvention());
        codegen.setIdentifierNamingConvention("anotherInvalid");
        Assertions.assertSame("snake_case", codegen.getIdentifierNamingConvention());
    }

    @Test
    public void testGetIdentifierNamingConvention() {
        final MysqlSchemaCodegen codegen = new MysqlSchemaCodegen();
        Assertions.assertSame("original", codegen.getIdentifierNamingConvention());
        codegen.setIdentifierNamingConvention("snake_case");
        Assertions.assertSame("snake_case", codegen.getIdentifierNamingConvention());
    }

}
