package io.swagger.codegen;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class CodegenHelper {

    static Set<String> getDefaultIncludes() {
        return new HashSet<>(
                Arrays.asList("double",
                        "int",
                        "long",
                        "short",
                        "char",
                        "float",
                        "String",
                        "boolean",
                        "Boolean",
                        "Double",
                        "Void",
                        "Integer",
                        "Long",
                        "Float")
        );
    }

    static Map<String, String> getTypeMappings() {
        final Map<String, String> typeMapping = new HashMap<>();
        typeMapping.put("array", "List");
        typeMapping.put("map", "Map");
        typeMapping.put("List", "List");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("string", "String");
        typeMapping.put("int", "Integer");
        typeMapping.put("float", "Float");
        typeMapping.put("number", "BigDecimal");
        typeMapping.put("DateTime", "Date");
        typeMapping.put("long", "Long");
        typeMapping.put("short", "Short");
        typeMapping.put("char", "String");
        typeMapping.put("double", "Double");
        typeMapping.put("object", "Object");
        typeMapping.put("integer", "Integer");
        typeMapping.put("ByteArray", "byte[]");
        typeMapping.put("binary", "byte[]");
        typeMapping.put("file", "File");
        typeMapping.put("UUID", "UUID");
        typeMapping.put("BigDecimal", "BigDecimal");
        return typeMapping;
    }

    static Map<String, String> getImportMappings() {
        final Map<String, String> importMapping = new HashMap<>();
        importMapping.put("BigDecimal", "java.math.BigDecimal");
        importMapping.put("UUID", "java.util.UUID");
        importMapping.put("File", "java.io.File");
        importMapping.put("Date", "java.util.Date");
        importMapping.put("Timestamp", "java.sql.Timestamp");
        importMapping.put("Map", "java.util.Map");
        importMapping.put("HashMap", "java.util.HashMap");
        importMapping.put("Array", "java.util.List");
        importMapping.put("ArrayList", "java.util.ArrayList");
        importMapping.put("List", "java.util.*");
        importMapping.put("Set", "java.util.*");
        importMapping.put("DateTime", "org.joda.time.*");
        importMapping.put("LocalDateTime", "org.joda.time.*");
        importMapping.put("LocalDate", "org.joda.time.*");
        importMapping.put("LocalTime", "org.joda.time.*");
        return importMapping;
    }

    static void initalizeSpecialCharacterMapping(Map<String, String> specialCharReplacements) {
        specialCharReplacements.put("$", "Dollar");
        specialCharReplacements.put("^", "Caret");
        specialCharReplacements.put("|", "Pipe");
        specialCharReplacements.put("=", "Equal");
        specialCharReplacements.put("*", "Star");
        specialCharReplacements.put("-", "Minus");
        specialCharReplacements.put("&", "Ampersand");
        specialCharReplacements.put("%", "Percent");
        specialCharReplacements.put("#", "Hash");
        specialCharReplacements.put("@", "At");
        specialCharReplacements.put("!", "Exclamation");
        specialCharReplacements.put("+", "Plus");
        specialCharReplacements.put(":", "Colon");
        specialCharReplacements.put(">", "Greater_Than");
        specialCharReplacements.put("<", "Less_Than");
        specialCharReplacements.put(".", "Period");
        specialCharReplacements.put("_", "Underscore");
        specialCharReplacements.put("?", "Question_Mark");
        specialCharReplacements.put(",", "Comma");
        specialCharReplacements.put("'", "Quote");
        specialCharReplacements.put("\"", "Double_Quote");
        specialCharReplacements.put("/", "Slash");
        specialCharReplacements.put("\\", "Back_Slash");
        specialCharReplacements.put("(", "Left_Parenthesis");
        specialCharReplacements.put(")", "Right_Parenthesis");
        specialCharReplacements.put("{", "Left_Curly_Bracket");
        specialCharReplacements.put("}", "Right_Curly_Bracket");
        specialCharReplacements.put("[", "Left_Square_Bracket");
        specialCharReplacements.put("]", "Right_Square_Bracket");
        specialCharReplacements.put("~", "Tilde");
        specialCharReplacements.put("`", "Backtick");
        specialCharReplacements.put("<=", "Less_Than_Or_Equal_To");
        specialCharReplacements.put(">=", "Greater_Than_Or_Equal_To");
        specialCharReplacements.put("!=", "Not_Equal");
    }
}
