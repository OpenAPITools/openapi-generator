package org.openapitools.codegen.languages;

/** Emits exact scalar and deep-JSON literals for densified schema IR rows. */
final class Oas31ExactLiteralEmitter {

    private Oas31ExactLiteralEmitter() {
    }

    static void appendNodeLiterals(
            StringBuilder sb,
            Oas31SchemaIrEmitter.IrNode node) {
        appendSetExact(sb, "n.minimum", "n.hasMinimum", node.minimum);
        appendSetExact(sb, "n.maximum", "n.hasMaximum", node.maximum);
        appendSetExact(sb, "n.exclusiveMinimum", "n.hasExclusiveMinimum", node.exclusiveMinimum);
        appendSetExact(sb, "n.exclusiveMaximum", "n.hasExclusiveMaximum", node.exclusiveMaximum);
        appendSetExact(sb, "n.multipleOf", "n.hasMultipleOf", node.multipleOf);

        for (String lexeme : node.enumNumbers) {
            sb.append("        n.enumNumbers.push_back(ExactNumber::parseLexeme(\"")
                    .append(lexeme).append("\"));\n");
        }
        for (String value : node.enumStrings) {
            sb.append("        n.enumStrings.push_back(\"").append(value).append("\");\n");
        }
        for (String value : node.enumBooleans) {
            sb.append("        n.enumBooleans.push_back(").append(value).append(");\n");
        }

        if (node.constNumber != null) {
            sb.append("        n.hasConst = true;\n");
            sb.append("        n.constNumber = ExactNumber::parseLexeme(\"")
                    .append(node.constNumber).append("\");\n");
            sb.append("        n.constIsNumber = true;\n");
        }
        if (node.constString != null) {
            sb.append("        n.hasConst = true;\n");
            sb.append("        n.constString = \"").append(node.constString).append("\";\n");
            sb.append("        n.constIsString = true;\n");
        }
        if (node.constBool != null) {
            sb.append("        n.hasConst = true;\n");
            sb.append("        n.constBool = ").append(node.constBool).append(";\n");
            sb.append("        n.constIsBool = true;\n");
        }

        if (node.constJson != null) {
            sb.append("        n.hasConst = true;\n");
            sb.append("        n.constIsJson = true;\n");
            appendJsonParse(sb, "n.constJson", node.constJson);
        }
        if (node.enumJson != null) {
            sb.append("        n.hasEnumJson = true;\n");
            sb.append("        { ExactJsonValue _exact = parseExactJson(");
            appendCppRawString(sb, node.enumJson);
            sb.append(");\n");
            sb.append("          n.enumJsonLexemes = std::move(_exact.lexemes);\n");
            sb.append("          for (boost::json::value& _e : _exact.value.as_array()) ")
                    .append("n.enumJson.push_back(std::move(_e)); }\n");
        }
    }

    static void appendSetExact(
            StringBuilder sb,
            String field,
            String hasField,
            String lexeme) {
        if (lexeme != null) {
            sb.append("        setExact(").append(field).append(", ").append(hasField)
                    .append(", \"").append(lexeme).append("\");\n");
        }
    }

    private static void appendJsonParse(StringBuilder sb, String field, String json) {
        sb.append("        { ExactJsonValue _exact = parseExactJson(");
        appendCppRawString(sb, json);
        sb.append("); ").append(field).append(" = std::move(_exact.value); ")
                .append(field).append("Lexemes = std::move(_exact.lexemes); }\n");
    }

    private static void appendCppRawString(StringBuilder sb, String value) {
        int suffix = 0;
        String delimiter;
        do {
            delimiter = "OAS" + Integer.toUnsignedString(suffix, 36);
            suffix += 1;
        }
        while (value.contains(")" + delimiter + "\""));
        sb.append("R\"").append(delimiter).append("(")
                .append(value).append(")").append(delimiter).append("\"");
    }
}
