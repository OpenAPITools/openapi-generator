package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.models.properties.Property;

import java.util.Arrays;

abstract public class AbstractCppCodegen extends DefaultCodegen implements CodegenConfig {

    public AbstractCppCodegen() {
        super();

        /*
         * Reserved words.  Override this with reserved words specific to your language
         */
        setReservedWordsLowerCase(
                Arrays.asList(
                        "auto",
                        "break",
                        "case",
                        "char",
                        "const",
                        "continue",
                        "default",
                        "do",
                        "double",
                        "else",
                        "enum",
                        "extern",
                        "float",
                        "for",
                        "goto",
                        "if",
                        "int",
                        "long",
                        "register",
                        "return",
                        "short",
                        "signed",
                        "sizeof",
                        "static",
                        "struct",
                        "switch",
                        "typedef",
                        "union",
                        "unsigned",
                        "void",
                        "volatile",
                        "while",
                        "asm",
                        "bool",
                        "catch",
                        "class",
                        "const_cast",
                        "delete",
                        "dynamic_cast",
                        "explicit",
                        "false",
                        "friend",
                        "inline",
                        "mutable",
                        "namespace",
                        "new",
                        "operator",
                        "private",
                        "public",
                        "protected",
                        "reinterpret_cast",
                        "static_cast",
                        "template",
                        "this",
                        "throw",
                        "true",
                        "try",
                        "typeid",
                        "typename",
                        "using",
                        "virtual",
                        "wchar_t",
                        "and",
                        "and_eq",
                        "bitand",
                        "bitor",
                        "compl",
                        "not",
                        "not_eq",
                        "or",
                        "or_eq",
                        "xor",
                        "xor_eq")
        );
    }

    @Override
    public String toVarName(String name) {
        if (typeMapping.keySet().contains(name) || typeMapping.values().contains(name)
                || importMapping.values().contains(name) || defaultIncludes.contains(name)
                || languageSpecificPrimitives.contains(name)) {
            return sanitizeName(name);
        }

        if (isReservedWord(name)) {
            return escapeReservedWord(name);
        }

        if (name.length() > 1) {
            return sanitizeName(Character.toUpperCase(name.charAt(0)) + name.substring(1));
        }

        return sanitizeName(name);
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle
     * escaping those terms here. This logic is only called if a variable
     * matches the reseved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        if(this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return sanitizeName("_" + name);
    }

    @Override
    public String toParamName(String name) {
        return sanitizeName(super.toParamName(name));
    }

    @Override
    public CodegenProperty fromProperty(String name, Property p) {
        CodegenProperty property = super.fromProperty(name, p);
        String nameInCamelCase = property.nameInCamelCase;
        if (nameInCamelCase.length() > 1) {
            nameInCamelCase = sanitizeName(Character.toLowerCase(nameInCamelCase.charAt(0)) + nameInCamelCase.substring(1));
        } else {
            nameInCamelCase = sanitizeName(nameInCamelCase);
        }
        property.nameInCamelCase = nameInCamelCase;
        return property;
    }
}
