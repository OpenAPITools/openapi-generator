package io.swagger.codegen.languages;

import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenProperty;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.models.properties.Property;

import java.util.Arrays;

abstract public class AbstractAdaCodegen extends DefaultCodegen implements CodegenConfig {

    public AbstractAdaCodegen() {
        super();

        /*
         * Reserved words.  Override this with reserved words specific to your language
         */
        setReservedWordsLowerCase(
                Arrays.asList(
                        "abort",
                        "abs",
                        "abstract",
                        "accept",
                        "access",
                        "aliased",
                        "all",
                        "and",
                        "array",
                        "at",
                        "begin",
                        "body",
                        "case",
                        "constant",
                        "declare",
                        "delay",
                        "digits",
                        "do",
                        "else",
                        "elsif",
                        "end",
                        "entry",
                        "exception",
                        "exit",
                        "for",
                        "function",
                        "generic",
                        "goto",
                        "if",
                        "in",
                        "interface",
                        "is",
                        "limited",
                        "loop",
                        "mod",
                        "new",
                        "not",
                        "null",
                        "of",
                        "or",
                        "others",
                        "out",
                        "overriding",
                        "package",
                        "pragma",
                        "private",
                        "procedure",
                        "protected",
                        "raise",
                        "range",
                        "record",
                        "rem",
                        "renames",
                        "requeue",
                        "return",
                        "reverse",
                        "select",
                        "separate",
                        "some",
                        "subtype",
                        "synchronized",
                        "tagged",
                        "task",
                        "terminate",
                        "then",
                        "type",
                        "until",
                        "use",
                        "when",
                        "while",
                        "with",
                        "xor")
        );
    }

    /**
     * Turn a parameter name, operation name into an Ada identifier.
     *
     * Ada programming standard avoid the camelcase syntax and prefer the underscore
     * notation.  We also have to make sure the identifier is not a reserved keyword.
     * When this happens, we add the configurable prefix.  The function translates:
     *
     * body              - P_Body
     * petId             - Pet_Id
     * updatePetWithForm - Update_Pet_With_Form
     *
     * @param name the parameter name.
     * @param prefix the optional prefix in case the parameter name is a reserved keyword.
     * @return the Ada identifier to be used.
     */
    protected String toAdaIdentifier(String name, String prefix) {
        // We cannot use reserved keywords for identifiers
        if (isReservedWord(name)) {
            LOGGER.warn("Identifier '" + name + "' is a reserved word, renamed to " + prefix + name);
            name = prefix + name;
        }
        StringBuilder result = new StringBuilder();
        boolean needUpperCase = true;
        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            if (needUpperCase) {
                needUpperCase = false;
                result.append(Character.toUpperCase(c));

            } else if (Character.isUpperCase((c))) {
                if (!needUpperCase) {
                    result.append('_');
                }
                result.append(c);
                needUpperCase = false;
            } else {
                result.append(c);
                if (c == '_') {
                    needUpperCase = true;
                }
            }
        }
        return result.toString();
    }

    @Override
    public String toOperationId(String operationId) {
        return toAdaIdentifier(sanitizeName(operationId), "Call_");
    }

    @Override
    public String toVarName(String name) {
        return toAdaIdentifier(sanitizeName(name), "P_");
    }

    @Override
    public String toParamName(String name) {
        return toAdaIdentifier(super.toParamName(name), "P_");
    }

    @Override
    public CodegenProperty fromProperty(String name, Property p) {
        CodegenProperty property = super.fromProperty(name, p);
        String nameInCamelCase = property.nameInCamelCase;
        nameInCamelCase = sanitizeName(nameInCamelCase);
        property.nameInCamelCase = nameInCamelCase;
        return property;
    }
}
