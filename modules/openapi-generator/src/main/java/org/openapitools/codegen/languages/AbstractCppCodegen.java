/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

import io.swagger.v3.oas.models.media.Schema;

import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.DefaultCodegen;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;

abstract public class AbstractCppCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(AbstractCppCodegen.class);

    public AbstractCppCodegen() {
        super();

        /*
         * Reserved words.  Override this with reserved words specific to your language
         */
        setReservedWordsLowerCase(
                Arrays.asList(
                        "alignas",
                        "alignof",
                        "and",
                        "and_eq",
                        "asm",
                        "auto",
                        "bitand",
                        "bitor",
                        "bool",
                        "break",
                        "case",
                        "catch",
                        "char",
                        "char16_t",
                        "char32_t",
                        "class",
                        "compl",
                        "concept",
                        "const",
                        "constexpr",
                        "const_cast",
                        "continue",
                        "decltype",
                        "default",
                        "delete",
                        "do",
                        "double",
                        "dynamic_cast",
                        "else",
                        "enum",
                        "explicit",
                        "export",
                        "extern",
                        "false",
                        "float",
                        "for",
                        "friend",
                        "goto",
                        "if",
                        "inline",
                        "int",
                        "linux",
                        "long",
                        "mutable",
                        "namespace",
                        "new",
                        "noexcept",
                        "not",
                        "not_eq",
                        "nullptr",
                        "operator",
                        "or",
                        "or_eq",
                        "private",
                        "protected",
                        "public",
                        "register",
                        "reinterpret_cast",
                        "requires",
                        "return",
                        "short",
                        "signed",
                        "sizeof",
                        "static",
                        "static_assert",
                        "static_cast",
                        "struct",
                        "switch",
                        "template",
                        "this",
                        "thread_local",
                        "throw",
                        "true",
                        "try",
                        "typedef",
                        "typeid",
                        "typename",
                        "union",
                        "unsigned",
                        "using",
                        "virtual",
                        "void",
                        "volatile",
                        "wchar_t",
                        "while",
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
     * matches the reserved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return sanitizeName("_" + name);
    }

    @Override
    public String toOperationId(String operationId) {
        if (isReservedWord(operationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + escapeReservedWord(operationId));
            return escapeReservedWord(operationId);
        }
        return sanitizeName(super.toOperationId(operationId));
    }

    @Override
    public String toParamName(String name) {
        return sanitizeName(super.toParamName(name));
    }

    @Override
    public CodegenProperty fromProperty(String name, Schema p) {
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

    /**
     * Output the Getter name for boolean property, e.g. isActive
     *
     * @param name the name of the property
     * @return getter name based on naming convention
     */
    public String toBooleanGetter(String name) {
        return "is" + getterAndSetterCapitalize(name);
    }

    @Override
    public String getTypeDeclaration(String str) {
        return "std::shared_ptr<" + toModelName(str) + ">";
    }
}
