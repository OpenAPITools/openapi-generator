/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;

import java.io.File;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class CppTizenClientCodegen extends AbstractCppCodegen implements CodegenConfig {
    protected static String PREFIX = "ArtikCloud";
    protected String sourceFolder = "src";
    protected String documentationFolder = "doc";

    public CppTizenClientCodegen() {
        super();

        // TODO: cpp-tizen maintainer review
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BearerToken
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling,
                        GlobalFeature.MultiServer
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        outputFolder = "";
        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-body.mustache", ".cpp");
        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-body.mustache", ".cpp");
        embeddedTemplateDir = templateDir = "cpp-tizen-client";
        modelPackage = "";

        defaultIncludes = new HashSet<>(
                Arrays.asList(
                        "bool",
                        "int",
                        "long long",
                        "double",
                        "float")
        );
        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "bool",
                        "int",
                        "long long",
                        "double",
                        "float",
                        "std::string")
        );

        additionalProperties().put("prefix", PREFIX);

        setReservedWordsLowerCase(
                Arrays.asList(
                        "alignas", "alignof", "and", "and_eq", "asm", "atomic_cancel", "atomic_commit", "atomic_noexcept",
                        "auto", "bitand", "bitor", "bool", "break", "case", "catch", "char", "char16_t", "char32_t",
                        "class", "compl", "concept", "const", "constexpr", "const_cast", "continue", "decltype", "default",
                        "delete", "do", "double", "dynamic_cast", "else", "enum", "explicit", "export", "extern", "false",
                        "float", "for", "friend", "goto", "if", "inline", "int", "import", "long", "module", "mutable",
                        "namespace", "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq", "private",
                        "protected", "public", "register", "reinterpret_cast", "requires", "return", "short", "signed",
                        "sizeof", "static", "static_assert", "static_cast", "struct", "switch", "synchronized", "template",
                        "this", "thread_local", "throw", "true", "try", "typedef", "typeid", "typename", "union",
                        "unsigned", "using", "virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq"
                ));

        super.typeMapping = new HashMap<>();

        //typeMapping.put("Date", "DateTime");
        //typeMapping.put("DateTime", "DateTime");
        typeMapping.put("string", "std::string");
        typeMapping.put("integer", "int");
        typeMapping.put("float", "float");
        typeMapping.put("long", "long long");
        typeMapping.put("boolean", "bool");
        typeMapping.put("double", "double");
        typeMapping.put("array", "std::list");
        typeMapping.put("map", "std::map");
        typeMapping.put("number", "long long");
        typeMapping.put("object", "std::string");
        typeMapping.put("binary", "std::string");
        typeMapping.put("password", "std::string");
        //TODO:Maybe use better formats for dateTime?
        typeMapping.put("file", "std::string");
        typeMapping.put("DateTime", "std::string");
        typeMapping.put("Date", "std::string");
        typeMapping.put("UUID", "std::string");
        typeMapping.put("URI", "std::string");

        importMapping = new HashMap<>();

        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("helpers-header.mustache", sourceFolder, "Helpers.h"));
        supportingFiles.add(new SupportingFile("helpers-body.mustache", sourceFolder, "Helpers.cpp"));
        supportingFiles.add(new SupportingFile("netclient-header.mustache", sourceFolder, "NetClient.h"));
        supportingFiles.add(new SupportingFile("netclient-body.mustache", sourceFolder, "NetClient.cpp"));
        supportingFiles.add(new SupportingFile("object.mustache", sourceFolder, "Object.h"));
        supportingFiles.add(new SupportingFile("requestinfo.mustache", sourceFolder, "RequestInfo.h"));
        supportingFiles.add(new SupportingFile("error-header.mustache", sourceFolder, "Error.h"));
        supportingFiles.add(new SupportingFile("error-body.mustache", sourceFolder, "Error.cpp"));
        supportingFiles.add(new SupportingFile("Doxyfile.mustache", documentationFolder, "Doxyfile"));
        supportingFiles.add(new SupportingFile("generateDocumentation.mustache", documentationFolder, "generateDocumentation.sh"));
        supportingFiles.add(new SupportingFile("doc-readme.mustache", documentationFolder, "README.md"));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "cpp-tizen";
    }

    @Override
    public String getHelp() {
        return "Generates a Samsung Tizen C++ client library.";
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isMapSchema(p)) {
            return instantiationTypes.get("map");
        } else if (ModelUtils.isArraySchema(p)) {
            return instantiationTypes.get("array");
        } else {
            return null;
        }
    }

    @Override
    public String getTypeDeclaration(String name) {
        if (languageSpecificPrimitives.contains(name)) {
            return name;
        } else {
            return name;
        }
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return toModelName(type);
            }
        } else {
            type = openAPIType;
        }
        return toModelName(type);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        String openAPIType = getSchemaType(p);
        if (languageSpecificPrimitives.contains(openAPIType)) {
            return toModelName(openAPIType);
        } else {
            return openAPIType;
        }
    }

    @Override
    public String toModelName(String type) {
        if (typeMapping.keySet().contains(type) ||
                typeMapping.values().contains(type) ||
                importMapping.values().contains(type) ||
                defaultIncludes.contains(type) ||
                languageSpecificPrimitives.contains(type)) {
            return type;
        } else {
            return Character.toUpperCase(type.charAt(0)) + type.substring(1);
        }
    }

    @Override
    public String toModelImport(String name) {
        if (name.equals("std::string")) {
            return "#include <string>";
        } else if (name.equals("std::map")) {
            return "#include <map>";
        } else if (name.equals("std::list")) {
            return "#include <list>";
        }
        return "#include \"" + name + ".h\"";
    }

    //Might not be needed
    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            return "bool(false)";
        } else if (ModelUtils.isNumberSchema(p)) {
            if (SchemaTypeUtil.FLOAT_FORMAT.equals(p.getFormat())) {
                return "float(0)";
            }
            return "double(0)";

        } else if (ModelUtils.isIntegerSchema(p)) {
            if (SchemaTypeUtil.INTEGER64_FORMAT.equals(p.getFormat())) {
                return "long(0)";
            }
            return "int(0)";
        } else if (ModelUtils.isMapSchema(p)) {
            return "new std::map()";
        } else if (ModelUtils.isArraySchema(p)) {
            return "new std::list()";
        } else if (!StringUtils.isEmpty(p.get$ref())) {
            return "new " + toModelName(ModelUtils.getSimpleRef(p.get$ref())) + "()";
        } else if (ModelUtils.isDateSchema(p) || ModelUtils.isDateTimeSchema(p)) {
            return "null";
        } else if (ModelUtils.isStringSchema(p)) {
            return "std::string()";
        }
        return "null";
    }


    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder;
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder;
    }

    @Override
    public String toModelFilename(String name) {
        return camelize(name);
    }

    @Override
    public String toApiName(String name) {
        return camelize(name) + "Manager";
    }

    @Override
    public String toApiFilename(String name) {
        return camelize(name) + "Manager";
    }

    @Override
    public String toVarName(String name) {
        String paramName = name.replaceAll("[^a-zA-Z0-9_]", "");
        if (name.length() > 0 ) {
            // additionalProperties name is "" so name.length() == 0
            paramName = Character.toLowerCase(paramName.charAt(0)) + paramName.substring(1);
        }
        if (isReservedWord(paramName)) {
            return escapeReservedWord(paramName);
        }
        return paramName;
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return$
        if (isReservedWord(operationId)) {
            operationId = escapeReservedWord(operationId);
        }

        // add_pet_by_id => addPetById
        return camelize(operationId, true);
    }
    /**
     * Output the Getter name for boolean property, e.g. getActive
     *
     * @param name the name of the property
     * @return getter name based on naming convention
     */
    @Override
    public String toBooleanGetter(String name) {
        return "get" + getterAndSetterCapitalize(name);
    }
}
