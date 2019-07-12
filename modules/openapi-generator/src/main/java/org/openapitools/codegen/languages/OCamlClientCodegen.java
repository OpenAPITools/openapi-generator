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

import com.google.common.base.Strings;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class OCamlClientCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(OCamlClientCodegen.class);
    public static final String PACKAGE_NAME = "packageName";
    public static final String PACKAGE_VERSION = "packageVersion";

    public static final String CO_HTTP = "cohttp";

    protected String packageName = "openapi";
    protected String packageVersion = "1.0.0";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";
    protected String apiFolder = "src/apis";
    protected String modelFolder = "src/models";

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "ocaml-client";
    }

    public String getHelp() {
        return "Generates an OCaml client library (beta).";
    }

    public OCamlClientCodegen() {
        super();
        outputFolder = "generated-code/ocaml-client";
        modelTemplateFiles.put("model.mustache", ".ml");

        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        embeddedTemplateDir = templateDir = "ocaml-client";

        setReservedWordsLowerCase(
                Arrays.asList(
                        "and", "as", "assert", "asr", "begin", "class",
                        "constraint", "do", "done", "downto", "else", "end",
                        "exception", "external", "false", "for ", "fun", "function",
                        "functor", "if", "in", "include", "inherit", "initializer",
                        "land", "lazy", "let", "lor", "lsl", "lsr",
                        "lxor", "match", "method", "mod", "module", "mutable",
                        "new", "nonrec", "object", "of", "open", "or",
                        "private", "rec", "sig", "struct", "then", "to",
                        "true", "try", "type", "val", "virtual", "when",
                        "while", "with"
                )
        );

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("dune.mustache", "", "stack.yaml"));

        defaultIncludes = new HashSet<String>(
                Arrays.asList(
                        "map",
                        "array")
        );

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "bool",
                        "string",
                        "int",
                        "float",
                        "char",
                        "double",
                        "list"
                )
        );

        instantiationTypes.clear();
        /*instantiationTypes.put("array", "GoArray");
        instantiationTypes.put("map", "GoMap");*/

        typeMapping.clear();
        typeMapping.put("boolean", "bool");
        typeMapping.put("int", "int");
        typeMapping.put("long", "Int64.t");
        typeMapping.put("short", "int");
        typeMapping.put("char", "char");
        typeMapping.put("float", "float");
        typeMapping.put("double", "double");
        typeMapping.put("integer", "int");
        typeMapping.put("file", "FilePath");
        // lib
        typeMapping.put("string", "string");
        typeMapping.put("UUID", "string");
        typeMapping.put("URI", "string");
        typeMapping.put("any", "`Any");
        typeMapping.put("set", "`Set");
        typeMapping.put("passsword", "string");
        typeMapping.put("DateTime", "string");

//        supportedLibraries.put(CO_HTTP, "HTTP client: CoHttp.");
//
//        CliOption libraryOption = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use.");
//        libraryOption.setEnum(supportedLibraries);
//        // set hyper as the default
//        libraryOption.setDefault(CO_HTTP);
//        cliOptions.add(libraryOption);
//        setLibrary(CO_HTTP);
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        // process enum in models
        return postProcessModelsEnum(objs);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        //apiTemplateFiles.put("/api.mustache", ".ml");

        modelPackage = packageName;
        apiPackage = packageName;

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("dune.mustache", "", "dune"));
        supportingFiles.add(new SupportingFile("types.mustache", "", "types.ml"));
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return '_' + name;
    }

    @Override
    public String apiFileFolder() {
        return (outputFolder + File.separator + apiFolder).replace("/", File.separator);
    }

    public String modelFileFolder() {
        return (outputFolder + File.separator + modelFolder).replace("/", File.separator);
    }

    @Override
    public String toVarName(String name) {
        // replace - with _ e.g. created-at => created_at
        name = sanitizeName(name.replaceAll("-", "_"));

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$"))
            return name;

        // snake_case, e.g. PetId => pet_id
        name = underscore(name);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name))
            name = escapeReservedWord(name);

        // for reserved word or word starting with number, append _
        if (name.matches("^\\d.*"))
            name = "var_" + name;

        return name;
    }

    @Override
    public String toParamName(String name) {
        return toVarName(name);
    }

    @Override
    public String toModelName(String name) {
        // camelize the model name
        // phone_number => PhoneNumber
        return toModelFilename(name).toLowerCase();
    }

    @Override
    public String toModelFilename(String name) {

        if (!Strings.isNullOrEmpty(modelNamePrefix)) {
            name = modelNamePrefix + "_" + name;
        }

        if (!Strings.isNullOrEmpty(modelNameSuffix)) {
            name = name + "_" + modelNameSuffix;
        }

        name = sanitizeName(name);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(name)) {
            LOGGER.warn(name + " (reserved word) cannot be used as model name. Renamed to " + ("model_" + name));
            name = "model_" + name; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + ("model_" + name));
            name = "model_" + name; // e.g. 200Response => Model200Response (after camelize)
        }

        return underscore(name);
    }

    @Override
    public String toApiFilename(String name) {
        // replace - with _ e.g. created-at => created_at
        name = name.replaceAll("-", "_"); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        // e.g. PetApi.rs => pet_api.rs
        return underscore(name) + "_api";
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String toModelDocFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiDocFilename(String name) {
        return toApiName(name);
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            if (inner == null) {
                LOGGER.warn(ap.getName() + "(array property) does not have a proper inner type defined.Default to string");
                inner = new StringSchema().description("TODO default missing array inner type to string");
            }
            return getTypeDeclaration(inner) + " list";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            if (inner == null) {
                LOGGER.warn(p.getName() + "(map property) does not have a proper inner type defined. Default to string");
                inner = new StringSchema().description("TODO default missing map inner type to string");
            }
            return "(string, " + getTypeDeclaration(inner) + ") Hashtbl.t";
        }

        // Not using the supertype invocation, because we want to UpperCamelize
        // the type.
        String schemaType = getSchemaType(p);
        if (typeMapping.containsKey(schemaType)) {
            return typeMapping.get(schemaType);
        }

        if (typeMapping.containsValue(schemaType)) {
            return schemaType;
        }

        if (languageSpecificPrimitives.contains(schemaType)) {
            return schemaType;
        }

        return toModelName(schemaType);
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type))
                return (type);
        } else
            type = schemaType;
        return type;
    }

    @Override
    public String toOperationId(String operationId) {
        String sanitizedOperationId = sanitizeName(operationId);

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(sanitizedOperationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + StringUtils.underscore("call_" + operationId));
            sanitizedOperationId = "call_" + sanitizedOperationId;
        }

        return StringUtils.underscore(sanitizedOperationId);
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        @SuppressWarnings("unchecked")
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
        @SuppressWarnings("unchecked")
        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");
        for (CodegenOperation operation : operations) {
            // http method verb conversion, depending on client library (e.g. Hyper: PUT => Put, Reqwest: PUT => put)
            if (CO_HTTP.equals(getLibrary())) {
                operation.httpMethod = StringUtils.camelize(operation.httpMethod.toLowerCase(Locale.ROOT));
            }
            // update return type to conform to rust standard
            /*
            if (operation.returnType != null) {
                if ( operation.returnType.startsWith("Vec") && !languageSpecificPrimitives.contains(operation.returnBaseType)) {
                    // array of model
                    String rt = operation.returnType;
                    int end = rt.lastIndexOf(">");
                    if ( end > 0 ) {
                        operation.vendorExtensions.put("x-returnTypeInMethod", "Vec<super::" + rt.substring("Vec<".length(), end).trim() + ">");
                        operation.returnContainer = "List";
                    }
                } else if (operation.returnType.startsWith("::std::collections::HashMap<String, ") && !languageSpecificPrimitives.contains(operation.returnBaseType)) {
                    LOGGER.info("return base type:" + operation.returnBaseType);
                    // map of model
                    String rt = operation.returnType;
                    int end = rt.lastIndexOf(">");
                    if ( end > 0 ) {
                        operation.vendorExtensions.put("x-returnTypeInMethod", "::std::collections::HashMap<String, super::" + rt.substring("::std::collections::HashMap<String, ".length(), end).trim() + ">");
                        operation.returnContainer = "Map";
                    }
                } else if (!languageSpecificPrimitives.contains(operation.returnType)) {
                    // add super:: to model, e.g. super::pet
                    operation.vendorExtensions.put("x-returnTypeInMethod", "super::" + operation.returnType);
                } else {
                    // primitive type or array/map of primitive type
                    operation.vendorExtensions.put("x-returnTypeInMethod", operation.returnType);
                }
            }

            for (CodegenParameter p : operation.allParams) {
                if (p.isListContainer && !languageSpecificPrimitives.contains(p.dataType)) {
                    // array of model
                    String rt = p.dataType;
                    int end = rt.lastIndexOf(">");
                    if ( end > 0 ) {
                        p.dataType = "Vec<" + rt.substring("Vec<".length(), end).trim() + ">";
                    }
                } else if (p.isMapContainer && !languageSpecificPrimitives.contains(p.dataType)) {
                    // map of model
                    String rt = p.dataType;
                    int end = rt.lastIndexOf(">");
                    if ( end > 0 ) {
                        p.dataType = "::std::collections::HashMap<String, super::" + rt.substring("::std::collections::HashMap<String, ".length(), end).trim() + ">";
                    }
                } else if (!languageSpecificPrimitives.contains(p.dataType)) {
                    // add super:: to model, e.g. super::pet
                    p.dataType = "super::" + p.dataType;
                }
            }*/
        }

        return objs;
    }

    @Override
    protected boolean needToImport(String type) {
        return !defaultIncludes.contains(type)
                && !languageSpecificPrimitives.contains(type);
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }


    @Override
    public String toEnumValue(String value, String datatype) {
        if ("int".equals(datatype) || "double".equals(datatype) || "float".equals(datatype)) {
            return value;
        } else {
            return escapeText(value);
        }
    }

    @Override
    public String toEnumDefaultValue(String value, String datatype) {
        return datatype + "_" + value;
    }

    @Override
    public String toEnumVarName(String name, String datatype) {
        if (name.length() == 0) {
            return "Empty";
        }

        // number
        if ("int".equals(datatype) || "double".equals(datatype) || "float".equals(datatype)) {
            String varName = name;
            varName = varName.replaceAll("-", "Minus");
            varName = varName.replaceAll("\\+", "Plus");
            varName = varName.replaceAll("\\.", "Dot");
            return varName;
        }

        // for symbol, e.g. $, #
        if (getSymbolName(name) != null) {
            return getSymbolName(name);
        }

        // string
        String enumName = sanitizeName(camelize(name));
        enumName = enumName.replaceFirst("^_", "");
        enumName = enumName.replaceFirst("_$", "");

        if (isReservedWord(enumName) || enumName.matches("\\d.*")) { // reserved word or starts with number
            return escapeReservedWord(enumName);
        } else {
            return enumName;
        }
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        // camelize the enum name
        // phone_number => PhoneNumber
        String enumName = camelize(toModelName(property.name));

        // remove [] for array or map of enum
        enumName = enumName.replace("[]", "");

        if (enumName.matches("\\d.*")) { // starts with number
            return "_" + enumName;
        } else {
            return enumName;
        }
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (p.getDefault() != null) {
            return p.getDefault().toString();
        } else {
            return null;
        }
    }
}
