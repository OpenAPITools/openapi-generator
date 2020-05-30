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

import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

import static org.openapitools.codegen.utils.OnceLogger.once;

public class ScalaFinchServerCodegen extends DefaultCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(ScalaFinchServerCodegen.class);
    protected String invokerPackage = "org.openapitools.client";
    protected String groupId = "org.openapitools";
    protected String artifactId = "finch-server";
    protected String artifactVersion = "1.0.0";
    protected String sourceFolder = "src/main/scala";
    protected String packageName = "org.openapitools";

    public ScalaFinchServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        outputFolder = "generated-code/finch";
        modelTemplateFiles.put("model.mustache", ".scala");
        apiTemplateFiles.put("api.mustache", ".scala");
        embeddedTemplateDir = templateDir = "scala-finch";

        apiPackage = packageName + ".apis";
        modelPackage = packageName + ".models";

        setReservedWordsLowerCase(
                Arrays.asList(
                        // Scala
                        "abstract", "case", "catch", "class", "def",
                        "do", "else", "extends", "false", "final",
                        "finally", "for", "forSome", "if", "implicit",
                        "import", "lazy", "match", "new", "null",
                        "object", "override", "package", "private", "protected",
                        "return", "sealed", "super", "this", "throw",
                        "trait", "try", "true", "type", "val",
                        "var", "while", "with", "yield",
                        // Scala-interop languages keywords
                        "abstract", "continue", "switch", "assert",
                        "default", "synchronized", "goto",
                        "break", "double", "implements", "byte",
                        "public", "throws", "enum", "instanceof", "transient",
                        "int", "short", "char", "interface", "static",
                        "void", "finally", "long", "strictfp", "volatile", "const", "float",
                        "native")
        );

        defaultIncludes = new HashSet<String>(
                Arrays.asList("double",
                        "Int",
                        "Long",
                        "Float",
                        "Double",
                        "char",
                        "float",
                        "String",
                        "boolean",
                        "Boolean",
                        "Double",
                        "Integer",
                        "Long",
                        "Float",
                        "List",
                        "Set",
                        "Map")
        );

        typeMapping = new HashMap<String, String>();
        typeMapping.put("string", "String");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("integer", "Int");
        typeMapping.put("float", "Float");
        typeMapping.put("long", "Long");
        typeMapping.put("double", "Double");
        typeMapping.put("number", "BigDecimal");
        typeMapping.put("date-time", "ZonedDateTime");
        typeMapping.put("date", "LocalDateTime");
        typeMapping.put("file", "File");
        typeMapping.put("array", "Seq");
        typeMapping.put("list", "List");
        typeMapping.put("map", "Map");
        typeMapping.put("object", "Object");
        typeMapping.put("binary", "Array[Byte]");
        typeMapping.put("Date", "LocalDateTime");
        typeMapping.put("DateTime", "ZonedDateTime");

        additionalProperties.put("modelPackage", modelPackage());
        additionalProperties.put("apiPackage", apiPackage());
        additionalProperties.put("appName", "OpenAPI Sample");
        additionalProperties.put("appDescription", "A sample openapi server");
        additionalProperties.put("infoUrl", "http://org.openapitools");
        additionalProperties.put("infoEmail", "team@openapitools.org");
        additionalProperties.put("licenseInfo", "Apache 2.0");
        additionalProperties.put("licenseUrl", "http://apache.org/licenses/LICENSE-2.0.html");
        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.sbt", "", "build.sbt"));
        supportingFiles.add(new SupportingFile("Server.mustache", sourceFolder, "Server.scala"));
        supportingFiles.add(new SupportingFile("DataAccessor.mustache", sourceFolder, "DataAccessor.scala"));

        supportingFiles.add(new SupportingFile("project/build.properties", "project", "build.properties"));
        supportingFiles.add(new SupportingFile("project/plugins.sbt", "project", "plugins.sbt"));
        supportingFiles.add(new SupportingFile("sbt", "", "sbt"));

        supportingFiles.add(new SupportingFile("endpoint.mustache", sourceFolder, "endpoint.scala"));
        supportingFiles.add(new SupportingFile("errors.mustache", sourceFolder, "errors.scala"));

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList(
                        "String",
                        "Boolean",
                        "Double",
                        "Int",
                        "Integer",
                        "Long",
                        "Float",
                        "Any",
                        "AnyVal",
                        "AnyRef",
                        "Object")
        );
        instantiationTypes.put("array", "ArrayList");
        instantiationTypes.put("map", "HashMap");

        importMapping = new HashMap<String, String>();
        importMapping.put("BigDecimal", "java.math.BigDecimal");
        importMapping.put("UUID", "java.util.UUID");
        importMapping.put("URI", "java.net.URI");
        importMapping.put("File", "java.io.File");
        importMapping.put("Date", "java.util.Date");
        importMapping.put("Timestamp", "java.sql.Timestamp");
        importMapping.put("Map", "scala.collection.immutable.Map");
        importMapping.put("HashMap", "scala.collection.immutable.HashMap");
        importMapping.put("Seq", "scala.collection.immutable.Seq");
        importMapping.put("ArrayBuffer", "scala.collection.mutable.ArrayBuffer");
        importMapping.put("DateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDateTime", "java.time.LocalDateTime");
        importMapping.put("LocalDate", "java.time.LocalDate");
        importMapping.put("LocalTime", "java.time.LocalTime");
        importMapping.put("ZonedDateTime", "java.time.ZonedDateTime");

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Finch package name (e.g. org.openapitools).")
                .defaultValue(this.packageName));
        cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "scala-finch";
    }

    @Override
    public String getHelp() {
        return "Generates a Scala server application with Finch.";
    }

    @Override
    public String escapeReservedWord(String name) {
        return "_" + name;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @SuppressWarnings("unchecked")
    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {

            // Converts GET /foo/bar => get("foo" :: "bar")
            generateScalaPath(op);

            // Generates e.g. uuid :: header("boo") :: params("baa") under key "x-codegen-path-params"
            // Generates e.g. (id: UUID, headerBoo: String, paramBaa: String) under key "x-codegen-typed-input-params"
            // Generates e.g. (id, headerBoo, paramBaa) under key "x-codegen-input-params"
            generateInputParameters(op);

            //Generate Auth parameters using security: definition
            //Results in header("apiKey") or param("apiKey")
            authParameters(op);

            //Concatenates all parameters
            concatParameters(op);
        }

        return objs;
    }


    @SuppressWarnings("Duplicates")
    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = getAdditionalProperties(p);

            return getSchemaType(p) + "[String, " + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
    }

    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type)) {
                return toModelName(type);
            }
        } else {
            type = schemaType;
        }
        return toModelName(type);
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

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }


    /**
     * @param prim          primitive type
     * @param isRequired    true if it's required
     * @param canBeOptional true if it can be optional
     * @return string representation of the primitive type
     */
    private String toPrimitive(String prim, Boolean isRequired, Boolean canBeOptional) {

        String converter = ".map(_.to" + prim + ")";
        return (canBeOptional ? (isRequired ? converter : ".map(_" + converter + ")") : "");
    }

    //All path parameters are String initially, for primitives these need to be converted
    private String toPathParameter(CodegenParameter p, String paramType, Boolean canBeOptional) {

        Boolean isNotAString = !p.dataType.equals("String");

        return paramType + (canBeOptional && !p.required ? "Option" : "") + "(\"" + p.baseName + "\")" + (isNotAString ? toPrimitive(p.dataType, p.required, canBeOptional) : "");
    }

    private String toInputParameter(CodegenParameter p) {
        return (p.required ? "" : "Option[") + p.dataType + (p.required ? "" : "]");
    }

    private String concat(String original, String addition, String op) {
        return original + (original.isEmpty() ? "" : (addition.isEmpty() ? "" : op)) + addition;
    }

    // a, b
    private String csvConcat(String original, String addition) {
        return concat(original, addition, ", ");
    }

    // a :: b
    private String colConcat(String original, String addition) {
        return concat(original, addition, " :: ");
    }

    private void authParameters(CodegenOperation op) {
        String authParams = "";
        String authInputParams = "";
        String typedAuthInputParams = "";

        //Append apikey security to path params and create input parameters for functions
        if (op.authMethods != null) {

            for (CodegenSecurity s : op.authMethods) {
                if (s.isApiKey && s.isKeyInHeader) {
                    authParams = colConcat(authParams, "header(\"" + s.keyParamName + "\")");
                } else if (s.isApiKey && s.isKeyInQuery) {
                    authParams = colConcat(authParams, "param(\"" + s.keyParamName + "\")");
                }
                if (s.isApiKey) {
                    typedAuthInputParams = csvConcat(typedAuthInputParams, "authParam" + s.name + ": String");
                    authInputParams = csvConcat(authInputParams, "authParam" + s.name);
                }
            }
        }

        op.vendorExtensions.put("x-codegen-auth-params", authParams);
        op.vendorExtensions.put("x-codegen-auth-input-params", authInputParams);
        op.vendorExtensions.put("x-codegen-typed-auth-input-params", typedAuthInputParams);
    }

    private void generateScalaPath(CodegenOperation op) {
        op.httpMethod = op.httpMethod.toLowerCase(Locale.ROOT);

        String path = op.path;

        // remove first /
        if (path.startsWith("/")) {
            path = path.substring(1);
        }

        // remove last /
        if (path.endsWith("/")) {
            path = path.substring(0, path.length() - 1);
        }

        String[] items = path.split("/", -1);
        String scalaPath = "";
        Integer pathParamIndex = 0;

        for (String item : items) {

            if (item.matches("^\\{(.*)}$")) { // wrap in {}
                // find the datatype of the parameter
                final CodegenParameter cp = op.pathParams.get(pathParamIndex);

                // TODO: Handle non-primitives…
                scalaPath = colConcat(scalaPath, cp.dataType.toLowerCase(Locale.ROOT));

                pathParamIndex++;
            } else {
                scalaPath = colConcat(scalaPath, "\"" + item + "\"");
            }
        }

        op.vendorExtensions.put("x-codegen-path", scalaPath);

    }


    private void concatParameters(CodegenOperation op) {
        String path = colConcat(colConcat(op.vendorExtensions.get("x-codegen-path").toString(), op.vendorExtensions.get("x-codegen-path-params").toString()), op.vendorExtensions.get("x-codegen-auth-params").toString());
        String parameters = csvConcat(op.vendorExtensions.get("x-codegen-input-params").toString(), op.vendorExtensions.get("x-codegen-auth-input-params").toString());
        String typedParameters = csvConcat(op.vendorExtensions.get("x-codegen-typed-input-params").toString(), op.vendorExtensions.get("x-codegen-typed-auth-input-params").toString());

        // The input parameters for functions
        op.vendorExtensions.put("x-codegen-paths", path);
        op.vendorExtensions.put("x-codegen-params", parameters);
        op.vendorExtensions.put("x-codegen-typed-params", typedParameters);

    }


    private void generateInputParameters(CodegenOperation op) {

        String inputParams = "";
        String typedInputParams = "";
        String pathParams = "";

        for (CodegenParameter p : op.allParams) {
            // TODO: This hacky, should be converted to mappings if possible to keep it clean.
            // This could also be done using template imports

            if (p.isBodyParam) {
                p.vendorExtensions.put("x-codegen-normalized-path-type", "jsonBody[" + p.dataType + "]");
                p.vendorExtensions.put("x-codegen-normalized-input-type", p.dataType);
            } else if (p.isContainer || p.isListContainer) {
                p.vendorExtensions.put("x-codegen-normalized-path-type", toPathParameter(p, "params", false));
                p.vendorExtensions.put("x-codegen-normalized-input-type", p.dataType.replaceAll("^[^\\[]+", "Seq"));
            } else if (p.isQueryParam) {
                p.vendorExtensions.put("x-codegen-normalized-path-type", toPathParameter(p, "param", true));
                p.vendorExtensions.put("x-codegen-normalized-input-type", toInputParameter(p));
            } else if (p.isHeaderParam) {
                p.vendorExtensions.put("x-codegen-normalized-path-type", toPathParameter(p, "header", true));
                p.vendorExtensions.put("x-codegen-normalized-input-type", toInputParameter(p));
            } else if (p.isFile) {
                p.vendorExtensions.put("x-codegen-normalized-path-type", "fileUpload(\"" + p.paramName + "\")");
                p.vendorExtensions.put("x-codegen-normalized-input-type", "FileUpload");
            } else if (p.isPrimitiveType && !p.isPathParam) {
                if (!p.required) {
                    // Generator's current version of Finch doesn't support something like stringOption, but finch aggregates all
                    // parameter types under "param", so optional params can be grabbed by "paramOption".
                    p.vendorExtensions.put("x-codegen-normalized-path-type", toPathParameter(p, "param", true));
                } else {
                    // If parameter is primitive and required, we can rely on data types like "string" or "long"
                    p.vendorExtensions.put("x-codegen-normalized-path-type", p.dataType.toLowerCase(Locale.ROOT));
                }
                p.vendorExtensions.put("x-codegen-normalized-input-type", toInputParameter(p));
            } else {
                //Path paremeters are handled in generateScalaPath()
                p.vendorExtensions.put("x-codegen-normalized-input-type", p.dataType);
            }
            if (p.vendorExtensions.get("x-codegen-normalized-path-type") != null) {
                pathParams = colConcat(pathParams, p.vendorExtensions.get("x-codegen-normalized-path-type").toString());
            }
            inputParams = csvConcat(inputParams, p.paramName);
            typedInputParams = csvConcat(typedInputParams, p.paramName + ": " + p.vendorExtensions.get("x-codegen-normalized-input-type"));

        }

        // All body, path, query and header parameters
        op.vendorExtensions.put("x-codegen-path-params", pathParams);

        // The input parameters for functions
        op.vendorExtensions.put("x-codegen-input-params", inputParams);
        op.vendorExtensions.put("x-codegen-typed-input-params", typedInputParams);

    }

}
