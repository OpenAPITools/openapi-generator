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

import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class ErlangServerCodegen extends DefaultCodegen implements CodegenConfig {

    private static final Logger LOGGER = LoggerFactory.getLogger(ErlangServerCodegen.class);

    protected String apiVersion = "1.0.0";
    protected String apiPath = "src";
    protected String packageName = "openapi";
    protected String openApiSpecName = "openapi";

    public ErlangServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
                ))
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

        // set the output folder here
        outputFolder = "generated-code/erlang-server";

        /**
         * Models.  You can write model files using the modelTemplateFiles map.
         * if you want to create one template for file, you can do so here.
         * for multiple files for model, just put another entry in the `modelTemplateFiles` with
         * a different extension
         */
        modelTemplateFiles.clear();

        /**
         * Api classes.  You can write classes for each Api file with the apiTemplateFiles map.
         * as with models, add multiple entries with different extensions for multiple files per
         * class
         */
        apiTemplateFiles.put(
                "handler.mustache",   // the template to use
                ".erl");       // the extension for each file to write

        /**
         * Template Location.  This is the location which templates will be read from.  The generator
         * will use the resource stream to attempt to read the templates.
         */
        embeddedTemplateDir = templateDir = "erlang-server";

        /**
         * Reserved words.  Override this with reserved words specific to your language
         */
        setReservedWordsLowerCase(
                Arrays.asList(
                        "after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl", "bsr", "bxor", "case",
                        "catch", "cond", "div", "end", "fun", "if", "let", "not", "of", "or", "orelse", "receive",
                        "rem", "try", "when", "xor"
                )
        );

        instantiationTypes.clear();

        typeMapping.clear();
        typeMapping.put("enum", "binary");
        typeMapping.put("date", "date");
        typeMapping.put("datetime", "datetime");
        typeMapping.put("boolean", "boolean");
        typeMapping.put("string", "binary");
        typeMapping.put("integer", "integer");
        typeMapping.put("int", "integer");
        typeMapping.put("float", "integer");
        typeMapping.put("long", "integer");
        typeMapping.put("double", "float");
        typeMapping.put("array", "list");
        typeMapping.put("map", "map");
        typeMapping.put("number", "integer");
        typeMapping.put("bigdecimal", "float");
        typeMapping.put("List", "list");
        typeMapping.put("object", "object");
        typeMapping.put("file", "file");
        typeMapping.put("binary", "binary");
        typeMapping.put("bytearray", "binary");
        typeMapping.put("byte", "binary");
        typeMapping.put("uuid", "binary");
        typeMapping.put("uri", "binary");
        typeMapping.put("password", "binary");

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "Erlang package name (convention: lowercase).")
                .defaultValue(this.packageName));

        cliOptions.add(new CliOption(CodegenConstants.OPEN_API_SPEC_NAME, "Openapi Spec Name.")
                .defaultValue(this.openApiSpecName));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        if (additionalProperties.containsKey(CodegenConstants.OPEN_API_SPEC_NAME)) {
            setOpenApiSpecName((String) additionalProperties.get(CodegenConstants.OPEN_API_SPEC_NAME));
        } else {
            additionalProperties.put(CodegenConstants.OPEN_API_SPEC_NAME, openApiSpecName);
        }

        /**
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("apiVersion", apiVersion);
        additionalProperties.put("apiPath", apiPath);
        /**
         * Supporting Files.  You can write single files for the generator with the
         * entire object tree available.  If the input file has a suffix of `.mustache
         * it will be processed by the template engine.  Otherwise, it will be copied
         */
        supportingFiles.add(new SupportingFile("rebar.config.mustache", "", "rebar.config"));
        supportingFiles.add(new SupportingFile("app.src.mustache", "", "src" + File.separator + this.packageName + ".app.src"));
        supportingFiles.add(new SupportingFile("router.mustache", "", toSourceFilePath("router", "erl")));
        supportingFiles.add(new SupportingFile("api.mustache", "", toSourceFilePath("api", "erl")));
        supportingFiles.add(new SupportingFile("server.mustache", "", toSourceFilePath("server", "erl")));
        supportingFiles.add(new SupportingFile("utils.mustache", "", toSourceFilePath("utils", "erl")));
        supportingFiles.add(new SupportingFile("auth.mustache", "", toSourceFilePath("auth", "erl")));
        supportingFiles.add(new SupportingFile("openapi.mustache", "", toPrivFilePath(this.openApiSpecName, "json")));
        supportingFiles.add(new SupportingFile("default_logic_handler.mustache", "", toSourceFilePath("default_logic_handler", "erl")));
        supportingFiles.add(new SupportingFile("logic_handler.mustache", "", toSourceFilePath("logic_handler", "erl")));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md")
            .doNotOverwrite());
    }

    @Override
    public String apiPackage() {
        return apiPath;
    }

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see org.openapitools.codegen.CodegenType
     */
    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "erlang-server";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates an Erlang server library (beta) using OpenAPI Generator (https://openapi-generator.tech). By default, " +
                "it will also generate service classes, which can be disabled with the `-Dnoservice` environment variable.";
    }

    @Override
    public String toApiName(String name) {
        if (name.length() == 0) {
            return this.packageName + "_default_handler";
        }
        return this.packageName + "_" + underscore(name) + "_handler";
    }

    /**
     * Escapes a reserved word as defined in the `reservedWords` array. Handle escaping
     * those terms here.  This logic is only called if a variable matches the reserved words
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "_" + name;
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String toModelName(String name) {
        return camelize(toModelFilename(name));
    }

    @Override
    public String toOperationId(String operationId) {
        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            LOGGER.warn(operationId + " (reserved word) cannot be used as method name. Renamed to " + camelize(sanitizeName("call_" + operationId)));
            operationId = "call_" + operationId;
        }

        return camelize(operationId);
    }

    @Override
    public String toApiFilename(String name) {
        return toHandlerName(name);
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            if (op.path != null) {
                op.path = op.path.replaceAll("\\{(.*?)\\}", ":$1");
            }
        }
        return objs;
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateJSONSpecFile(objs);
        return super.postProcessSupportingFileData(objs);
    }

    public void setPackageName(String packageName) {
        this.packageName = packageName;
    }

    public void setOpenApiSpecName(String openApiSpecName) {
        this.openApiSpecName = openApiSpecName;
    }

    protected String toHandlerName(String name) {
        return toModuleName(name) + "_handler";
    }

    protected String toModuleName(String name) {
        return this.packageName + "_" + underscore(name.replaceAll("-", "_"));
    }

    protected String toSourceFilePath(String name, String extension) {
        return "src" + File.separator + toModuleName(name) + "." + extension;
    }

    protected String toIncludeFilePath(String name, String extension) {
        return "include" + File.separator + toModuleName(name) + "." + extension;
    }

    protected String toPrivFilePath(String name, String extension) {
        return "priv" + File.separator + name + "." + extension;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove ' to avoid code injection
        return input.replace("'", "");
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // ref: http://stackoverflow.com/a/30421295/677735
        return input.replace("-ifdef", "- if def").replace("-endif", "- end if");
    }

}
