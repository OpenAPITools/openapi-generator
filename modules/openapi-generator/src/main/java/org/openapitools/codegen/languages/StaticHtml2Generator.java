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

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.Markdown;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.*;

public class StaticHtml2Generator extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(StaticHtml2Generator.class);

    protected String invokerPackage = "org.openapitools.client"; // default for Java and Android
    protected String phpInvokerPackage = "OpenAPITools\\Client"; // default for PHP
    protected String packageName = "Org.OpenAPITools"; // default for C#
    protected String groupId = "org.openapitools";
    protected String artifactId = "openapi-client";
    protected String artifactVersion = "1.0.0";
    protected String jsProjectName;
    protected String jsModuleName;
    protected String perlModuleName = "WWW::OPenAPIClient";
    protected String pythonPackageName = "openapi_client";

    public StaticHtml2Generator() {
        super();

        modifyFeatureSet(features -> features
                .documentationFeatures(EnumSet.allOf(DocumentationFeature.class))
                .dataTypeFeatures(EnumSet.allOf(DataTypeFeature.class))
                .wireFormatFeatures(EnumSet.allOf(WireFormatFeature.class))
                .securityFeatures(EnumSet.allOf(SecurityFeature.class))
                .globalFeatures(EnumSet.allOf(GlobalFeature.class))
                .parameterFeatures(EnumSet.allOf(ParameterFeature.class))
                .schemaSupportFeatures(EnumSet.allOf(SchemaSupportFeature.class))
        );

        outputFolder = "docs";
        embeddedTemplateDir = templateDir = "htmlDocs2";

        defaultIncludes = new HashSet<>();

        cliOptions.add(new CliOption("appName", "short name of the application"));
        cliOptions.add(new CliOption("appDescription", "description of the application"));
        cliOptions.add(new CliOption("infoUrl", "a URL where users can get more information about the application"));
        cliOptions.add(new CliOption("infoEmail", "an email address to contact for inquiries about the application"));
        cliOptions.add(new CliOption("licenseInfo", "a short description of the license"));
        cliOptions.add(new CliOption("licenseUrl", "a URL pointing to the full license"));
        cliOptions.add(new CliOption(CodegenConstants.INVOKER_PACKAGE, CodegenConstants.INVOKER_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.PHP_INVOKER_PACKAGE, CodegenConstants.PHP_INVOKER_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.PERL_MODULE_NAME, CodegenConstants.PERL_MODULE_NAME_DESC));
        cliOptions.add(new CliOption(CodegenConstants.PYTHON_PACKAGE_NAME, CodegenConstants.PYTHON_PACKAGE_NAME_DESC));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, "C# package name"));
        cliOptions.add(new CliOption(CodegenConstants.GROUP_ID, CodegenConstants.GROUP_ID_DESC));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_ID, CodegenConstants.ARTIFACT_ID_DESC));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_VERSION, CodegenConstants.ARTIFACT_VERSION_DESC));

        additionalProperties.put("appName", "OpenAPI Sample");
        additionalProperties.put("appDescription", "A sample OpenAPI server");
        additionalProperties.put("infoUrl", "https://openapi-generator.tech");
        additionalProperties.put("infoEmail", "team@openapitools.org");
        additionalProperties.put("licenseInfo", "All rights reserved");
        additionalProperties.put("licenseUrl", "http://apache.org/licenses/LICENSE-2.0.html");
        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.PHP_INVOKER_PACKAGE, phpInvokerPackage);
        additionalProperties.put(CodegenConstants.PERL_MODULE_NAME, perlModuleName);
        additionalProperties.put(CodegenConstants.PYTHON_PACKAGE_NAME, pythonPackageName);
        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);

        supportingFiles.add(new SupportingFile("index.mustache", "", "index.html"));
        reservedWords = new HashSet<>();

        languageSpecificPrimitives = new HashSet<>();
        importMapping = new HashMap<>();
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    @Override
    public String getName() {
        return "html2";
    }

    @Override
    public String escapeText(String input) {
        // newline escaping disabled for HTML documentation for markdown to work correctly
        return input;
    }

    @Override
    public String getHelp() {
        return "Generates a static HTML file.";
    }

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
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        for (CodegenOperation op : operationList) {
            op.httpMethod = op.httpMethod.toLowerCase(Locale.ROOT);
            for (CodegenResponse response : op.responses) {
                if ("0".equals(response.code)) {
                    response.code = "default";
                }
            }
            op.formParams = postProcessParameterEnum(op.formParams);
        }
        return objs;
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);

        if (openAPI.getInfo() != null) {
            Info info = openAPI.getInfo();
            if (StringUtils.isBlank(jsProjectName) && info.getTitle() != null) {
                // when jsProjectName is not specified, generate it from info.title
                jsProjectName = sanitizeName(dashize(info.getTitle()));
            }
        }

        // default values
        if (StringUtils.isBlank(jsProjectName)) {
            jsProjectName = "openapi-js-client";
        }
        if (StringUtils.isBlank(jsModuleName)) {
            jsModuleName = camelize(underscore(jsProjectName));
        }

        additionalProperties.put("jsProjectName", jsProjectName);
        additionalProperties.put("jsModuleName", jsModuleName);

        prepareHtmlForGlobalDescription(openAPI);

        Map<String, Object> vendorExtensions = openAPI.getExtensions();
        if (vendorExtensions != null) {
            for (Map.Entry<String, Object> vendorExtension : vendorExtensions.entrySet()) {
                // Vendor extensions could be Maps (objects). If we wanted to iterate through them in our template files
                // without knowing the keys beforehand, the default `toString` method renders them unusable. Instead, we
                // convert them to JSON strings now, which means we can easily use them later.
                if (vendorExtension.getValue() instanceof Map) {
                    this.vendorExtensions().put(vendorExtension.getKey(), Json.mapper().convertValue(vendorExtension.getValue(), JsonNode.class));
                } else {
                    this.vendorExtensions().put(vendorExtension.getKey(), vendorExtension.getValue());
                }
            }
        }
        openAPI.setExtensions(this.vendorExtensions);

    }

    @Override
    public CodegenOperation fromOperation(String path, String httpMethod, Operation operation, List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);
        if (op.returnType != null) {
            op.returnType = normalizeType(op.returnType);
        }

        //path is an unescaped variable in the mustache template api.mustache line 82 '<&path>'
        op.path = sanitizePath(op.path);

        String methodUpperCase = httpMethod.toUpperCase(Locale.ROOT);
        op.vendorExtensions.put("x-codegen-http-method-upper-case", methodUpperCase);

        return op;
    }

    /**
     * Parse Markdown to HTML for the main "Description" attribute
     *
     * @param openAPI The base object containing the global description through "Info" class
     */
    private void prepareHtmlForGlobalDescription(OpenAPI openAPI) {
        if (openAPI.getInfo() == null) {
            return;
        }

        String currentDescription = openAPI.getInfo().getDescription();
        if (currentDescription != null && !currentDescription.isEmpty()) {
            Markdown markInstance = new Markdown();
            openAPI.getInfo().setDescription(markInstance.toHtml(currentDescription));
        } else {
            LOGGER.error("OpenAPI object description is empty [{}]", openAPI.getInfo().getTitle());
        }
    }

    /**
     * Format to HTML the enums contained in every operations
     *
     * @param parameterList The whole parameters contained in one operation
     * @return String | Html formatted enum
     */
    public List<CodegenParameter> postProcessParameterEnum(List<CodegenParameter> parameterList) {
        String enumFormatted = "";

        for (CodegenParameter parameter : parameterList) {
            if (parameter.isEnum) {
                for (int i = 0; i < parameter._enum.size(); i++) {
                    String spacer = (i == (parameter._enum.size() - 1)) ? " " : ", ";

                    if (parameter._enum.get(i) != null)
                        enumFormatted += "`" + parameter._enum.get(i) + "`" + spacer;
                }
                Markdown markInstance = new Markdown();
                if (!enumFormatted.isEmpty()) {
                    String formattedExtension = markInstance.toHtml(enumFormatted);
                    parameter.vendorExtensions.put("x-eum-formatted", formattedExtension);
                }
            }
        }
        return parameterList;
    }

    private String sanitizePath(String p) {
        //prefer replace a ', instead of a fuLL URL encode for readability
        return p.replaceAll("'", "%27");
    }

    /**
     * Normalize type by wrapping primitive types with single quotes.
     *
     * @param type Primitive type
     * @return Normalized type
     */
    public String normalizeType(String type) {
        return type.replaceAll("\\b(Boolean|Integer|Number|String|Date)\\b", "'$1'");
    }

    @Override
    public String escapeQuotationMark(String input) {
        // just return the original string
        return input;
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // just return the original string
        return input;
    }

    @Override
    public GeneratorLanguage generatorLanguage() { return null; }
}
