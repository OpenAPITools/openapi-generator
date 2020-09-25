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

import com.samskivert.mustache.Escapers;
import com.samskivert.mustache.Mustache.Compiler;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.Markdown;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.*;

import static org.openapitools.codegen.utils.StringUtils.escape;

public class StaticHtmlGenerator extends DefaultCodegen implements CodegenConfig {
    protected String invokerPackage = "org.openapitools.client";
    protected String groupId = "org.openapitools";
    protected String artifactId = "openapi-client";
    protected String artifactVersion = "1.0.0";

    public StaticHtmlGenerator() {
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
        embeddedTemplateDir = templateDir = "htmlDocs";

        defaultIncludes = new HashSet<String>();

        cliOptions.add(new CliOption("appName", "short name of the application"));
        cliOptions.add(new CliOption("appDescription", "description of the application"));
        cliOptions.add(new CliOption("infoUrl", "a URL where users can get more information about the application"));
        cliOptions.add(new CliOption("infoEmail", "an email address to contact for inquiries about the application"));
        cliOptions.add(new CliOption("licenseInfo", "a short description of the license"));
        cliOptions.add(new CliOption("licenseUrl", "a URL pointing to the full license"));
        cliOptions.add(new CliOption(CodegenConstants.INVOKER_PACKAGE, CodegenConstants.INVOKER_PACKAGE_DESC));
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
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);

        supportingFiles.add(new SupportingFile("index.mustache", "", "index.html"));
        reservedWords = new HashSet<String>();

        languageSpecificPrimitives = new HashSet<String>();
        importMapping = new HashMap<String, String>();
    }

    /**
     * Convert Markdown (CommonMark) to HTML. This class also disables normal HTML
     * escaping in the Mustache engine.
     */
    @Override
    public String escapeText(String input) {
        // newline escaping disabled for HTML documentation for markdown to work correctly
        return toHtml(input);
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    @Override
    public String getName() {
        return "html";
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
        }
        return objs;
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

    /**
     * Markdown conversion emits HTML and by default, the Mustache
     * {@link Compiler} will escape HTML. For example a summary
     * <code>"Text with **bold**"</code> is converted from Markdown to HTML as
     * <code>"Text with &lt;strong&gt;bold&lt;/strong&gt; text"</code> and then
     * the default compiler with HTML escaping on turns this into
     * <code>"Text with &amp;lt;strong&amp;gt;bold&amp;lt;/strong&amp;gt; text"</code>.
     * Here, we disable escaping by setting the compiler to {@link Escapers#NONE
     * Escapers.NONE}
     */
    @Override
    public Compiler processCompiler(Compiler compiler) {
        return compiler.withEscaper(Escapers.NONE);
    }

    private Markdown markdownConverter = new Markdown();

    private static final boolean CONVERT_TO_MARKDOWN_VIA_ESCAPE_TEXT = false;

    /**
     * Convert Markdown text to HTML
     *
     * @param input text in Markdown; may be null.
     * @return the text, converted to Markdown. For null input, "" is returned.
     */
    public String toHtml(String input) {
        if (input == null)
            return "";
        return markdownConverter.toHtml(input);
    }

    // DefaultCodegen converts model names to UpperCamelCase
    // but for static HTML, we want the names to be preserved as coded in the OpenApi
    // so HTML links work
    @Override
    public String toModelName(final String name) {
        return name;
    }

    // DefaultCodegen converts snake_case property names to snakeUnderscorecase
    // but for static HTML, we want to preserve snake_case names
    @Override
    public String toVarName(String name) {
        if (reservedWords.contains(name)) {
            return escapeReservedWord(name);
        } else if (((CharSequence) name).chars().anyMatch(character -> specialCharReplacements.keySet().contains("" + ((char) character)))) {
            return escape(name, specialCharReplacements, Arrays.asList("_"), null);
        } else {
            return name;
        }
    }

    public void preprocessOpenAPI(OpenAPI openAPI) {
        Info info = openAPI.getInfo();
        info.setDescription(toHtml(info.getDescription()));
        info.setTitle(toHtml(info.getTitle()));
        Map<String, Schema> models = ModelUtils.getSchemas(openAPI);
        for (Schema model : models.values()) {
            model.setDescription(toHtml(model.getDescription()));
            model.setTitle(toHtml(model.getTitle()));
        }
    }

    // override to post-process any parameters
    public void postProcessParameter(CodegenParameter parameter) {
        parameter.description = toHtml(parameter.description);
        parameter.unescapedDescription = toHtml(
                parameter.unescapedDescription);
    }

    // override to post-process any model properties
    public void postProcessModelProperty(CodegenModel model,
                                         CodegenProperty property) {
        property.description = toHtml(property.description);
        property.unescapedDescription = toHtml(
                property.unescapedDescription);
    }

}
