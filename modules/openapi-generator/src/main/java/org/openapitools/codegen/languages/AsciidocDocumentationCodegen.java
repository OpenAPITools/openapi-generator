/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;

import org.openapitools.codegen.meta.features.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;

import io.swagger.v3.oas.models.OpenAPI;

/**
 * basic asciidoc markup generator.
 *
 * @see <a href="https://asciidoctor.org">asciidoctor</a>
 */
public class AsciidocDocumentationCodegen extends DefaultCodegen implements CodegenConfig {

    private final Logger LOGGER = LoggerFactory.getLogger(AsciidocDocumentationCodegen.class);

    public static final String SPEC_DIR = "specDir";
    public static final String SNIPPET_DIR = "snippetDir";
    public static final String HEADER_ATTRIBUTES_FLAG = "headerAttributes";
    public static final String USE_INTRODUCTION_FLAG = "useIntroduction";
    public static final String SKIP_EXAMPLES_FLAG = "skipExamples";
    public static final String USE_METHOD_AND_PATH_FLAG = "useMethodAndPath";
    public static final String USE_TABLE_TITLES_FLAG = "useTableTitles";

    /**
     * Lambda emitting an asciidoc "include::filename.adoc[]" if file is found in
     * path. Use:
     *
     * <pre>
     * {{#includemarkup}}{{name}}/description.adoc{{/includemarkup}}
     * </pre>
     */
    public class IncludeMarkupLambda implements Mustache.Lambda {

        private long includeCount = 0;
        private long notFoundCount = 0;
        private String attributePathReference;
        private String basePath;

        public IncludeMarkupLambda(final String attributePathReference, final String basePath) {
            this.attributePathReference = attributePathReference;
            this.basePath = basePath;
        }

        public String resetCounter() {
            String msg = "included: " + includeCount + " notFound: " + notFoundCount + " from " + basePath;
            includeCount = 0;
            notFoundCount = 0;
            return msg;
        }

        @Override
        public void execute(final Template.Fragment frag, final Writer out) throws IOException {

            final String relativeFileName = AsciidocDocumentationCodegen.sanitize(frag.execute());
            final Path filePathToInclude = Paths.get(basePath, relativeFileName).toAbsolutePath();

            String includeStatement = "include::{" + attributePathReference + "}" + escapeCurlyBrackets(relativeFileName) + "[opts=optional]";
            if (Files.isRegularFile(filePathToInclude)) {
                LOGGER.debug("including {}. file into markup from: {}", ++includeCount, filePathToInclude);
                out.write("\n" + includeStatement + "\n");
            } else {
                LOGGER.debug("{}. file not found, skip include for: {}", ++notFoundCount, filePathToInclude);
                out.write("\n// markup not found, no " + includeStatement + "\n");
            }
        }

        private String escapeCurlyBrackets(String relativeFileName) {
            return relativeFileName.replaceAll("\\{","\\\\{").replaceAll("\\}","\\\\}");
        }
    }

    /**
     * Lambda emitting an asciidoc "http link" if file is found in path. Use:
     *
     * <pre>
     * {{#snippetLink}}markup until koma, /{{name}}.json{{/snippetLink}}
     * </pre>
     */
    public class LinkMarkupLambda implements Mustache.Lambda {

        private long linkedCount = 0;
        private long notFoundLinkCount = 0;
        private String basePath;

        public LinkMarkupLambda(final String basePath) {
            this.basePath = basePath;
        }

        public String resetCounter() {
            String msg = "linked:" + linkedCount + " notFound: " + notFoundLinkCount + " from " + basePath;
            linkedCount = 0;
            notFoundLinkCount = 0;
            return msg;
        }

        @Override
        public void execute(final Template.Fragment frag, final Writer out) throws IOException {

            final String content = frag.execute();
            final String[] tokens = content.split(",", 2);

            final String linkName = tokens.length > 0 ? tokens[0] : "";

            final String relativeFileName = AsciidocDocumentationCodegen
                    .sanitize(tokens.length > 1 ? tokens[1] : linkName);

            final Path filePathToLinkTo = Paths.get(basePath, relativeFileName).toAbsolutePath();

            if (Files.isRegularFile(filePathToLinkTo)) {
                LOGGER.debug("linking {}. file into markup from: {}", ++linkedCount, filePathToLinkTo);
                out.write("\n" + linkName + " link:" + relativeFileName + "[]\n");
            } else {
                LOGGER.debug("{}. file not found, skip link for: {}", ++notFoundLinkCount, filePathToLinkTo);
                out.write("\n// file not found, no " + linkName + " link :" + relativeFileName + "[]\n");
            }
        }
    }

    protected String invokerPackage = "org.openapitools.client";
    protected String groupId = "org.openapitools";
    protected String artifactId = "openapi-client";
    protected String artifactVersion = "1.0.0";
    protected boolean headerAttributes = true;
    protected boolean useIntroduction = false;
    protected boolean skipExamples = false;
    protected boolean useMethodAndPath = false;
    protected boolean useTableTitles = false;

    private IncludeMarkupLambda includeSpecMarkupLambda;
    private IncludeMarkupLambda includeSnippetMarkupLambda;
    private LinkMarkupLambda linkSnippetMarkupLambda;

    @Override
    public CodegenType getTag() {
        return CodegenType.DOCUMENTATION;
    }

    /**
     * extracted filter value should be relative to be of use as link or include
     * file.
     *
     * @param name filename to sanitize
     * @return trimmed and striped path part or empty string.
     */
    static String sanitize(final String name) {
        String sanitized = name == null ? "" : name.trim();
        sanitized = sanitized.replace("//", "/"); // rest paths may or may not end with slashes, leading to redundant
        // path separators.
        return sanitized.startsWith(File.separator) || sanitized.startsWith("/") ? sanitized.substring(1) : sanitized;
    }

    @Override
    public String getName() {
        return "asciidoc";
    }

    @Override
    public String getHelp() {
        return "Generates asciidoc markup based documentation.";
    }

    public String getSpecDir() {
        return additionalProperties.get("specDir").toString();
    }

    public String getSnippetDir() {
        return additionalProperties.get("snippetDir").toString();
    }

    public AsciidocDocumentationCodegen() {
        super();

        // TODO: Asciidoc maintainer review.
        modifyFeatureSet(features -> features
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .documentationFeatures(EnumSet.noneOf(DocumentationFeature.class))
                .globalFeatures(EnumSet.noneOf(GlobalFeature.class))
                .schemaSupportFeatures(EnumSet.noneOf(SchemaSupportFeature.class))
                .clientModificationFeatures(EnumSet.noneOf(ClientModificationFeature.class))
        );

        LOGGER.trace("start asciidoc codegen");

        outputFolder = "generated-code" + File.separator + "asciidoc";
        embeddedTemplateDir = templateDir = "asciidoc-documentation";

        defaultIncludes = new HashSet<>();

        cliOptions.add(new CliOption("appName", "short name of the application"));
        cliOptions.add(new CliOption("appDescription", "description of the application"));
        cliOptions.add(new CliOption("infoUrl", "a URL where users can get more information about the application"));
        cliOptions.add(new CliOption("infoEmail", "an email address to contact for inquiries about the application"));
        cliOptions.add(new CliOption("licenseInfo", "a short description of the license"));
        cliOptions.add(new CliOption(CodegenConstants.LICENSE_URL, "a URL pointing to the full license"));
        cliOptions.add(new CliOption(CodegenConstants.INVOKER_PACKAGE, CodegenConstants.INVOKER_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.GROUP_ID, CodegenConstants.GROUP_ID_DESC));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_ID, CodegenConstants.ARTIFACT_ID_DESC));
        cliOptions.add(new CliOption(CodegenConstants.ARTIFACT_VERSION, CodegenConstants.ARTIFACT_VERSION_DESC));

        cliOptions.add(new CliOption(SNIPPET_DIR,
                "path with includable markup snippets (e.g. test output generated by restdoc, default: .)")
                .defaultValue("."));
        cliOptions.add(new CliOption(SPEC_DIR,
                "path with includable markup spec files (e.g. handwritten additional docs, default: ..)")
                .defaultValue(".."));
        cliOptions.add(CliOption.newBoolean(HEADER_ATTRIBUTES_FLAG,
                "generation of asciidoc header meta data attributes (set to false to suppress, default: true)",
                true));
        cliOptions.add(CliOption.newBoolean(USE_INTRODUCTION_FLAG,
                "use introduction section, rather than an initial abstract (default: false)",
                false));
        cliOptions.add(CliOption.newBoolean(SKIP_EXAMPLES_FLAG,
                "skip examples sections (default: false)",
                false));
        cliOptions.add(CliOption.newBoolean(USE_METHOD_AND_PATH_FLAG,
                "Use HTTP method and path as operation heading, instead of operation id (default: false)",
                false));
        cliOptions.add(CliOption.newBoolean(USE_TABLE_TITLES_FLAG,
                "Use titles for tables, rather than wrapping tables instead their own section (default: false)",
                false));

        additionalProperties.put("appName", "OpenAPI Sample description");
        additionalProperties.put("appDescription", "A sample OpenAPI documentation");
        additionalProperties.put("infoUrl", "https://openapi-generator.tech");
        additionalProperties.put("infoEmail", "team@openapitools.org");
        additionalProperties.put("licenseInfo", "All rights reserved");
        additionalProperties.put(CodegenConstants.LICENSE_URL, "http://apache.org/licenses/LICENSE-2.0.html");
        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);

        supportingFiles.add(new SupportingFile("index.mustache", "", "index.adoc"));
        reservedWords = new HashSet<>();

        languageSpecificPrimitives = new HashSet<>();
        importMapping = new HashMap<>();

    }

    @Override
    public String escapeQuotationMark(String input) {
        return input; // just return the original string
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input; // just return the original string
    }

    public boolean isHeaderAttributes() {
        return headerAttributes;
    }

    public void setHeaderAttributes(boolean headerAttributes) {
        this.headerAttributes = headerAttributes;
    }

    public boolean isUseIntroduction() {
        return useIntroduction;
    }

    public void setUseIntroduction(boolean useIntroduction) {
        this.useIntroduction = useIntroduction;
    }

    public boolean isSkipExamples() {
        return skipExamples;
    }

    public void setSkipExamples(boolean skipExamples) {
        this.skipExamples = skipExamples;
    }

    public boolean isUseMethodAndPath() {
        return useMethodAndPath;
    }

    public void setUseMethodAndPath(boolean useMethodAndPath) {
        this.useMethodAndPath = useMethodAndPath;
    }

    public boolean isUseTableTitles() {
        return useTableTitles;
    }

    public void setUseTableTitles(boolean useTableTitles) {
        this.useTableTitles = useTableTitles;
    }

    @Override
    public void processOpts() {
        super.processOpts();

        String specDir = String.valueOf(this.additionalProperties.get(SPEC_DIR));
        if (!Files.isDirectory(Paths.get(specDir))) {
            LOGGER.warn("base part for include markup lambda not found: {} as {}", specDir, Paths.get(specDir).toAbsolutePath());
        }

        this.includeSpecMarkupLambda = new IncludeMarkupLambda(SPEC_DIR,specDir);
        additionalProperties.put("specinclude", this.includeSpecMarkupLambda);

        String snippetDir = String.valueOf(this.additionalProperties.get(SNIPPET_DIR));
        if (!Files.isDirectory(Paths.get(snippetDir))) {
            LOGGER.warn("base part for include markup lambda not found: {} as {}", snippetDir, Paths.get(snippetDir).toAbsolutePath());
        }

        this.includeSnippetMarkupLambda = new IncludeMarkupLambda(SNIPPET_DIR,snippetDir);
        additionalProperties.put("snippetinclude", this.includeSnippetMarkupLambda);

        this.linkSnippetMarkupLambda = new LinkMarkupLambda(snippetDir);
        additionalProperties.put("snippetlink", this.linkSnippetMarkupLambda);

        processBooleanFlag(HEADER_ATTRIBUTES_FLAG, headerAttributes);
        processBooleanFlag(USE_INTRODUCTION_FLAG, useIntroduction);
        processBooleanFlag(SKIP_EXAMPLES_FLAG, skipExamples);
        processBooleanFlag(USE_METHOD_AND_PATH_FLAG, useMethodAndPath);
        processBooleanFlag(USE_TABLE_TITLES_FLAG, useTableTitles);
    }

    private void processBooleanFlag(String flag, boolean value) {
        if (additionalProperties.containsKey(flag)) {
            this.setHeaderAttributes(convertPropertyToBooleanAndWriteBack(flag));
        } else {
            additionalProperties.put(flag, value);
        }
    }

    @Override
    public void processOpenAPI(OpenAPI openAPI) {
        if (this.includeSpecMarkupLambda != null) {
            LOGGER.debug("specs: " + ": " + this.includeSpecMarkupLambda.resetCounter());
        }
        if (this.includeSnippetMarkupLambda != null) {
            LOGGER.debug("snippets: " + ": " + this.includeSnippetMarkupLambda.resetCounter());
        }
        super.processOpenAPI(openAPI);
    }

}
