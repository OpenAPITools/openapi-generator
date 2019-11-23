/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import org.openapitools.codegen.*;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.HashSet;

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

    private static final Logger LOGGER = LoggerFactory.getLogger(AsciidocDocumentationCodegen.class);

    public static final String SPEC_DIR = "specDir";
    public static final String SNIPPET_DIR = "snippetDir";

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
        private String basePath;

        public IncludeMarkupLambda(final String basePath) {
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

            if (Files.isRegularFile(filePathToInclude)) {
                LOGGER.debug(
                        "including " + ++includeCount + ". file into markup from: " + filePathToInclude.toString());
                out.write("\ninclude::" + relativeFileName + "[opts=optional]\n");
            } else {
                LOGGER.debug(++notFoundCount + ". file not found, skip include for: " + filePathToInclude.toString());
                out.write("\n// markup not found, no include ::" + relativeFileName + "[opts=optional]\n");
            }
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
                LOGGER.debug("linking " + ++linkedCount + ". file into markup from: " + filePathToLinkTo.toString());
                out.write("\n" + linkName + " link:" + relativeFileName + "[]\n");
            } else {
                LOGGER.debug(++notFoundLinkCount + ". file not found, skip link for: " + filePathToLinkTo.toString());
                out.write("\n// file not found, no " + linkName + " link :" + relativeFileName + "[]\n");
            }
        }
    }

    protected String invokerPackage = "org.openapitools.client";
    protected String groupId = "org.openapitools";
    protected String artifactId = "openapi-client";
    protected String artifactVersion = "1.0.0";

    private IncludeMarkupLambda includeSpecMarkupLambda;
    private IncludeMarkupLambda includeSnippetMarkupLambda;
    private LinkMarkupLambda linkSnippetMarkupLambda;

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

    public String getName() {
        return "asciidoc";
    }

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

        LOGGER.trace("start asciidoc codegen");

        outputFolder = "generated-code" + File.separator + "asciidoc";
        embeddedTemplateDir = templateDir = "asciidoc-documentation";

        defaultIncludes = new HashSet<String>();

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
                "path with includable markup snippets (e.g. test output generated by restdoc, default: .")
                        .defaultValue("."));
        cliOptions.add(new CliOption(SPEC_DIR,
                "path with includable markup spec files (e.g. handwritten additional docs, default: .")
                        .defaultValue(".."));

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
        reservedWords = new HashSet<String>();

        languageSpecificPrimitives = new HashSet<String>();
        importMapping = new HashMap<String, String>();

    }

    @Override
    public String escapeQuotationMark(String input) {
        return input; // just return the original string
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input; // just return the original string
    }

    @Override
    public void processOpts() {
        super.processOpts();

        String specDir = this.additionalProperties.get(SPEC_DIR) + "";
        if (!Files.isDirectory(Paths.get(specDir))) {
            LOGGER.warn("base part for include markup lambda not found: " + specDir + " as "
                    + Paths.get(specDir).toAbsolutePath());
        }

        this.includeSpecMarkupLambda = new IncludeMarkupLambda(specDir);
        additionalProperties.put("specinclude", this.includeSpecMarkupLambda);

        String snippetDir = this.additionalProperties.get(SNIPPET_DIR) + "";
        if (!Files.isDirectory(Paths.get(snippetDir))) {
            LOGGER.warn("base part for include markup lambda not found: " + snippetDir + " as "
                    + Paths.get(snippetDir).toAbsolutePath());
        }

        this.includeSnippetMarkupLambda = new IncludeMarkupLambda(snippetDir);
        additionalProperties.put("snippetinclude", this.includeSnippetMarkupLambda);

        this.linkSnippetMarkupLambda = new LinkMarkupLambda(snippetDir);
        additionalProperties.put("snippetlink", this.linkSnippetMarkupLambda);
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
