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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import org.openapitools.codegen.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class NodeJSFastifyServerCodegen extends DefaultCodegen implements CodegenConfig {

    private static final Logger LOGGER = LoggerFactory.getLogger(NodeJSServerCodegen.class);

    protected String projectName = "openapi-server";

    public NodeJSFastifyServerCodegen() {
        super();

        outputFolder = "generated-code/nodejs-fastify";

        modelTemplateFiles.clear();
        apiTemplateFiles.clear();

        embeddedTemplateDir = templateDir = "nodejs-fastify";

        setReservedWordsLowerCase(
                Arrays.asList(
                        "break", "case", "class", "catch", "const", "continue", "debugger",
                        "default", "delete", "do", "else", "export", "extends", "finally",
                        "for", "function", "if", "import", "in", "instanceof", "let", "new",
                        "return", "super", "switch", "this", "throw", "try", "typeof", "var",
                        "void", "while", "with", "yield")
        );

        supportingFiles.add(new SupportingFile("index.mustache", "", "index.js"));
        supportingFiles.add(new SupportingFile("readme.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("openapi.mustache", "", "openapi.json"));
        supportingFiles.add(new SupportingFile("package.mustache", "", "package.json"));

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
        return "nodejs-fastify-server";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a NodeJS Fastify server library";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        writeOptional(outputFolder, new SupportingFile("service.mustache", "", "service.js"));
    }

    @Override
    public Map<String, Object> postProcessSupportingFileData(Map<String, Object> objs) {
        generateJSONSpecFile(objs);

        return super.postProcessSupportingFileData(objs);
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {

        if (openAPI.getInfo() != null) {
            Info info = openAPI.getInfo();
            if (info.getTitle() != null) {
                // when info.title is defined, use it for projectName
                // used in package.json
                projectName = info.getTitle()
                        .replaceAll("[^a-zA-Z0-9]", "-")
                        .replaceAll("^[-]*", "")
                        .replaceAll("[-]*$", "")
                        .replaceAll("[-]{2,}", "-")
                        .toLowerCase(Locale.ROOT);
            }
        }

        this.additionalProperties.put("projectName", projectName);

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

    @Override
    public String escapeUnsafeCharacters(String input) {
        return input.replace("*/", "*_/").replace("/*", "/_*");
    }

    @Override
    public String escapeQuotationMark(String input) {
        // remove " to avoid code injection
        return input.replace("\"", "");
    }


}