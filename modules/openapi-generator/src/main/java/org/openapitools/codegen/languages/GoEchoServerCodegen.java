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
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;

import java.io.File;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;

public class GoEchoServerCodegen extends AbstractGoCodegen {
    protected String apiVersion = "1.0.0";
    protected int serverPort = 8080;
    protected String projectName = "openapi-go-echo-server";
    protected String apiPath = "go";

    private static final String MODEL_PACKAGE_NAME = "models";
    private static final String API_PACKAGE_NAME = "handlers";
    private static final String OUTPUT_PATH = "generated-code" + File.separator + "go-echo-server";

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "go-echo-server";
    }

    public String getHelp() {
        return "Generates a go-echo server. (Beta)";
    }

    public GoEchoServerCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.noneOf(
                        SecurityFeature.class
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

        outputFolder = OUTPUT_PATH;
        modelTemplateFiles.put("model.mustache", ".go");
        setModelPackage(MODEL_PACKAGE_NAME);
        apiTemplateFiles.put("api.mustache", ".go");
        embeddedTemplateDir = templateDir = "go-echo-server";

        /*
         * Reserved words.  Override this with reserved words specific to your language
         */
        setReservedWordsLowerCase(
                Arrays.asList(
                        // data type
                        "string", "bool", "uint", "uint8", "uint16", "uint32", "uint64",
                        "int", "int8", "int16", "int32", "int64", "float32", "float64",
                        "complex64", "complex128", "rune", "byte", "uintptr",
                        "break", "default", "func", "interface", "select",
                        "case", "defer", "go", "map", "struct",
                        "chan", "else", "goto", "package", "switch",
                        "const", "fallthrough", "if", "range", "type",
                        "continue", "for", "import", "return", "var", "error", "nil")
                // Added "error" as it's used so frequently that it may as well be a keyword
        );

        CliOption optServerPort = new CliOption("serverPort", "The network port the generated server binds to");
        optServerPort.setType("int");
        optServerPort.defaultValue(Integer.toString(serverPort));
        cliOptions.add(optServerPort);
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);

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
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("openapi");
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, this.packageName);
        }

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        if (additionalProperties.containsKey("apiVersion")) {
            this.apiVersion = (String) additionalProperties.get("apiVersion");
        } else {
            additionalProperties.put("apiVersion", apiVersion);
        }

        if (additionalProperties.containsKey("serverPort")) {
            this.serverPort = Integer.parseInt((String) additionalProperties.get("serverPort"));
        } else {
            additionalProperties.put("serverPort", serverPort);
        }

        if (additionalProperties.containsKey("apiPath")) {
            this.apiPath = (String) additionalProperties.get("apiPath");
        } else {
            additionalProperties.put("apiPath", apiPath);
        }

        if (additionalProperties.containsKey(CodegenConstants.ENUM_CLASS_PREFIX)) {
            setEnumClassPrefix(Boolean.parseBoolean(additionalProperties.get(CodegenConstants.ENUM_CLASS_PREFIX).toString()));
            if (enumClassPrefix) {
                additionalProperties.put(CodegenConstants.ENUM_CLASS_PREFIX, true);
            }
        }

        /*
        set model and package names, this is mainly used inside the templates.
         */
        modelPackage = MODEL_PACKAGE_NAME;
        apiPackage = API_PACKAGE_NAME;

        /*
         * Supporting Files.  You can write single files for the generator with the
         * entire object tree available.  If the input file has a suffix of `.mustache
         * it will be processed by the template engine.  Otherwise, it will be copied
         */
        supportingFiles.add(new SupportingFile("openapi.mustache", ".docs/api", "openapi.yaml"));
        supportingFiles.add(new SupportingFile("hello-world.mustache", "models", "hello-world.go"));
        supportingFiles.add(new SupportingFile("go-mod.mustache", "", "go.mod"));
        supportingFiles.add(new SupportingFile("handler-container.mustache", "handlers", "container.go"));
        supportingFiles.add(new SupportingFile("main.mustache", "", "main.go"));
        supportingFiles.add(new SupportingFile("Dockerfile.mustache", "", "Dockerfile"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md")
                .doNotOverwrite());
    }
}
