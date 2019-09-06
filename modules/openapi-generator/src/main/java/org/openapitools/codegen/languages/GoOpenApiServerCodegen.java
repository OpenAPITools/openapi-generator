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

import io.swagger.v3.core.util.Json;
import org.openapitools.codegen.*;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;

import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenResponse;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.languages.features.BeanValidationFeatures;
import org.openapitools.codegen.utils.URLPathUtils;
import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.openapitools.codegen.mustache.TitlecaseLambda;
import org.openapitools.codegen.mustache.RemoveSpacesLambda;
import java.util.List;
import java.util.ArrayList;

import java.io.File;
import java.util.Arrays;
import java.util.Map;

public class GoOpenApiServerCodegen extends AbstractGoOpenApiCodegen {

    private static final Logger LOGGER = LoggerFactory.getLogger(GoOpenApiServerCodegen.class);

    protected String apiVersion = "1.0.0";
    protected int serverPort = 8080;
    protected String projectName = "openapi-server";
	protected String invokerPackage = "apihandlers";

    public GoOpenApiServerCodegen() {
        super();

        outputFolder = "generated-code/go";

        /*
         * Models.  You can write model files using the modelTemplateFiles map.
         */
        modelTemplateFiles.put(
                "model.mustache",
                ".go");

        apiTemplateFiles.put(
                "controller-api.mustache",
                ".go");
        apiTemplateFiles.put(
                "params.mustache",
                ".go");
        apiTemplateFiles.put(
                "response.mustache",
                ".go");
        apiTemplateFiles.put(
                "urlbuilder.mustache",
                ".go");
        apiTemplateFiles.put(
                "serve.mustache",
                ".go");


        /* Template Location.*/
        embeddedTemplateDir = templateDir = "go-openapi-server";

        /*
         * Reserved words.  
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
    }

    @Override
    public void processOpts() {
        super.processOpts();

        /* Additional Properties.  */
        if (!additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)) {
            setModelPackage("model");
        }
        if (!additionalProperties.containsKey(CodegenConstants.API_PACKAGE)) {
            setApiPackage("api");
        }
        if (!additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
            invokerPackage = "apihandlers";
        } else {
            invokerPackage = (String) additionalProperties.get(CodegenConstants.INVOKER_PACKAGE);
        }
        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage.substring(invokerPackage.lastIndexOf('.')+1));
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage().substring(modelPackage().lastIndexOf('.')+1));
        additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage().substring(apiPackage().lastIndexOf('.')+1));
        additionalProperties.put(CodegenConstants.MODEL_PACKAGE + "Path", modelPackage().replace('.', File.separatorChar));
        additionalProperties.put(CodegenConstants.API_PACKAGE + "Path", apiPackage.replace('.', File.separatorChar));
        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE + "Path", invokerPackage.replace('.', File.separatorChar));

        additionalProperties.put("apiVersion", apiVersion);
        additionalProperties.put("serverPort", serverPort);
        additionalProperties.put("apiPath", apiPackage());
        addMustacheLambdas(additionalProperties);


        /*
         * Supporting Files.  You can write single files for the generator with the
         * entire object tree available.  If the input file has a suffix of `.mustache
         * it will be processed by the template engine.  Otherwise, it will be copied
         */
        supportingFiles.add(new SupportingFile("openapi.mustache", apiPackage(), "openapi.yaml"));
        supportingFiles.add(new SupportingFile("jsonopenapi.mustache", apiPackage(), "openapi.json"));
        supportingFiles.add(new SupportingFile("handlers.mustache", apiPackage(), "handlers.go"));
        supportingFiles.add(new SupportingFile("configure.mustache", invokerPackage.replace('.', File.separatorChar), "configure.go"));
        supportingFiles.add(new SupportingFile("server.mustache", "server", "server.go"));
        supportingFiles.add(new SupportingFile("main.mustache", "server", "main.go"));
        supportingFiles.add(new SupportingFile("Dockerfile.mustache", "", "Dockerfile"));
        supportingFiles.add(new SupportingFile("paths.mustache", modelPackage(), "paths.go"));

    }

    private void addMustacheLambdas(Map<String, Object> objs) {

        Map<String, Mustache.Lambda> lambdas = new ImmutableMap.Builder<String, Mustache.Lambda>()
                .put("titlecase", new TitlecaseLambda())
                .put("removespaces", new RemoveSpacesLambda())
                .build();

        if (objs.containsKey("lambda")) {
            LOGGER.warn("A property named 'lambda' already exists. Mustache lambdas renamed from 'lambda' to '_lambda'. " +
                    "You'll likely need to use a custom template, " +
                    "see https://github.com/swagger-api/swagger-codegen#modifying-the-client-library-format. "); // TODO: update the URL
            objs.put("_lambda", lambdas);
        } else {
            objs.put("lambda", lambdas);
        }
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        List<CodegenOperation> opList = operations.get(resourcePath);
        if (opList == null || opList.isEmpty()) {
            opList = new ArrayList<CodegenOperation>();
            operations.put(co.nickname, opList);
        }
        opList.add(co);
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
        return "go-openapi-server";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a Go openapi server library using OpenAPI-Generator. By default, " +
                "it will also generate service classes -- which you can disable with the `-Dnoservice` environment variable.";
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
    public String modelFileFolder() {
        return outputFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String suffix = apiTemplateFiles().get(templateName);
	    if ("controller-api.mustache" == templateName) {
		    return outputFolder + File.separator + invokerPackage.replace('.', File.separatorChar) + File.separator + toApiFilename(tag) + suffix;
	    }
        return apiFileFolder() +  File.separator + toApiFilename(templateName.substring(0, templateName.lastIndexOf(".")) + tag) + suffix;
    }
}
