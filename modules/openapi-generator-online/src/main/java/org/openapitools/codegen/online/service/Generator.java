 /*
  * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
  * Copyright 2018 SmartBear Software
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *     https://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */

package org.openapitools.codegen.online.service;

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.AuthorizationValue;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConfigLoader;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.online.model.GeneratorInput;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public class Generator {
    private static final Logger LOGGER = LoggerFactory.getLogger(Generator.class);

    public static Map<String, CliOption> getOptions(String language) {
        CodegenConfig config = loadCodegenConfig(language);
        Map<String, CliOption> map = (Map<String, CliOption>) config.cliOptions();
        return map;
    }

    public static String generateClient(String language, GeneratorInput opts) {
        return generate(language, opts, new ClientGenerator());
    }

    public static String generateServer(String language, GeneratorInput opts) {
        return generate(language, opts, new ServerGenerator());
    }

    private static String generate(String language, GeneratorInput opts, CodeGenerator codeGenerator) {
        if (opts == null) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "No options were supplied");
        }
        OpenAPI openapi = getOpenAPI(opts);
        validateOpenAPI(openapi);

        String destPath = getDestinationPath(opts);
        String outputFolder = getOutputFolder(destPath, codeGenerator.getTypeName());
        String outputFilename = getOutputFilename(outputFolder);

        CodegenConfig codegenConfig = loadCodegenConfig(language);
        configureCodegen(opts, openapi, codegenConfig);

        generateCode(opts, outputFolder, outputFilename, openapi, codegenConfig);

        return outputFilename;
    }

    private static OpenAPI getOpenAPI(GeneratorInput opts) {
        JsonNode node = opts.getSpec();
        if (node != null && "{}".equals(node.toString())) {
            LOGGER.debug("ignoring empty spec");
            node = null;
        }

        ParseOptions parseOptions = new ParseOptions();
        parseOptions.setResolve(true);

        if (node == null) {
            if (opts.getOpenAPIUrl() != null) {
                return readOpenAPIFromUrl(opts, parseOptions);
            } else {
                throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "No OpenAPI specification was supplied");
            }
        } else {
            return readOpenAPIFromContents(opts, node, parseOptions);
        }
    }

    private static OpenAPI readOpenAPIFromUrl(GeneratorInput opts, ParseOptions parseOptions) {
        if (opts.getAuthorizationValue() != null) {
            List<AuthorizationValue> authorizationValues = new ArrayList<>();
            authorizationValues.add(opts.getAuthorizationValue());
            return new OpenAPIParser().readLocation(opts.getOpenAPIUrl(), authorizationValues, parseOptions).getOpenAPI();
        } else {
            return new OpenAPIParser().readLocation(opts.getOpenAPIUrl(), null, parseOptions).getOpenAPI();
        }
    }

    private static OpenAPI readOpenAPIFromContents(GeneratorInput opts, JsonNode node, ParseOptions parseOptions) {
        if (opts.getAuthorizationValue() != null) {
            List<AuthorizationValue> authorizationValues = new ArrayList<>();
            authorizationValues.add(opts.getAuthorizationValue());
            return new OpenAPIParser().readContents(node.toString(), authorizationValues, parseOptions).getOpenAPI();
        } else {
            return new OpenAPIParser().readContents(node.toString(), null, parseOptions).getOpenAPI();
        }
    }

    private static void validateOpenAPI(OpenAPI openapi) {
        if (openapi == null) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "The OpenAPI specification supplied was not valid");
        }
    }

    private static String getDestinationPath(GeneratorInput opts) {
        return opts.getOptions() != null ? opts.getOptions().get("outputFolder") : null;
    }

    private static String getOutputFolder(String destPath, String typeName) {
        return destPath != null ? destPath : typeName + "-" + typeName;
    }

    private static String getOutputFilename(String outputFolder) {
        return outputFolder + "-bundle.zip";
    }

    private static CodegenConfig loadCodegenConfig(String language) {
        try {
            return CodegenConfigLoader.forName(language);
        } catch (Exception e) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, String.format(Locale.ROOT, "Unsupported target %s supplied. %s",
                    language, e));
        }
    }

    private static void configureCodegen(GeneratorInput opts, OpenAPI openapi, CodegenConfig codegenConfig) {
        if (opts.getOptions() != null) {
            codegenConfig.additionalProperties().putAll(opts.getOptions());
            codegenConfig.additionalProperties().put("openAPI", openapi);
        }

        codegenConfig.setOutputDir(getOutputFolder(opts));
    }

    private static void generateCode(GeneratorInput opts, String outputFolder, String outputFilename, OpenAPI openapi, CodegenConfig codegenConfig) {
        try {
            List<File> files = new DefaultGenerator().opts(getClientOptInput(opts, openapi, codegenConfig)).generate();
            handleGeneratedFiles(files, outputFolder, outputFilename);
        } catch (Exception e) {
            handleGenerationException(e);
        }
    }

    private static ClientOptInput getClientOptInput(GeneratorInput opts, OpenAPI openapi, CodegenConfig codegenConfig) {
        ClientOptInput clientOptInput = new ClientOptInput();
        clientOptInput.openAPI(openapi);
        clientOptInput.config(codegenConfig);
        return clientOptInput;
    }

    private static void handleGeneratedFiles(List<File> files, String outputFolder, String outputFilename) throws IOException {
        if (files.size() > 0) {
            List<File> filesToAdd = new ArrayList<>();
            filesToAdd.add(new File(outputFolder));
            ZipUtil zip = new ZipUtil();
            zip.compressFiles(filesToAdd, outputFilename);
        } else {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST,
                    "A target generation was attempted, but no files were created!");
        }

        deleteFiles(files);
        deleteFolder(outputFolder);
    }

    private static void deleteFiles(List<File> files) {
        for (File file : files) {
            try {
                file.delete();
            } catch (Exception e) {
                LOGGER.error("unable to delete file " + file.getAbsolutePath(), e);
            }
        }
    }

    private static void deleteFolder(String outputFolder) {
        try {
            new File(outputFolder).delete();
        } catch (Exception e) {
            LOGGER.error("unable to delete output folder " + outputFolder, e);
        }
    }

    private static void handleGenerationException(Exception e) {
        throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Unable to build target: " + e.getMessage(), e);
    }

    private static String getOutputFolder(GeneratorInput opts) {
        return opts.getOptions() != null ? opts.getOptions().get("outputFolder") : null;
    }

    public abstract static class CodeGenerator {
        public abstract String getTypeName();
    }

    public static class ClientGenerator extends CodeGenerator {
        @Override
        public String getTypeName() {
            return "client";
        }
    }

    public static class ServerGenerator extends CodeGenerator {
        @Override
        public String getTypeName() {
            return "server";
        }
    }
}
