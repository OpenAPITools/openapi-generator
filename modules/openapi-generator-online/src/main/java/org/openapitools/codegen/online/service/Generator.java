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

package org.openapitools.codegen.online.service;

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.AuthorizationValue;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.*;
import org.openapitools.codegen.online.model.GeneratorInput;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import java.io.File;
import java.nio.file.Files;
import java.util.*;

public class Generator {
    private static Logger LOGGER = LoggerFactory.getLogger(Generator.class);

    public static Map<String, CliOption> getOptions(String language) {
        CodegenConfig config;
        try {
            config = CodegenConfigLoader.forName(language);
        } catch (Exception e) {
            throw new ResponseStatusException(HttpStatus.NOT_FOUND, String.format(Locale.ROOT, "Unsupported target %s supplied. %s",
                    language, e));
        }
        Map<String, CliOption> map = new LinkedHashMap<>();
        for (CliOption option : config.cliOptions()) {
            map.put(option.getOpt(), option);
        }
        return map;
    }

    public enum Type {
        CLIENT("client"), SERVER("server");

        private String name;

        Type(String name) {
            this.name = name;
        }

        String getTypeName() {
            return name;
        }
    }

    public static String generateClient(String language, GeneratorInput opts) {
        return generate(language, opts, Type.CLIENT);
    }

    public static String generateServer(String language, GeneratorInput opts) {
        return generate(language, opts, Type.SERVER);
    }

    private static String generate(String language, GeneratorInput opts, Type type) {
        LOGGER.debug(String.format(Locale.ROOT, "generate %s for %s", type.getTypeName(), language));
        if (opts == null) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "No options were supplied");
        }
        JsonNode node = opts.getSpec();
        if (node != null && "{}".equals(node.toString())) {
            LOGGER.debug("ignoring empty spec");
            node = null;
        }
        OpenAPI openapi;
        ParseOptions parseOptions = new ParseOptions();
        parseOptions.setResolve(true);
        if (node == null) {
            if (opts.getOpenAPIUrl() != null) {
                if (opts.getAuthorizationValue() != null) {
                    List<AuthorizationValue> authorizationValues = new ArrayList<>();
                    authorizationValues.add(opts.getAuthorizationValue());
                    openapi = new OpenAPIParser().readLocation(opts.getOpenAPIUrl(), authorizationValues, parseOptions).getOpenAPI();
                } else {
                    openapi = new OpenAPIParser().readLocation(opts.getOpenAPIUrl(), null, parseOptions).getOpenAPI();
                }
            } else {
                throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "No OpenAPI specification was supplied");
            }
        } else if (opts.getAuthorizationValue() != null) {
            List<AuthorizationValue> authorizationValues = new ArrayList<>();
            authorizationValues.add(opts.getAuthorizationValue());
            openapi = new OpenAPIParser().readContents(node.toString(), authorizationValues, parseOptions).getOpenAPI();

        } else {
            openapi = new OpenAPIParser().readContents(node.toString(), null, parseOptions).getOpenAPI();
        }
        if (openapi == null) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "The OpenAPI specification supplied was not valid");
        }


        // do not use opts.getOptions().get("outputFolder") as the input can contain ../../
        // to access other folders in the server
        String destPath = language + "-" + type.getTypeName();

        ClientOptInput clientOptInput = new ClientOptInput();
        String outputFolder = getTmpFolder().getAbsolutePath() + File.separator + destPath;
        String outputFilename = outputFolder + "-bundle.zip";

        clientOptInput.openAPI(openapi);

        CodegenConfig codegenConfig;
        try {
            codegenConfig = CodegenConfigLoader.forName(language);
        } catch (RuntimeException e) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Unsupported target " + language + " supplied");
        }

        if (opts.getOptions() != null) {
            codegenConfig.additionalProperties().putAll(opts.getOptions());
            codegenConfig.additionalProperties().put("openAPI", openapi);
        }

        if (opts.getOpenapiNormalizer() != null && !opts.getOpenapiNormalizer().isEmpty()) {
            for (String rule : opts.getOpenapiNormalizer()) {
                String[] ruleOperands = rule.split("=");
                if (ruleOperands.length != 2) {
                    throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "In rule: " + rule + "the operands were not provided in the form of <Rule>=<Value>");
                }
                codegenConfig.openapiNormalizer().put(ruleOperands[0], ruleOperands[1]);
            }
        }

        codegenConfig.setOutputDir(outputFolder);

        clientOptInput.config(codegenConfig);

        try {
            List<File> files = new DefaultGenerator().opts(clientOptInput).generate();
            if (files.size() > 0) {
                List<File> filesToAdd = new ArrayList<>();
                LOGGER.debug("adding to {}", outputFolder);
                filesToAdd.add(new File(outputFolder));
                ZipUtil zip = new ZipUtil();
                zip.compressFiles(filesToAdd, outputFilename);
            } else {
                throw new ResponseStatusException(HttpStatus.BAD_REQUEST,
                        "A target generation was attempted, but no files were created!");
            }
            for (File file : files) {
                try {
                    file.delete();
                } catch (Exception e) {
                    LOGGER.error("unable to delete file " + file.getAbsolutePath(), e);
                }
            }
            try {
                new File(outputFolder).delete();
            } catch (Exception e) {
                LOGGER.error("unable to delete output folder " + outputFolder, e);
            }
        } catch (Exception e) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Unable to build target: " + e.getMessage(), e);
        }
        return outputFilename;
    }

    private static File getTmpFolder() {
        try {
            File outputFolder = Files.createTempDirectory("codegen-tmp").toFile();
            outputFolder.deleteOnExit();
            return outputFolder;
        } catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException("Cannot access tmp folder");
        }
    }
}
