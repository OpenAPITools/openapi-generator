package org.openapitools.codegen.online.service;

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.parser.OpenAPIParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.core.models.ParseOptions;
import org.openapitools.codegen.*;
import io.swagger.v3.parser.core.models.AuthorizationValue;
import io.swagger.util.Json;
import org.openapitools.codegen.online.model.GeneratorInput;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class Generator {
    static Logger LOGGER = LoggerFactory.getLogger(Generator.class);

    public static Map<String, CliOption> getOptions(String language) {
        CodegenConfig config = null;
        try {
            config = CodegenConfigLoader.forName(language);
        } catch (Exception e) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, String.format("Unsupported target %s supplied. %s",
                    language, e));
        }
        Map<String, CliOption> map = new LinkedHashMap<String, CliOption>();
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
        LOGGER.debug(String.format("generate %s for %s", type.getTypeName(), language));
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
            List<AuthorizationValue> authorizationValues = new ArrayList<AuthorizationValue>();
            authorizationValues.add(opts.getAuthorizationValue());
            openapi = new OpenAPIParser().readContents(node.toString(), authorizationValues, parseOptions).getOpenAPI();

        } else {
            openapi = new OpenAPIParser().readContents(node.toString(), null, parseOptions).getOpenAPI();
        }
        if (openapi == null) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "The OpenAPI specification supplied was not valid");
        }

        String destPath = null;

        if (opts != null && opts.getOptions() != null) {
            destPath = opts.getOptions().get("outputFolder");
        }
        if (destPath == null) {
            destPath = language + "-" + type.getTypeName();
        }

        ClientOptInput clientOptInput = new ClientOptInput();
        ClientOpts clientOpts = new ClientOpts();
        String outputFolder = getTmpFolder().getAbsolutePath() + File.separator + destPath;
        String outputFilename = outputFolder + "-bundle.zip";

        clientOptInput.opts(clientOpts).openAPI(openapi);

        CodegenConfig codegenConfig = null;
        try {
            codegenConfig = CodegenConfigLoader.forName(language);
        } catch (RuntimeException e) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Unsupported target " + language + " supplied");
        }

        if (opts.getOptions() != null) {
            codegenConfig.additionalProperties().putAll(opts.getOptions());
            codegenConfig.additionalProperties().put("openAPI", openapi);
        }

        codegenConfig.setOutputDir(outputFolder);

        LOGGER.debug(Json.pretty(clientOpts));

        clientOptInput.setConfig(codegenConfig);

        try {
            List<File> files = new DefaultGenerator().opts(clientOptInput).generate();
            if (files.size() > 0) {
                List<File> filesToAdd = new ArrayList<File>();
                LOGGER.debug("adding to " + outputFolder);
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

    protected static File getTmpFolder() {
        try {
            File outputFolder = File.createTempFile("codegen-", "-tmp");
            outputFolder.delete();
            outputFolder.mkdir();
            outputFolder.deleteOnExit();
            return outputFolder;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
}
