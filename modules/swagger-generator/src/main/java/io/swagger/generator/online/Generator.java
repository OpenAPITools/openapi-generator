package io.swagger.generator.online;

import com.fasterxml.jackson.databind.JsonNode;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.ClientOpts;
import io.swagger.codegen.Codegen;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenConfigLoader;
import io.swagger.generator.exception.ApiException;
import io.swagger.generator.exception.BadRequestException;
import io.swagger.generator.model.GeneratorInput;
import io.swagger.generator.model.InputOption;
import io.swagger.generator.util.ZipUtil;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;
import io.swagger.util.Json;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class Generator {
    static Logger LOGGER = LoggerFactory.getLogger(Generator.class);

    public static Map<String, CliOption> getOptions(String language) throws ApiException {
        CodegenConfig config = null;
        try {
            config = CodegenConfigLoader.forName(language);
        } catch (Exception e) {
            throw new BadRequestException(String.format("Unsupported target %s supplied. %s", language, e));
        }
        Map<String, CliOption> map = new LinkedHashMap<String, CliOption>();
        for (CliOption option : config.cliOptions()) {
            map.put(option.getOpt(), option);
        }
        return map;
    }

    public enum Type {
        CLIENT("client"),
        SERVER("server");

        private String name;

        Type(String name) {
            this.name = name;
        }

        String getTypeName() {
            return name;
        }
    }

    public static String generateClient(String language, GeneratorInput opts) throws ApiException {
        return generate(language, opts, Type.CLIENT);
    }

    public static String generateServer(String language, GeneratorInput opts) throws ApiException {
        return generate(language, opts, Type.SERVER);
    }

    private static String generate(String language, GeneratorInput opts, Type type) throws ApiException {
        LOGGER.debug(String.format("generate %s for %s", type.getTypeName(), language));
        if (opts == null) {
            throw new BadRequestException("No options were supplied");
        }
        JsonNode node = opts.getSpec();
        if(node != null && "{}".equals(node.toString())) {
            LOGGER.debug("ignoring empty spec");
            node = null;
        }
        Swagger swagger;
        if (node == null) {
            if (opts.getSwaggerUrl() != null) {
                swagger = new SwaggerParser().read(opts.getSwaggerUrl());
            } else {
                throw new BadRequestException("No swagger specification was supplied");
            }
        } else {
            swagger = new SwaggerParser().read(node, true);
        }
        if (swagger == null) {
            throw new BadRequestException("The swagger specification supplied was not valid");
        }

        ClientOptInput clientOptInput = new ClientOptInput();
        ClientOpts clientOpts = new ClientOpts();
        String outputFolder = getTmpFolder().getAbsolutePath() + File.separator + language + "-"
                + type.getTypeName();
        String outputFilename = outputFolder + "-bundle.zip";

        clientOptInput
                .opts(clientOpts)
                .swagger(swagger);

        CodegenConfig codegenConfig=null;
        try {
            codegenConfig = CodegenConfigLoader.forName(language);
        } catch(RuntimeException e) {
            throw new BadRequestException("Unsupported target " + language + " supplied");
        }

        if (opts.getOptions() != null) {
            codegenConfig.additionalProperties().putAll(opts.getOptions());
            codegenConfig.additionalProperties().put("swagger", swagger);
        }

        codegenConfig.setOutputDir(outputFolder);

        LOGGER.debug(Json.pretty(clientOpts));

        clientOptInput.setConfig(codegenConfig);

        try {
            List<File> files = new Codegen().opts(clientOptInput).generate();
            if (files.size() > 0) {
                List<File> filesToAdd = new ArrayList<File>();
                LOGGER.debug("adding to " + outputFolder);
                filesToAdd.add(new File(outputFolder));
                ZipUtil zip = new ZipUtil();
                zip.compressFiles(filesToAdd, outputFilename);
            } else {
                throw new BadRequestException("A target generation was attempted, but no files were created!");
            }
            for(File file: files) {
                try {
                    file.delete();
                }
                catch(Exception e) {
                    LOGGER.error("unable to delete file " + file.getAbsolutePath());
                }
            }
            try {
                new File(outputFolder).delete();
            }
            catch (Exception e) {
                LOGGER.error("unable to delete output folder " + outputFolder);
            }
        } catch (Exception e) {
            throw new BadRequestException("Unable to build target: " + e.getMessage());
        }
        return outputFilename;
    }

    public static InputOption clientOptions(@SuppressWarnings("unused") String language) {
        return null;
    }

    public static InputOption serverOptions(@SuppressWarnings("unused") String language) {
        return null;
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
