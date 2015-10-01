package io.swagger.generator.online;

import com.fasterxml.jackson.databind.JsonNode;
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

import static org.apache.commons.lang3.StringUtils.isNotEmpty;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class Generator {
    static Logger LOGGER = LoggerFactory.getLogger(Generator.class);

    public static String generateClient(String language, GeneratorInput opts) throws ApiException {
        Swagger swagger;
        LOGGER.debug("generate client for " + language);
        if (opts == null) {
            throw new BadRequestException(400, "No options were supplied");
        }
        JsonNode node = opts.getSpec();
        if (node == null) {
            if (opts.getSwaggerUrl() != null) {
                swagger = new SwaggerParser().read(opts.getSwaggerUrl());
            } else {
                throw new BadRequestException(400, "No swagger specification was supplied");
            }
        } else {
            swagger = new SwaggerParser().read(node, true);
        }
        if (swagger == null) {
            throw new BadRequestException(400, "The swagger specification supplied was not valid");
        }

        ClientOptInput clientOptInput = new ClientOptInput();
        ClientOpts clientOpts = new ClientOpts();
        String outputFolder = getTmpFolder().getAbsolutePath() + File.separator + language + "-client";
        String outputFilename = outputFolder + "-bundle.zip";

        clientOptInput
                .opts(clientOpts)
                .swagger(swagger);

        CodegenConfig codegenConfig=null;
        try {
            codegenConfig = CodegenConfigLoader.forName(language);
        } catch(RuntimeException e) {
            throw new BadRequestException(400, "Unsupported target " + language + " supplied");
        }
        
        if (opts.getOptions() != null) {
            for(String key : new String[]{"apiPackage", "modelPackage", "invokerPackage", "groupId", "artifactId", "artifactVersion"}) {
                if(isNotEmpty(opts.getOptions().get(key))) {
                    codegenConfig.additionalProperties().put(key , opts.getOptions().get(key));
                }
            }
            
            if (isNotEmpty(opts.getOptions().get("library"))) {
                codegenConfig.setLibrary(opts.getOptions().get("library"));
            }
        }

        codegenConfig.setOutputDir(outputFolder);

        Json.prettyPrint(clientOpts);

        clientOptInput.setConfig(codegenConfig);

        try {
            List<File> files = new Codegen().opts(clientOptInput).generate();
            if (files.size() > 0) {
                List<File> filesToAdd = new ArrayList<File>();
                System.out.println("adding to " + outputFolder);
                filesToAdd.add(new File(outputFolder));
                ZipUtil zip = new ZipUtil();
                zip.compressFiles(filesToAdd, outputFilename);
            } else {
                throw new BadRequestException(400, "A target generation was attempted, but no files were created!");
            }
        } catch (Exception e) {
            throw new BadRequestException(500, "Unable to build target: " + e.getMessage());
        }
        return outputFilename;
    }

    public static String generateServer(String language, GeneratorInput opts) throws ApiException {
        LOGGER.debug("generate server for " + language);
        Swagger swagger;
        if (opts == null) {
            throw new BadRequestException(400, "No options were supplied");
        }
        if (opts == null) {
            throw new BadRequestException(400, "No options were supplied");
        }
        JsonNode node = opts.getSpec();
        if (node == null) {
            if (opts.getSwaggerUrl() != null) {
                swagger = new SwaggerParser().read(opts.getSwaggerUrl());
            } else {
                throw new BadRequestException(400, "No swagger specification was supplied");
            }
        } else {
            swagger = new SwaggerParser().read(node, true);
        }
        if (swagger == null) {
            throw new BadRequestException(400, "The swagger specification supplied was not valid");
        }

        ClientOptInput clientOptInput = new ClientOptInput();
        ClientOpts clientOpts = new ClientOpts();
        String outputFolder = getTmpFolder().getAbsolutePath() + File.separator + language + "-server";
        String outputFilename = outputFolder + "-bundle.zip";

        clientOptInput
                .opts(clientOpts)
                .swagger(swagger);

        CodegenConfig codegenConfig = Codegen.getConfig(language);
        if (codegenConfig == null) {
            throw new BadRequestException(400, "Unsupported target " + language + " supplied");
        }

        codegenConfig.setOutputDir(outputFolder);

        Json.prettyPrint(clientOpts);

        clientOptInput.setConfig(codegenConfig);

        try {
            List<File> files = new Codegen().opts(clientOptInput).generate();
            if (files.size() > 0) {
                List<File> filesToAdd = new ArrayList<File>();
                filesToAdd.add(new File(outputFolder));
                ZipUtil zip = new ZipUtil();
                zip.compressFiles(filesToAdd, outputFilename);
            } else {
                throw new BadRequestException(400, "A target generation was attempted, but no files were created!");
            }
        } catch (Exception e) {
            throw new BadRequestException(500, "Unable to build target: " + e.getMessage());
        }
        return outputFilename;
    }

    public static InputOption clientOptions(String language) {
        return null;
    }

    public static InputOption serverOptions(String language) {
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
