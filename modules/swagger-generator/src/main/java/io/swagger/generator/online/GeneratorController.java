package io.swagger.generator.online;

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.CodegenConfig;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.config.CodegenConfigurator;
import io.swagger.generator.model.GenerationRequest;
import io.swagger.generator.util.GeneratorUtil;
import io.swagger.generator.util.ZipUtil;
import io.swagger.oas.inflector.models.RequestContext;
import io.swagger.oas.inflector.models.ResponseContext;
import io.swagger.v3.core.util.Json;
import io.swagger.v3.core.util.Yaml;
import io.swagger.v3.parser.util.RemoteUrl;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.ServiceLoader;

public class GeneratorController {

    static Logger LOGGER = LoggerFactory.getLogger(GeneratorController.class);
    static List<String> CLIENTS = new ArrayList<>();
    static List<String> SERVERS = new ArrayList<>();

    static {
        final ServiceLoader<CodegenConfig> loader = ServiceLoader.load(CodegenConfig.class);

        loader.forEach(config -> {
            if (config.getTag().equals(CodegenType.CLIENT) || config.getTag().equals(CodegenType.DOCUMENTATION)) {
                CLIENTS.add(config.getName());
            } else if (config.getTag().equals(CodegenType.SERVER)) {
                SERVERS.add(config.getName());
            }
        });
        Collections.sort(CLIENTS, String.CASE_INSENSITIVE_ORDER);
        Collections.sort(SERVERS, String.CASE_INSENSITIVE_ORDER);
    }

    public ResponseContext getClients(RequestContext requestContext) {
        return new ResponseContext()
                .status(Response.Status.OK.getStatusCode())
                .entity(CLIENTS);

    }

    public ResponseContext getServers(RequestContext requestContext) {
        return new ResponseContext()
                .status(Response.Status.OK.getStatusCode())
                .entity(SERVERS);
    }

    public ResponseContext generateFiles(RequestContext context, String argumentsUrl) {
        final String content;

        try {
            content = RemoteUrl.urlToString(argumentsUrl, null);
        } catch (Exception e) {
            LOGGER.error("Unable to read url: " + argumentsUrl, e);
            return new ResponseContext()
                    .status(500)
                    .contentType(MediaType.APPLICATION_JSON_TYPE)
                    .entity("Could not read arguments from: " + argumentsUrl);
        }

        if (StringUtils.isBlank(content)) {
            LOGGER.error("Not arguments found from: " + argumentsUrl);
            return new ResponseContext()
                    .status(401)
                    .contentType(MediaType.APPLICATION_JSON_TYPE)
                    .entity("Not arguments found from: " + argumentsUrl);
        }

        JsonNode node = null;
        try {
            node = Json.mapper().readTree(content.getBytes());
        } catch (IOException e) {
            node = null;
        }

        if (node == null) {
            try {
                node = Yaml.mapper().readTree(content.getBytes());
            } catch (IOException e) {
                return new ResponseContext()
                        .status(500)
                        .contentType(MediaType.APPLICATION_JSON_TYPE)
                        .entity("Could not process arguments from: " + argumentsUrl);
            }
        }
        if (!node.has("lang")) {
            return new ResponseContext()
                    .status(401)
                    .contentType(MediaType.APPLICATION_JSON_TYPE)
                    .entity("property 'lang' not found on: " + argumentsUrl);
        }
        if (!node.has("spec")) {
            return new ResponseContext()
                    .status(401)
                    .contentType(MediaType.APPLICATION_JSON_TYPE)
                    .entity("property 'spec' not found on: " + argumentsUrl);
        }

        File outputRootFolder = getTmpFolder();
        File outputContentFolder = new File(outputRootFolder, "content");
        File outputFile = new File(outputRootFolder, node.findValue("lang").textValue() + "-bundle.zip");

        final ClientOptInput clientOptInput = GeneratorUtil.getClientOptInput(node, outputContentFolder.getAbsolutePath());
        return generate(clientOptInput, outputRootFolder, outputContentFolder, outputFile);
    }

    public ResponseContext generate(RequestContext context, String language, String specUrl, String library) {
        File outputRootFolder = getTmpFolder();
        File outputContentFolder = new File(outputRootFolder, "content");
        File outputFile = new File(outputRootFolder, language + "-bundle.zip");

        ClientOptInput clientOptInput = new CodegenConfigurator()
                .setLang(language)
                .setInputSpec(specUrl)
                .setOutputDir(outputContentFolder.getAbsolutePath())
                .setLibrary(library)
                .toClientOptInput();
        return generate(clientOptInput, outputRootFolder, outputContentFolder, outputFile);
    }

    public ResponseContext generate(RequestContext context, GenerationRequest generationRequest) {
        File outputRootFolder = getTmpFolder();
        File outputContentFolder = new File(outputRootFolder, "content");
        File outputFile = new File(outputRootFolder, generationRequest.getOptions().getLang() + "-bundle.zip");
        final ClientOptInput clientOptInput = GeneratorUtil.getClientOptInput(generationRequest, outputContentFolder.getAbsolutePath());
        return generate(clientOptInput, outputRootFolder, outputContentFolder, outputFile);
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

    private ResponseContext generate(ClientOptInput clientOptInput, File outputRootFolder, File outputContentFolder, File outputFile) {
        new DefaultGenerator().opts(clientOptInput).generate();

        final ZipUtil zipUtil = new ZipUtil();
        try {
            zipUtil.compressFiles(Arrays.asList(outputContentFolder.listFiles()), outputFile.getAbsolutePath());
        } catch (IOException e) {
            return new ResponseContext()
                    .status(500)
                    .contentType(MediaType.APPLICATION_JSON_TYPE)
                    .entity("Could not generate zip file.");
        }

        if (!outputFile.exists()) {
            return new ResponseContext()
                    .status(500)
                    .contentType(MediaType.APPLICATION_JSON_TYPE)
                    .entity("File was not generated.");
        }
        byte[] bytes = null;
        try {
            bytes = FileUtils.readFileToByteArray(outputFile);
        } catch (IOException ex) {
            LOGGER.error("Error converting file to bytes.", ex);
            bytes = null;
        }
        try {
            FileUtils.deleteDirectory(outputRootFolder);
        } catch (IOException ex) {
            LOGGER.error("Could not delete files.", ex);
        }
        if (bytes != null) {
            return new ResponseContext().status(200)
                    .entity(bytes)
                    .contentType(MediaType.APPLICATION_OCTET_STREAM_TYPE)
                    .header("Content-Disposition", String.format("attachment; filename=\"generated-%s\"", outputFile.getName()))
                    .header("Accept-Range", "bytes")
                    .header("Content-Length", String.valueOf(bytes.length));
        }
        return new ResponseContext()
                .status(500)
                .contentType(MediaType.APPLICATION_JSON_TYPE)
                .entity("Could not generate files.");
    }
}
