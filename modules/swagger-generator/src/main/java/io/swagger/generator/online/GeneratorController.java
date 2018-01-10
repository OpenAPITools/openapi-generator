package io.swagger.generator.online;

import com.fasterxml.jackson.databind.JsonNode;
import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.config.CodegenConfigurator;
import io.swagger.generator.model.GenerationRequest;
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
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class GeneratorController {

    static Logger LOGGER = LoggerFactory.getLogger(GeneratorController.class);

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

        GenerationRequest generationRequest = new GenerationRequest()
                .lang(node.findValue("lang").textValue())
                .spec(node.findValue("spec").textValue());

        if (node.has("library")) {
            generationRequest.setLibrary(node.findValue("library").textValue());
        }
        JsonNode propertiesNode = node.findValue("additionalProperties");
        if (propertiesNode != null && propertiesNode.isArray()) {
            Map<String, Object> additionalProperties = new HashMap();
            for (JsonNode jsonNode : propertiesNode) {
                String value = jsonNode.textValue();
                if (value.contains("=")) {
                    String[] values = value.split("=");
                    additionalProperties.put(values[0], values[1]);
                } else {
                    additionalProperties.put(value, Boolean.TRUE);
                }
            }
        }
        return generate(context, generationRequest);
    }

    public ResponseContext generate(RequestContext context, String language, String specUrl, String library) {
        return generate(context, new GenerationRequest()
                .lang(language)
                .library(library)
                .spec(specUrl));
    }

    public ResponseContext generate(RequestContext context, GenerationRequest generationRequest) {

        File outputRootFolder = getTmpFolder();
        File outputContentFolder = new File(outputRootFolder, "content");
        File outputFile = new File(outputRootFolder, generationRequest.getLang() + "-bundle.zip");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setLang(generationRequest.getLang())
                .setLibrary(generationRequest.getLibrary())
                .setInputSpec(generationRequest.getSpec())
                .setOutputDir(outputContentFolder.getAbsolutePath());

        if (generationRequest.getAdditionalProperties() != null && !generationRequest.getAdditionalProperties().isEmpty()) {
            for (String key : generationRequest.getAdditionalProperties().keySet()) {
                final Object value = generationRequest.getAdditionalProperties().get(key);
                configurator.addAdditionalProperty(key, value);
            }
        }

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
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
