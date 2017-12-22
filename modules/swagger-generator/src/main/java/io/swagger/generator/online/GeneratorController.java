package io.swagger.generator.online;

import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.config.CodegenConfigurator;
import io.swagger.generator.model.GenerationRequest;
import io.swagger.generator.util.ZipUtil;
import io.swagger.oas.inflector.models.RequestContext;
import io.swagger.oas.inflector.models.ResponseContext;
import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.ws.rs.core.MediaType;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;

public class GeneratorController {

    static Logger LOGGER = LoggerFactory.getLogger(GeneratorController.class);


    public ResponseContext generate(RequestContext context, String language, String specUrl, String library) {
        return generate(context, new GenerationRequest()
                .language(language)
                .library(library)
                .specUrl(specUrl));
    }

    public ResponseContext generate(RequestContext context, GenerationRequest generationRequest) {

        File outputRootFolder = getTmpFolder();
        File outputContentFolder = new File(outputRootFolder, "content");
        File outputFile = new File(outputRootFolder, generationRequest.getLanguage() + "-bundle.zip");

        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setLang(generationRequest.getLanguage())
                .setLibrary(generationRequest.getLibrary())
                .setInputSpec(generationRequest.getSpecUrl())
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
