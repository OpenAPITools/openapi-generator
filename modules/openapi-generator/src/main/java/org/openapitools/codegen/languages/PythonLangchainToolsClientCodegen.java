package org.openapitools.codegen.languages;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;

import org.openapitools.codegen.SupportingFile;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PythonLangchainToolsClientCodegen extends PythonClientCodegen {
    public static final String PROJECT_NAME = "projectName";

    private final Logger LOGGER = LoggerFactory.getLogger(PythonLangchainToolsClientCodegen.class);

    public PythonLangchainToolsClientCodegen() {
        super();
        apiTemplateFiles.put("api_tools.mustache", "_tools.py");
        supportingFiles.add(new SupportingFile("all_tools.mustache", "", "all_tools.py"));
    }

    @Override
    public String getName() {
        return "python-langchain-tools";
    }

    @Override
    public String getHelp() {
        return "Generates a Python client for LangChain agent tools, grouped by tags.";
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // Now that packageName is processed and available, we can add the supporting file
        // for the tools package __init__.py in the correct directory.
        final String toolsPackagePath = packageName.replace(".", File.separator) + File.separator + "tools";
        supportingFiles.add(new SupportingFile("init.mustache", toolsPackagePath, "__init__.py"));
    }

    @Override
    public boolean isEnablePostProcessFile() {
        return true;
    }

    /**
     * This is a way to add a dependency without duplicating the parent template.
     */
    @Override
    public void postProcessFile(File file, String fileType) {
        super.postProcessFile(file, fileType);
        if (file == null) {
            return;
        }
        final String filename = file.getName();
        if ("requirements.txt".equals(filename)) {
            try {
                String langchainDep = "\nlangchain >=0.3, <0.4\n";
                Files.write(file.toPath(), langchainDep.getBytes(StandardCharsets.UTF_8), StandardOpenOption.APPEND);
            } catch (IOException e) {
                throw new RuntimeException("Unable to write to requirements.txt", e);
            }
        }
    }

    /**
     * Overriding this method to change the output location of our tool files.
     */
    @Override
    public String apiFilename(String templateName, String tag) {
        final String originalFilename = super.apiFilename(templateName, tag);

        if ("api_tools.mustache".equals(templateName)) {
            return originalFilename.replace(apiPackage().replace(".", File.separator),
                packageName.replace(".", File.separator) + File.separator + "tools");
        }

        return originalFilename;
    }

}
