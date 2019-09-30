package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;
import io.swagger.models.properties.ArrayProperty;
import io.swagger.models.properties.MapProperty;
import io.swagger.models.properties.Property;
import io.swagger.models.parameters.Parameter;

import java.io.File;
import java.util.*;

import org.apache.commons.lang3.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FsharpFunctionsServerCodegen extends AbstractFSharpCodegen {
    public static final String PROJECT_NAME = "projectName";

    static Logger LOGGER = LoggerFactory.getLogger(FsharpFunctionsServerCodegen.class);

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "fsharp-functions";
    }

    public String getHelp() {
        return "Generates a fsharp-functions server.";
    }

    public FsharpFunctionsServerCodegen() {
        super();

        // CLI options
        addOption(CodegenConstants.LICENSE_URL,
                CodegenConstants.LICENSE_URL_DESC,
                licenseUrl);

        addOption(CodegenConstants.LICENSE_NAME,
                CodegenConstants.LICENSE_NAME_DESC,
                licenseName);

        addOption(CodegenConstants.PACKAGE_COPYRIGHT,
                CodegenConstants.PACKAGE_COPYRIGHT_DESC,
                packageCopyright);

        addOption(CodegenConstants.PACKAGE_AUTHORS,
                CodegenConstants.PACKAGE_AUTHORS_DESC,
                packageAuthors);

        addOption(CodegenConstants.PACKAGE_TITLE,
                CodegenConstants.PACKAGE_TITLE_DESC,
                packageTitle);

        addOption(CodegenConstants.PACKAGE_NAME,
                "F# module name (convention: Title.Case).",
                packageName);

        addOption(CodegenConstants.PACKAGE_VERSION,
                "F# package version.",
                packageVersion);

        addOption(CodegenConstants.OPTIONAL_PROJECT_GUID,
                CodegenConstants.OPTIONAL_PROJECT_GUID_DESC,
                null);

        addOption(CodegenConstants.SOURCE_FOLDER,
                  CodegenConstants.SOURCE_FOLDER_DESC,
                  sourceFolder);
    }
    
    @Override
    public void processOpts() {
        super.processOpts();

        modelPackage = "Model";
        embeddedTemplateDir = templateDir = "fsharp-functions-server";

        apiTemplateFiles.put("Handler.mustache", "Handler.fs");    
        apiTemplateFiles.put("HandlerParams.mustache", "HandlerParams.fs"); 
        apiTemplateFiles.put("ServiceInterface.mustache", "ServiceInterface.fs"); 
        apiTemplateFiles.put("ServiceImpl.mustache", "Service.fs"); 
        modelTemplateFiles.put("Model.mustache", ".fs");

        String implFolder = sourceFolder + File.separator + "impl";

        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("build.sh.mustache", projectFolder, "build.sh"));
        supportingFiles.add(new SupportingFile("build.bat.mustache", projectFolder, "build.bat"));
        supportingFiles.add(new SupportingFile("host.json", "", "host.json"));
        supportingFiles.add(new SupportingFile("local.settings.json", "", "local.settings.json"));
        supportingFiles.add(new SupportingFile("Project.fsproj.mustache", projectFolder, packageName + ".fsproj"));


    }

    @Override
    public String modelFileFolder() {
      return super.modelFileFolder().replace("Model","model");
    }
    
    @Override
    public String apiFileFolder() {
        return super.apiFileFolder() + File.separator + "api";
    }

    private String implFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + "impl";
    }

    @Override()
    public String toModelImport(String name) {
      return packageName + "." + modelPackage() + "." + name;
    }

    @Override
    public String apiFilename(String templateName, String tag) {
        String result = super.apiFilename(templateName, tag);
        if (templateName.endsWith("Impl.mustache")) {
            int ix = result.lastIndexOf(File.separatorChar);
            result = result.substring(0, ix) + result.substring(ix, result.length() - 2) + "fs";
            result = result.replace(apiFileFolder(), implFileFolder());
        }
        return result;
    }
}
