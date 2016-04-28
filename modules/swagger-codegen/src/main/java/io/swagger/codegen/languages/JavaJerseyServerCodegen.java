package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.Operation;

import java.io.File;
import java.util.*;

public class JavaJerseyServerCodegen extends AbstractJavaJAXRSServerCodegen {
    public JavaJerseyServerCodegen() {
        super();

        sourceFolder = "src/gen/java";
        invokerPackage = "io.swagger.api";
        artifactId = "swagger-jaxrs-server";
        outputFolder = "generated-code/JavaJaxRS-Jersey";

        modelTemplateFiles.put("model.mustache", ".java");
        apiTemplateFiles.put("api.mustache", ".java");
        apiTemplateFiles.put("apiService.mustache", ".java");
        apiTemplateFiles.put("apiServiceImpl.mustache", ".java");
        apiTemplateFiles.put("apiServiceFactory.mustache", ".java");

        apiPackage = "io.swagger.api";
        modelPackage = "io.swagger.model";

        additionalProperties.put("title", title);

        embeddedTemplateDir = templateDir = JAXRS_TEMPLATE_DIRECTORY_NAME + File.separator + "jersey1_18";

        for ( int i = 0; i < cliOptions.size(); i++ ) {
            if ( CodegenConstants.LIBRARY.equals(cliOptions.get(i).getOpt()) ) {
                cliOptions.remove(i);
                break;
            }
        }

        CliOption library = new CliOption(CodegenConstants.LIBRARY, "library template (sub-template) to use");
        library.setDefault(DEFAULT_LIBRARY);

        Map<String, String> supportedLibraries = new LinkedHashMap<String, String>();

        supportedLibraries.put(DEFAULT_LIBRARY, "Jersey core 1.18.1");
        supportedLibraries.put("jersey2", "Jersey core 2.x");
        library.setEnum(supportedLibraries);

        cliOptions.add(library);
        cliOptions.add(new CliOption(CodegenConstants.IMPL_FOLDER, CodegenConstants.IMPL_FOLDER_DESC));
        cliOptions.add(new CliOption("title", "a title describing the application"));
    }

    @Override
    public String getName()
    {
        return "jaxrs"; // TODO should be renamed as "jaxrs-jersey"
    }

    @Override
    public String getHelp()
    {
        return "Generates a Java JAXRS Server application based on Jersey framework.";
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        if("null".equals(property.example)) {
            property.example = null;
        }
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // clear model and api doc template as this codegen 
        // does not support auto-generated markdown doc at the moment
        modelDocTemplateFiles.remove("model_doc.mustache");
        apiDocTemplateFiles.remove("api_doc.mustache");

        if ( additionalProperties.containsKey(CodegenConstants.IMPL_FOLDER) ) {
            implFolder = (String) additionalProperties.get(CodegenConstants.IMPL_FOLDER);
        }

        supportingFiles.clear();
        writeOptional(outputFolder, new SupportingFile("pom.mustache", "", "pom.xml"));
        writeOptional(outputFolder, new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("ApiException.mustache", (sourceFolder + '/' + apiPackage).replace(".", "/"), "ApiException.java"));
        supportingFiles.add(new SupportingFile("ApiOriginFilter.mustache", (sourceFolder + '/' + apiPackage).replace(".", "/"), "ApiOriginFilter.java"));
        supportingFiles.add(new SupportingFile("ApiResponseMessage.mustache", (sourceFolder + '/' + apiPackage).replace(".", "/"), "ApiResponseMessage.java"));
        supportingFiles.add(new SupportingFile("NotFoundException.mustache", (sourceFolder + '/' + apiPackage).replace(".", "/"), "NotFoundException.java"));
        supportingFiles.add(new SupportingFile("jacksonJsonProvider.mustache", (sourceFolder + '/' + apiPackage).replace(".", "/"), "JacksonJsonProvider.java"));
        writeOptional(outputFolder, new SupportingFile("bootstrap.mustache", (implFolder + '/' + apiPackage).replace(".", "/"), "Bootstrap.java"));

        writeOptional(outputFolder, new SupportingFile("web.mustache", ("src/main/webapp/WEB-INF"), "web.xml"));
        supportingFiles.add(new SupportingFile("StringUtil.mustache", (sourceFolder + '/' + apiPackage).replace(".", "/"), "StringUtil.java"));

        if ( additionalProperties.containsKey("dateLibrary") ) {
            setDateLibrary(additionalProperties.get("dateLibrary").toString());
            additionalProperties.put(dateLibrary, "true");
        }
        if(DEFAULT_LIBRARY.equals(library) || library == null) {
            if(templateDir.startsWith(JAXRS_TEMPLATE_DIRECTORY_NAME)) {
                // set to the default location
                templateDir = JAXRS_TEMPLATE_DIRECTORY_NAME + File.separator + "jersey1_18";
            }
            else {
                templateDir += File.separator + "jersey1_18";
            }
        }
        if("jersey2".equals(library)) {
            if(templateDir.startsWith(JAXRS_TEMPLATE_DIRECTORY_NAME)) {
                // set to the default location
                templateDir = JAXRS_TEMPLATE_DIRECTORY_NAME + File.separator + "jersey2";
            }
            else {
                templateDir += File.separator + "jersey2";
            }
        }

        if ( "joda".equals(dateLibrary) ) {
            supportingFiles.add(new SupportingFile("JodaDateTimeProvider.mustache", (sourceFolder + '/' + apiPackage).replace(".", "/"), "JodaDateTimeProvider.java"));
            supportingFiles.add(new SupportingFile("JodaLocalDateProvider.mustache", (sourceFolder + '/' + apiPackage).replace(".", "/"), "JodaLocalDateProvider.java"));
        } else if ( "java8".equals(dateLibrary) ) {
            supportingFiles.add(new SupportingFile("LocalDateTimeProvider.mustache", (sourceFolder + '/' + apiPackage).replace(".", "/"), "LocalDateTimeProvider.java"));
            supportingFiles.add(new SupportingFile("LocalDateProvider.mustache", (sourceFolder + '/' + apiPackage).replace(".", "/"), "LocalDateProvider.java"));
        }
    }

    @Override
    public void addOperationToGroup(String tag, String resourcePath, Operation operation, CodegenOperation co, Map<String, List<CodegenOperation>> operations) {
        String basePath = resourcePath;
        if (basePath.startsWith("/")) {
            basePath = basePath.substring(1);
        }
        int pos = basePath.indexOf("/");
        if (pos > 0) {
            basePath = basePath.substring(0, pos);
        }

        if (basePath == "") {
            basePath = "default";
        } else {
            if (co.path.startsWith("/" + basePath)) {
                co.path = co.path.substring(("/" + basePath).length());
            }
            co.subresourceOperation = !co.path.isEmpty();
        }
        List<CodegenOperation> opList = operations.get(basePath);
        if (opList == null) {
            opList = new ArrayList<CodegenOperation>();
            operations.put(basePath, opList);
        }
        opList.add(co);
        co.baseName = basePath;
    }

    public void hideGenerationTimestamp(boolean hideGenerationTimestamp) {
        this.hideGenerationTimestamp = hideGenerationTimestamp;
    }
}
