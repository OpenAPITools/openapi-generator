package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.models.Operation;

import java.io.File;
import java.util.*;

public class JavaJerseyServerCodegen extends AbstractJavaJAXRSServerCodegen
{

    public JavaJerseyServerCodegen()
    {
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
        library.setEnum(supportedLibraries);

        cliOptions.add(library);
        cliOptions.add(new CliOption(CodegenConstants.IMPL_FOLDER, CodegenConstants.IMPL_FOLDER_DESC));
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
    public void processOpts()
    {
        super.processOpts();

        if ( additionalProperties.containsKey(CodegenConstants.IMPL_FOLDER) ) {
            implFolder = (String) additionalProperties.get(CodegenConstants.IMPL_FOLDER);
        }

        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("ApiException.mustache", (sourceFolder + '/' + apiPackage).replace(".", "/"), "ApiException.java"));
        supportingFiles.add(new SupportingFile("ApiOriginFilter.mustache", (sourceFolder + '/' + apiPackage).replace(".", "/"), "ApiOriginFilter.java"));
        supportingFiles.add(new SupportingFile("ApiResponseMessage.mustache", (sourceFolder + '/' + apiPackage).replace(".", "/"), "ApiResponseMessage.java"));
        supportingFiles.add(new SupportingFile("NotFoundException.mustache", (sourceFolder + '/' + apiPackage).replace(".", "/"), "NotFoundException.java"));
        supportingFiles.add(new SupportingFile("web.mustache", ("src/main/webapp/WEB-INF"), "web.xml"));
        supportingFiles.add(new SupportingFile("StringUtil.mustache", (sourceFolder + '/' + apiPackage).replace(".", "/"), "StringUtil.java"));

        if ( additionalProperties.containsKey("dateLibrary") ) {
            setDateLibrary(additionalProperties.get("dateLibrary").toString());
            additionalProperties.put(dateLibrary, "true");
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
}
