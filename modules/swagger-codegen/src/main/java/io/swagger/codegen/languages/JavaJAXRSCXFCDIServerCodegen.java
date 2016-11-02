package io.swagger.codegen.languages;

import io.swagger.codegen.*;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenProperty;

import java.io.File;

public class JavaJAXRSCXFCDIServerCodegen extends JavaJAXRSSpecServerCodegen {
    public JavaJAXRSCXFCDIServerCodegen() {
        outputFolder = "generated-code/JavaJaxRS-CXF-CDI";
        artifactId = "swagger-jaxrs-cxf-cdi-server";
        sourceFolder = "src" + File.separator + "gen" + File.separator + "java";

        // Three API templates to support CDI injection
        apiTemplateFiles.put("apiService.mustache", ".java");
        apiTemplateFiles.put("apiServiceImpl.mustache", ".java");

        // Use standard types
        typeMapping.put("DateTime", "java.util.Date");

        // Updated template directory
        embeddedTemplateDir = templateDir = JAXRS_TEMPLATE_DIRECTORY_NAME + File.separator + "cxf-cdi";
    }

    @Override
    public String getName() {
        return "jaxrs-cxf-cdi";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        supportingFiles.clear(); // Don't need extra files provided by AbstractJAX-RS & Java Codegen
        writeOptional(outputFolder, new SupportingFile("pom.mustache", "", "pom.xml"));
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);

        // Reinstate JsonProperty
        model.imports.add("JsonProperty");
    }

    @Override
    public String getHelp() {
        return "Generates a Java JAXRS Server according to JAXRS 2.0 specification, assuming an Apache CXF runtime and a Java EE runtime with CDI enabled.";
    }

}
