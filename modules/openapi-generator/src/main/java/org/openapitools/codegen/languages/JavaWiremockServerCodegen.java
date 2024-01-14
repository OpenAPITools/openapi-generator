package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;

import java.io.File;
import java.util.*;

import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;

public class JavaWiremockServerCodegen extends AbstractJavaCodegen implements CodegenConfig {

    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    public String getName() {
        return "java-wiremock";
    }

    public String getHelp() {
        return "Generates Java Wiremock stubs, requests and responses samples.";
    }

    public JavaWiremockServerCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        outputFolder = "generated-code" + File.separator + "java";
        embeddedTemplateDir = templateDir = "java-wiremock";
        invokerPackage = "org.openapitools.mockserver";
        artifactId = "openapi-java-mockserver";
        apiPackage = "org.openapitools.mockserver.api";

        apiDocTemplateFiles = new HashMap<>();
        apiTestTemplateFiles = new HashMap<>();
        modelDocTemplateFiles = new HashMap<>();
        modelTemplateFiles = new HashMap<>();

        apiTemplateFiles.clear();
        apiTemplateFiles.put("wiremock.mustache", "MockServer.java");

        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml")
                .doNotOverwrite());

        // Ensure the OAS 3.x discriminator mappings include any descendent schemas that allOf
        // inherit from self, any oneOf schemas, any anyOf schemas, any x-discriminator-values,
        // and the discriminator mapping schemas in the OAS document.
        this.setLegacyDiscriminatorBehavior(false);

    }
}
