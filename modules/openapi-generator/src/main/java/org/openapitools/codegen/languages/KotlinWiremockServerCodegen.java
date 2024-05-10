package org.openapitools.codegen.languages;

import org.openapitools.codegen.CodegenType;

public class KotlinWiremockServerCodegen extends AbstractKotlinCodegen {

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "kotlin-wiremock";
    }

    @Override
    public String getHelp() {
        return "Generates Kotlin WireMock stubs, requests and responses samples.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        apiTemplateFiles.put("api.mustache", "Stub.kt");
    }
}
