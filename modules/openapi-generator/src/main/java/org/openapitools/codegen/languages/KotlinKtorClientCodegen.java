package org.openapitools.codegen.languages;

import org.openapitools.codegen.*;

import java.io.File;
import java.util.HashMap;

public class KotlinKtorClientCodegen extends AbstractKotlinCodegen {

    // By default, generated code is considered public
    private boolean nonPublicApi = Boolean.FALSE;

    public KotlinKtorClientCodegen() {
        super();

        artifactId = "kotlin-ktor-client";
        packageName = "org.openapitools.client";

        outputFolder = "generated-code" + File.separator + "kotlin-ktor-client";
        modelTemplateFiles.put("model.mustache", ".kt");
        apiTemplateFiles.put("api.mustache", ".kt");
        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");
        embeddedTemplateDir = templateDir = "kotlin-ktor-client";
        //These should already be taken care off by AbstractKotlinCodegen TODO: double-check
        apiPackage = packageName + ".apis";
        modelPackage = packageName + ".models";

        typeMapping.put("number", "kotlin.Double");
        //TODO: find a date library/solution for kotlin native
        typeMapping.put("date-time", "kotlin.String");
        typeMapping.put("date", "kotlin.String");
        typeMapping.put("Date", "kotlin.String");
        typeMapping.put("DateTime", "kotlin.String");

        //TODO: change to something else that works with kotlin native (without reflection)
        typeMapping.put("file", "kotlin.String");
        typeMapping.put("object", "kotlin.String"); //Using 'Any' fails on platforms without reflection

        importMapping = new HashMap<>();

        addSwitch(CodegenConstants.NON_PUBLIC_API, CodegenConstants.NON_PUBLIC_API_DESC, this.nonPublicApi);
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "kotlin-ktor-client";
    }

    @Override
    public String getHelp() {
        return "Generates a Kotline-Native client using ktor for http requests and kotlinx serialization for models.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CodegenConstants.NON_PUBLIC_API)) {
            nonPublicApi = convertPropertyToBooleanAndWriteBack(CodegenConstants.NON_PUBLIC_API);
        } else {
            additionalProperties.put(CodegenConstants.NON_PUBLIC_API, nonPublicApi);
        }

        //At the point of writing this plugin kotlinx serialization does not yet support enum names that aren't exact matches.
        enumPropertyNaming = CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.original;

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        final String infrastructureFolder = (sourceFolder + File.separator + packageName + File.separator + "infrastructure").replace(".", "/");
        supportingFiles.add(new SupportingFile("infrastructure/ErrorWrapper.kt.mustache", infrastructureFolder, "ErrorWrapper.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/HttpClientConvenience.kt.mustache", infrastructureFolder, "HttpClientConvenience.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/authentication/AuthProvider.kt.mustache", infrastructureFolder+"/authentication", "AuthProvider.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/authentication/Base64.kt.mustache", infrastructureFolder+"/authentication", "Base64.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/authentication/HttpBasicAuth.kt.mustache", infrastructureFolder+"/authentication", "HttpBasicAuth.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/authentication/ApiKeyAuth.kt.mustache", infrastructureFolder+"/authentication", "ApiKeyAuth.kt"));
        supportingFiles.add(new SupportingFile("infrastructure/authentication/OAuth.kt.mustache", infrastructureFolder+"/authentication", "OAuth.kt"));
    }

    @Override
    public String toModelImport(String name) {
        if (name.startsWith("io.ktor.")) {
            return name;
        }

        return super.toModelImport(name);
    }
}
