package org.openapitools.codegen.languages;

import org.openapitools.codegen.CodegenConstants;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.SupportingFile;

import java.io.File;

public class KotlinKtorClientCodegen extends AbstractKotlinCodegen {
    
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

        //At the point of writing this plugin kotlinx serialization does not yet support enum names that aren't exact matches.
        enumPropertyNaming = CodegenConstants.ENUM_PROPERTY_NAMING_TYPE.original;

        //TODO: find a date library/solution for kotlin native
        typeMapping.put("date-time", "kotlin.String");
        typeMapping.put("date", "kotlin.String");
        typeMapping.put("Date", "kotlin.String");
        typeMapping.put("DateTime", "kotlin.String");

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        final String infrastructureFolder = (sourceFolder + File.separator + packageName + File.separator + "infrastructure").replace(".", "/");

        //TODO: Add infrastructure classes

    }
}
