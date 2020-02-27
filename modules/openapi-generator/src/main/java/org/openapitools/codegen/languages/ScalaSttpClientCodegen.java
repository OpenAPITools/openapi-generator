package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.servers.Server;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.SupportingFile;

import java.io.File;
import java.util.List;

public class ScalaSttpClientCodegen extends ScalaAkkaClientCodegen implements CodegenConfig {
    protected String mainPackage = "org.openapitools.client";

    public ScalaSttpClientCodegen() {
        super();
    }


    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey("mainPackage")) {
            setMainPackage((String) additionalProperties.get("mainPackage"));
            additionalProperties.replace("configKeyPath", this.configKeyPath);
            apiPackage = mainPackage + ".api";
            modelPackage = mainPackage + ".model";
            invokerPackage = mainPackage + ".core";
            additionalProperties.put("apiPackage", apiPackage);
            additionalProperties.put("modelPackage", modelPackage);
        }

        if (!additionalProperties.containsKey("java8")) {
            additionalProperties.put("joda", "true");
        }

        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt"));
        final String invokerFolder = (sourceFolder + File.separator + invokerPackage).replace(".", File.separator);
        supportingFiles.add(new SupportingFile("requests.mustache", invokerFolder, "requests.scala"));
        supportingFiles.add(new SupportingFile("apiInvoker.mustache", invokerFolder, "ApiInvoker.scala"));
        final String apiFolder = (sourceFolder + File.separator + apiPackage).replace(".", File.separator);
        supportingFiles.add(new SupportingFile("enumsSerializers.mustache", apiFolder, "EnumsSerializers.scala"));
        supportingFiles.add(new SupportingFile("serializers.mustache", invokerFolder, "Serializers.scala"));
    }

    @Override
    public String getName() {
        return "scala-sttp";
    }

    @Override
    public String getHelp() {
        return "Generates a Scala client library base on Sttp.";
    }

    @Override
    public String encodePath(String input) {
        String result = super.encodePath(input);
        return result.replace("{","${");
    }

    @Override
    public CodegenOperation fromOperation(String path,
                                          String httpMethod,
                                          Operation operation,
                                          List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);
        op.path = encodePath(path);
        return op;
    }
}
