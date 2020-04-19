/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.servers.Server;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;

import java.io.File;
import java.util.List;

public class ScalaSttpClientCodegen extends ScalaAkkaClientCodegen implements CodegenConfig {
    public ScalaSttpClientCodegen() {
        super();
        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        embeddedTemplateDir = templateDir = "scala-sttp";
        outputFolder = "generated-code/scala-sttp";
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
        return "Generates a Scala client library (beta) based on Sttp.";
    }

    @Override
    public String encodePath(String input) {
        String result = super.encodePath(input);
        return result.replace("{", "${");
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
