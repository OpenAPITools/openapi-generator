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
import org.openapitools.codegen.CliOption;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.SupportingFile;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class ScalaSttpClientCodegen extends ScalaAkkaClientCodegen implements CodegenConfig {
    public static final StringProperty STTP_CLIENT_VERSION = new StringProperty("sttpClientVersion", "The version of " +
            "sttp " +
                                                                              "client", "2.1.5");
    public static final BooleanProperty USE_SEPARATE_ERROR_CHANNEL = new BooleanProperty("separateErrorChannel",
            "Whether to " +
                    "return response as " +
                    "F[Either[ResponseError[ErrorType], ReturnType]]] or to flatten " +
                    "response's error raising them through enclosing monad (F[ReturnType]).", true);
    public static final StringProperty JODA_TIME_VERSION = new StringProperty("jodaTimeVersion","The version of " +
            "joda-time library","2.10.6");
    public static final StringProperty JSON4S_VERSION = new StringProperty("json4sVersion", "The version of json4s " +
            "library", "3.6.8");

    public static final List<Property> properties = Arrays.asList(STTP_CLIENT_VERSION, USE_SEPARATE_ERROR_CHANNEL,
            JODA_TIME_VERSION, JSON4S_VERSION);

    public ScalaSttpClientCodegen() {
        super();
        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        embeddedTemplateDir = templateDir = "scala-sttp";
        outputFolder = "generated-code/scala-sttp";

        properties.forEach(p->cliOptions.add(p.toCliOption()));
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
        properties.forEach(p-> p.updateAdditionalProperties(additionalProperties));

        supportingFiles.clear();
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt"));
        final String invokerFolder = (sourceFolder + File.separator + invokerPackage).replace(".", File.separator);
        supportingFiles.add(new SupportingFile("requests.mustache", invokerFolder, "requests.scala"));
        supportingFiles.add(new SupportingFile("jsonSupport.mustache", invokerFolder, "JsonSupport.scala"));
        supportingFiles.add(new SupportingFile("project/build.properties.mustache", "project", "build.properties"));
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

    public static abstract class Property<T> {
        final String name;
        final String description;
        final T defaultValue;

        private Property(String name, String description, T defaultValue) {
            this.name = name;
            this.description = description;
            this.defaultValue = defaultValue;
        }

        public abstract CliOption toCliOption();

        public abstract void updateAdditionalProperties(Map<String, Object> additionalProperties);
    }

    public static class StringProperty extends Property<String> {
        private StringProperty(String name, String description, String defaultValue) {
            super(name, description, defaultValue);
        }

        @Override
        public CliOption toCliOption() {
            return CliOption.newString(name, description).defaultValue(defaultValue);
        }

        @Override
        public void updateAdditionalProperties(Map<String, Object> additionalProperties) {
            if(!additionalProperties.containsKey(name)) {
                additionalProperties.put(name, defaultValue);
            }
        }
    }

    public static class BooleanProperty extends Property<Boolean> {
        private BooleanProperty(String name, String description, Boolean defaultValue) {
            super(name, description, defaultValue);
        }

        @Override
        public CliOption toCliOption() {
            return CliOption.newBoolean(name, description, defaultValue);
        }

        @Override
        public void updateAdditionalProperties(Map<String, Object> additionalProperties) {
            String stringedBoolean = additionalProperties.getOrDefault(name, defaultValue.toString()).toString();
            additionalProperties.put(name, Boolean.valueOf(stringedBoolean));
        }
    }

}
