/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

/*
 * This generator has been deprecated. Please use scala-akka instead.
 */
public class ScalaHttpClientCodegen extends AbstractScalaCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(ScalaHttpClientCodegen.class);

    protected String authScheme = "";
    protected String gradleWrapperPackage = "gradle.wrapper";
    protected boolean authPreemptive;
    protected boolean asyncHttpClient = !authScheme.isEmpty();
    protected String groupId = "org.openapitools";
    protected String artifactId = "openapi-scala-client";
    protected String artifactVersion = "1.0.0";
    protected String clientName = "AsyncClient";

    public ScalaHttpClientCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.DEPRECATED)
                .build();

        outputFolder = "generated-code/scala-http-client";
        modelTemplateFiles.put("model.mustache", ".scala");
        apiTemplateFiles.put("api.mustache", ".scala");
        embeddedTemplateDir = templateDir = "scala-httpclient";
        apiPackage = "org.openapitools.client.api";
        modelPackage = "org.openapitools.client.model";

        setReservedWordsLowerCase(
                Arrays.asList(
                        // local variable names used in API methods (endpoints)
                        "path", "contentTypes", "contentType", "queryParams", "headerParams",
                        "formParams", "postBody", "mp", "basePath", "apiInvoker",

                        // scala reserved words
                        "abstract", "case", "catch", "class", "def", "do", "else", "extends",
                        "false", "final", "finally", "for", "forSome", "if", "implicit",
                        "import", "lazy", "match", "new", "null", "object", "override", "package",
                        "private", "protected", "return", "sealed", "super", "this", "throw",
                        "trait", "try", "true", "type", "val", "var", "while", "with", "yield")
        );

        additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        additionalProperties.put("asyncHttpClient", asyncHttpClient);
        additionalProperties.put("authScheme", authScheme);
        additionalProperties.put("authPreemptive", authPreemptive);
        additionalProperties.put("clientName", clientName);
        additionalProperties.put(CodegenConstants.STRIP_PACKAGE_NAME, stripPackageName);

        supportingFiles.add(new SupportingFile("pom.mustache", "", "pom.xml"));
        supportingFiles.add(new SupportingFile("apiInvoker.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", java.io.File.separator), "ApiInvoker.scala"));
        supportingFiles.add(new SupportingFile("client.mustache",
                (sourceFolder + File.separator + invokerPackage).replace(".", java.io.File.separator), clientName + ".scala"));
        supportingFiles.add(new SupportingFile("git_push.sh.mustache", "", "git_push.sh"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        // gradle settings
        supportingFiles.add(new SupportingFile("build.gradle.mustache", "", "build.gradle"));
        supportingFiles.add(new SupportingFile("settings.gradle.mustache", "", "settings.gradle"));
        supportingFiles.add(new SupportingFile("gradle.properties.mustache", "", "gradle.properties"));
        // gradleWrapper files
        supportingFiles.add(new SupportingFile("gradlew.mustache", "", "gradlew"));
        supportingFiles.add(new SupportingFile("gradlew.bat.mustache", "", "gradlew.bat"));
        supportingFiles.add(new SupportingFile("gradle-wrapper.properties.mustache",
                gradleWrapperPackage.replace(".", File.separator), "gradle-wrapper.properties"));
        supportingFiles.add(new SupportingFile("gradle-wrapper.jar",
                gradleWrapperPackage.replace(".", File.separator), "gradle-wrapper.jar"));

        supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt"));

        importMapping.remove("List");
        importMapping.remove("Set");
        importMapping.remove("Map");

        importMapping.put("Date", "java.util.Date");
        importMapping.put("ListBuffer", "scala.collection.mutable.ListBuffer");

        typeMapping = new HashMap<String, String>();
        typeMapping.put("enum", "NSString");
        typeMapping.put("array", "List");
        typeMapping.put("set", "Set");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("string", "String");
        typeMapping.put("int", "Int");
        typeMapping.put("long", "Long");
        typeMapping.put("float", "Float");
        typeMapping.put("byte", "Byte");
        typeMapping.put("short", "Short");
        typeMapping.put("char", "Char");
        typeMapping.put("double", "Double");
        typeMapping.put("object", "Any");
        typeMapping.put("file", "File");
        typeMapping.put("binary", "File");
        typeMapping.put("ByteArray", "Array[Byte]");
        typeMapping.put("ArrayByte", "Array[Byte]");
        typeMapping.put("date-time", "Date");
        typeMapping.put("DateTime", "Date");

        instantiationTypes.put("array", "ListBuffer");
        instantiationTypes.put("map", "HashMap");

        cliOptions.add(new CliOption(CodegenConstants.MODEL_PROPERTY_NAMING, CodegenConstants.MODEL_PROPERTY_NAMING_DESC).defaultValue("camelCase"));
    }

    @Override
    public void processOpts() {
        LOGGER.warn("IMPORTANT: This generator (scala-http-client-deprecated) is no longer actively maintained and will be deprecated. " +
                "PLease use 'scala-akka' generator instead.");

        super.processOpts();
        if (additionalProperties.containsKey(CodegenConstants.MODEL_PROPERTY_NAMING)) {
            setModelPropertyNaming((String) additionalProperties.get(CodegenConstants.MODEL_PROPERTY_NAMING));
        }
    }

    public void setModelPropertyNaming(String naming) {
        if ("original".equals(naming) || "camelCase".equals(naming) ||
                "PascalCase".equals(naming) || "snake_case".equals(naming)) {
            this.modelPropertyNaming = naming;
        } else {
            throw new IllegalArgumentException("Invalid model property naming '" +
                    naming + "'. Must be 'original', 'camelCase', " +
                    "'PascalCase' or 'snake_case'");
        }
    }

    public String getModelPropertyNaming() {
        return this.modelPropertyNaming;
    }

    @Override
    public String toVarName(String name) {
        // sanitize name
        name = sanitizeName(name); // FIXME: a parameter should not be assigned. Also declare the methods parameters as 'final'.

        if ("_".equals(name)) {
            name = "_u";
        }

        // if it's all uppper case, do nothing
        if (name.matches("^[A-Z_]*$")) {
            return name;
        }

        name = getNameUsingModelPropertyNaming(name);

        // for reserved word or word starting with number, append _
        if (isReservedWord(name) || name.matches("^\\d.*")) {
            name = escapeReservedWord(name);
        }

        return name;
    }

    @Override
    public String toParamName(String name) {
        // should be the same as variable name
        return toVarName(name);
    }

    public String getNameUsingModelPropertyNaming(String name) {
        switch (CodegenConstants.MODEL_PROPERTY_NAMING_TYPE.valueOf(getModelPropertyNaming())) {
            case original:
                return name;
            case camelCase:
                return camelize(name, true);
            case PascalCase:
                return camelize(name);
            case snake_case:
                return underscore(name);
            default:
                throw new IllegalArgumentException("Invalid model property naming '" +
                        name + "'. Must be 'original', 'camelCase', " +
                        "'PascalCase' or 'snake_case'");
        }

    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "scala-httpclient-deprecated";
    }

    @Override
    public String getHelp() {
        return "Generates a Scala client library (beta). IMPORTANT: " +
                "This generator is no longer actively maintained and will be deprecated. " +
                "PLease use 'scala-akka' generator instead.";
    }

    @Override
    public String toOperationId(String operationId) {
        // throw exception if method name is empty
        if (StringUtils.isEmpty(operationId)) {
            throw new RuntimeException("Empty method name (operationId) not allowed");
        }

        // method name cannot use reserved keyword, e.g. return
        if (isReservedWord(operationId)) {
            throw new RuntimeException(operationId + " (reserved word) cannot be used as method name");
        }

        return camelize(operationId, true);
    }

    @Override
    public String toModelName(final String name) {
        final String sanitizedName = sanitizeName(modelNamePrefix + this.stripPackageName(name) + modelNameSuffix);

        // camelize the model name
        // phone_number => PhoneNumber
        final String camelizedName = camelize(sanitizedName);

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(camelizedName)) {
            final String modelName = "Model" + camelizedName;
            LOGGER.warn(camelizedName + " (reserved word) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        // model name starts with number
        if (name.matches("^\\d.*")) {
            final String modelName = "Model" + camelizedName; // e.g. 200Response => Model200Response (after camelize)
            LOGGER.warn(name + " (model name starts with number) cannot be used as model name. Renamed to " + modelName);
            return modelName;
        }

        return camelizedName;
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return formatIdentifier(stripPackageName(property.baseName), true);
    }

}
