/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import org.openapitools.codegen.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class AvroCodegen extends DefaultCodegen implements CodegenConfig {
    private static final String AVRO = "avro";

    public AvroCodegen() {
        outputFolder = "generated-code/avro";
        modelTemplateFiles.put("model.mustache", ".avsc");
        apiPackage = "api";
        modelPackage = "model";


        // default HIDE_GENERATION_TIMESTAMP to true
        hideGenerationTimestamp = Boolean.TRUE;

        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList("null", "boolean", "int", "integer", "long", "float", "double", "bytes", "string",
                        "BigDecimal", "UUID", "number", "date", "DateTime")
        );
        defaultIncludes = new HashSet<>(languageSpecificPrimitives);

        instantiationTypes.put("array", "Array");
        instantiationTypes.put("list", "Array");
        instantiationTypes.put("map", "Object");
        typeMapping.clear();
        typeMapping.put("number", "double");
        typeMapping.put("DateTime", "string");
        typeMapping.put("date", "string");
        typeMapping.put("short", "int");
        typeMapping.put("char", "string");
        typeMapping.put("integer", "int");
        typeMapping.put("ByteArray", "bytes");
        typeMapping.put("binary", "File");
        typeMapping.put("file", "File");
        typeMapping.put("UUID", "string");
        typeMapping.put("BigDecimal", "string");

        importMapping.clear();

        cliOptions.add(new CliOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC).defaultValue("src"));
        cliOptions.add(new CliOption(CodegenConstants.INVOKER_PACKAGE, CodegenConstants.INVOKER_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.API_PACKAGE, CodegenConstants.API_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.MODEL_PACKAGE, CodegenConstants.MODEL_PACKAGE_DESC));
        cliOptions.add(new CliOption(CodegenConstants.LICENSE_NAME, "name of the license the project uses (Default: using info.license.name)"));
        cliOptions.add(new CliOption(CodegenConstants.HIDE_GENERATION_TIMESTAMP, CodegenConstants.HIDE_GENERATION_TIMESTAMP_DESC).defaultValue(Boolean.TRUE.toString()));
        cliOptions.add(new CliOption(CodegenConstants.MODEL_PROPERTY_NAMING, CodegenConstants.MODEL_PROPERTY_NAMING_DESC).defaultValue("camelCase"));

        embeddedTemplateDir = AVRO;
        templateDir = AVRO;
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SCHEMA;
    }

    @Override
    public String getName() {
        return "avro";
    }

    @Override
    public String getHelp() {
        return "Generates a Avro model.";
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + "/" ;
    }

    @Override
    public Map<String, Object> postProcessModels(Map<String, Object> objs) {
        return postProcessModelsEnum(objs);
    }

    @Override
    protected void setNonArrayMapProperty(CodegenProperty property, String type) {
        super.setNonArrayMapProperty(property, type);
        if (property.isModel) {
            property.dataType = camelize(modelNamePrefix + property.dataType + modelNameSuffix );
        }
    }

}
