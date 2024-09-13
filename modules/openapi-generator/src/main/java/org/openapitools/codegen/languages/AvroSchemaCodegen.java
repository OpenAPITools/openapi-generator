/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;

import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelsMap;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.StringUtils.camelize;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AvroSchemaCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(AvroSchemaCodegen.class);
    private static final String AVRO = "avro-schema";
    protected String packageName = "model";

    public AvroSchemaCodegen() {
        super();

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        // TODO: Avro maintainer review.
        modifyFeatureSet(features -> features
                .parameterFeatures(EnumSet.noneOf(ParameterFeature.class))
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .wireFormatFeatures(EnumSet.noneOf(WireFormatFeature.class))
                .documentationFeatures(EnumSet.noneOf(DocumentationFeature.class))
                .globalFeatures(EnumSet.noneOf(GlobalFeature.class))
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism,
                        SchemaSupportFeature.Union
                )
                .clientModificationFeatures(EnumSet.noneOf(ClientModificationFeature.class))
        );

        outputFolder = "generated-code/avro-schema";
        modelTemplateFiles.put("model.mustache", ".avsc");
        // Force the model package to the package name so import can be fully qualified
        modelPackage = packageName;
        importMapping.clear();
        embeddedTemplateDir = templateDir = AVRO;

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

        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME, CodegenConstants.PACKAGE_NAME_DESC));
    }

    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            packageName = (String) additionalProperties.get(CodegenConstants.PACKAGE_NAME);

            // Force the model package to the package name so import can be fully qualified
            modelPackage = packageName;
        }

        additionalProperties.put("packageName", packageName);
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SCHEMA;
    }

    @Override
    public String getName() {
        return "avro-schema";
    }

    @Override
    public String getHelp() {
        return "Generates a Avro model (beta).";
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator;
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        return postProcessModelsEnum(objs);
    }

    @Override
    protected void setNonArrayMapProperty(CodegenProperty property, String type) {
        super.setNonArrayMapProperty(property, type);
        if (property.isModel) {
            property.dataType = camelize(modelNamePrefix + property.dataType + modelNameSuffix);
        }
    }

    @Override
    public String escapeUnsafeCharacters(String input) {
        // do nothing as it's a schema conversion
        return input;
    }

    @Override
    public String escapeQuotationMark(String input) {
        // do nothing as it's a schema conversion
        return input;
    }

    @Override
    protected List<Map<String, Object>> buildEnumVars(List<Object> values, String dataType) {
        List<Object> sanitizedValues = values.stream().map(Object::toString).map(this::sanitizeEnumValue)
                .collect(Collectors.toList());
        removeEnumValueCollisions(sanitizedValues);
        return super.buildEnumVars(sanitizedValues, dataType);
    }

    /**
     * Valid enums in Avro need to adhere to [A-Za-z_][A-Za-z0-9_]*
     * See https://avro.apache.org/docs/1.12.0/specification/#enums
     */
    private String sanitizeEnumValue(String value) {
        // Replace any non-alphanumeric characters with an underscore
        String sanitizedValue = value.replaceAll("[^A-Za-z0-9_]", "_");
        // If the enum starts with a number, prefix it with an underscore
        sanitizedValue = sanitizedValue.replaceAll("^([0-9])", "_$1");
        return sanitizedValue;
    }

    private void removeEnumValueCollisions(List<Object> values) {
        Collections.reverse(values);
        for (int i = 0; i < values.size(); i++) {
            final String value = values.get(i).toString();
            long count = values.stream().filter(v1 -> v1.equals(value)).count();
            if (count > 1) {
                String uniqueEnumValue = getUniqueEnumValue(value.toString(), values);
                LOGGER.debug("Changing duplicate enumeration value from " + value + " to " + uniqueEnumValue);
                values.set(i, uniqueEnumValue);
            }
        }
        Collections.reverse(values);
    }

    private String getUniqueEnumValue(String value, List<Object> values) {
        long count = values.stream().filter(v -> v.equals(value)).count();
        return count > 1
                ? getUniqueEnumValue(value + count, values)
                : value;
    }
}