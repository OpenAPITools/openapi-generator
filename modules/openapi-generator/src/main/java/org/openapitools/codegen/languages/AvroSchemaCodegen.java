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

import io.swagger.v3.oas.models.media.Schema;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class AvroSchemaCodegen extends DefaultCodegen implements CodegenConfig {
    private final Logger LOGGER = LoggerFactory.getLogger(AvroSchemaCodegen.class);
    private static final String AVRO = "avro-schema";

    /**
     * See https://avro.apache.org/docs/++version++/specification/#logical-types
     */
    public static final String USE_LOGICAL_TYPES = "useLogicalTypes";
    public static final String USE_LOGICAL_TYPES_DESC = "Use logical types for fields, when matching OpenAPI types. Currently supported: `date-time`, `date`.";
    /**
     * See https://avro.apache.org/docs/++version++/specification/#timestamps
     */
    public static final String LOGICAL_TYPES_TIME_QUANTIFIER = "logicalTypeTimeQuantifier";
    public static final String LOGICAL_TYPES_TIME_QUANTIFIER_DESC = "The quantifier for time-related logical types (`timestamp` and `local-timestamp`).";

    protected String packageName = "model";

    @Getter @Setter
    protected boolean useLogicalTypes = false; // this defaults to false for backwards compatibility

    @Getter @Setter
    protected String logicalTypeTimeQuantifier = "millis";

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
        cliOptions.add(CliOption.newBoolean(USE_LOGICAL_TYPES, USE_LOGICAL_TYPES_DESC).defaultValue(Boolean.FALSE.toString()));

        CliOption logicalTimeQuantifier = new CliOption(LOGICAL_TYPES_TIME_QUANTIFIER, LOGICAL_TYPES_TIME_QUANTIFIER_DESC).defaultValue(this.getLogicalTypeTimeQuantifier());
        Map<String, String> timeQuantifierOptions = new HashMap<>();
        timeQuantifierOptions.put("nanos", "nanoseconds");
        timeQuantifierOptions.put("micros", "microseconds");
        timeQuantifierOptions.put("millis", "milliseconds");
        logicalTimeQuantifier.setEnum(timeQuantifierOptions);
        cliOptions.add(logicalTimeQuantifier);
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("AVRO_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable AVRO_POST_PROCESS_FILE not defined so the Avro schemas may not be properly formatted. To define it, try `export AVRO_POST_PROCESS_FILE=\"/usr/local/bin/prettier -w\"` (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        } else if (!this.isEnablePostProcessFile()) {
            LOGGER.info("Warning: Environment variable 'AVRO_POST_PROCESS_FILE' is set but file post-processing is not enabled. To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            packageName = (String) additionalProperties.get(CodegenConstants.PACKAGE_NAME);

            // Force the model package to the package name so import can be fully qualified
            modelPackage = packageName;
        }

        additionalProperties.put("packageName", packageName);

        if (!convertPropertyToBooleanAndWriteBack(USE_LOGICAL_TYPES, this::setUseLogicalTypes)) {
            // This sets the default if the option was not specified.
            additionalProperties.put(USE_LOGICAL_TYPES, useLogicalTypes);
        }

        if (convertPropertyToStringAndWriteBack(LOGICAL_TYPES_TIME_QUANTIFIER, this::setLogicalTypeTimeQuantifier) == null) {
            // This sets the default if the option was not specified.
            additionalProperties.put(LOGICAL_TYPES_TIME_QUANTIFIER, logicalTypeTimeQuantifier);
        }
    }

    /**
     * Return the default value of the property
     *
     * @param p OpenAPI property object
     * @return string presentation of the default value of the property
     */
    @Override
    public String toDefaultValue(Schema p) {
        if (p.getDefault() == null) {
            return null;
        }

        if (ModelUtils.isDateSchema(p) || ModelUtils.isDateTimeSchema(p) || ModelUtils.isStringSchema(p)) {
            return "\"" + p.getDefault().toString() + "\"";
        }

        return p.getDefault().toString();
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
    public void postProcessFile(File file, String fileType) {
        super.postProcessFile(file, fileType);
        if (file == null) {
            return;
        }
        String avroPostProcessFile = System.getenv("AVRO_POST_PROCESS_FILE");
        if (StringUtils.isEmpty(avroPostProcessFile)) {
            return; // skip if AVRO_POST_PROCESS_FILE env variable is not defined
        }
        // only process files with avsc extension
        if ("avsc".equals(FilenameUtils.getExtension(file.toString()))) {
            this.executePostProcessor(new String[]{avroPostProcessFile, file.toString()});
        }
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
        List<Object> sanitizedValues = values.stream()
                .filter(x -> x != null)
                .map(Object::toString)
                .map(this::sanitizeEnumValue)
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