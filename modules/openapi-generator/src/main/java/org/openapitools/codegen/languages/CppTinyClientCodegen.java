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

import org.openapitools.codegen.*;
import io.swagger.v3.oas.models.media.Schema;

import io.swagger.v3.parser.util.SchemaTypeUtil;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.utils.ModelUtils;

import java.io.File;
import java.util.*;

import org.apache.commons.lang3.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CppTinyClientCodegen extends AbstractCppCodegen implements CodegenConfig {
    public static final String PROJECT_NAME = "TinyClient";

    final Logger LOGGER = LoggerFactory.getLogger(CppTinyClientCodegen.class);

    public static final String MICROCONTROLLER = "controller";
    public static final String rootFolder = "";
    protected String controller = "esp32";


    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    /**
     * Configures a friendly name for the generator. This will be used by the
     * generator to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "cpp-tiny";
    }

    /**
     * Returns human-friendly help for the generator. Provide the consumer with
     * help tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a C++ Arduino REST API client.";
    }

    public void addControllerToAdditionalProperties() {
        Map<String, String> supportedControllers = new HashMap<>();
        supportedControllers.put("esp32", "isESP32");
        supportedControllers.put("esp8266", "isESP8266");
        if (supportedControllers.containsKey(controller)) {
            additionalProperties.put(supportedControllers.get(controller), true);
        } else {
            //String msg = String.format("The specified controller: %s is not supported.\nSupported controllers are: %s",
            //        controller,
            //        supportedControllers.keySet());
            throw new UnsupportedOperationException("Supported controllers are: ESP32, ESP8266");
        }
    }

    public CppTinyClientCodegen() {
        super();

        modifyFeatureSet(feature -> feature
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling,
                        GlobalFeature.MultiServer)
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie,
                        ParameterFeature.Header,
                        ParameterFeature.FormUnencoded,
                        ParameterFeature.FormMultipart,
                        ParameterFeature.Query
                )
                .excludeDataTypeFeatures(
                        DataTypeFeature.Enum,
                        DataTypeFeature.Maps,
                        DataTypeFeature.MapOfCollectionOfEnum,
                        DataTypeFeature.MapOfCollectionOfModel,
                        DataTypeFeature.MapOfCollectionOfPrimitives,
                        DataTypeFeature.MapOfEnum,
                        DataTypeFeature.MapOfModel

                )
                .excludeWireFormatFeatures(
                        WireFormatFeature.XML,
                        WireFormatFeature.PROTOBUF,
                        WireFormatFeature.Custom
                )
                .includeDocumentationFeatures(
                        DocumentationFeature.Readme
                ));

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
                .build();

        outputFolder = "generated-code" + File.separator + "cpp-tiny";
        embeddedTemplateDir = templateDir = "cpp-tiny";

        String libFolder = "lib";
        // MODELS
        modelPackage = libFolder + File.separator + "Models";
        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-body.mustache", ".cpp");

        // MODELS: Helpers
        supportingFiles.add(new SupportingFile("helpers-header.mustache", modelPackage, "Helpers.h"));
        supportingFiles.add(new SupportingFile("helpers-body.mustache", modelPackage, "Helpers.cpp"));

        // MODELS: TESTS
        testPackage = libFolder + File.separator + "TestFiles";
        modelTestTemplateFiles.put("unit-test-model.mustache", ".cpp");
        supportingFiles.add(new SupportingFile("run-tests.mustache", "test", "RunTests.cpp"));

        // SERVICES
        apiPackage = CppTinyClientCodegen.libFolder + File.separator + "service";
        apiTemplateFiles.put("service/api-header.mustache".replace('/', File.separatorChar), ".h");
        apiTemplateFiles.put("service/api-body.mustache".replace('/', File.separatorChar), ".cpp");

        // SERVICES: Helpers
        supportingFiles.add(new SupportingFile("service/Response.h.mustache", serviceFolder, "Response.h"));
        supportingFiles.add(new SupportingFile("service/Service.h.mustache", serviceFolder, "Service.h"));
        supportingFiles.add(new SupportingFile("service/Service.cpp.mustache", serviceFolder, "Service.cpp"));

        // Main
        supportingFiles.add(new SupportingFile("main.mustache", CppTinyClientCodegen.sourceFolder, "main.cpp")); // TODO no overwrite

        // Config files
        supportingFiles.add(new SupportingFile("README.mustache", rootFolder, "README.md"));
        supportingFiles.add(new SupportingFile("platformio.ini.mustache", rootFolder, "platformio.ini")); // TODO no overwrite
        supportingFiles.add(new SupportingFile("root.cert.mustache", rootFolder, "root.cert")); // TODO no overwrite
        supportingFiles.add(new SupportingFile("pre_compiling_bourne.py.mustache", rootFolder, "pre_compiling_bourne.py"));

        defaultIncludes = new HashSet<>(
                Arrays.asList(
                        "bool",
                        "int",
                        "long",
                        "double",
                        "float")
        );
        languageSpecificPrimitives = new HashSet<>(
                Arrays.asList(
                        "bool",
                        "int",
                        "long",
                        "double",
                        "float",
                        "std::string")
        );

        super.typeMapping = new HashMap<>();
        typeMapping.put("string", "std::string");
        typeMapping.put("integer", "int");
        typeMapping.put("boolean", "bool");
        typeMapping.put("array", "std::list");
        typeMapping.put("DateTime", "std::string");

        cliOptions.add(new CliOption(MICROCONTROLLER, "name of microcontroller (e.g esp32 or esp8266)").
                defaultValue("esp32"));

        makeTypeMappings();

    }

    // FilePaths
    private static final String sourceFolder = "src";
    private static final String libFolder = "lib";
    private static final String serviceFolder = libFolder + File.separator + "service";

    @Override
    public String getTypeDeclaration(String str) {
        return str;
    }

    private void makeTypeMappings() {
        // Types
        String cpp_array_type = "std::list";
        typeMapping = new HashMap<>();

        typeMapping.put("string", "std::string");
        typeMapping.put("integer", "int");
        typeMapping.put("float", "float");
        typeMapping.put("long", "long");
        typeMapping.put("boolean", "bool");
        typeMapping.put("double", "double");
        typeMapping.put("array", cpp_array_type);
        typeMapping.put("number", "long");
        typeMapping.put("binary", "std::string");
        typeMapping.put("password", "std::string");
        typeMapping.put("file", "std::string");
        typeMapping.put("DateTime", "std::string");
        typeMapping.put("Date", "std::string");
        typeMapping.put("UUID", "std::string");
        typeMapping.put("URI", "std::string");
    }

    @Override
    public void processOpts() {
        super.processOpts();

        // -- --additional-properties=controller=<controllername>
        if (additionalProperties.containsKey(MICROCONTROLLER)) {
            controller = additionalProperties.get(MICROCONTROLLER).toString();
        }

        addControllerToAdditionalProperties();

        LOGGER.info("Generator targeting the following microcontroller: {}", controller);
    }

    @Override
    public String toInstantiationType(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            return instantiationTypes.get("array");
        } else {
            return null;
        }
    }

    @Override
    public String getTypeDeclaration(Schema p) {
        String openAPIType = getSchemaType(p);
        if (languageSpecificPrimitives.contains(openAPIType)) {
            return toModelName(openAPIType);
        } else {
            return openAPIType;
        }
    }

    @Override
    public String getSchemaType(Schema p) {
        String openAPIType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
            if (languageSpecificPrimitives.contains(type)) {
                return toModelName(type);
            }
        } else {
            type = openAPIType;
        }
        return toModelName(type);
    }

    @Override
    public String toModelName(String type) {
        if (typeMapping.keySet().contains(type) ||
                typeMapping.values().contains(type) ||
                defaultIncludes.contains(type) ||
                languageSpecificPrimitives.contains(type)) {
            return type;
        } else {
            return Character.toUpperCase(type.charAt(0)) + type.substring(1);
        }
    }

    @Override
    public String toModelImport(String name) {
        if (name.equals("std::string")) {
            return "#include <string>";
        } else if (name.equals("std::list")) {
            return "#include <list>";
        } else if (name.equals("Map")) {
            return "#include <map>";
        }
        return "#include \"" + name + ".h\"";
    }

    @Override
    public String toApiImport(String name) {
        return super.toApiImport(name);
    }

    @Override
    public String toVarName(String name) {
        String paramName = name.replaceAll("[^a-zA-Z0-9_]", "");
        if (name.length() > 0) {
            paramName = Character.toLowerCase(paramName.charAt(0)) + paramName.substring(1);
        }
        if (isReservedWord(paramName)) {
            return escapeReservedWord(paramName);
        }
        return paramName;
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isBooleanSchema(p)) {
            return "bool(false)";
        } else if (ModelUtils.isNumberSchema(p)) {
            return "float(0)";
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (SchemaTypeUtil.INTEGER64_FORMAT.equals(p.getFormat())) {
                return "long(0)";
            }
            return "int(0)";
        } else if (ModelUtils.isArraySchema(p)) {
            return "std::list";
        } else if (!StringUtils.isEmpty(p.get$ref())) {
            return toModelName(ModelUtils.getSimpleRef(p.get$ref())) + "()";
        } else if (ModelUtils.isDateSchema(p) || ModelUtils.isDateTimeSchema(p)) {
            return "std::string()";
        } else if (ModelUtils.isStringSchema(p)) {
            return "std::string()";
        }
        return "null";
    }


}
