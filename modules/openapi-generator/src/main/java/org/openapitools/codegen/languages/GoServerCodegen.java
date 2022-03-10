/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;

public class GoServerCodegen extends AbstractGoCodegen {

    /**
     * Name of additional property for switching routers
     */
    private static final String ROUTER_SWITCH = "router";

    /**
     * Description of additional property for switching routers
     */
    private static final String ROUTER_SWITCH_DESC = "Specify the router which should be used.";

    /**
     * List of available routers
     */
    private static final String[] ROUTERS = {"mux", "chi"};

    private final Logger LOGGER = LoggerFactory.getLogger(GoServerCodegen.class);

    protected String packageVersion = "1.0.0";
    protected int serverPort = 8080;
    protected String projectName = "openapi-server";
    protected String sourceFolder = "go";
    protected Boolean corsFeatureEnabled = false;
    protected Boolean addResponseHeaders = false;
    protected Boolean outputAsLibrary = false;
    protected Boolean onlyInterfaces = false;


    public GoServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML))
                .securityFeatures(EnumSet.noneOf(
                        SecurityFeature.class
                ))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .excludeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        // set the output folder here
        outputFolder = "generated-code/go";

        cliOptions.add(new CliOption(CodegenConstants.SOURCE_FOLDER, CodegenConstants.SOURCE_FOLDER_DESC)
                .defaultValue(sourceFolder));

        CliOption frameworkOption = new CliOption(ROUTER_SWITCH, ROUTER_SWITCH_DESC);
        for (String option : ROUTERS) {
            frameworkOption.addEnum(option, option);
        }
        frameworkOption.defaultValue(ROUTERS[0]);
        cliOptions.add(frameworkOption);

        CliOption optServerPort = new CliOption("serverPort", "The network port the generated server binds to");
        optServerPort.setType("int");
        optServerPort.defaultValue(Integer.toString(serverPort));
        cliOptions.add(optServerPort);

        CliOption optFeatureCORS = new CliOption("featureCORS", "Enable Cross-Origin Resource Sharing middleware");
        optFeatureCORS.setType("bool");
        optFeatureCORS.defaultValue(corsFeatureEnabled.toString());
        cliOptions.add(optFeatureCORS);

        cliOptions.add(CliOption.newBoolean(CodegenConstants.ENUM_CLASS_PREFIX, CodegenConstants.ENUM_CLASS_PREFIX_DESC));

        // option to include headers in the response
        CliOption optAddResponseHeaders = new CliOption("addResponseHeaders", "To include response headers in ImplResponse");
        optAddResponseHeaders.setType("bool");
        optAddResponseHeaders.defaultValue(addResponseHeaders.toString());
        cliOptions.add(optAddResponseHeaders);

        
        // option to exclude service factories; only interfaces are rendered
        CliOption optOnlyInterfaces = new CliOption("onlyInterfaces", "Exclude default service creators from output; only generate interfaces");
        optOnlyInterfaces.setType("bool");
        optOnlyInterfaces.defaultValue(onlyInterfaces.toString());
        cliOptions.add(optOnlyInterfaces);

        // option to exclude main package (main.go), Dockerfile, and go.mod files
        CliOption optOutputAsLibrary = new CliOption("outputAsLibrary", "Exclude main.go, go.mod, and Dockerfile from output");
        optOutputAsLibrary.setType("bool");
        optOutputAsLibrary.defaultValue(outputAsLibrary.toString());
        cliOptions.add(optOutputAsLibrary);
        /*
         * Models.  You can write model files using the modelTemplateFiles map.
         * if you want to create one template for file, you can do so here.
         * for multiple files for model, just put another entry in the `modelTemplateFiles` with
         * a different extension
         */
        modelTemplateFiles.put(
                "model.mustache",
                ".go");

        /*
         * Api classes.  You can write classes for each Api file with the apiTemplateFiles map.
         * as with models, add multiple entries with different extensions for multiple files per
         * class
         */
        apiTemplateFiles.put(
                "controller-api.mustache",   // the template to use
                ".go");       // the extension for each file to write

        /*
         * Service templates.  You can write services for each Api file with the apiTemplateFiles map.
            These services are skeletons built to implement the logic of your api using the
            expected parameters and response.
         */
        apiTemplateFiles.put(
                "service.mustache",   // the template to use
                "_service.go");       // the extension for each file to write

        /*
         * Template Location.  This is the location which templates will be read from.  The generator
         * will use the resource stream to attempt to read the templates.
         */
        embeddedTemplateDir = templateDir = "go-server";

        /*
         * Reserved words.  Override this with reserved words specific to your language
         */
        setReservedWordsLowerCase(
                Arrays.asList(
                        // data type
                        "string", "bool", "uint", "uint8", "uint16", "uint32", "uint64",
                        "int", "int8", "int16", "int32", "int64", "float32", "float64",
                        "complex64", "complex128", "rune", "byte", "uintptr",

                        "break", "default", "func", "interface", "select",
                        "case", "defer", "go", "map", "struct",
                        "chan", "else", "goto", "package", "switch",
                        "const", "fallthrough", "if", "range", "type",
                        "continue", "for", "import", "return", "var", "error", "nil")
                // Added "error" as it's used so frequently that it may as well be a keyword
        );
    }

    @Override
    public void processOpts() {
        super.processOpts();
        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            setPackageName((String) additionalProperties.get(CodegenConstants.PACKAGE_NAME));
        } else {
            setPackageName("openapi");
            additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        }

        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_VERSION)) {
            this.setPackageVersion((String) additionalProperties.get(CodegenConstants.PACKAGE_VERSION));
        } else {
            additionalProperties.put(CodegenConstants.PACKAGE_VERSION, packageVersion);
        }

        if (additionalProperties.containsKey(CodegenConstants.SOURCE_FOLDER)) {
            this.setSourceFolder((String) additionalProperties.get(CodegenConstants.SOURCE_FOLDER));
        } else {
            additionalProperties.put(CodegenConstants.SOURCE_FOLDER, sourceFolder);
        }

        if (additionalProperties.containsKey("serverPort") && additionalProperties.get("serverPort") instanceof Integer) {
            this.setServerPort((int) additionalProperties.get("serverPort"));
        } else if (additionalProperties.containsKey("serverPort") && additionalProperties.get("serverPort") instanceof String) {
            try {
                this.setServerPort(Integer.parseInt(additionalProperties.get("serverPort").toString()));
                additionalProperties.put("serverPort", serverPort);
            } catch (NumberFormatException e) {
                LOGGER.warn("serverPort is not a valid integer... defaulting to {}", serverPort);
                additionalProperties.put("serverPort", serverPort);
            }
        } else {
            additionalProperties.put("serverPort", serverPort);
        }

        if (additionalProperties.containsKey("featureCORS")) {
            this.setFeatureCORS(convertPropertyToBooleanAndWriteBack("featureCORS"));
        } else {
            additionalProperties.put("featureCORS", corsFeatureEnabled);
        }

        if (additionalProperties.containsKey("addResponseHeaders")) {
            this.setAddResponseHeaders(convertPropertyToBooleanAndWriteBack("addResponseHeaders"));
        } else {
            additionalProperties.put("addResponseHeaders", addResponseHeaders);
        }

        if (additionalProperties.containsKey("onlyInterfaces")) {
            this.setOnlyInterfaces(convertPropertyToBooleanAndWriteBack("onlyInterfaces"));
        } else {
            additionalProperties.put("onlyInterfaces", onlyInterfaces);
        }

        if (this.onlyInterfaces) {
          apiTemplateFiles.remove("service.mustache");
        }

        if (additionalProperties.containsKey("outputAsLibrary")) {
            this.setOutputAsLibrary(convertPropertyToBooleanAndWriteBack("outputAsLibrary"));
        } else {
            additionalProperties.put("outputAsLibrary", outputAsLibrary);
        }

        if (additionalProperties.containsKey(CodegenConstants.ENUM_CLASS_PREFIX)) {
            setEnumClassPrefix(Boolean.parseBoolean(additionalProperties.get(CodegenConstants.ENUM_CLASS_PREFIX).toString()));
            if (enumClassPrefix) {
                additionalProperties.put(CodegenConstants.ENUM_CLASS_PREFIX, true);
            }
        }

        additionalProperties.putIfAbsent(ROUTER_SWITCH, ROUTERS[0]);

        final Object propRouter = additionalProperties.get(ROUTER_SWITCH);
        final Map<String, Boolean> routers = new HashMap<>();
        for (String router : ROUTERS) {
            routers.put(router, router.equals(propRouter));
        }
        additionalProperties.put("routers", routers);

        modelPackage = packageName;
        apiPackage = packageName;

        /*
         * Supporting Files.  You can write single files for the generator with the
         * entire object tree available.  If the input file has a suffix of `.mustache
         * it will be processed by the template engine.  Otherwise, it will be copied
         */
        if (!outputAsLibrary) {
          supportingFiles.add(new SupportingFile("main.mustache", "", "main.go"));
          supportingFiles.add(new SupportingFile("Dockerfile.mustache", "", "Dockerfile"));
          supportingFiles.add(new SupportingFile("go.mod.mustache", "", "go.mod"));
        }
        supportingFiles.add(new SupportingFile("openapi.mustache", "api", "openapi.yaml"));
        supportingFiles.add(new SupportingFile("routers.mustache", sourceFolder, "routers.go"));
        supportingFiles.add(new SupportingFile("logger.mustache", sourceFolder, "logger.go"));
        supportingFiles.add(new SupportingFile("impl.mustache", sourceFolder, "impl.go"));
        supportingFiles.add(new SupportingFile("helpers.mustache", sourceFolder, "helpers.go"));
        supportingFiles.add(new SupportingFile("api.mustache", sourceFolder, "api.go"));
        supportingFiles.add(new SupportingFile("error.mustache", sourceFolder, "error.go"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md")
                .doNotOverwrite());
    }

    @Override
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);
        @SuppressWarnings("unchecked")
        Map<String, Object> objectMap = (Map<String, Object>) objs.get("operations");
        @SuppressWarnings("unchecked")
        List<CodegenOperation> operations = (List<CodegenOperation>) objectMap.get("operation");

        List<Map<String, String>> imports = (List<Map<String, String>>) objs.get("imports");
        if (imports == null)
            return objs;

        // override imports to only include packages for interface parameters
        imports.clear();

        boolean addedTimeImport = false;
        boolean addedOSImport = false;
        for (CodegenOperation operation : operations) {
            for (CodegenParameter param : operation.allParams) {
                // import "os" if the operation uses files
                if (!addedOSImport && ("*os.File".equals(param.dataType) || ("[]*os.File".equals(param.dataType)))) {
                    imports.add(createMapping("import", "os"));
                    addedOSImport = true;
                }

                // import "time" if the operation has a required time parameter
                if (param.required) {
                    if (!addedTimeImport && "time.Time".equals(param.dataType)) {
                        imports.add(createMapping("import", "time"));
                        addedTimeImport = true;
                    }
                }
            }
        }

        return objs;
    }

    @Override
    public String apiPackage() {
        return sourceFolder;
    }

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     * @see org.openapitools.codegen.CodegenType
     */
    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "go-server";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    @Override
    public String getHelp() {
        return "Generates a Go server library using OpenAPI-Generator. By default, " +
                "it will also generate service classes -- which you can disable with the `-Dnoservice` environment variable.";
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    public void setSourceFolder(String sourceFolder) {
        this.sourceFolder = sourceFolder;
    }

    public void setPackageVersion(String packageVersion) {
        this.packageVersion = packageVersion;
    }

    public void setServerPort(int serverPort) {
        this.serverPort = serverPort;
    }

    public void setFeatureCORS(Boolean featureCORS) {
        this.corsFeatureEnabled = featureCORS;
    }

    public void setAddResponseHeaders(Boolean addResponseHeaders) {
        this.addResponseHeaders = addResponseHeaders;
    }

    public void setOnlyInterfaces(Boolean onlyInterfaces) {
        this.onlyInterfaces = onlyInterfaces;
    }

    public void setOutputAsLibrary(Boolean outputAsLibrary) {
        this.outputAsLibrary = outputAsLibrary;
    }

    @Override
    protected void updateModelForObject(CodegenModel m, Schema schema) {
        /**
         * we have a custom version of this function so we only set isMap to true if
         * ModelUtils.isMapSchema
         * In other generators, isMap is true for all type object schemas
         */
        if (schema.getProperties() != null || schema.getRequired() != null && !(schema instanceof ComposedSchema)) {
            // passing null to allProperties and allRequired as there's no parent
            addVars(m, unaliasPropertySchema(schema.getProperties()), schema.getRequired(), null, null);
        }
        if (ModelUtils.isMapSchema(schema)) {
            // an object or anyType composed schema that has additionalProperties set
            addAdditionPropertiesToCodeGenModel(m, schema);
        } else {
            m.setIsMap(false);
            if (ModelUtils.isFreeFormObject(openAPI, schema)) {
                // non-composed object type with no properties + additionalProperties
                // additionalProperties must be null, ObjectSchema, or empty Schema
                addAdditionPropertiesToCodeGenModel(m, schema);
            }
        }
        // process 'additionalProperties'
        setAddProps(schema, m);
    }
}
