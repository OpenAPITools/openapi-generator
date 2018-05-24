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

import org.openapitools.codegen.*;
import org.openapitools.codegen.utils.*;
import org.openapitools.codegen.mustache.*;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.*;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.parameters.*;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.util.*;
import java.io.File;

public class ScalaGatlingCodegen extends AbstractScalaCodegen implements CodegenConfig {
    private static final Logger LOGGER = LoggerFactory.getLogger(ScalaGatlingCodegen.class);

    // source folder where to write the files
    protected String resourceFolder = "src" + File.separator + "gatling" + File.separator + "resources";
    protected String confFolder = resourceFolder + File.separator + "conf";
    protected String dataFolder = resourceFolder + File.separator + "data";
    protected String apiVersion = "1.0.0";

    /**
     * Configures the type of generator.
     *
     * @return the CodegenType for this generator
     */
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the generator
     * to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    public String getName() {
        return "scala-gatling";
    }

    /**
     * Returns human-friendly help for the generator.  Provide the consumer with help
     * tips, parameters here
     *
     * @return A string value for the help message
     */
    public String getHelp() {
        return "Generates a gatling simulation library (beta).";
    }

    public ScalaGatlingCodegen() {
        super();

        sourceFolder = "src" + File.separator + "gatling" + File.separator + "scala";

        // set the output folder here
        outputFolder = "generated-code/gatling";

        /**
         * Api classes.  You can write classes for each Api file with the apiTemplateFiles map.
         * as with models, add multiple entries with different extensions for multiple files per
         * class
         */
        apiTemplateFiles.put(
                "api.mustache",   // the template to use
                "Simulation.scala");       // the extension for each file to write

        modelTemplateFiles.put("model.mustache", ".scala");

        /**
         * Template Location.  This is the location which templates will be read from.  The generator
         * will use the resource stream to attempt to read the templates.
         */
        templateDir = "scala-gatling";

        /**
         * Api Package.  Optional, if needed, this can be used in templates
         */
        apiPackage = "org.openapitools.client.api";

        /**
         * Model Package.  Optional, if needed, this can be used in templates
         */
        modelPackage = "org.openapitools.client.model";

        /**
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("apiVersion", apiVersion);

        /**
         * Supporting Files.  You can write single files for the generator with the
         * entire object tree available.  If the input file has a suffix of `.mustache
         * it will be processed by the template engine.  Otherwise, it will be copied
         */
        supportingFiles.add(new SupportingFile("build.gradle",
                "",
                "build.gradle"));
        supportingFiles.add(new SupportingFile("logback.xml",
                confFolder,
                "logback.xml"));
        supportingFiles.add(new SupportingFile("default.conf.mustache",
                confFolder,
                "default.conf"));
        supportingFiles.add(new SupportingFile("default.conf.mustache",
                confFolder,
                "CI.conf"));
        supportingFiles.add(new SupportingFile("default.conf.mustache",
                confFolder,
                "CD.conf"));
        supportingFiles.add(new SupportingFile("default.conf.mustache",
                confFolder,
                "stress.conf"));
        supportingFiles.add(new SupportingFile("default.conf.mustache",
                confFolder,
                "baseline.conf"));
        supportingFiles.add(new SupportingFile("default.conf.mustache",
                confFolder,
                "longevity.conf"));


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
        typeMapping.put("binary", "String");
        typeMapping.put("ByteArray", "String");
        typeMapping.put("date-time", "Date");
        typeMapping.put("DateTime", "Date");

        instantiationTypes.put("array", "ListBuffer");
        instantiationTypes.put("map", "HashMap");

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
    }

    /**
     * Gatling does not need the models to have escaped words as it builds models dynamically instead of through
     * an instance of the object.
     *
     * @return the escaped term
     */
    @Override
    public String escapeReservedWord(String name) {
        return name;
    }

    /**
     * Location to write model files.  You can use the modelPackage() as defined when the class is
     * instantiated
     */
    public String modelFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + modelPackage().replace('.', File.separatorChar);
    }

    /**
     * Location to write api files.  You can use the apiPackage() as defined when the class is
     * instantiated
     */
    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + sourceFolder + "/" + apiPackage().replace('.', File.separatorChar);
    }

    /**
     * Modifies the openapi doc to make mustache easier to use
     *
     * @param openAPI input openapi document
     */
    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        for (String pathname : openAPI.getPaths().keySet()) {
            PathItem path = openAPI.getPaths().get(pathname);
            if (path.readOperations() == null) {
                continue;
            }
            for (Operation operation : path.readOperations()) {
                if (!operation.getExtensions().keySet().contains("x-gatling-path")) {
                    if (pathname.contains("{")) {
                        String gatlingPath = pathname.replaceAll("\\{", "\\$\\{");
                        operation.addExtension("x-gatling-path", gatlingPath);
                    } else {
                        operation.addExtension("x-gatling-path", pathname);
                    }
                }

                Set<Parameter> headerParameters = new HashSet<>();
                Set<Parameter> formParameters = new HashSet<>();
                Set<Parameter> queryParameters = new HashSet<>();
                Set<Parameter> pathParameters = new HashSet<>();

                if (operation.getParameters() != null) {

                    for (Parameter parameter : operation.getParameters()) {
                        if (parameter.getIn().equalsIgnoreCase("header")) {
                            headerParameters.add(parameter);
                        }
                    /* need to revise below as form parameter is no longer in the parameter list
                    if (parameter.getIn().equalsIgnoreCase("formData")) {
                        formParameters.add(parameter);
                    }
                    */
                        if (parameter.getIn().equalsIgnoreCase("query")) {
                            queryParameters.add(parameter);
                        }
                        if (parameter.getIn().equalsIgnoreCase("path")) {
                            pathParameters.add(parameter);
                        }
                    /* TODO need to revise below as body is no longer in the parameter
                    if (parameter.getIn().equalsIgnoreCase("body")) {
                        BodyParameter bodyParameter = (BodyParameter) parameter;
                        Model model = bodyParameter.getSchema();
                        if (model instanceof RefModel) {
                            String[] refArray = model.getReference().split("\\/");
                            operation.setVendorExtension("x-gatling-body-object", refArray[refArray.length - 1] + ".toStringBody");
                            Set<String> bodyFeederParams = new HashSet<>();
                            Set<String> sessionBodyVars = new HashSet<>();
                            for (Map.Entry<String, Model> modelEntry : swagger.getDefinitions().entrySet()) {
                                if (refArray[refArray.length - 1].equalsIgnoreCase(modelEntry.getKey())) {
                                    for (Map.Entry<String, Property> propertyEntry : modelEntry.getValue().getProperties().entrySet()) {
                                        bodyFeederParams.add(propertyEntry.getKey());
                                        sessionBodyVars.add("\"${" + propertyEntry.getKey() + "}\"");
                                    }
                                }
                            }
                            operation.setVendorExtension("x-gatling-body-feeder", operation.getOperationId() + "BodyFeeder");
                            operation.setVendorExtension("x-gatling-body-feeder-params", StringUtils.join(sessionBodyVars, ","));
                            try {
                                FileUtils.writeStringToFile(new File(outputFolder + File.separator + dataFolder + File.separator + operation.getOperationId() + "-" + "bodyParams.csv"), StringUtils.join(bodyFeederParams, ","));
                            } catch (IOException ioe) {
                                LOGGER.error("Could not create feeder file for operationId" + operation.getOperationId(), ioe);
                            }

                        } else if (model instanceof ArrayModel) {
                            operation.setVendorExtension("x-gatling-body-object", "StringBody(\"[]\")");
                        } else {
                            operation.setVendorExtension("x-gatling-body-object", "StringBody(\"{}\")");
                        }

                    }
                    */
                    }
                }

                prepareGatlingData(operation, headerParameters, "header");
                prepareGatlingData(operation, formParameters, "form");
                prepareGatlingData(operation, queryParameters, "query");
                prepareGatlingData(operation, pathParameters, "path");
            }
        }

    }

    /**
     * Creates all the necessary openapi vendor extensions and feeder files for gatling
     *
     * @param operation     OpoenAPI Operation
     * @param parameters    OpenAPI Parameters
     * @param parameterType OpenAPI Parameter Type
     */
    private void prepareGatlingData(Operation operation, Set<Parameter> parameters, String parameterType) {
        if (parameters.size() > 0) {
            List<String> parameterNames = new ArrayList<>();
            List<Object> vendorList = new ArrayList<>();
            for (Parameter parameter : parameters) {
                Map<String, Object> extensionMap = new HashMap<>();
                extensionMap.put("gatlingParamName", parameter.getName());
                extensionMap.put("gatlingParamValue", "${" + parameter.getName() + "}");
                vendorList.add(extensionMap);
                parameterNames.add(parameter.getName());
            }
            operation.addExtension("x-gatling-" + parameterType.toLowerCase() + "-params", vendorList);
            operation.addExtension("x-gatling-" + parameterType.toLowerCase() + "-feeder", operation.getOperationId() + parameterType.toUpperCase() + "Feeder");
            try {
                FileUtils.writeStringToFile(new File(outputFolder + File.separator + dataFolder + File.separator + operation.getOperationId() + "-" + parameterType.toLowerCase() + "Params.csv"), StringUtils.join(parameterNames, ","));
            } catch (IOException ioe) {
                LOGGER.error("Could not create feeder file for operationId" + operation.getOperationId(), ioe);
            }
        }
    }

    /**
     * Optional - type declaration.  This is a String which is used by the templates to instantiate your
     * types.  There is typically special handling for different property types
     *
     * @return a string value used as the `dataType` field for model templates, `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            return getSchemaType(p) + "[" + getTypeDeclaration(inner) + "]";
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = (Schema) p.getAdditionalProperties();
            return getSchemaType(p) + "[String, " + getTypeDeclaration(inner) + "]";
        }
        return super.getTypeDeclaration(p);
    }

    /**
     * Optional - openapi type conversion.  This is used to map openapi types in a `Schema` into
     * either language specific types via `typeMapping` or into complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     */
    @Override
    public String getSchemaType(Schema p) {
        String schemaType = super.getSchemaType(p);
        String type = null;
        if (typeMapping.containsKey(schemaType)) {
            type = typeMapping.get(schemaType);
            if (languageSpecificPrimitives.contains(type))
                return toModelName(type);
        } else
            type = schemaType;
        return toModelName(type);
    }
}
