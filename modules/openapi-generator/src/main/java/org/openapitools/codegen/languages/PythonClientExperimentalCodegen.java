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
import io.swagger.v3.oas.models.examples.Example;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.responses.ApiResponse;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.examples.ExampleGenerator;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.io.File;
import java.util.*;
import java.util.regex.Pattern;

import static org.openapitools.codegen.utils.StringUtils.camelize;
import static org.openapitools.codegen.utils.StringUtils.underscore;

public class PythonClientExperimentalCodegen extends PythonClientCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(PythonClientExperimentalCodegen.class);

    public PythonClientExperimentalCodegen() {
        super();

        // Composed schemas can have the 'additionalProperties' keyword, as specified in JSON schema.
        // In principle, this should be enabled by default for all code generators. However due to limitations
        // in other code generators, support needs to be enabled on a case-by-case basis.
        supportsAdditionalPropertiesWithComposedSchema = true;

        // When the 'additionalProperties' keyword is not present in a OAS schema, allow
        // undeclared properties. This is compliant with the JSON schema specification.
        this.setDisallowAdditionalPropertiesIfNotPresent(false);

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.BearerToken,
                        SecurityFeature.ApiKey,
                        SecurityFeature.OAuth2_Implicit
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

        // this may set datatype right for additional properties
        instantiationTypes.put("map", "dict");

        languageSpecificPrimitives.add("file_type");
        languageSpecificPrimitives.add("none_type");

        apiTemplateFiles.remove("api.mustache");
        apiTemplateFiles.put("python-experimental/api.mustache", ".py");

        apiDocTemplateFiles.remove("api_doc.mustache");
        apiDocTemplateFiles.put("python-experimental/api_doc.mustache", ".md");

        apiTestTemplateFiles.remove("api_test.mustache", ".py");
        apiTestTemplateFiles.put("python-experimental/api_test.mustache", ".py");

        modelDocTemplateFiles.remove("model_doc.mustache");
        modelDocTemplateFiles.put("python-experimental/model_doc.mustache", ".md");

        modelTemplateFiles.remove("model.mustache");
        modelTemplateFiles.put("python-experimental/model.mustache", ".py");

        modelTestTemplateFiles.remove("model_test.mustache", ".py");
        modelTestTemplateFiles.put("python-experimental/model_test.mustache", ".py");

        // this generator does not use SORT_PARAMS_BY_REQUIRED_FLAG
        // this generator uses the following order for endpoint paramters and model properties
        // required params/props with no enum of length one
        // required params/props with enum of length one (which is used to set a default value as a python named arg value)
        // optional params/props with **kwargs in python
        cliOptions.remove(4);

        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.EXPERIMENTAL)
                .build();
    }

    @Override
    public void processOpts() {
        this.setLegacyDiscriminatorBehavior(false);

        super.processOpts();
        modelPackage = packageName + "." + "model";

        supportingFiles.remove(new SupportingFile("api_client.mustache", packagePath(), "api_client.py"));
        supportingFiles.add(new SupportingFile("python-experimental/api_client.mustache", packagePath(), "api_client.py"));

        supportingFiles.add(new SupportingFile("python-experimental/model_utils.mustache", packagePath(), "model_utils.py"));

        supportingFiles.remove(new SupportingFile("__init__model.mustache", packagePath() + File.separatorChar + "models", "__init__.py"));
        supportingFiles.add(new SupportingFile("python-experimental/__init__model.mustache", packagePath() + File.separatorChar + "model", "__init__.py"));

        supportingFiles.remove(new SupportingFile("__init__package.mustache", packagePath(), "__init__.py"));
        supportingFiles.add(new SupportingFile("python-experimental/__init__package.mustache", packagePath(), "__init__.py"));

        // add the models and apis folders
        supportingFiles.add(new SupportingFile("python-experimental/__init__models.mustache", packagePath() + File.separatorChar + "models", "__init__.py"));
        supportingFiles.add(new SupportingFile("python-experimental/__init__apis.mustache", packagePath() + File.separatorChar + "apis", "__init__.py"));

        // Generate the 'signing.py' module, but only if the 'HTTP signature' security scheme is specified in the OAS.
        Map<String, SecurityScheme> securitySchemeMap = openAPI != null ?
           (openAPI.getComponents() != null ? openAPI.getComponents().getSecuritySchemes() : null) : null;
        List<CodegenSecurity> authMethods = fromSecurity(securitySchemeMap);
        if (ProcessUtils.hasHttpSignatureMethods(authMethods)) {
            supportingFiles.add(new SupportingFile("python-experimental/signing.mustache", packagePath(), "signing.py"));
        }

        Boolean generateSourceCodeOnly = false;
        if (additionalProperties.containsKey(CodegenConstants.SOURCECODEONLY_GENERATION)) {
            generateSourceCodeOnly = Boolean.valueOf(additionalProperties.get(CodegenConstants.SOURCECODEONLY_GENERATION).toString());
        }

        // remove what PythonClientCodegen did
        String readmePath = "README.md";
        String readmeTemplate = "README.mustache";
        if (generateSourceCodeOnly) {
            readmePath = packagePath() + "_" + readmePath;
            readmeTemplate = "README_onlypackage.mustache";
        }
        supportingFiles.remove(new SupportingFile(readmeTemplate, "", readmePath));
        // add the correct readme
        readmeTemplate = "python-experimental/README.mustache";
        if (generateSourceCodeOnly) {
            readmeTemplate = "python-experimental/README_onlypackage.mustache";
        }
        supportingFiles.add(new SupportingFile(readmeTemplate, "", readmePath));

        if (!generateSourceCodeOnly) {
          supportingFiles.remove(new SupportingFile("setup.mustache", "", "setup.py"));
          supportingFiles.add(new SupportingFile("python-experimental/setup.mustache", "", "setup.py"));
          supportingFiles.remove(new SupportingFile("requirements.mustache", "", "requirements.txt"));
          supportingFiles.add(new SupportingFile("python-experimental/requirements.mustache", "", "requirements.txt"));
          supportingFiles.remove(new SupportingFile("test-requirements.mustache", "", "test-requirements.txt"));
          supportingFiles.add(new SupportingFile("python-experimental/test-requirements.mustache", "", "test-requirements.txt"));
        }

        // default this to true so the python ModelSimple models will be generated
        ModelUtils.setGenerateAliasAsModel(true);
        LOGGER.info(CodegenConstants.GENERATE_ALIAS_AS_MODEL + " is hard coded to true in this generator. Alias models will only be generated if they contain validations or enums");
    }

    /**
     * Configures a friendly name for the generator.  This will be used by the
     * generator to select the library with the -g flag.
     *
     * @return the friendly name for the generator
     */
    @Override
    public String getName() {
        return "python-experimental";
    }

    public String dateToString(Schema p, OffsetDateTime date, DateTimeFormatter dateFormatter, DateTimeFormatter dateTimeFormatter) {
        // converts a date into a date or date-time python string
        if (!(ModelUtils.isDateSchema(p) || ModelUtils.isDateTimeSchema(p))) {
            throw new RuntimeException("passed schema must be of type Date or DateTime");
        }
        if (ModelUtils.isDateSchema(p)) {
            return "dateutil_parser('" + date.format(dateFormatter) + "').date()";
        }
        return "dateutil_parser('" + date.format(dateTimeFormatter) + "')";
    }

    /**
     * Return the default value of the property
     *
     * @param p OpenAPI property object
     * @return string presentation of the default value of the property
     */
    @Override
    public String toDefaultValue(Schema p) {
        // if a variable has no default set and only has one allowed value
        // using enum of length == 1 we use that value. Server/client usage:
        // python servers: should only use default values for optional params
        // python clients: should only use default values for required params
        Object defaultObject = null;
        Boolean enumLengthOne = (p.getEnum() != null && p.getEnum().size() == 1);
        if (p.getDefault() != null) {
            defaultObject = p.getDefault();
        } else if (enumLengthOne) {
            defaultObject = p.getEnum().get(0);
        }

        // convert datetime and date enums if they exist
        DateTimeFormatter iso8601Date = DateTimeFormatter.ISO_DATE;
        DateTimeFormatter iso8601DateTime = DateTimeFormatter.ISO_DATE_TIME;

        if (ModelUtils.isDateSchema(p) || ModelUtils.isDateTimeSchema(p)) {
            List<Object> currentEnum = p.getEnum();
            List<String> fixedEnum = new ArrayList<String>();
            String fixedValue = null;
            OffsetDateTime date = null;
            if (currentEnum != null && !currentEnum.isEmpty()) {
                for (Object enumItem : currentEnum) {
                    date = (OffsetDateTime) enumItem;
                    fixedValue = dateToString(p, date, iso8601Date, iso8601DateTime);
                    fixedEnum.add(fixedValue);
                }
                p.setEnum(fixedEnum);
            }

            // convert the example if it exists
            Object currentExample = p.getExample();
            if (currentExample != null) {
                try {
                    date = (OffsetDateTime) currentExample;
                } catch (ClassCastException e) {
                    date = ((Date) currentExample).toInstant().atOffset(ZoneOffset.UTC);
                    LOGGER.warn("Invalid `date-time` format for value {}", currentExample);
                }
                fixedValue = dateToString(p, date, iso8601Date, iso8601DateTime);
                fixedEnum.add(fixedValue);
                p.setExample(fixedValue);
                LOGGER.warn(fixedValue);
            }

            // fix defaultObject
            if (defaultObject != null) {
                date = (OffsetDateTime) defaultObject;
                fixedValue = dateToString(p, date, iso8601Date, iso8601DateTime);
                p.setDefault(fixedValue);
                defaultObject = fixedValue;
            }
        }

        if (defaultObject == null) {
            return null;
        }

        String defaultValue = null;
        if (ModelUtils.isStringSchema(p)) {
            defaultValue = defaultObject.toString();
            if (ModelUtils.isDateSchema(p) || ModelUtils.isDateTimeSchema(p)) {
                return defaultValue;
            }

            if (!ModelUtils.isByteArraySchema(p) && !ModelUtils.isBinarySchema(p) && !ModelUtils.isFileSchema(p) && !ModelUtils.isUUIDSchema(p) && !ModelUtils.isEmailSchema(p) && !ModelUtils.isDateTimeSchema(p) && !ModelUtils.isDateSchema(p)) {
                if (Pattern.compile("\r\n|\r|\n").matcher((String) defaultValue).find()) {
                    defaultValue = "'''" + defaultValue + "'''";
                } else {
                    defaultValue = "'" + defaultValue + "'";
                }
            }
            return defaultValue;
        } else if (ModelUtils.isIntegerSchema(p) || ModelUtils.isNumberSchema(p) || ModelUtils.isBooleanSchema(p)) {
            defaultValue = String.valueOf(defaultObject);
            if (ModelUtils.isBooleanSchema(p)) {
                if (Boolean.valueOf(defaultValue) == false) {
                    return "False";
                } else {
                    return "True";
                }
            }
            return defaultValue;
        } else {
            return defaultObject.toString();
        }
    }

    @Override
    public String toModelImport(String name) {
        // name looks like cat.Cat
        String moduleName = name.split("\\.")[0];
        // https://exceptionshub.com/circular-or-cyclic-imports-in-python.html
        return "from " + modelPackage() + " import "+ moduleName;
    }

    private String robustImport(String name) {
        // name looks like cat.Cat
        String moduleName = name.split("\\.")[0];
        // https://exceptionshub.com/circular-or-cyclic-imports-in-python.html
        String modelImport = "try:\n    from " + modelPackage() +
          " import " + moduleName+ "\nexcept ImportError:\n    " +
          moduleName + " = sys.modules[\n        '" + modelPackage() + "." + moduleName + "']";
        return modelImport;
    }

    private String getPythonClassName(String name) {
        // name looks like cat.Cat or Cat
        String[] pieces = name.split("\\.");
        if (pieces.length == 1) {
            return pieces[0];
        }
        return pieces[1];
    }

    private void fixOperationImports(Set<String> imports) {
        if (imports.size() == 0) {
            return;
        }
        String[] modelNames = imports.toArray(new String[0]);
        imports.clear();
        // loops through imports and converts them all from 'module.Class' to 'from  models.module import module'
        for (String modelName : modelNames) {
            // if a modelName lacks the module (Pet) then we convert it to pet.Pet
            if (modelName.indexOf(".") == -1) {
                modelName = toModelName(modelName);
            }
            imports.add(toModelImport(modelName));
        }
    }

    @Override
    @SuppressWarnings("static-method")
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        HashMap<String, Object> val = (HashMap<String, Object>)objs.get("operations");
        ArrayList<CodegenOperation> operations = (ArrayList<CodegenOperation>) val.get("operation");
        ArrayList<HashMap<String, String>> imports = (ArrayList<HashMap<String, String>>)objs.get("imports");
        imports.clear();
        for (CodegenOperation operation : operations) {
            fixOperationImports(operation.imports);
            for (String thisImport : operation.imports) {
                HashMap<String, String> higherImport = new HashMap<String, String>();
                higherImport.put("import", thisImport);
                if (!imports.contains(higherImport)) {
                    imports.add(higherImport);
                }
            }
        }
        return objs;
    }

    private void fixModelImports(Set<String> imports) {
        // loops through imports and converts them all from 'module.Class' to 'import models.module as module'
        if (imports.size() == 0) {
            return;
        }
        String[] modelNames = imports.toArray(new String[0]);
        imports.clear();
        for (String modelName : modelNames) {
            imports.add(robustImport(modelName));
        }
    }

    /**
     * Override with special post-processing for all models.
     */
    @SuppressWarnings({"static-method", "unchecked"})
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
        // loop through all models and delete ones where type!=object and the model has no validations and enums
        // we will remove them because they are not needed
        Map<String, Schema> modelSchemasToRemove = new HashMap<String, Schema>();
        for (Map.Entry<String, Object> entry : objs.entrySet()) {
            Map<String, Object> inner = (Map<String, Object>) entry.getValue();
            List<Map<String, Object>> models = (List<Map<String, Object>>) inner.get("models");
            for (Map<String, Object> mo : models) {
                CodegenModel cm = (CodegenModel) mo.get("model");

                // make sure discriminator models are included in imports
                CodegenDiscriminator discriminator = cm.discriminator;
                if (discriminator != null) {
                    Set<CodegenDiscriminator.MappedModel> mappedModels = discriminator.getMappedModels();
                    for (CodegenDiscriminator.MappedModel mappedModel : mappedModels) {
                        String otherModelName = mappedModel.getModelName();
                        cm.imports.add(otherModelName);
                    }
                }

                // add imports for anyOf, allOf, oneOf
                ArrayList<Set<String>> composedSchemaSets = new ArrayList<Set<String>>();
                composedSchemaSets.add(cm.allOf);
                composedSchemaSets.add(cm.anyOf);
                composedSchemaSets.add(cm.oneOf);
                for (Set<String> importSet : composedSchemaSets) {
                    for (String otherModelName : importSet) {
                        if (!languageSpecificPrimitives.contains(otherModelName)) {
                            cm.imports.add(otherModelName);
                        }
                    }
                }

                Schema modelSchema = ModelUtils.getSchema(this.openAPI, cm.name);
                CodegenProperty modelProperty = fromProperty("value", modelSchema);

                // import complex type from additional properties
                if (cm.additionalPropertiesType != null && modelProperty.items != null && modelProperty.items.complexType != null) {
                    cm.imports.add(modelProperty.items.complexType);
                }

                // fix the imports that each model has, change them to absolute
                fixModelImports(cm.imports);

                if (cm.isEnum || cm.isAlias) {
                    if (!modelProperty.isEnum && !modelProperty.hasValidation && !cm.isArrayModel) {
                        // remove these models because they are aliases and do not have any enums or validations
                        modelSchemasToRemove.put(cm.name, modelSchema);
                    }
                }
            }
        }

        // Remove modelSchemasToRemove models from objs
        for (String modelName : modelSchemasToRemove.keySet()) {
            objs.remove(modelName);
        }
        return objs;
    }

    /**
     * Convert OAS Property object to Codegen Property object
     *
     * @param name name of the property
     * @param p    OAS property object
     * @return Codegen Property object
     */
    @Override
    public CodegenProperty fromProperty(String name, Schema p) {
        // we have a custom version of this function to always set allowableValues.enumVars on all enum variables
        CodegenProperty result = super.fromProperty(name, p);
        if (result.isEnum) {
            updateCodegenPropertyEnum(result);
        }
        return result;
    }

    /**
     * Update codegen property's enum by adding "enumVars" (with name and value)
     *
     * @param var list of CodegenProperty
     */
    @Override
    public void updateCodegenPropertyEnum(CodegenProperty var) {
        // we have a custom version of this method to omit overwriting the defaultValue
        Map<String, Object> allowableValues = var.allowableValues;

        // handle array
        if (var.mostInnerItems != null) {
            allowableValues = var.mostInnerItems.allowableValues;
        }

        if (allowableValues == null) {
            return;
        }

        List<Object> values = (List<Object>) allowableValues.get("values");
        if (values == null) {
            return;
        }

        String varDataType = var.mostInnerItems != null ? var.mostInnerItems.dataType : var.dataType;
        Optional<Schema> referencedSchema = ModelUtils.getSchemas(openAPI).entrySet().stream()
                .filter(entry -> Objects.equals(varDataType, toModelName(entry.getKey())))
                .map(Map.Entry::getValue)
                .findFirst();
        String dataType = (referencedSchema.isPresent()) ? getTypeDeclaration(referencedSchema.get()) : varDataType;

        // put "enumVars" map into `allowableValues", including `name` and `value`
        List<Map<String, Object>> enumVars = buildEnumVars(values, dataType);

        // if "x-enum-varnames" or "x-enum-descriptions" defined, update varnames
        Map<String, Object> extensions = var.mostInnerItems != null ? var.mostInnerItems.getVendorExtensions() : var.getVendorExtensions();
        if (referencedSchema.isPresent()) {
            extensions = referencedSchema.get().getExtensions();
        }
        updateEnumVarsWithExtensions(enumVars, extensions);
        allowableValues.put("enumVars", enumVars);
        // overwriting defaultValue omitted from here
    }

    @Override
    public CodegenParameter fromRequestBody(RequestBody body, Set<String> imports, String bodyParameterName) {
        CodegenParameter result = super.fromRequestBody(body, imports, bodyParameterName);
        // if we generated a model with a non-object type because it has validations or enums,
        // make sure that the datatype of that body parameter refers to our model class
        Content content = body.getContent();
        Set<String> keySet = content.keySet();
        Object[] keyArray = (Object[]) keySet.toArray();
        MediaType mediaType = content.get(keyArray[0]);
        Schema schema = mediaType.getSchema();
        String ref = schema.get$ref();
        if (ref == null) {
            return result;
        }
        String modelName = ModelUtils.getSimpleRef(ref);
        // the result lacks validation info so we need to make a CodegenProperty from the schema to check
        // if we have validation and enum info exists
        Schema realSchema = ModelUtils.getSchema(this.openAPI, modelName);
        CodegenProperty modelProp = fromProperty("body", realSchema);
        if (modelProp.isPrimitiveType && (modelProp.hasValidation || modelProp.isEnum)) {
            String simpleDataType = result.dataType;
            result.dataType = toModelName(modelName);
            result.baseType = getPythonClassName(result.dataType);
            imports.remove(modelName);
            imports.add(result.dataType);
            // set the example value
            if (modelProp.isEnum) {
                String value = modelProp._enum.get(0).toString();
                result.example = result.dataType + "(" + toEnumValue(value, simpleDataType) + ")";
            } else {
                result.example = result.dataType + "(" + result.example + ")";
            }
        } else if (!result.isPrimitiveType) {
            // fix the baseType for the api docs so the .md link to the class's documentation file is correct
            result.baseType = getPythonClassName(result.baseType);
        }
        return result;
    }

    /**
     * Convert OAS Response object to Codegen Response object
     *
     * @param responseCode HTTP response code
     * @param response     OAS Response object
     * @return Codegen Response object
     */
    @Override
    public CodegenResponse fromResponse(String responseCode, ApiResponse response) {
        // if a response points at a model whose type != object and it has validations and/or enums, then we will
        // generate the model, and response.baseType must be the name
        // of the model. Point responses at models if the model is python class type ModelSimple
        // When we serialize/deserialize ModelSimple models, validations and enums will be checked.
        Schema responseSchema;
        if (this.openAPI != null && this.openAPI.getComponents() != null) {
            responseSchema = ModelUtils.unaliasSchema(this.openAPI, ModelUtils.getSchemaFromResponse(response), importMapping);
        } else { // no model/alias defined
            responseSchema = ModelUtils.getSchemaFromResponse(response);
        }

        String newBaseType = null;
        if (responseSchema != null) {
            CodegenProperty cp = fromProperty("response", responseSchema);
            if (cp.complexType != null) {
                String modelName = getPythonClassName(cp.complexType);
                Schema modelSchema = ModelUtils.getSchema(this.openAPI, modelName);
                if (modelSchema != null && !"object".equals(modelSchema.getType())) {
                    CodegenProperty modelProp = fromProperty("response", modelSchema);
                    if (modelProp.isEnum == true || modelProp.hasValidation == true) {
                        // this model has validations and/or enums so we will generate it
                        newBaseType = modelName;
                    }
                }
            } else {
                if (cp.isEnum == true || cp.hasValidation == true) {
                    // this model has validations and/or enums so we will generate it
                    Schema sc = ModelUtils.getSchemaFromResponse(response);
                    newBaseType = ModelUtils.getSimpleRef(sc.get$ref());
                }
            }
        }

        CodegenResponse result = super.fromResponse(responseCode, response);
        if (newBaseType != null) {
            result.dataType = toModelName(newBaseType);
            // baseType is used to set the link to the model .md documentation
            result.baseType = getPythonClassName(newBaseType);
        } else if (!result.primitiveType) {
            // fix the baseType for the api docs so the .md link to the class's documentation file is correct
            result.baseType = getPythonClassName(result.baseType);
        }

        return result;
    }

    /**
     * Set op's returnBaseType, returnType, examples etc.
     *
     * @param operation      endpoint Operation
     * @param schemas        a map of the schemas in the openapi spec
     * @param op             endpoint CodegenOperation
     * @param methodResponse the default ApiResponse for the endpoint
     */
    @Override
    public void handleMethodResponse(Operation operation,
                                     Map<String, Schema> schemas,
                                     CodegenOperation op,
                                     ApiResponse methodResponse) {
        handleMethodResponse(operation, schemas, op, methodResponse, Collections.<String, String>emptyMap());
    }

    /**
     * Set op's returnBaseType, returnType, examples etc.
     *
     * @param operation      endpoint Operation
     * @param schemas        a map of the schemas in the openapi spec
     * @param op             endpoint CodegenOperation
     * @param methodResponse the default ApiResponse for the endpoint
     * @param importMappings mappings of external types to be omitted by unaliasing
     */
    @Override
    protected void handleMethodResponse(Operation operation,
                                        Map<String, Schema> schemas,
                                        CodegenOperation op,
                                        ApiResponse methodResponse,
                                        Map<String, String> importMappings) {
        // we have a custom version of this method to handle endpoints that return models where
        // type != object the model has validations and/or enums
        // we do this by invoking our custom fromResponse method to create defaultResponse
        // which we then use to set op.returnType and op.returnBaseType
        CodegenResponse defaultResponse = fromResponse("defaultResponse", methodResponse);
        Schema responseSchema = ModelUtils.unaliasSchema(this.openAPI, ModelUtils.getSchemaFromResponse(methodResponse), importMappings);

        if (responseSchema != null) {
            op.returnBaseType = defaultResponse.baseType;

            // generate examples
            String exampleStatusCode = "200";
            for (String key : operation.getResponses().keySet()) {
                if (operation.getResponses().get(key) == methodResponse && !key.equals("default")) {
                    exampleStatusCode = key;
                }
            }
            op.examples = new ExampleGenerator(schemas, this.openAPI).generateFromResponseSchema(exampleStatusCode, responseSchema, getProducesInfo(this.openAPI, operation));
            op.defaultResponse = toDefaultValue(responseSchema);
            op.returnType = defaultResponse.dataType;
            op.hasReference = schemas.containsKey(op.returnBaseType);

            // lookup discriminator
            Schema schema = schemas.get(op.returnBaseType);
            if (schema != null) {
                CodegenModel cmod = fromModel(op.returnBaseType, schema);
                op.discriminator = cmod.discriminator;
            }

            if (defaultResponse.isListContainer) {
                op.isListContainer = true;
            } else if (defaultResponse.isMapContainer) {
                op.isMapContainer = true;
            } else {
                op.returnSimpleType = true;
            }
            if (languageSpecificPrimitives().contains(op.returnBaseType) || op.returnBaseType == null) {
                op.returnTypeIsPrimitive = true;
            }
        }
        addHeaders(methodResponse, op.responseHeaders);
    }


    /**
     * Return the sanitized variable name for enum
     *
     * @param value    enum variable name
     * @param datatype data type
     * @return the sanitized variable name for enum
     */
    public String toEnumVarName(String value, String datatype) {
        // our enum var names are keys in a python dict, so change spaces to underscores
        if (value.length() == 0) {
            return "EMPTY";
        }

        String var = value.replaceAll("\\s+", "_").toUpperCase(Locale.ROOT);
        return var;
    }

    /**
     * Return the enum value in the language specified format
     * e.g. status becomes "status"
     *
     * @param value    enum variable name
     * @param datatype data type
     * @return the sanitized value for enum
     */
    public String toEnumValue(String value, String datatype) {
        if (datatype.equals("int") || datatype.equals("float")) {
            return value;
        } else {
            return "\"" + escapeText(value) + "\"";
        }
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty p) {
        postProcessPattern(p.pattern, p.vendorExtensions);
        // set property.complexType so the model docs will link to the ClassName.md
        if (p.complexType != null && !languageSpecificPrimitives.contains(p.complexType)) {
            p.complexType = getPythonClassName(p.complexType);
        } else if (p.isListContainer && p.mostInnerItems.complexType != null && !languageSpecificPrimitives.contains(p.mostInnerItems.complexType)) {
            // fix ListContainers
            p.complexType = getPythonClassName(p.mostInnerItems.complexType);
        }
        // if a model has a property that is of type self, remove the module name from the dataType
        if (p.complexType != null && p.dataType.contains(model.classname)) {
            String classNameNoModule = getPythonClassName(model.classname);
            p.dataType = p.dataType.replace(model.classname, classNameNoModule);
        }
    }

    @Override
    public void postProcessParameter(CodegenParameter p) {
        postProcessPattern(p.pattern, p.vendorExtensions);
        // set baseType to null so the api docs will not point to a model for languageSpecificPrimitives
        if (p.baseType != null && languageSpecificPrimitives.contains(p.baseType)){
            p.baseType = null;
        } else if (p.isListContainer && p.mostInnerItems.complexType != null && !languageSpecificPrimitives.contains(p.mostInnerItems.complexType)) {
            // fix ListContainers
            p.baseType = getPythonClassName(p.mostInnerItems.complexType);
        }
    }

    private void addNullDefaultToOneOfAnyOfReqProps(Schema schema, CodegenModel result){
        // for composed schema models, if the required properties are only from oneOf or anyOf models
        // give them a nulltype.Null so the user can omit including them in python
        ComposedSchema cs = (ComposedSchema) schema;

        // these are the properties that are from properties in self cs or cs allOf
        Map<String, Schema> selfProperties = new LinkedHashMap<String, Schema>();
        List<String> selfRequired = new ArrayList<String>();

        // these are the properties that are from properties in cs oneOf or cs anyOf
        Map<String, Schema> otherProperties = new LinkedHashMap<String, Schema>();
        List<String> otherRequired = new ArrayList<String>();

        List<Schema> oneOfanyOfSchemas = new ArrayList<>();
        List<Schema> oneOf = cs.getOneOf();
        if (oneOf != null) {
            oneOfanyOfSchemas.addAll(oneOf);
        }
        List<Schema> anyOf = cs.getAnyOf();
        if (anyOf != null) {
            oneOfanyOfSchemas.addAll(anyOf);
        }
        for (Schema sc: oneOfanyOfSchemas) {
            Schema refSchema = ModelUtils.getReferencedSchema(this.openAPI, sc);
            addProperties(otherProperties, otherRequired, refSchema);
        }
        Set<String> otherRequiredSet = new HashSet<String>(otherRequired);

        List<Schema> allOf = cs.getAllOf();
        if ((schema.getProperties() != null && !schema.getProperties().isEmpty()) || allOf != null) {
            // NOTE: this function also adds the allOf propesrties inside schema
            addProperties(selfProperties, selfRequired, schema);
        }
        if (result.discriminator != null) {
            selfRequired.add(result.discriminator.getPropertyBaseName());
        }
        Set<String> selfRequiredSet = new HashSet<String>(selfRequired);

        List<CodegenProperty> reqVars = result.getRequiredVars();
        if (reqVars != null) {
            for (CodegenProperty cp: reqVars) {
                String propName = cp.baseName;
                if (otherRequiredSet.contains(propName) && !selfRequiredSet.contains(propName)) {
                    // if var is in otherRequiredSet and is not in selfRequiredSet and is in result.requiredVars
                    // then set it to nullable because the user doesn't have to give a value for it
                    cp.setDefaultValue("nulltype.Null");
                }
            }
        }
    }

    /**
     * Convert OAS Model object to Codegen Model object
     *
     * @param name   the name of the model
     * @param schema OAS Model object
     * @return Codegen Model object
     */
    @Override
    public CodegenModel fromModel(String name, Schema schema) {
        // we have a custom version of this function so we can produce
        // models for components whose type != object and which have validations and enums
        // this ensures that endpoint (operation) responses with validations and enums
        // will generate models, and when those endpoint responses are received in python
        // the response is cast as a model, and the model will validate the response using the enums and validations
        Map<String, String> propertyToModelName = new HashMap<String, String>();
        Map<String, Schema> propertiesMap = schema.getProperties();
        if (propertiesMap != null) {
            for (Map.Entry<String, Schema> entry : propertiesMap.entrySet()) {
                String schemaPropertyName = entry.getKey();
                String pythonPropertyName = toVarName(schemaPropertyName);
                Schema propertySchema = entry.getValue();
                String ref = propertySchema.get$ref();
                if (ref == null) {
                    continue;
                }
                Schema refSchema = ModelUtils.getReferencedSchema(this.openAPI, propertySchema);
                String refType = refSchema.getType();
                if (refType == null || refType.equals("object")) {
                    continue;
                }
                CodegenProperty modelProperty = fromProperty("_fake_name", refSchema);
                if (modelProperty.isEnum == false && modelProperty.hasValidation == false) {
                    continue;
                }
                String modelName = ModelUtils.getSimpleRef(ref);
                propertyToModelName.put(pythonPropertyName, toModelName(modelName));
            }
        }
        CodegenModel result = super.fromModel(name, schema);
        // use this to store the model name like Cat
        // we can't use result.name because that is used to lookup models in the spec
        // we can't use result.classname because that stores cat.Cat
        // we can't use result.classVarName because that stores the variable for making example instances
        result.unescapedDescription = simpleModelName(name);

        // make non-object type models have one property so we can use it to store enums and validations
        if (result.isAlias || result.isEnum || result.isArrayModel) {
            Schema modelSchema = ModelUtils.getSchema(this.openAPI, result.name);
            CodegenProperty modelProperty = fromProperty("value", modelSchema);
            if (modelProperty.isEnum == true || modelProperty.hasValidation == true || result.isArrayModel) {
                // these models are non-object models with enums and/or validations
                // add a single property to the model so we can have a way to access validations
                result.isAlias = true;
                modelProperty.required = true;
                List<CodegenProperty> theProperties = Arrays.asList(modelProperty);
                result.setAllVars(theProperties);
                result.setVars(theProperties);
                result.setRequiredVars(theProperties);
                // post process model properties
                if (result.vars != null) {
                    for (CodegenProperty prop : result.vars) {
                        postProcessModelProperty(result, prop);
                    }
                }
            }
        }

        // set regex values, before it was only done on model.vars
        // fix all property references to non-object models, make those properties non-primitive and
        // set their dataType and complexType to the model name, so documentation will refer to the correct model
        ArrayList<List<CodegenProperty>> listOfLists = new ArrayList<List<CodegenProperty>>();
        listOfLists.add(result.vars);
        listOfLists.add(result.allVars);
        listOfLists.add(result.requiredVars);
        listOfLists.add(result.optionalVars);
        listOfLists.add(result.readOnlyVars);
        listOfLists.add(result.readWriteVars);
        for (List<CodegenProperty> cpList : listOfLists) {
            for (CodegenProperty cp : cpList) {
                // set regex values, before it was only done on model.vars
                postProcessModelProperty(result, cp);
                // fix references to non-object models
                if (!propertyToModelName.containsKey(cp.name)) {
                    continue;
                }
                cp.isPrimitiveType = false;
                String modelName = propertyToModelName.get(cp.name);
                cp.complexType = getPythonClassName(modelName);
                cp.dataType = modelName;
                cp.isEnum = false;
                cp.hasValidation = false;
                result.imports.add(modelName);
            }
        }
        // if a class has a property of type self, remove the self import from imports
        if (result.imports.contains(result.classname)) {
            result.imports.remove(result.classname);
        }

        if (result.requiredVars.size() > 0 && (result.oneOf.size() > 0 || result.anyOf.size() > 0)) {
            addNullDefaultToOneOfAnyOfReqProps(schema, result);
        }

        return result;
    }

    /**
     * Output the type declaration of the property
     *
     * @param schema property schema
     * @return a string presentation of the property type
     */
    public String getSimpleTypeDeclaration(Schema schema) {
        String oasType = getSchemaType(schema);
        if (typeMapping.containsKey(oasType)) {
            return typeMapping.get(oasType);
        }
        return oasType;
    }

    /**
     * Return a string representation of the Python types for the specified OAS schema.
     * Primitive types in the OAS specification are implemented in Python using the corresponding
     * Python primitive types.
     * Composed types (e.g. allAll, oneOf, anyOf) are represented in Python using list of types.
     *
     * The caller should set the prefix and suffix arguments to empty string, except when
     * getTypeString invokes itself recursively. A non-empty prefix/suffix may be specified
     * to wrap the return value in a python dict, list or tuple.
     *
     * Examples:
     * - "bool, date, float"  The data must be a bool, date or float.
     * - "[bool, date]"       The data must be an array, and the array items must be a bool or date.
     *
     * @param p The OAS schema.
     * @param prefix prepended to the returned value.
     * @param suffix appended to the returned value.
     * @param referencedModelNames a list of models that are being referenced while generating the types,
     *          may be used to generate imports.
     * @return a comma-separated string representation of the Python types
     */
    private String getTypeString(Schema p, String prefix, String suffix, List<String> referencedModelNames) {
        String fullSuffix = suffix;
        if (")".equals(suffix)) {
            fullSuffix = "," + suffix;
        }
        if (StringUtils.isNotEmpty(p.get$ref())) {
            // The input schema is a reference. If the resolved schema is
            // a composed schema, convert the name to a Python class.
            Schema s = ModelUtils.getReferencedSchema(this.openAPI, p);
            if (s instanceof ComposedSchema) {
                String modelName = ModelUtils.getSimpleRef(p.get$ref());
                if (referencedModelNames != null) {
                    referencedModelNames.add(modelName);
                }
                return prefix + toModelName(modelName) + fullSuffix;
            }
        }
        if (isAnyTypeSchema(p)) {
            return prefix + "bool, date, datetime, dict, float, int, list, str, none_type" + suffix;
        }
        // Resolve $ref because ModelUtils.isXYZ methods do not automatically resolve references.
        if (ModelUtils.isNullable(ModelUtils.getReferencedSchema(this.openAPI, p))) {
            fullSuffix = ", none_type" + suffix;
        }
        if (isFreeFormObject(p) && getAdditionalProperties(p) == null) {
            return prefix + "bool, date, datetime, dict, float, int, list, str" + fullSuffix;
        }
        if ((ModelUtils.isMapSchema(p) || "object".equals(p.getType())) && getAdditionalProperties(p) != null) {
            Schema inner = getAdditionalProperties(p);
            return prefix + "{str: " + getTypeString(inner, "(", ")", referencedModelNames) + "}" + fullSuffix;
        } else if (ModelUtils.isArraySchema(p)) {
            ArraySchema ap = (ArraySchema) p;
            Schema inner = ap.getItems();
            if (inner == null) {
                // In OAS 3.0.x, the array "items" attribute is required.
                // In OAS >= 3.1, the array "items" attribute is optional such that the OAS
                // specification is aligned with the JSON schema specification.
                // When "items" is not specified, the elements of the array may be anything at all.
                // In that case, the return value should be:
                //    "[bool, date, datetime, dict, float, int, list, str, none_type]"
                // Using recursion to wrap the allowed python types in an array.
                Schema anyType = new Schema(); // A Schema without any attribute represents 'any type'.
                return getTypeString(anyType, "[", "]", referencedModelNames);
            } else {
                return prefix + getTypeString(inner, "[", "]", referencedModelNames) + fullSuffix;
            }
        }
        if (ModelUtils.isFileSchema(p)) {
            return prefix + "file_type" + fullSuffix;
        }
        String baseType = getSimpleTypeDeclaration(p);
        return prefix + baseType + fullSuffix;
    }

    /**
     * Output the type declaration of a given name
     *
     * @param p property schema
     * @return a string presentation of the type
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        // this is used to set dataType, which defines a python tuple of classes
        // in Python we will wrap this in () to make it a tuple but here we
        // will omit the parens so the generated documentaion will not include
        // them
        return getTypeString(p, "", "", null);
    }

    @Override
    public String toInstantiationType(Schema property) {
        if (ModelUtils.isArraySchema(property) || ModelUtils.isMapSchema(property) || property.getAdditionalProperties() != null) {
            return getSchemaType(property);
        }
        return super.toInstantiationType(property);
    }

    @Override
    protected void addAdditionPropertiesToCodeGenModel(CodegenModel codegenModel, Schema schema) {
        Schema addProps = getAdditionalProperties(schema);
        if (addProps != null) {
            // if AdditionalProperties exists, get its datatype and
            // store it in codegenModel.additionalPropertiesType.
            // The 'addProps' may be a reference, getTypeDeclaration will resolve
            // the reference.
            List<String> referencedModelNames = new ArrayList<String>();
            codegenModel.additionalPropertiesType = getTypeString(addProps, "", "", referencedModelNames);
            if (referencedModelNames.size() != 0) {
                // Models that are referenced in the 'additionalPropertiesType' keyword
                // must be added to the imports.
                codegenModel.imports.addAll(referencedModelNames);
            }
        }
        // If addProps is null, the value of the 'additionalProperties' keyword is set
        // to false, i.e. no additional properties are allowed.
    }

    @Override
    public void setParameterExampleValue(CodegenParameter p) {
        // we have a custom version of this function so we can set the file
        // type example value
        String example;

        if (p.defaultValue == null) {
            example = p.example;
        } else {
            p.example = p.defaultValue;
            return;
        }

        String type = p.baseType;
        if (type == null) {
            type = p.dataType;
        }

        if ("String".equalsIgnoreCase(type) || "str".equalsIgnoreCase(type)) {
            if (example == null) {
                example = p.paramName + "_example";
            }
            example = "'" + escapeText(example) + "'";
        } else if ("Integer".equals(type) || "int".equals(type)) {
            if (example == null) {
                example = "56";
            }
        } else if ("Float".equalsIgnoreCase(type) || "Double".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "3.4";
            }
        } else if ("BOOLEAN".equalsIgnoreCase(type) || "bool".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "True";
            }
        } else if ("file".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "/path/to/file";
            }
            example = "open('"+example+"', 'rb')";
        } else if ("Date".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "2013-10-20";
            }
            example = "'" + escapeText(example) + "'";
        } else if ("DateTime".equalsIgnoreCase(type)) {
            if (example == null) {
                example = "2013-10-20T19:20:30+01:00";
            }
            example = "'" + escapeText(example) + "'";
        } else if (!languageSpecificPrimitives.contains(type)) {
            // type is a model class, e.g. user.User
            example = type + "()";
        } else {
            LOGGER.warn("Type " + type + " not handled properly in setParameterExampleValue");
        }

        if (example == null) {
            example = "None";
        } else if (Boolean.TRUE.equals(p.isListContainer)) {
            example = "[" + example + "]";
        } else if (Boolean.TRUE.equals(p.isMapContainer)) {
            example = "{'key': " + example + "}";
        }

        p.example = example;
    }

    private String simpleModelName(String name) {
        // this returns a model name like Cat
        String modelName = sanitizeName(name);
        // remove dollar sign
        modelName = modelName.replaceAll("$", "");

        // model name cannot use reserved keyword, e.g. return
        if (isReservedWord(modelName)) {
            LOGGER.warn(modelName + " (reserved word) cannot be used as model name. Renamed to " + camelize("model_" + modelName));
            modelName = "model_" + modelName; // e.g. return => ModelReturn (after camelize)
        }

        // model name starts with number
        if (modelName.matches("^\\d.*")) {
            LOGGER.warn(modelName + " (model name starts with number) cannot be used as model name. Renamed to " + camelize("model_" + modelName));
            modelName = "model_" + modelName; // e.g. 200Response => Model200Response (after camelize)
        }

        if (!StringUtils.isEmpty(modelNamePrefix)) {
            modelName = modelNamePrefix + "_" + modelName;
        }

        if (!StringUtils.isEmpty(modelNameSuffix)) {
            modelName = modelName + "_" + modelNameSuffix;
        }

        // camelize the model name
        // phone_number => PhoneNumber
        return camelize(modelName);
    }


    @Override
    public String toModelFilename(String name) {
        // underscore the model file name
        // PhoneNumber => phone_number
        return underscore(dropDots(simpleModelName(name)));
    }

    @Override
    public String toModelName(String name) {
        // we have a custom version of this function so we can support circular references in python 2 and 3
        return toModelFilename(name)+"."+simpleModelName(name);
    }

    @Override
    public String toModelDocFilename(String name) {
        // this is used to generate the model's .md documentation file
        return simpleModelName(name);
    }
}
