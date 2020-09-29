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

import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.media.*;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.parameters.RequestBody;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.CodegenDiscriminator.MappedModel;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.github.curiousoddman.rgxgen.RgxGen;

import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.io.File;
import java.util.*;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

import static org.openapitools.codegen.utils.OnceLogger.once;

public class PythonClientExperimentalCodegen extends PythonClientCodegen {
    private static final Logger LOGGER = LoggerFactory.getLogger(PythonClientExperimentalCodegen.class);
    // A cache to efficiently lookup a Schema instance based on the return value of `toModelName()`.
    private Map<String, Schema> modelNameToSchemaCache;
    private DateTimeFormatter iso8601Date = DateTimeFormatter.ISO_DATE;
    private DateTimeFormatter iso8601DateTime = DateTimeFormatter.ISO_DATE_TIME;

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
                .includeGlobalFeatures(
                        GlobalFeature.ParameterizedServer
                )
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

        supportingFiles.remove(new SupportingFile("configuration.mustache", packagePath(), "configuration.py"));
        supportingFiles.add(new SupportingFile("python-experimental/configuration.mustache", packagePath(), "configuration.py"));

        supportingFiles.remove(new SupportingFile("__init__api.mustache", packagePath() + File.separatorChar + "api", "__init__.py"));
        supportingFiles.add(new SupportingFile("python-experimental/__init__api.mustache", packagePath() + File.separatorChar + "api", "__init__.py"));

        supportingFiles.remove(new SupportingFile("exceptions.mustache", packagePath(), "exceptions.py"));
        supportingFiles.add(new SupportingFile("python-experimental/exceptions.mustache", packagePath(), "exceptions.py"));

        if ("urllib3".equals(getLibrary())) {
            supportingFiles.remove(new SupportingFile("rest.mustache", packagePath(), "rest.py"));
            supportingFiles.add(new SupportingFile("python-experimental/rest.mustache", packagePath(), "rest.py"));
        }

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
            supportingFiles.remove(new SupportingFile("travis.mustache", "", ".travis.yml"));
            supportingFiles.add(new SupportingFile("python-experimental/travis.mustache", "", ".travis.yml"));
            supportingFiles.remove(new SupportingFile("gitlab-ci.mustache", "", ".gitlab-ci.yml"));
            supportingFiles.add(new SupportingFile("python-experimental/gitlab-ci.mustache", "", ".gitlab-ci.yml"));
            supportingFiles.remove(new SupportingFile("tox.mustache", "", "tox.ini"));
            supportingFiles.add(new SupportingFile("python-experimental/tox.mustache", "", "tox.ini"));
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

    @Override
    public Schema unaliasSchema(Schema schema, Map<String, String> usedImportMappings) {
        Map<String, Schema> allSchemas = ModelUtils.getSchemas(openAPI);
        if (allSchemas == null || allSchemas.isEmpty()) {
            // skip the warning as the spec can have no model defined
            //LOGGER.warn("allSchemas cannot be null/empty in unaliasSchema. Returned 'schema'");
            return schema;
        }

        if (schema != null && StringUtils.isNotEmpty(schema.get$ref())) {
            String simpleRef = ModelUtils.getSimpleRef(schema.get$ref());
            if (usedImportMappings.containsKey(simpleRef)) {
                LOGGER.debug("Schema unaliasing of {} omitted because aliased class is to be mapped to {}", simpleRef, usedImportMappings.get(simpleRef));
                return schema;
            }
            Schema ref = allSchemas.get(simpleRef);
            if (ref == null) {
                once(LOGGER).warn("{} is not defined", schema.get$ref());
                return schema;
            } else if (ref.getEnum() != null && !ref.getEnum().isEmpty()) {
                // top-level enum class
                return schema;
            } else if (ModelUtils.isArraySchema(ref)) {
                if (ModelUtils.isGenerateAliasAsModel(ref)) {
                    return schema; // generate a model extending array
                } else {
                    return unaliasSchema(allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())),
                            usedImportMappings);
                }
            } else if (ModelUtils.isComposedSchema(ref)) {
                return schema;
            } else if (ModelUtils.isMapSchema(ref)) {
                if (ref.getProperties() != null && !ref.getProperties().isEmpty()) // has at least one property
                    return schema; // treat it as model
                else {
                    if (ModelUtils.isGenerateAliasAsModel(ref)) {
                        return schema; // generate a model extending map
                    } else {
                        // treat it as a typical map
                        return unaliasSchema(allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())),
                                usedImportMappings);
                    }
                }
            } else if (ModelUtils.isObjectSchema(ref)) { // model
                if (ref.getProperties() != null && !ref.getProperties().isEmpty()) { // has at least one property
                    return schema;
                } else {
                    // free form object (type: object)
                    if (ModelUtils.hasValidation(ref)) {
                        return schema;
                    } else if (getAllOfDescendants(simpleRef, openAPI).size() > 0) {
                        return schema;
                    }
                    return unaliasSchema(allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())),
                            usedImportMappings);
                }
            } else if (ModelUtils.hasValidation(ref)) {
                // non object non array non map schemas that have validations
                // are returned so we can generate those schemas as models
                // we do this to:
                // - preserve the validations in that model class in python
                // - use those validations when we use this schema in composed oneOf schemas
                return schema;
            } else {
                return unaliasSchema(allSchemas.get(ModelUtils.getSimpleRef(schema.get$ref())), usedImportMappings);
            }
        }
        return schema;
    }

    public String pythonDate(Object dateValue) {
        String strValue = null;
        if (dateValue instanceof OffsetDateTime) {
            OffsetDateTime date = null;
            try {
                date = (OffsetDateTime) dateValue;
            } catch (ClassCastException e) {
                LOGGER.warn("Invalid `date` format for value {}", dateValue.toString());
                date = ((Date) dateValue).toInstant().atOffset(ZoneOffset.UTC);
            }
            strValue = date.format(iso8601Date);
        } else {
            strValue = dateValue.toString();
        }
        return "dateutil_parser('" + strValue + "').date()";
    }

    public String pythonDateTime(Object dateTimeValue) {
        String strValue = null;
        if (dateTimeValue instanceof OffsetDateTime) {
            OffsetDateTime dateTime = null;
            try {
                dateTime = (OffsetDateTime) dateTimeValue;
            } catch (ClassCastException e) {
                LOGGER.warn("Invalid `date-time` format for value {}", dateTimeValue.toString());
                dateTime = ((Date) dateTimeValue).toInstant().atOffset(ZoneOffset.UTC);
            }
            strValue = dateTime.format(iso8601DateTime);
        } else {
            strValue = dateTimeValue.toString();
        }
        return "dateutil_parser('" + strValue + "')";
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

        if (defaultObject == null) {
            return null;
        }

        String defaultValue = defaultObject.toString();
        if (ModelUtils.isDateSchema(p)) {
            defaultValue = pythonDate(defaultObject);
        } else if (ModelUtils.isDateTimeSchema(p)) {
            defaultValue = pythonDateTime(defaultObject);
        } else if (ModelUtils.isStringSchema(p) && !ModelUtils.isByteArraySchema(p) && !ModelUtils.isBinarySchema(p) && !ModelUtils.isFileSchema(p) && !ModelUtils.isUUIDSchema(p) && !ModelUtils.isEmailSchema(p)) {
            defaultValue =  ensureQuotes(defaultValue);
        } else if (ModelUtils.isBooleanSchema(p)) {
            if (Boolean.valueOf(defaultValue) == false) {
                defaultValue = "False";
            } else {
                defaultValue = "True";
            }
        }
        return defaultValue;
    }

    @Override
    public String toModelImport(String name) {
        // name looks like Cat
        return "from " + modelPackage() + "." + toModelFilename(name) + " import "+ toModelName(name);
    }

    @Override
    @SuppressWarnings("static-method")
    public Map<String, Object> postProcessOperationsWithModels(Map<String, Object> objs, List<Object> allModels) {
        // fix the imports that each model has, add the module reference to the model
        // loops through imports and converts them all
        // from 'Pet' to 'from petstore_api.model.pet import Pet'

        HashMap<String, Object> val = (HashMap<String, Object>)objs.get("operations");
        ArrayList<CodegenOperation> operations = (ArrayList<CodegenOperation>) val.get("operation");
        ArrayList<HashMap<String, String>> imports = (ArrayList<HashMap<String, String>>)objs.get("imports");
        for (CodegenOperation operation : operations) {
            if (operation.imports.size() == 0) {
                continue;
            }
            String[] modelNames = operation.imports.toArray(new String[0]);
            operation.imports.clear();
            for (String modelName : modelNames) {
                operation.imports.add(toModelImport(modelName));
            }
        }
        return objs;
    }

    /***
     * Override with special post-processing for all models.
     * we have a custom version of this method to:
     * - remove any primitive models that do not contain validations
     *      these models are unaliased as inline definitions wherever the spec has them as refs
     *      this means that the generated client does not use these models
     *      because they are not used we do not write them
     * - fix the model imports, go from model name to the full import string with toModelImport + globalImportFixer
     *
     * @param objs a map going from the model name to a object hoding the model info
     * @return the updated objs
     */
    @Override
    public Map<String, Object> postProcessAllModels(Map<String, Object> objs) {
         super.postProcessAllModels(objs);

        List<String> modelsToRemove = new ArrayList<>();
        Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
        for (String schemaName: allDefinitions.keySet()) {
            Schema refSchema = new Schema().$ref("#/components/schemas/"+schemaName);
            Schema unaliasedSchema = unaliasSchema(refSchema, importMapping);
            String modelName = toModelName(schemaName);
            if (unaliasedSchema.get$ref() == null) {
                modelsToRemove.add(modelName);
            } else {
                HashMap<String, Object> objModel = (HashMap<String, Object>) objs.get(modelName);
                List<Map<String, Object>> models = (List<Map<String, Object>>) objModel.get("models");
                for (Map<String, Object> model : models) {
                    CodegenModel cm = (CodegenModel) model.get("model");
                    String[] importModelNames = cm.imports.toArray(new String[0]);
                    cm.imports.clear();
                    for (String importModelName : importModelNames) {
                        cm.imports.add(toModelImport(importModelName));
                        String globalImportFixer = "globals()['" + importModelName + "'] = " + importModelName;
                        cm.imports.add(globalImportFixer);
                    }
                }
            }
        }

        for (String modelName : modelsToRemove) {
            objs.remove(modelName);
        }

        return objs;
    }

    /**
     * Convert OAS Property object to Codegen Property object
     * We have a custom version of this method to always set allowableValues.enumVars on all enum variables
     * Together with unaliasSchema this sets primitive types with validations as models
     * This method is used by fromResponse
     *
     * @param name name of the property
     * @param p    OAS property object
     * @return Codegen Property object
     */
    @Override
    public CodegenProperty fromProperty(String name, Schema p) {
        CodegenProperty cp = super.fromProperty(name, p);
        if (cp.isEnum) {
            updateCodegenPropertyEnum(cp);
        }
        if (cp.isPrimitiveType && p.get$ref() != null) {
            cp.complexType = cp.dataType;
        }
        if (cp.isListContainer && cp.complexType == null && cp.mostInnerItems.complexType != null) {
            cp.complexType = cp.mostInnerItems.complexType;
        }
        return cp;
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
        Schema referencedSchema = getModelNameToSchemaCache().get(varDataType);
        String dataType = (referencedSchema != null) ? getTypeDeclaration(referencedSchema) : varDataType;

        // put "enumVars" map into `allowableValues", including `name` and `value`
        List<Map<String, Object>> enumVars = buildEnumVars(values, dataType);

        // if "x-enum-varnames" or "x-enum-descriptions" defined, update varnames
        Map<String, Object> extensions = var.mostInnerItems != null ? var.mostInnerItems.getVendorExtensions() : var.getVendorExtensions();
        if (referencedSchema != null) {
            extensions = referencedSchema.getExtensions();
        }
        updateEnumVarsWithExtensions(enumVars, extensions);
        allowableValues.put("enumVars", enumVars);
        // overwriting defaultValue omitted from here
    }

    /***
     * We have a custom version of this method to produce links to models when they are
     * primitive type (not map, not array, not object) and include validations or are enums
     *
     * @param body requesst body
     * @param imports import collection
     * @param bodyParameterName body parameter name
     * @return the resultant CodegenParameter
     */
    @Override
    public CodegenParameter fromRequestBody(RequestBody body, Set<String> imports, String bodyParameterName) {
        CodegenParameter cp = super.fromRequestBody(body, imports, bodyParameterName);
        Schema schema = ModelUtils.getSchemaFromRequestBody(body);
        if (schema.get$ref() == null) {
            return cp;
        }
        Schema unaliasedSchema = unaliasSchema(schema, importMapping);
        CodegenProperty unaliasedProp = fromProperty("body", unaliasedSchema);
        Boolean dataTypeMismatch = !cp.dataType.equals(unaliasedProp.dataType);
        Boolean baseTypeMismatch = !cp.baseType.equals(unaliasedProp.complexType) && unaliasedProp.complexType != null;
        if (dataTypeMismatch || baseTypeMismatch) {
            cp.dataType = unaliasedProp.dataType;
            cp.baseType = unaliasedProp.complexType;
        }
        return cp;
    }

    /***
     * Adds the body model schema to the body parameter
     * We have a custom version of this method so we can flip forceSimpleRef
     * to True based upon the results of unaliasSchema
     * With this customization, we ensure that when schemas are passed to getSchemaType
     * - if they have ref in them they are a model
     * - if they do not have ref in them they are not a model
     *
     * @param codegenParameter the body parameter
     * @param name model schema ref key in components
     * @param schema the model schema (not refed)
     * @param imports collection of imports
     * @param bodyParameterName body parameter name
     * @param forceSimpleRef if true use a model reference
     */
    @Override
    protected void addBodyModelSchema(CodegenParameter codegenParameter, String name, Schema schema, Set<String> imports, String bodyParameterName, boolean forceSimpleRef) {
        if (name != null) {
            Schema bodySchema = new Schema().$ref("#/components/schemas/" + name);
            Schema unaliased = unaliasSchema(bodySchema, importMapping);
            if (unaliased.get$ref() != null) {
                forceSimpleRef = true;
            }
        }
        super.addBodyModelSchema(codegenParameter, name, schema, imports, bodyParameterName, forceSimpleRef);

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
            return ensureQuotes(value);
        }
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty p) {
        postProcessPattern(p.pattern, p.vendorExtensions);
        // set property.complexType so the model docs will link to the ClassName.md
        if (p.complexType == null && p.isListContainer && p.mostInnerItems.complexType != null && !languageSpecificPrimitives.contains(p.mostInnerItems.complexType)) {
            // fix ListContainers
            p.complexType = p.mostInnerItems.complexType;
        }
    }

    @Override
    public void postProcessParameter(CodegenParameter p) {
        postProcessPattern(p.pattern, p.vendorExtensions);
        if (p.baseType != null && languageSpecificPrimitives.contains(p.baseType)){
            // set baseType to null so the api docs will not point to a model for languageSpecificPrimitives
            p.baseType = null;
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
     * Sets the value of the 'model.parent' property in CodegenModel
     * We have a custom version of this function so we can add the dataType on the ArrayModel
    */
    @Override
    protected void addParentContainer(CodegenModel model, String name, Schema schema) {
        super.addParentContainer(model, name, schema);

        List<String> referencedModelNames = new ArrayList<String>();
        model.dataType = getTypeString(schema, "", "", referencedModelNames);
    }

    /**
     * Convert OAS Model object to Codegen Model object
     * We have a custom version of this method so we can:
     * - set the correct regex values for requiredVars + optionalVars
     * - set model.defaultValue and model.hasRequired per the three use cases defined in this method
     *
     * @param name   the name of the model
     * @param sc OAS Model object
     * @return Codegen Model object
     */
    @Override
    public CodegenModel fromModel(String name, Schema sc) {
        CodegenModel cm = super.fromModel(name, sc);
        if (cm.requiredVars.size() > 0 && (cm.oneOf.size() > 0 || cm.anyOf.size() > 0)) {
            addNullDefaultToOneOfAnyOfReqProps(sc, cm);
        }
        ArrayList<List<CodegenProperty>> listOfLists = new ArrayList<List<CodegenProperty>>();
        listOfLists.add(cm.requiredVars);
        listOfLists.add(cm.optionalVars);
        for (List<CodegenProperty> cpList : listOfLists) {
            for (CodegenProperty cp : cpList) {
                // sets regex values
                postProcessModelProperty(cm, cp);
            }
        }
        Boolean isNotPythonModelSimpleModel = (ModelUtils.isComposedSchema(sc) || ModelUtils.isObjectSchema(sc) || ModelUtils.isMapSchema(sc));
        if (isNotPythonModelSimpleModel) {
            return cm;
        }
        // Use cases for default values / enums of length one
        // 1. no default exists
        //      schema does not contain default
        //      cm.defaultValue unset, cm.hasRequired = true
        // 2. server has a default
        //      schema contains default
        //      cm.defaultValue set, cm.hasRequired = true
        //      different value here to differentiate between use case 3 below
        //      This defaultValue is used in the client docs only and is not sent to the server
        // 3. only one value is allowed in an enum
        //      schema does not contain default
        //      cm.defaultValue set, cm.hasRequired = false
        //      because we know what value needs to be set so the user doesn't need to input it
        //      This defaultValue is used in the client and is sent to the server
        String defaultValue = toDefaultValue(sc);
        if (sc.getDefault() == null && defaultValue == null) {
            cm.hasRequired = true;
        } else if (sc.getDefault() != null) {
            cm.defaultValue = defaultValue;
            cm.hasRequired = true;
        } else if (defaultValue != null && cm.defaultValue == null) {
            cm.defaultValue = defaultValue;
            cm.hasRequired = false;
        }
        return cm;
    }

    /**
     * Returns the python type for the property.
     *
     * @param schema property schema
     * @return string presentation of the type
     **/
    @SuppressWarnings("static-method")
    @Override
    public String getSchemaType(Schema schema) {
        String openAPIType = getSingleSchemaType(schema);
        if (typeMapping.containsKey(openAPIType)) {
            String type = typeMapping.get(openAPIType);
            return type;
        }
        return toModelName(openAPIType);
    }

    public String getModelName(Schema sc) {
        if (sc.get$ref() != null) {
            Schema unaliasedSchema = unaliasSchema(sc, importMapping);
            if (unaliasedSchema.get$ref() != null) {
                return toModelName(ModelUtils.getSimpleRef(sc.get$ref()));
            }
        }
        return null;
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
            Schema unaliasedSchema = unaliasSchema(p, importMapping);
            if (unaliasedSchema.get$ref() != null) {
                String modelName = toModelName(ModelUtils.getSimpleRef(p.get$ref()));
                if (referencedModelNames != null) {
                    referencedModelNames.add(modelName);
                }
                return prefix + modelName + fullSuffix;
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
        String baseType = getSchemaType(p);
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

    /**
     * Gets an example if it exists
     *
     * @param sc input schema
     * @return the example value
     */
    protected Object getObjectExample(Schema sc) {
        Schema schema = sc;
        String ref = sc.get$ref();
        if (ref != null) {
            schema = ModelUtils.getSchema(this.openAPI, ModelUtils.getSimpleRef(ref));
        }
        // TODO handle examples in object models in the future
        Boolean objectModel = (ModelUtils.isObjectSchema(schema) || ModelUtils.isMapSchema(schema) || ModelUtils.isComposedSchema(schema));
        if (objectModel) {
            return null;
        }
        if (schema.getExample() != null) {
            return schema.getExample();
        } if (schema.getDefault() != null) {
            return schema.getDefault();
        } else if (schema.getEnum() != null && !schema.getEnum().isEmpty()) {
            return schema.getEnum().get(0);
        }
        return null;
    }

    /***
     * Ensures that the string has a leading and trailing quote
     *
     * @param in input string
     * @return quoted string
     */
    private String ensureQuotes(String in) {
        Pattern pattern = Pattern.compile("\r\n|\r|\n");
        Matcher matcher = pattern.matcher(in);
        if (matcher.find()) {
            // if a string has a new line in it add triple quotes to make it a python multiline string
            return "'''" + in + "'''";
        }
        String strPattern = "^['\"].*?['\"]$";
        if (in.matches(strPattern)) {
            return in;
        }
        return "\"" + in + "\"";
    }

    public String toExampleValue(Schema schema, Object objExample) {
        String modelName = getModelName(schema);
        return toExampleValueRecursive(modelName, schema, objExample, 1, "", 0);
    }

    private Boolean simpleStringSchema(Schema schema) {
        Schema sc = schema;
        String ref = schema.get$ref();
        if (ref != null) {
            sc = ModelUtils.getSchema(this.openAPI, ModelUtils.getSimpleRef(ref));
        }
        if (ModelUtils.isStringSchema(sc) && !ModelUtils.isDateSchema(sc) && !ModelUtils.isDateTimeSchema(sc) && !"Number".equalsIgnoreCase(sc.getFormat()) && !ModelUtils.isByteArraySchema(sc) && !ModelUtils.isBinarySchema(sc) && schema.getPattern() == null) {
            return true;
        }
        return false;
    }

    private MappedModel getDiscriminatorMappedModel(CodegenDiscriminator disc) {
        for ( MappedModel mm : disc.getMappedModels() ) {
            String modelName = mm.getModelName();
            Schema modelSchema = getModelNameToSchemaCache().get(modelName);
            if (ModelUtils.isObjectSchema(modelSchema)) {
                return mm;
            }
        }
        return null;
    }

    /***
     * Recursively generates string examples for schemas
     *
     * @param modelName the string name of the refed model that will be generated for the schema or null
     * @param schema the schema that we need an example for
     * @param objExample the example that applies to this schema, for now only string example are used
     * @param indentationLevel integer indentation level that we are currently at
     *                         we assume the indentaion amount is 4 spaces times this integer
     * @param prefix the string prefix that we will use when assigning an example for this line
     *               this is used when setting key: value, pairs "key: " is the prefix
     *               and this is used when setting properties like some_property='some_property_example'
     * @param exampleLine this is the current line that we are generatign an example for, starts at 0
     *                    we don't indentin the 0th line because using the example value looks like:
     *                    prop = ModelName( line 0
     *                        some_property='some_property_example' line 1
     *                    ) line 2
     *                    and our example value is:
     *                    ModelName( line 0
     *                        some_property='some_property_example' line 1
     *                    ) line 2
     * @return the string example
     */
    private String toExampleValueRecursive(String modelName, Schema schema, Object objExample, int indentationLevel, String prefix, Integer exampleLine) {
        final String indentionConst = "    ";
        String currentIndentation = "";
        String closingIndentation = "";
        for (int i=0 ; i < indentationLevel ; i++) currentIndentation += indentionConst;
        if (exampleLine.equals(0)) {
            closingIndentation = currentIndentation;
            currentIndentation = "";
        } else {
            closingIndentation = currentIndentation;
        }
        String openChars = "";
        String closeChars = "";
        if (modelName != null) {
            openChars = modelName+"(";
            closeChars = ")";
        }

        String fullPrefix = currentIndentation + prefix + openChars;

        String example = null;
        if (objExample != null) {
            example = objExample.toString();
        }
        if (null != schema.get$ref()) {
            Map<String, Schema> allDefinitions = ModelUtils.getSchemas(this.openAPI);
            String ref = ModelUtils.getSimpleRef(schema.get$ref());
            Schema refSchema = allDefinitions.get(ref);
            if (null == refSchema) {
                LOGGER.warn("Unable to find referenced schema "+schema.get$ref()+"\n");
                return fullPrefix + "None" + closeChars;
            }
            String refModelName = getModelName(schema);
            return toExampleValueRecursive(refModelName, refSchema, objExample, indentationLevel, prefix, exampleLine);
        } else if (ModelUtils.isNullType(schema) || isAnyTypeSchema(schema)) {
            // The 'null' type is allowed in OAS 3.1 and above. It is not supported by OAS 3.0.x,
            // though this tooling supports it.
            return fullPrefix + "None" + closeChars;
        } else if (ModelUtils.isBooleanSchema(schema)) {
            if (objExample == null) {
                example = "True";
            } else {
                if ("false".equalsIgnoreCase(objExample.toString())) {
                    example = "False";
                } else {
                    example = "True";
                }
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isDateSchema(schema)) {
            if (objExample == null) {
                example = pythonDate("1970-01-01");
            } else {
                example = pythonDate(objExample);
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isDateTimeSchema(schema)) {
            if (objExample == null) {
                example = pythonDateTime("1970-01-01T00:00:00.00Z");
            } else {
                example = pythonDateTime(objExample);
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isBinarySchema(schema)) {
            if (objExample == null) {
                example = "/path/to/file";
            }
            example = "open('" + example + "', 'rb')";
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isByteArraySchema(schema)) {
            if (objExample == null) {
                example = "'YQ=='";
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isStringSchema(schema)) {
            if (objExample == null) {
                // a BigDecimal:
                if ("Number".equalsIgnoreCase(schema.getFormat())) {
                    example = "2";
                    return  fullPrefix + example + closeChars;
                } else if (StringUtils.isNotBlank(schema.getPattern())) {
                    String pattern = schema.getPattern();
                    RgxGen rgxGen = new RgxGen(pattern);
                    // this seed makes it so if we have [a-z] we pick a
                    Random random = new Random(18);
                    String sample = rgxGen.generate(random);
                    // omit leading / and trailing /, omit trailing /i
                    Pattern valueExtractor = Pattern.compile("^/?(.+?)/?.?$");
                    Matcher m = valueExtractor.matcher(sample);
                    if (m.find()) {
                        example = m.group(m.groupCount());
                    } else {
                        example = "";
                    }
                } else if (schema.getMinLength() != null) {
                    example = "";
                    int len = schema.getMinLength().intValue();
                    for (int i=0;i<len;i++) example += "a";
                } else if (ModelUtils.isUUIDSchema(schema)) {
                    example = "046b6c7f-0b8a-43b9-b35d-6489e6daee91";
                } else {
                    example = "string_example";
                }
            }
            return  fullPrefix + ensureQuotes(example) + closeChars;
        } else if (ModelUtils.isIntegerSchema(schema)) {
            if (objExample == null) {
                if (schema.getMinimum() != null) {
                    example = schema.getMinimum().toString();
                } else {
                    example = "1";
                }
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isNumberSchema(schema)) {
            if (objExample == null) {
                if (schema.getMinimum() != null) {
                    example = schema.getMinimum().toString();
                } else {
                    example = "3.14";
                }
            }
            return fullPrefix + example + closeChars;
        } else if (ModelUtils.isArraySchema(schema)) {
            ArraySchema arrayschema = (ArraySchema) schema;
            Schema itemSchema = arrayschema.getItems();
            String itemModelName = getModelName(itemSchema);
            example = fullPrefix + "[" + "\n" + toExampleValueRecursive(itemModelName, itemSchema, objExample, indentationLevel+1, "", exampleLine+1) + ",\n" + closingIndentation + "]" + closeChars;
            return example;
        } else if (ModelUtils.isMapSchema(schema)) {
            if (modelName == null) {
                fullPrefix += "{";
                closeChars = "}";
            }
            Object addPropsObj = schema.getAdditionalProperties();
            // TODO handle true case for additionalProperties
            if (addPropsObj instanceof Schema) {
                Schema addPropsSchema = (Schema) addPropsObj;
                String key = "key";
                Object addPropsExample = getObjectExample(addPropsSchema);
                if (addPropsSchema.getEnum() != null && !addPropsSchema.getEnum().isEmpty()) {
                    key = addPropsSchema.getEnum().get(0).toString();
                }
                addPropsExample = exampleFromStringOrArraySchema(addPropsSchema, addPropsExample, key);
                String addPropPrefix = ensureQuotes(key) + ": ";
                String addPropsModelName = getModelName(addPropsSchema);
                example = fullPrefix + "\n" +  toExampleValueRecursive(addPropsModelName, addPropsSchema, addPropsExample, indentationLevel + 1, addPropPrefix, exampleLine + 1) + ",\n" + closingIndentation + closeChars;
            } else {
                example = fullPrefix + closeChars;
            }
            return example;
        } else if (ModelUtils.isObjectSchema(schema)) {
            if (modelName == null) {
                fullPrefix += "{";
                closeChars = "}";
            }
            CodegenDiscriminator disc = createDiscriminator(modelName, schema, openAPI);
            if (disc != null) {
                MappedModel mm = getDiscriminatorMappedModel(disc);
                if (mm != null) {
                    String discPropNameValue = mm.getMappingName();
                    String chosenModelName = mm.getModelName();
                    // TODO handle this case in the future, this is when the discriminated
                    // schema allOf includes this schema, like Cat allOf includes Pet
                    // so this is the composed schema use case
                } else {
                    return fullPrefix + closeChars;
                }
            }
            return exampleForObjectModel(schema, fullPrefix, closeChars, null, indentationLevel, exampleLine, closingIndentation);
        } else if (ModelUtils.isComposedSchema(schema)) {
            // TODO add examples for composed schema models without discriminators

            CodegenDiscriminator disc = createDiscriminator(modelName, schema, openAPI);
            if (disc != null) {
                MappedModel mm = getDiscriminatorMappedModel(disc);
                if (mm != null) {
                    String discPropNameValue = mm.getMappingName();
                    String chosenModelName = mm.getModelName();
                    Schema modelSchema = getModelNameToSchemaCache().get(chosenModelName);
                    CodegenProperty cp = new CodegenProperty();
                    cp.setName(disc.getPropertyName());
                    cp.setExample(discPropNameValue);
                    return exampleForObjectModel(modelSchema, fullPrefix, closeChars, cp, indentationLevel, exampleLine, closingIndentation);
                } else {
                    return fullPrefix + closeChars;
                }
            }
            return fullPrefix + closeChars;
        } else {
            LOGGER.warn("Type " + schema.getType() + " not handled properly in toExampleValue");
        }

        return example;
    }

    private String exampleForObjectModel(Schema schema, String fullPrefix, String closeChars, CodegenProperty discProp, int indentationLevel, int exampleLine, String closingIndentation) {
        Map<String, Schema> requiredAndOptionalProps = schema.getProperties();
        if (requiredAndOptionalProps == null || requiredAndOptionalProps.isEmpty()) {
            return fullPrefix + closeChars;
        }

        String example = fullPrefix + "\n";
        for (Map.Entry<String, Schema> entry : requiredAndOptionalProps.entrySet()) {
            String propName = entry.getKey();
            Schema propSchema = entry.getValue();
            propName = toVarName(propName);
            String propModelName = null;
            Object propExample = null;
            if (discProp != null && propName.equals(discProp.name)) {
                propModelName = null;
                propExample = discProp.example;
            } else {
                propModelName = getModelName(propSchema);
                propExample = exampleFromStringOrArraySchema(propSchema, null, propName);
            }
            example += toExampleValueRecursive(propModelName, propSchema, propExample, indentationLevel + 1, propName + "=", exampleLine + 1) + ",\n";
        }
        // TODO handle additionalProperties also
        example += closingIndentation + closeChars;
        return example;
    }

    private Object exampleFromStringOrArraySchema(Schema sc, Object currentExample, String propName) {
        if (currentExample != null) {
            return currentExample;
        }
        Schema schema = sc;
        String ref = sc.get$ref();
        if (ref != null) {
            schema = ModelUtils.getSchema(this.openAPI, ModelUtils.getSimpleRef(ref));
        }
        Object example = getObjectExample(schema);
        if (example != null) {
            return example;
        } else if (simpleStringSchema(schema)) {
            return propName + "_example";
        } else if (ModelUtils.isArraySchema(schema)) {
            ArraySchema arraySchema = (ArraySchema) schema;
            Schema itemSchema = arraySchema.getItems();
            example = getObjectExample(itemSchema);
            if (example != null ) {
                return example;
            } else if (simpleStringSchema(itemSchema)) {
                return propName + "_example";
            }
        }
        return null;
    }


    /***
     *
     * Set the codegenParameter example value
     * We have a custom version of this function so we can invoke toExampleValue
     *
     * @param codegenParameter the item we are setting the example on
     * @param parameter the base parameter that came from the spec
     */
    @Override
    public void setParameterExampleValue(CodegenParameter codegenParameter, Parameter parameter) {
        Schema schema = parameter.getSchema();
        if (schema == null) {
            LOGGER.warn("CodegenParameter.example defaulting to null because parameter lacks a schema");
            return;
        }

        Object example = null;
        if (codegenParameter.vendorExtensions != null && codegenParameter.vendorExtensions.containsKey("x-example")) {
            example = codegenParameter.vendorExtensions.get("x-example");
        } else if (parameter.getExample() != null) {
            example = parameter.getExample();
        } else if (parameter.getExamples() != null && !parameter.getExamples().isEmpty() && parameter.getExamples().values().iterator().next().getValue() != null) {
            example = parameter.getExamples().values().iterator().next().getValue();
        } else {
            example = getObjectExample(schema);
        }
        example = exampleFromStringOrArraySchema(schema, example, parameter.getName());
        String finalExample = toExampleValue(schema, example);
        codegenParameter.example = finalExample;
    }

    /**
     * Return the example value of the parameter.
     *
     * @param codegenParameter Codegen parameter
     * @param requestBody      Request body
     */
    @Override
    public void setParameterExampleValue(CodegenParameter codegenParameter, RequestBody requestBody) {
        if (codegenParameter.vendorExtensions != null && codegenParameter.vendorExtensions.containsKey("x-example")) {
            codegenParameter.example = Json.pretty(codegenParameter.vendorExtensions.get("x-example"));
        }

        Content content = requestBody.getContent();

        if (content.size() > 1) {
            // @see ModelUtils.getSchemaFromContent()
            once(LOGGER).warn("Multiple MediaTypes found, using only the first one");
        }

        MediaType mediaType = content.values().iterator().next();
        Schema schema = mediaType.getSchema();
        if (schema == null) {
            LOGGER.warn("CodegenParameter.example defaulting to null because requestBody content lacks a schema");
            return;
        }

        Object example = null;
        if (mediaType.getExample() != null) {
            example = mediaType.getExample();
        } else if (mediaType.getExamples() != null && !mediaType.getExamples().isEmpty() && mediaType.getExamples().values().iterator().next().getValue() != null) {
            example = mediaType.getExamples().values().iterator().next().getValue();
        } else {
            example = getObjectExample(schema);
        }
        example = exampleFromStringOrArraySchema(schema, example, codegenParameter.paramName);
        codegenParameter.example = toExampleValue(schema, example);
    }

    /**
     * Create a CodegenParameter for a Form Property
     * We have a custom version of this method so we can invoke
     * setParameterExampleValue(codegenParameter, parameter)
     * rather than setParameterExampleValue(codegenParameter)
     * This ensures that all of our samples are generated in
     * toExampleValueRecursive
     *
     * @param name the property name
     * @param propertySchema the property schema
     * @param imports our import set
     * @return the resultant CodegenParameter
     */
    @Override
    public CodegenParameter fromFormProperty(String name, Schema propertySchema, Set<String> imports) {
        CodegenParameter cp = super.fromFormProperty(name, propertySchema, imports);
        Parameter p = new Parameter();
        p.setSchema(propertySchema);
        p.setName(cp.paramName);
        setParameterExampleValue(cp, p);
        return cp;
    }

    /**
     * Return a map from model name to Schema for efficient lookup.
     *
     * @return map from model name to Schema.
     */
    protected Map<String, Schema> getModelNameToSchemaCache() {
        if (modelNameToSchemaCache == null) {
            // Create a cache to efficiently lookup schema based on model name.
            Map<String, Schema> m = new HashMap<String, Schema>();
            ModelUtils.getSchemas(openAPI).forEach((key, schema) -> {
                m.put(toModelName(key), schema);
            });
            modelNameToSchemaCache = Collections.unmodifiableMap(m);
        }
        return modelNameToSchemaCache;
    }
}
