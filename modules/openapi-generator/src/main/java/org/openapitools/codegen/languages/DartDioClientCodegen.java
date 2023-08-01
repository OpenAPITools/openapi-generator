/*
 * Copyright 2021 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import com.google.common.collect.Sets;
import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Discriminator;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.CodegenDiscriminator.MappedModel;
import org.openapitools.codegen.api.TemplatePathLocator;
import org.openapitools.codegen.config.GlobalSettings;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.ClientModificationFeature;
import org.openapitools.codegen.meta.features.SchemaSupportFeature;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.templating.CommonTemplateContentLocator;
import org.openapitools.codegen.templating.GeneratorTemplateContentLocator;
import org.openapitools.codegen.templating.MustacheEngineAdapter;
import org.openapitools.codegen.templating.TemplateManagerOptions;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.ProcessUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.StringUtils.underscore;

public class DartDioClientCodegen extends AbstractDartCodegen {

    private final Logger LOGGER = LoggerFactory.getLogger(DartDioClientCodegen.class);

    public static final String DATE_LIBRARY = "dateLibrary";
    public static final String DATE_LIBRARY_CORE = "core";
    public static final String DATE_LIBRARY_TIME_MACHINE = "timemachine";
    public static final String DATE_LIBRARY_DEFAULT = DATE_LIBRARY_CORE;

    public static final String SERIALIZATION_LIBRARY_BUILT_VALUE = "built_value";
    public static final String SERIALIZATION_LIBRARY_JSON_SERIALIZABLE = "json_serializable";
    public static final String SERIALIZATION_LIBRARY_DEFAULT = SERIALIZATION_LIBRARY_BUILT_VALUE;

    private static final String DIO_IMPORT = "package:dio/dio.dart";
    public static final String FINAL_PROPERTIES = "finalProperties";
    public static final String FINAL_PROPERTIES_DEFAULT_VALUE = "true";

    private static final String CLIENT_NAME = "clientName";

    private String dateLibrary;

    private String clientName;

    private TemplateManager templateManager;

    public DartDioClientCodegen() {
        super();

        modifyFeatureSet(features -> features
                .includeClientModificationFeatures(
                        ClientModificationFeature.Authorizations,
                        ClientModificationFeature.UserAgent
                ).includeSchemaSupportFeatures(
                    SchemaSupportFeature.Polymorphism,
                    SchemaSupportFeature.Union,
                    SchemaSupportFeature.Composite,
                    SchemaSupportFeature.allOf,
                    SchemaSupportFeature.oneOf,
                    SchemaSupportFeature.anyOf
                )
        );
        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.STABLE)
                .build();

        outputFolder = "generated-code/dart-dio";
        embeddedTemplateDir = "dart/libraries/dio";
        this.setTemplateDir(embeddedTemplateDir);

        supportedLibraries.put(SERIALIZATION_LIBRARY_BUILT_VALUE, "[DEFAULT] built_value");
        supportedLibraries.put(SERIALIZATION_LIBRARY_JSON_SERIALIZABLE, "[BETA] json_serializable");
        final CliOption serializationLibrary = CliOption.newString(CodegenConstants.SERIALIZATION_LIBRARY, "Specify serialization library");
        serializationLibrary.setEnum(supportedLibraries);
        serializationLibrary.setDefault(SERIALIZATION_LIBRARY_DEFAULT);
        cliOptions.add(serializationLibrary);

        // Date Library Option
        final CliOption dateOption = CliOption.newString(DATE_LIBRARY, "Specify Date library");
        dateOption.setDefault(DATE_LIBRARY_DEFAULT);

        final CliOption finalProperties = CliOption.newBoolean(FINAL_PROPERTIES, "Whether properties are marked as final when using Json Serializable for serialization");
        finalProperties.setDefault("true");
        cliOptions.add(finalProperties);

        final Map<String, String> dateOptions = new HashMap<>();
        dateOptions.put(DATE_LIBRARY_CORE, "[DEFAULT] Dart core library (DateTime)");
        dateOptions.put(DATE_LIBRARY_TIME_MACHINE, "Time Machine is date and time library for Flutter, Web, and Server with support for timezones, calendars, cultures, formatting and parsing.");
        dateOption.setEnum(dateOptions);
        cliOptions.add(dateOption);
    }

    public String getDateLibrary() {
        return dateLibrary;
    }

    public void setDateLibrary(String library) {
        this.dateLibrary = library;
    }

    public String getClientName() {
        return clientName;
    }

    public void setClientName(String clientName) {
        this.clientName = clientName;
    }

    @Override
    public String getName() {
        return "dart-dio";
    }

    @Override
    public String getHelp() {
        return "Generates a Dart Dio client library.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (StringUtils.isEmpty(System.getenv("DART_POST_PROCESS_FILE"))) {
            LOGGER.info("Environment variable DART_POST_PROCESS_FILE not defined so the Dart code may not be properly formatted. To define it, try `export DART_POST_PROCESS_FILE=\"/usr/local/bin/dartfmt -w\"` (Linux/Mac)");
            LOGGER.info("NOTE: To enable file post-processing, 'enablePostProcessFile' must be set to `true` (--enable-post-process-file for CLI).");
        }

        if (!additionalProperties.containsKey(CodegenConstants.SERIALIZATION_LIBRARY)) {
            additionalProperties.put(CodegenConstants.SERIALIZATION_LIBRARY, SERIALIZATION_LIBRARY_DEFAULT);
            LOGGER.debug("Serialization library not set, using default {}", SERIALIZATION_LIBRARY_DEFAULT);
        }
        setLibrary(additionalProperties.get(CodegenConstants.SERIALIZATION_LIBRARY).toString());
        if (SERIALIZATION_LIBRARY_BUILT_VALUE.equals(library)) {
            this.setLegacyDiscriminatorBehavior(false);
        }

        if (!additionalProperties.containsKey(DATE_LIBRARY)) {
            additionalProperties.put(DATE_LIBRARY, DATE_LIBRARY_DEFAULT);
            LOGGER.debug("Date library not set, using default {}", DATE_LIBRARY_DEFAULT);
        }
        setDateLibrary(additionalProperties.get(DATE_LIBRARY).toString());

        if (!additionalProperties.containsKey(FINAL_PROPERTIES)) {
            additionalProperties.put(FINAL_PROPERTIES, Boolean.parseBoolean(FINAL_PROPERTIES_DEFAULT_VALUE));
            LOGGER.debug("finalProperties not set, using default {}", FINAL_PROPERTIES_DEFAULT_VALUE);
        }
        else {
            additionalProperties.put(FINAL_PROPERTIES, Boolean.parseBoolean(additionalProperties.get(FINAL_PROPERTIES).toString()));
        }

        if (!additionalProperties.containsKey(CLIENT_NAME)) {
            final String name = org.openapitools.codegen.utils.StringUtils.camelize(pubName);
            additionalProperties.put(CLIENT_NAME, name);
            LOGGER.debug("Client name not set, using default {}", DATE_LIBRARY_DEFAULT);
        }
        setClientName(additionalProperties.get(CLIENT_NAME).toString());

        supportingFiles.add(new SupportingFile("pubspec.mustache", "", "pubspec.yaml"));
        supportingFiles.add(new SupportingFile("analysis_options.mustache", "", "analysis_options.yaml"));
        supportingFiles.add(new SupportingFile("gitignore.mustache", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

        supportingFiles.add(new SupportingFile("lib.mustache", libPath, pubName + ".dart"));

        final String srcFolder = libPath + sourceFolder;
        supportingFiles.add(new SupportingFile("api_client.mustache", srcFolder, "api.dart"));

        final String authFolder = srcFolder + File.separator + "auth";
        supportingFiles.add(new SupportingFile("auth/api_key_auth.mustache", authFolder, "api_key_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/basic_auth.mustache", authFolder, "basic_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/bearer_auth.mustache", authFolder, "bearer_auth.dart"));
        supportingFiles.add(new SupportingFile("auth/oauth.mustache", authFolder, "oauth.dart"));
        supportingFiles.add(new SupportingFile("auth/auth.mustache", authFolder, "auth.dart"));

        configureSerializationLibrary(srcFolder);
        configureDateLibrary(srcFolder);
    }

    private void configureSerializationLibrary(String srcFolder) {
        switch (library) {
            case SERIALIZATION_LIBRARY_JSON_SERIALIZABLE:
                additionalProperties.put("useJsonSerializable", "true");
                configureSerializationLibraryJsonSerializable(srcFolder);
                break;
            default:
            case SERIALIZATION_LIBRARY_BUILT_VALUE:
                additionalProperties.put("useBuiltValue", "true");
                configureSerializationLibraryBuiltValue(srcFolder);
                break;
        }

        TemplateManagerOptions templateManagerOptions = new TemplateManagerOptions(isEnableMinimalUpdate(), isSkipOverwrite());
        TemplatePathLocator commonTemplateLocator = new CommonTemplateContentLocator();
        TemplatePathLocator generatorTemplateLocator = new GeneratorTemplateContentLocator(this);
        templateManager = new TemplateManager(
                templateManagerOptions,
                getTemplatingEngine(),
                new TemplatePathLocator[]{generatorTemplateLocator, commonTemplateLocator}
        );

        // A lambda which allows for easy includes of serialization library specific
        // templates without having to change the main template files.
        additionalProperties.put("includeLibraryTemplate", (Mustache.Lambda) (fragment, writer) -> {
            MustacheEngineAdapter engine = ((MustacheEngineAdapter) getTemplatingEngine());
            String templateFile = "serialization/" + library + "/" + fragment.execute() + ".mustache";
            Template tmpl = engine.getCompiler()
                    .withLoader(name -> engine.findTemplate(templateManager, name))
                    .defaultValue("")
                    .compile(templateManager.getFullTemplateContents(templateFile));

            fragment.executeTemplate(tmpl, writer);
        });
    }

    private void configureSerializationLibraryBuiltValue(String srcFolder) {
        supportingFiles.add(new SupportingFile("serialization/built_value/serializers.mustache", srcFolder, "serializers.dart"));
        supportingFiles.add(new SupportingFile("serialization/built_value/api_util.mustache", srcFolder, "api_util.dart"));

        typeMapping.put("Array", "BuiltList");
        typeMapping.put("array", "BuiltList");
        typeMapping.put("List", "BuiltList");
        typeMapping.put("set", "BuiltSet");
        typeMapping.put("map", "BuiltMap");
        typeMapping.put("file", "Uint8List");
        typeMapping.put("binary", "Uint8List");
        typeMapping.put("object", "JsonObject");
        typeMapping.put("AnyType", "JsonObject");

        imports.put("BuiltList", "package:built_collection/built_collection.dart");
        imports.put("BuiltSet", "package:built_collection/built_collection.dart");
        imports.put("BuiltMap", "package:built_collection/built_collection.dart");
        imports.put("JsonObject", "package:built_value/json_object.dart");
        imports.put("Uint8List", "dart:typed_data");
        imports.put("MultipartFile", DIO_IMPORT);
    }

    private void configureSerializationLibraryJsonSerializable(String srcFolder) {
        supportingFiles.add(new SupportingFile("serialization/json_serializable/build.yaml.mustache", "" /* main project dir */, "build.yaml"));
        supportingFiles.add(new SupportingFile("serialization/json_serializable/deserialize.mustache", srcFolder,
                "deserialize.dart"));

        // most of these are defined in AbstractDartCodegen, we are overriding
        // just the binary / file handling
        languageSpecificPrimitives.add("Object");
        imports.put("Uint8List", "dart:typed_data");
        imports.put("MultipartFile", DIO_IMPORT);
    }

    private void configureDateLibrary(String srcFolder) {
        switch (dateLibrary) {
            case DATE_LIBRARY_TIME_MACHINE:
                additionalProperties.put("useDateLibTimeMachine", "true");
                typeMapping.put("date", "OffsetDate");
                typeMapping.put("Date", "OffsetDate");
                typeMapping.put("DateTime", "OffsetDateTime");
                typeMapping.put("datetime", "OffsetDateTime");
                imports.put("OffsetDate", "package:time_machine/time_machine.dart");
                imports.put("OffsetDateTime", "package:time_machine/time_machine.dart");
                if (SERIALIZATION_LIBRARY_BUILT_VALUE.equals(library)) {
                    supportingFiles.add(new SupportingFile("serialization/built_value/offset_date_serializer.mustache", srcFolder, "local_date_serializer.dart"));
                }
                break;
            default:
            case DATE_LIBRARY_CORE:
                additionalProperties.put("useDateLibCore", "true");
                if (SERIALIZATION_LIBRARY_BUILT_VALUE.equals(library)) {
                    typeMapping.put("date", "Date");
                    typeMapping.put("Date", "Date");
                    importMapping.put("Date", "package:" + pubName + "/" + sourceFolder + "/" + modelPackage() + "/date.dart");
                    supportingFiles.add(new SupportingFile("serialization/built_value/date.mustache", srcFolder + File.separator + modelPackage(), "date.dart"));
                    supportingFiles.add(new SupportingFile("serialization/built_value/date_serializer.mustache", srcFolder, "date_serializer.dart"));
                }
                break;
        }
    }

    @Override
    public String toDefaultValue(Schema schema) {
        if (schema.getDefault() != null) {
            if (SERIALIZATION_LIBRARY_BUILT_VALUE.equals(library)) {
                if (ModelUtils.isArraySchema(schema)) {
                    if (ModelUtils.isSet(schema)) {
                        return "SetBuilder()";
                    }
                    return "ListBuilder()";
                }
                if (ModelUtils.isMapSchema(schema)) {
                    return "MapBuilder()";
                }
            }
            if (ModelUtils.isDateSchema(schema) || ModelUtils.isDateTimeSchema(schema)) {
                // this is currently not supported and would create compile errors
                return null;
            }
            if (ModelUtils.isStringSchema(schema)) {
                return "'" + schema.getDefault().toString().replaceAll("'", "\\'") + "'";
            }
            return schema.getDefault().toString();
        }
        return null;
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        objs = super.postProcessModels(objs);
        List<ModelMap> models = objs.getModels();
        ProcessUtils.addIndexToProperties(models, 1);
        return objs;
    }

    /// Gets all ancestors of a given model, and puts it in accumulator
    private void getAncestors(CodegenModel cm, Map<String, CodegenModel> allModels, Set<String> accumulator) {

        // get direct parents
        Set<String> directParentNames = cm.allOf;
        if (directParentNames != null && !directParentNames.isEmpty()) {
            for (String directParentName : directParentNames) {
                if (accumulator.add(directParentName)) {
                    CodegenModel parent = allModels.get(directParentName);
                    getAncestors(parent, allModels, accumulator);
                }
            }
        }
    }

    private void syncRootTypesWithInnerVars(Map<String, ModelsMap> objs) {
        Map<String, CodegenModel> allModels = new HashMap<>();
        for (ModelsMap modelsEntries : objs.values()) {
            for (ModelMap modelsMap : modelsEntries.getModels()) {
                CodegenModel model = modelsMap.getModel();
                allModels.put(model.getClassname(), model);
            }
        }

        for (CodegenModel model : allModels.values()) {
            syncRootTypesWithInnerVars(allModels, model);
        }
    }
    private void syncRootTypesWithInnerVars(Map<String, CodegenModel> objs, CodegenModel model) {
        List<CodegenProperty> allVars = new ArrayList<>();
        allVars.addAll(((Collection<CodegenProperty>) model.vendorExtensions.get(kSelfAndAncestorOnlyProps)));
        allVars.addAll(((Collection<CodegenProperty>) model.vendorExtensions.get(kSelfOnlyProps)));
        allVars.addAll(((Collection<CodegenProperty>) model.vendorExtensions.get(kAncestorOnlyProps)));

        for (CodegenProperty prop : allVars) {
            //check if type exists in parent map
            String type = prop.openApiType;
            if (objs.containsKey(type)) {
                //get the type
                CodegenModel relatedModel = objs.get(type);
                //fill the property's VendorExtensions with the type's VendorExtensions
                prop.getVendorExtensions().put(kIsParent, relatedModel.getVendorExtensions().get(kIsParent));
                prop.isEnum = relatedModel.isEnum;

            }
        }
    }
    private final String kIsChild = "x-is-child";
    private final String kIsParent = "x-is-parent";
    private final String kIsPure = "x-is-pure";
    private final String kSelfOnlyProps = "x-self-only-props";
    private final String kHasSelfOnlyProps = "x-has-self-only-props";
    private final String kAncestorOnlyProps = "x-ancestor-only-props";
    private final String kHasAncestorOnlyProps = "x-has-ancestor-only-props";
    private final String kSelfAndAncestorOnlyProps = "x-self-and-ancestor-only-props";
    private final String kHasSelfAndAncestorOnlyProps = "x-has-self-and-ancestor-only-props";
    private final String kParentDiscriminator = "x-parent-discriminator";

    // adapts codegen models and property to dart rules of inheritance
    private void adaptToDartInheritance(Map<String, ModelsMap> objs) {
        // get all models
        Map<String, CodegenModel> allModels = new HashMap<>();
        for (ModelsMap modelsEntries : objs.values()) {
            for (ModelMap modelsMap : modelsEntries.getModels()) {
                CodegenModel model = modelsMap.getModel();
                allModels.put(model.getClassname(), model);
            }
        }

        // all ancestors
        Set<String> allAncestorsForAllModelsFlat = new HashSet<>();
        // maps a model to its ancestors
        Map<String, Set<String>> allAncestorsForAllModels = new HashMap<>();
        for (java.util.Map.Entry<String, CodegenModel> cm : allModels.entrySet()) {
            Set<String> allAncestors = new HashSet<>();
            // get all ancestors
            // TODO: optimize this logic ?
            getAncestors(cm.getValue(), allModels, allAncestors);
            // just in case, a model can't be its own ancestor
            allAncestors.remove(cm.getKey());

            allAncestorsForAllModels.put(cm.getKey(), allAncestors);
            allAncestorsForAllModelsFlat.addAll(allAncestors);
        }



        Set<String> allPureClasses = new HashSet<>();
        // set isChild,isParent,isPure
        for (java.util.Map.Entry<String, CodegenModel> cmEntry : allModels.entrySet()) {
            String key = cmEntry.getKey();
            CodegenModel cm = cmEntry.getValue();
            // get all ancestors
            Set<String> allAncestors = allAncestorsForAllModels.get(key);

            // a class is a parent when it's an ancestor to another class
            boolean isParent = allAncestorsForAllModelsFlat.contains(key);
            // a class is a child when it has any ancestor
            boolean isChild = !allAncestors.isEmpty();
            // a class is pure when it's not a child, and has no oneOf nor anyOf
            boolean isPure = !isChild && (cm.oneOf == null || cm.oneOf.isEmpty())
                    && (cm.anyOf == null || cm.anyOf.isEmpty());

            cm.vendorExtensions.put(kIsChild, isChild);
            cm.vendorExtensions.put(kIsParent, isParent);
            cm.vendorExtensions.put(kIsPure, isPure);
            if (!isParent && (cm.oneOf == null || cm.oneOf.isEmpty())) {
                //discriminator has no meaning here
                if (cm.discriminator!=null) {
                    cm.vendorExtensions.put(kParentDiscriminator, cm.discriminator);
                    cm.discriminator=null;
                }

            }
            // when pure:
            // vars = allVars = selfOnlyProperties = kSelfAndAncestorOnlyProps
            // ancestorOnlyProps = empty
            if (isPure) {
                cm.vendorExtensions.put(kSelfOnlyProps, new ArrayList<>(cm.getVars()));
                cm.vendorExtensions.put(kHasSelfOnlyProps, !cm.getVars().isEmpty());
                cm.vendorExtensions.put(kAncestorOnlyProps, new ArrayList<CodegenProperty>());
                cm.vendorExtensions.put(kHasAncestorOnlyProps, false);
                cm.vendorExtensions.put(kSelfAndAncestorOnlyProps, new ArrayList<>(cm.getVars()));
                cm.vendorExtensions.put(kHasSelfAndAncestorOnlyProps, !cm.getVars().isEmpty());

                allPureClasses.add(key);
            }
        }

        // handle impure models
        for (java.util.Map.Entry<String, CodegenModel> cmEntry : allModels.entrySet()) {
            String key = cmEntry.getKey();
            CodegenModel cm = cmEntry.getValue();
            if (allPureClasses.contains(key)) {
                continue;
            }
            // get all ancestors
            Set<String> allAncestors = allAncestorsForAllModels.get(key);

            // get direct parents
            // Set<String> directParentNames = cm.allOf == null ? new HashSet<>() :
            // cm.allOf;
            Set<String> compositeProperties = new HashSet<>();

            Set<String> compositeModelNames = new HashSet<String>();
            compositeModelNames.addAll(ObjectUtils.firstNonNull(cm.oneOf, new HashSet<>()));
            compositeModelNames.addAll(ObjectUtils.firstNonNull(cm.anyOf, new HashSet<>()));
            compositeModelNames.addAll(allAncestors);

            for (String compositeModelName : compositeModelNames) {
                CodegenModel model = allModels.get(compositeModelName);
                if (model == null)
                    continue;
                List<CodegenProperty> allVars = ObjectUtils.firstNonNull(model.getAllVars(), new ArrayList<>());
                for (CodegenProperty prop : allVars) {
                    compositeProperties.add(prop.getName());
                }
            }
            // dart classes declare selfOnlyProperties as direct members (they exist in
            // "vars")
            // for pure models, this will equal vars
            Map<String, CodegenProperty> selfOnlyProperties = new HashMap<>();

            // ancestorOnlyProperties are properties defined by all ancestors
            // NOTE: oneOf,anyOf are NOT considered ancestors
            // since a child in dart must implement ALL OF the parent (using implements)
            Map<String, CodegenProperty> ancestorOnlyProperties = new HashMap<>();

            // combines both selfOnlyProperties and ancestorOnlyProperties
            // this will be used by the custom serializer as "x-handled-vars" and
            // "x-has-handled-vars"
            Map<String, CodegenProperty> selfAndAncestorOnlyProperties = new HashMap<>();

            // STEP 1: calculating selfOnlyProperties
            // get all vars of all ancestors and add them to ancestorPropNames
            // Set<String> _ancestorPropNames = new HashSet<>();
            for (String ancestorKey : allAncestors) {
                CodegenModel ancestorCM = allModels.get(ancestorKey);
                for (CodegenProperty prop : ancestorCM.getVars()) {
                    ancestorOnlyProperties.put(prop.getName(), prop);
                }
            }
            for (CodegenProperty p : cm.getVars()) {
                p.isInherited = ancestorOnlyProperties.containsKey(p.getName());
                if (!p.isInherited && !compositeProperties.contains(p.getName())) {
                    selfOnlyProperties.put(p.getName(), p);
                }
            }
            selfAndAncestorOnlyProperties.putAll(selfOnlyProperties);
            selfAndAncestorOnlyProperties.putAll(ancestorOnlyProperties);

            cm.vendorExtensions.put(kSelfOnlyProps, new ArrayList<>(selfOnlyProperties.values()));
            cm.vendorExtensions.put(kHasSelfOnlyProps, !selfOnlyProperties.isEmpty());
            cm.vendorExtensions.put(kAncestorOnlyProps, new ArrayList<>(ancestorOnlyProperties.values()));
            cm.vendorExtensions.put(kHasAncestorOnlyProps, !ancestorOnlyProperties.isEmpty());
            cm.vendorExtensions.put(kSelfAndAncestorOnlyProps, new ArrayList<>(selfAndAncestorOnlyProperties.values()));
            cm.vendorExtensions.put(kHasSelfAndAncestorOnlyProps, !selfAndAncestorOnlyProperties.isEmpty());
            // fixes missing imports
            Set<String> interfaceImports = new HashSet<String>();
            interfaceImports.addAll(cm.allOf);
            interfaceImports.addAll(cm.oneOf);
            interfaceImports.addAll(cm.anyOf);
            cm.imports.addAll(rewriteImports(interfaceImports, true));
        }
    }

    /// override the default behavior of createDiscriminator
    /// to remove extra mappings added as a side effect of setLegacyDiscriminatorBehavior(false)
    /// this ensures 1-1 schema mapping instead of 1-many
    @Override
    protected CodegenDiscriminator createDiscriminator(String schemaName, Schema schema) {
        CodegenDiscriminator sub = super.createDiscriminator(schemaName, schema);
        Discriminator originalDiscriminator = schema.getDiscriminator();
        if (originalDiscriminator!=null) {
            Map<String,String> originalMapping = originalDiscriminator.getMapping();
            if (originalMapping != null && !originalMapping.isEmpty()) {
                //we already have a discriminator mapping, remove everything else
                for (MappedModel currentMappings : new HashSet<>(sub.getMappedModels())) {
                    if (originalMapping.containsKey(currentMappings.getMappingName())) {
                        //all good
                    } else {
                        sub.getMapping().remove(currentMappings.getMappingName());
                        sub.getMappedModels().remove(currentMappings);
                    }
                }
            }
        }
        return sub;
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        objs = super.postProcessAllModels(objs);
        if (SERIALIZATION_LIBRARY_BUILT_VALUE.equals(library)) {
            adaptToDartInheritance(objs);
            syncRootTypesWithInnerVars(objs);
        }

        // loop through models to update the imports
        for (ModelsMap entry : objs.values()) {
            for (ModelMap mo : entry.getModels()) {
                CodegenModel cm = mo.getModel();
                cm.imports = rewriteImports(cm.imports, true);
                cm.vendorExtensions.put("x-has-vars", !cm.vars.isEmpty());
            }
        }

        return objs;
    }

    @Override
    public void postProcessModelProperty(CodegenModel model, CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        if (SERIALIZATION_LIBRARY_BUILT_VALUE.equals(library)) {
            if (property.isEnum && property.getComposedSchemas() == null) {
                // enums are generated with built_value and make use of BuiltSet
                model.imports.add("BuiltSet");
            }

            if (property.isContainer) {
                // Figure out if there are any container type additionalProperties
                // that need a custom serializer builder factory added.
                final CodegenProperty items = property.items;
                if (items.getAdditionalProperties() != null) {
                    addBuiltValueSerializer(new BuiltValueSerializer(
                            items.isArray,
                            items.getUniqueItems(),
                            items.isMap,
                            items.items.isNullable,
                            items.getAdditionalProperties().dataType
                    ));
                }
            }
        }
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        OperationMap operations = objs.getOperations();
        List<CodegenOperation> operationList = operations.getOperation();

        Set<String> resultImports = new HashSet<>();

        for (CodegenOperation op : operationList) {
            for (CodegenParameter param : op.allParams) {
                if (((op.isMultipart && param.isFormParam) || param.isBodyParam) && (param.isBinary || param.isFile)) {
                    param.dataType = param.dataType.replace("Uint8List", "MultipartFile");
                    param.baseType = param.baseType.replace("Uint8List", "MultipartFile");
                    op.imports.add("MultipartFile");

                    if (SERIALIZATION_LIBRARY_BUILT_VALUE.equals(library)) {
                        boolean skipFormModel = Boolean.parseBoolean(GlobalSettings.getProperty(CodegenConstants.SKIP_FORM_MODEL, "true"));
                        if (param.isFormParam && param.isContainer && !skipFormModel) {
                            // Because of skipFormModel=false, there is a model class generated which has
                            // "BuiltList<Uint8List>" as property and it requires the correct
                            // serializer imports to be added in order to compile.
                            addBuiltValueSerializerImport("Uint8List");
                        }
                    }
                }
            }

            // The MultipartFile handling above changes the type of some parameters from
            // `UInt8List`, the default for files, to `MultipartFile`.
            //
            // The following block removes the required import for Uint8List if it is no
            // longer in use.
            if (op.allParams.stream().noneMatch(param -> param.dataType.equals("Uint8List"))
                    && op.responses.stream().filter(response -> response.dataType != null)
                            .noneMatch(response -> response.dataType.equals("Uint8List"))) {
                // Remove unused imports after processing
                op.imports.remove("Uint8List");
            }

            resultImports.addAll(rewriteImports(op.imports, false));

            if (SERIALIZATION_LIBRARY_BUILT_VALUE.equals(library)) {

                for (CodegenParameter param : op.allParams) {
                    // Generate serializer factories for all container type parameters.
                    // But skip binary and file parameters, JSON serializers don't make sense there.
                    if (param.isContainer && !(param.isBinary || param.isFile)) {
                        addBuiltValueSerializer(new BuiltValueSerializer(
                                param.isArray,
                                param.uniqueItems,
                                param.isMap,
                                param.items.isNullable,
                                param.baseType
                        ));
                    }
                }
            }

            if (SERIALIZATION_LIBRARY_BUILT_VALUE.equals(library) && (op.getHasFormParams() || op.getHasQueryParams() || op.getHasPathParams())) {
                resultImports.add("package:" + pubName + "/" + sourceFolder + "/api_util.dart");
            }

            if (op.returnContainer != null && !(op.isResponseBinary || op.isResponseFile)) {
                addBuiltValueSerializer(new BuiltValueSerializer(
                        Objects.equals("array", op.returnContainer) || Objects.equals("set", op.returnContainer),
                        op.uniqueItems,
                        Objects.equals("map", op.returnContainer),
                        false,
                        op.returnBaseType
                ));
            }
        }
        // for some reason "import" structure is changed ..
        objs.put("imports", resultImports.stream().sorted().collect(Collectors.toList()));

        return objs;
    }

    private void addBuiltValueSerializerImport(String type) {
        additionalProperties.compute("builtValueSerializerImports", (k, v) -> {
            Set<String> imports = v == null ? Sets.newHashSet() : ((Set<String>) v);
            imports.addAll(rewriteImports(Sets.newHashSet(type), true));
            return imports;
        });
    }

    /**
     * Adds the serializer to the global list of custom built_value serializers.
     * @param serializer
     */
    private void addBuiltValueSerializer(BuiltValueSerializer serializer) {
        additionalProperties.compute("builtValueSerializers", (k, v) -> {
            Set<BuiltValueSerializer> serializers = v == null ? Sets.newHashSet() : ((Set<BuiltValueSerializer>) v);
            serializers.add(serializer);
            return serializers;
        });
    }

    private Set<String> rewriteImports(Set<String> originalImports, boolean isModel) {
        Set<String> resultImports = Sets.newHashSet();
        for (String modelImport : originalImports) {
            if (modelImport.startsWith("BuiltList", 0)) {
                modelImport = "BuiltList";
            } else if (modelImport.startsWith("BuiltSet", 0)) {
                modelImport = "BuiltSet";
            } else if (modelImport.startsWith("BuiltMap", 0)) {
                modelImport = "BuiltMap";
            }

            if (imports.containsKey(modelImport)) {
                String i = imports.get(modelImport);
                if (Objects.equals(i, DIO_IMPORT) && !isModel) {
                    // Don't add imports to operations that are already imported
                    continue;
                }
                resultImports.add(i);
            } else if (importMapping().containsKey(modelImport)) {
                resultImports.add(importMapping().get(modelImport));
            } else if (modelImport.startsWith("dart:")) { // import dart:* directly
                resultImports.add(modelImport);
            } else if (modelImport.startsWith("package:")) { // e.g. package:openapi/src/model/child.dart
                resultImports.add(modelImport);
            } else {
                resultImports.add("package:" + pubName + "/" + sourceFolder + "/" + modelPackage() + "/" + underscore(modelImport) + ".dart");
            }
        }
        return resultImports;
    }

    static class BuiltValueSerializer {

        final boolean isArray;

        final boolean uniqueItems;

        final boolean isMap;

        final boolean isNullable;

        final String dataType;

        private BuiltValueSerializer(boolean isArray, boolean uniqueItems, boolean isMap, boolean isNullable, String dataType) {
            this.isArray = isArray;
            this.uniqueItems = uniqueItems;
            this.isMap = isMap;
            this.isNullable = isNullable;
            this.dataType = dataType;
        }

        public boolean isArray() {
            return isArray;
        }

        public boolean isUniqueItems() {
            return uniqueItems;
        }

        public boolean isMap() {
            return isMap;
        }

        public boolean isNullable() {
            return isNullable;
        }

        public String getDataType() {
            return dataType;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            BuiltValueSerializer that = (BuiltValueSerializer) o;
            return isArray == that.isArray && uniqueItems == that.uniqueItems && isMap == that.isMap && isNullable == that.isNullable && dataType.equals(that.dataType);
        }

        @Override
        public int hashCode() {
            return Objects.hash(isArray, uniqueItems, isMap, isNullable, dataType);
        }
    }
}
