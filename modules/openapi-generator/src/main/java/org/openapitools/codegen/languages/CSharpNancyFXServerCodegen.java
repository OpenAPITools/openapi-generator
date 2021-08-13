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

import com.google.common.base.Predicate;
import com.google.common.collect.*;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.utils.ModelUtils;
import org.openapitools.codegen.utils.URLPathUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.net.URL;
import java.util.*;
import java.util.Map.Entry;

import static com.google.common.base.Strings.isNullOrEmpty;
import static java.util.Arrays.asList;
import static java.util.UUID.randomUUID;
import static org.apache.commons.lang3.StringUtils.capitalize;
import static org.openapitools.codegen.CodegenConstants.*;
import static org.openapitools.codegen.CodegenType.SERVER;
import static org.openapitools.codegen.utils.StringUtils.camelize;

public class CSharpNancyFXServerCodegen extends AbstractCSharpCodegen {
    private final Logger LOGGER = LoggerFactory.getLogger(CSharpNancyFXServerCodegen.class);

    private static final String API_NAMESPACE = "Modules";
    private static final String MODEL_NAMESPACE = "Models";
    private static final String IMMUTABLE_OPTION = "immutable";
    private static final String USE_BASE_PATH = "writeModulePath";
    private static final String PACKAGE_CONTEXT = "packageContext";
    private static final String ASYNC_SERVER = "asyncServer";

    private static final Map<String, Predicate<Schema>> propertyToOpenAPITypeMapping =
            createPropertyToOpenAPITypeMapping();

    private String packageGuid = "{" + randomUUID().toString().toUpperCase(Locale.ROOT) + "}";

    private final Map<String, DependencyInfo> dependencies = new HashMap<>();
    private final Set<String> parentModels = new HashSet<>();
    private final Multimap<String, CodegenModel> childrenByParent = ArrayListMultimap.create();
    private final BiMap<String, String> modelNameMapping = HashBiMap.create();

    /**
     * If set to true, we will generate c# async endpoints and service interfaces
     */
    private boolean asyncServer = false;

    public CSharpNancyFXServerCodegen() {
        super();

        modifyFeatureSet(features -> features
                .excludeDocumentationFeatures(DocumentationFeature.Readme)
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling
                )
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        outputFolder = "generated-code" + File.separator + getName();
        apiTemplateFiles.put("api.mustache", ".cs");

        // Early versions use no prefix for interfaces. Defaulting to I- common practice would break existing users.
        setInterfacePrefix("");

        // contextually reserved words
        setReservedWordsLowerCase(
                asList("var", "async", "await", "dynamic", "yield")
        );

        cliOptions.clear();

        // CLI options
        addOption(PACKAGE_NAME, "C# package name (convention: Title.Case).", packageName);
        addOption(PACKAGE_VERSION, "C# package version.", packageVersion);
        addOption(SOURCE_FOLDER, SOURCE_FOLDER_DESC, sourceFolder);
        addOption(INTERFACE_PREFIX, INTERFACE_PREFIX_DESC, interfacePrefix);
        addOption(OPTIONAL_PROJECT_GUID, OPTIONAL_PROJECT_GUID_DESC, null);
        addOption(PACKAGE_CONTEXT, "Optionally overrides the PackageContext which determines the namespace (namespace=packageName.packageContext). If not set, packageContext will default to basePath.", null);

        // CLI Switches
        addSwitch(SORT_PARAMS_BY_REQUIRED_FLAG, SORT_PARAMS_BY_REQUIRED_FLAG_DESC, sortParamsByRequiredFlag);
        addSwitch(OPTIONAL_PROJECT_FILE, OPTIONAL_PROJECT_FILE_DESC, optionalProjectFileFlag);
        addSwitch(USE_DATETIME_OFFSET, USE_DATETIME_OFFSET_DESC, useDateTimeOffsetFlag);
        addSwitch(USE_COLLECTION, USE_COLLECTION_DESC, useCollection);
        addSwitch(RETURN_ICOLLECTION, RETURN_ICOLLECTION_DESC, returnICollection);
        addSwitch(IMMUTABLE_OPTION, "Enabled by default. If disabled generates model classes with setters", true);
        addSwitch(USE_BASE_PATH, "Enabled by default. If disabled, module paths will not mirror api base path", true);
        addSwitch(ASYNC_SERVER, "Set to true to enable the generation of async routes/endpoints.", this.asyncServer);
        typeMapping.putAll(nodaTimeTypesMappings());
        languageSpecificPrimitives.addAll(nodaTimePrimitiveTypes());
    }

    @Override
    public CodegenType getTag() {
        return SERVER;
    }

    @Override
    public String getName() {
        return "csharp-nancyfx";
    }

    @Override
    public String getHelp() {
        return "Generates a C# NancyFX Web API server.";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        apiPackage = isNullOrEmpty(packageName) ? API_NAMESPACE : packageName + "." + API_NAMESPACE;
        modelPackage = isNullOrEmpty(packageName) ? MODEL_NAMESPACE : packageName + "." + MODEL_NAMESPACE;

        supportingFiles.add(new SupportingFile("gitignore", "", ".gitignore"));
        supportingFiles.add(new SupportingFile("parameters.mustache", sourceFile("Utils"), "Parameters.cs"));
        supportingFiles.add(new SupportingFile("localDateConverter.mustache", sourceFile("Utils"), "LocalDateConverter.cs"));
        supportingFiles.add(new SupportingFile("packages.config.mustache", sourceFolder(), "packages.config"));
        supportingFiles.add(new SupportingFile("nuspec.mustache", sourceFolder(), packageName + ".nuspec"));

        if (optionalProjectFileFlag) {
            supportingFiles.add(new SupportingFile("Solution.mustache", "", packageName + ".sln"));
            supportingFiles.add(new SupportingFile("Project.mustache", sourceFolder(), packageName + ".csproj"));
        }

        if (additionalProperties.containsKey(OPTIONAL_PROJECT_GUID)) {
            setPackageGuid((String) additionalProperties.get(OPTIONAL_PROJECT_GUID));
        }

        if (additionalProperties.containsKey(ASYNC_SERVER)) {
            setAsyncServer(convertPropertyToBooleanAndWriteBack(ASYNC_SERVER));
        } else {
            additionalProperties.put(ASYNC_SERVER, this.asyncServer);
        }

        additionalProperties.put("packageGuid", packageGuid);

        setupModelTemplate();
        processImportedMappings();
        appendDependencies();
    }

    private void setupModelTemplate() {
        final Object immutableOption = additionalProperties.get(IMMUTABLE_OPTION);
        if (immutableOption != null && "false".equalsIgnoreCase(immutableOption.toString())) {
            LOGGER.info("Using mutable model template");
            modelTemplateFiles.put("modelMutable.mustache", ".cs");
        } else {
            LOGGER.info("Using immutable model template");
            modelTemplateFiles.put("model.mustache", ".cs");
        }
    }

    private void processImportedMappings() {
        for (final Entry<String, String> entry : ImmutableSet.copyOf(importMapping.entrySet())) {
            final String model = entry.getKey();
            final String[] namespaceInfo = entry.getValue().split("\\s");
            final String[] namespace = (namespaceInfo.length > 0 ? namespaceInfo[0].trim() : "").split(":");
            final String namespaceName = namespace.length > 0 ? namespace[0].trim() : null;
            final String modelClass = namespace.length > 1 ? namespace[1].trim() : null;
            final String assembly = namespaceInfo.length > 1 ? namespaceInfo[1].trim() : null;
            final String assemblyVersion = namespaceInfo.length > 2 ? namespaceInfo[2].trim() : null;
            final String assemblyFramework = namespaceInfo.length > 3 ? namespaceInfo[3].trim() : "net45";

            if (isNullOrEmpty(model) || isNullOrEmpty(namespaceName)) {
                LOGGER.warn(String.format(Locale.ROOT, "Could not import: '%s' - invalid namespace: '%s'", model, entry.getValue()));
                importMapping.remove(model);
            } else {
                LOGGER.info(String.format(Locale.ROOT, "Importing: '%s' from '%s' namespace.", model, namespaceName));
                importMapping.put(model, namespaceName);
            }
            if (!isNullOrEmpty(modelClass)) {
                LOGGER.info(String.format(Locale.ROOT, "Mapping: '%s' class to '%s'", model, modelClass));
                modelNameMapping.put(model, modelClass);
            }
            if (assembly != null && assemblyVersion != null) {
                LOGGER.info(String.format(Locale.ROOT, "Adding dependency: '%s', version: '%s', framework: '%s'",
                        assembly, assemblyVersion, assemblyVersion));
                dependencies.put(assembly, new DependencyInfo(assemblyVersion, assemblyFramework));
            }
        }
    }

    private void appendDependencies() {
        final List<Map<String, String>> listOfDependencies = new ArrayList<>();
        for (final Entry<String, DependencyInfo> dependency : dependencies.entrySet()) {
            final Map<String, String> dependencyInfo = new HashMap<>();
            dependencyInfo.put("dependency", dependency.getKey());
            dependencyInfo.put("dependencyVersion", dependency.getValue().version);
            dependencyInfo.put("dependencyFramework", dependency.getValue().framework);
            listOfDependencies.add(dependencyInfo);
        }
        additionalProperties.put("dependencies", listOfDependencies);
    }

    private String sourceFolder() {
        return "src" + File.separator + packageName;
    }

    private String sourceFile(final String fileName) {
        return sourceFolder() + File.separator + fileName;
    }

    public void setPackageGuid(String packageGuid) {
        this.packageGuid = packageGuid;
    }

    public void setAsyncServer(boolean asyncServer) {
        this.asyncServer = asyncServer;
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder() + File.separator + API_NAMESPACE;
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder() + File.separator + MODEL_NAMESPACE;
    }

    @Override
    protected void processOperation(final CodegenOperation operation) {
        super.processOperation(operation);
        if (!isNullOrEmpty(operation.path) && operation.path.contains("?")) {
            operation.path = operation.path.replace("?", "/");
        }
        if (!isNullOrEmpty(operation.httpMethod)) {
            operation.httpMethod = capitalize(operation.httpMethod.toLowerCase(Locale.ROOT));
        }
    }

    @Override
    public Map<String, Object> postProcessAllModels(final Map<String, Object> models) {
        final Map<String, Object> processed = super.postProcessAllModels(models);
        postProcessParentModels(models);
        return processed;
    }

    private void postProcessParentModels(final Map<String, Object> models) {
        LOGGER.debug("Processing parents:  {}", parentModels);
        for (final String parent : parentModels) {
            final CodegenModel parentModel = ModelUtils.getModelByName(parent, models);
            if (parentModel != null) {
                parentModel.hasChildren = true;
                final Collection<CodegenModel> childrenModels = childrenByParent.get(parent);
                for (final CodegenModel child : childrenModels) {
                    processParentPropertiesInChildModel(parentModel, child);
                }
            }
        }
    }

    private void processParentPropertiesInChildModel(final CodegenModel parent, final CodegenModel child) {
        final Map<String, CodegenProperty> childPropertiesByName = new HashMap<>(child.vars.size());
        for (final CodegenProperty property : child.vars) {
            childPropertiesByName.put(property.name, property);
        }
        CodegenProperty previousParentVar = null;
        for (final CodegenProperty property : parent.vars) {
            final CodegenProperty duplicatedByParent = childPropertiesByName.get(property.name);
            if (duplicatedByParent != null) {
                LOGGER.info(String.format(Locale.ROOT, "Property: '%s' in '%s' model is inherited from '%s'",
                        property.name, child.classname, parent.classname));
                duplicatedByParent.isInherited = true;
                final CodegenProperty parentVar = duplicatedByParent.clone();
                child.parentVars.add(parentVar);
                previousParentVar = parentVar;
            }
        }
    }

    @Override
    public void postProcessModelProperty(final CodegenModel model, final CodegenProperty property) {
        super.postProcessModelProperty(model, property);
        if (!isNullOrEmpty(model.parent)) {
            parentModels.add(model.parent);
            if (!childrenByParent.containsEntry(model.parent, model)) {
                childrenByParent.put(model.parent, model);
            }
        }
    }

    @Override
    public String toEnumVarName(final String name, final String datatype) {
        if (name.length() == 0) {
            return "Empty";
        }

        final String enumName = camelize(
                sanitizeName(name)
                        .replaceFirst("^_", "")
                        .replaceFirst("_$", "")
                        .replaceAll("-", "_"));
        final String result;
        if (enumName.matches("\\d.*")) {
            result = "_" + enumName;
        } else {
            result = enumName;
        }
        LOGGER.debug(String.format(Locale.ROOT, "toEnumVarName('%s', %s) = '%s'", name, datatype, enumName));
        return result;
    }

    @Override
    public String toApiName(final String name) {
        final String apiName;
        if (isNullOrEmpty(name)) {
            apiName = "Default";
        } else {
            apiName = capitalize(name);
        }
        LOGGER.debug(String.format(Locale.ROOT, "toApiName('%s') = '%s'", name, apiName));
        return apiName;
    }

    @Override
    public String toApiFilename(final String name) {
        return super.toApiFilename(name) + "Module";
    }

    @Override
    public String toModelImport(final String name) {
        final String result;
        if (modelNameMapping.containsValue(name)) {
            final String modelName = modelNameMapping.inverse().get(name);
            result = importMapping.containsKey(modelName) ?
                    importMapping.get(modelName) : super.toModelImport(name);
        } else if (importMapping.containsKey(name)) {
            result = importMapping.get(name);
        } else {
            result = null;
        }
        LOGGER.debug(String.format(Locale.ROOT, "toModelImport('%s') = '%s'", name, result));
        return result;
    }

    @Override
    public String toModelName(final String name) {
        final String modelName = super.toModelName(name);
        final String mappedModelName = modelNameMapping.get(modelName);
        return isNullOrEmpty(mappedModelName) ? modelName : mappedModelName;
    }

    @Override
    public void preprocessOpenAPI(final OpenAPI openAPI) {
        URL url = URLPathUtils.getServerURL(openAPI, serverVariableOverrides());
        String path = URLPathUtils.getPath(url, "/");
        final String packageContextOption = (String) additionalProperties.get(PACKAGE_CONTEXT);
        additionalProperties.put("packageContext", packageContextOption == null ? sanitizeName(path) : packageContextOption);
        final Object basePathOption = additionalProperties.get(USE_BASE_PATH);
        additionalProperties.put("baseContext", basePathOption == null ? path : "/");
    }

    @Override
    public String toEnumName(final CodegenProperty property) {
        return sanitizeName(camelize(property.name)) + "Enum";
    }

    @Override
    public String getSchemaType(final Schema property) {
        for (Entry<String, Predicate<Schema>> entry : propertyToOpenAPITypeMapping.entrySet()) {
            if (entry.getValue().apply(property)) {
                return entry.getKey();
            }
        }
        return super.getSchemaType(property);
    }

    private static Map<String, Predicate<Schema>> createPropertyToOpenAPITypeMapping() {
        final ImmutableMap.Builder<String, Predicate<Schema>> mapping = ImmutableMap.builder();
        mapping.put("time", timeProperty());
        return mapping.build();
    }

    private static Predicate<Schema> timeProperty() {
        return new Predicate<Schema>() {
            @Override
            public boolean apply(Schema property) {
                return ModelUtils.isStringSchema(property) && "time".equalsIgnoreCase(property.getFormat());
            }
        };
    }

    private static Map<String, String> nodaTimeTypesMappings() {
        return ImmutableMap.of(
                "time", "LocalTime?",
                "date", "LocalDate?",
                "datetime", "ZonedDateTime?");
    }

    private static Set<String> nodaTimePrimitiveTypes() {
        return ImmutableSet.of("LocalTime?", "LocalDate?", "ZonedDateTime?");
    }

    private static class DependencyInfo {
        private final String version;
        private final String framework;

        private DependencyInfo(final String version, final String framework) {
            this.version = version;
            this.framework = framework;
        }
    }
}
