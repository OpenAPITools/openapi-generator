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
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.meta.GeneratorMetadata;
import org.openapitools.codegen.meta.Stability;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * <p>Mustache templates are located in {@code src/main/resources/scala-sttp/}.
 */
public class ScalaSttpClientCodegen extends AbstractScalaCodegen implements CodegenConfig {
    private static final StringProperty STTP_CLIENT_VERSION = new StringProperty("sttpClientVersion", "The version of " +
            "sttp client", "3.3.18");
    private static final BooleanProperty USE_SEPARATE_ERROR_CHANNEL = new BooleanProperty("separateErrorChannel",
            "Whether to return response as " +
                    "F[Either[ResponseError[ErrorType], ReturnType]]] or to flatten " +
                    "response's error raising them through enclosing monad (F[ReturnType]).", true);
    private static final StringProperty JODA_TIME_VERSION = new StringProperty("jodaTimeVersion", "The version of " +
            "joda-time library", "2.10.13");
    private static final StringProperty JSON4S_VERSION = new StringProperty("json4sVersion", "The version of json4s " +
            "library", "3.6.11");
    private static final StringProperty CIRCE_VERSION = new StringProperty("circeVersion", "The version of circe " +
            "library", "0.14.1");
    private static final JsonLibraryProperty JSON_LIBRARY_PROPERTY = new JsonLibraryProperty();

    public static final String DEFAULT_PACKAGE_NAME = "org.openapitools.client";
    private static final PackageProperty PACKAGE_PROPERTY = new PackageProperty();

    private static final List<Property<?>> properties = Arrays.asList(
            STTP_CLIENT_VERSION, USE_SEPARATE_ERROR_CHANNEL, JODA_TIME_VERSION,
            JSON4S_VERSION, CIRCE_VERSION, JSON_LIBRARY_PROPERTY, PACKAGE_PROPERTY);

    private final Logger LOGGER = LoggerFactory.getLogger(ScalaSttpClientCodegen.class);

    protected String groupId = "org.openapitools";
    protected String artifactId = "openapi-client";
    protected String artifactVersion = "1.0.0";
    protected boolean registerNonStandardStatusCodes = true;
    protected boolean renderJavadoc = true;
    protected boolean removeOAuthSecurities = true;

    Map<String, ModelsMap> enumRefs = new HashMap<>();

    public ScalaSttpClientCodegen() {
        super();
        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.STABLE)
                .build();

        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .wireFormatFeatures(EnumSet.of(WireFormatFeature.JSON, WireFormatFeature.XML, WireFormatFeature.Custom))
                .securityFeatures(EnumSet.of(
                        SecurityFeature.BasicAuth,
                        SecurityFeature.ApiKey,
                        SecurityFeature.BearerToken
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
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.oneOf,
                        SchemaSupportFeature.allOf
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath,
                        ClientModificationFeature.UserAgent
                )
        );

        outputFolder = "generated-code/scala-sttp";
        modelTemplateFiles.put("model.mustache", ".scala");
        apiTemplateFiles.put("api.mustache", ".scala");
        embeddedTemplateDir = templateDir = "scala-sttp";

        additionalProperties.put(CodegenConstants.GROUP_ID, groupId);
        additionalProperties.put(CodegenConstants.ARTIFACT_ID, artifactId);
        additionalProperties.put(CodegenConstants.ARTIFACT_VERSION, artifactVersion);
        if (renderJavadoc) {
            additionalProperties.put("javadocRenderer", new JavadocLambda());
        }
        additionalProperties.put("fnCapitalize", new CapitalizeLambda());
        additionalProperties.put("fnCamelize", new CamelizeLambda(false));
        additionalProperties.put("fnEnumEntry", new EnumEntryLambda());

//        importMapping.remove("Seq");
//        importMapping.remove("List");
//        importMapping.remove("Set");
//        importMapping.remove("Map");

        // TODO: there is no specific sttp mapping. All Scala Type mappings should be in AbstractScala
        typeMapping = new HashMap<>();
        typeMapping.put("array", "Seq");
        typeMapping.put("set", "Set");
        typeMapping.put("boolean", "Boolean");
        typeMapping.put("string", "String");
        typeMapping.put("int", "Int");
        typeMapping.put("integer", "Int");
        typeMapping.put("long", "Long");
        typeMapping.put("float", "Float");
        typeMapping.put("byte", "Byte");
        typeMapping.put("short", "Short");
        typeMapping.put("char", "Char");
        typeMapping.put("double", "Double");
        typeMapping.put("object", "Any");
        typeMapping.put("file", "File");
        typeMapping.put("binary", "File");
        typeMapping.put("number", "Double");
        typeMapping.put("decimal", "BigDecimal");
        typeMapping.put("ByteArray", "Array[Byte]");
        typeMapping.put("AnyType", "Any");

        instantiationTypes.put("array", "ListBuffer");
        instantiationTypes.put("map", "Map");

        properties.stream()
                .map(Property::toCliOptions)
                .flatMap(Collection::stream)
                .forEach(option -> cliOptions.add(option));
    }

    @Override
    public void processOpts() {
        super.processOpts();
        properties.forEach(p -> p.updateAdditionalProperties(additionalProperties));
        invokerPackage = PACKAGE_PROPERTY.getInvokerPackage(additionalProperties);
        apiPackage = PACKAGE_PROPERTY.getApiPackage(additionalProperties);
        modelPackage = PACKAGE_PROPERTY.getModelPackage(additionalProperties);

        String jsonLibrary = JSON_LIBRARY_PROPERTY.getValue(additionalProperties);
        String jsonValueClass = "circe".equals(jsonLibrary) ? "io.circe.Json" : "org.json4s.JValue";
        typeMapping.put("object", jsonValueClass);
        typeMapping.put("AnyType", jsonValueClass);

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt"));
        final String invokerFolder = (sourceFolder + File.separator + invokerPackage).replace(".", File.separator);
        supportingFiles.add(new SupportingFile("jsonSupport.mustache", invokerFolder, "JsonSupport.scala"));
        supportingFiles.add(new SupportingFile("additionalTypeSerializers.mustache", invokerFolder, "AdditionalTypeSerializers.scala"));
        supportingFiles.add(new SupportingFile("project/build.properties.mustache", "project", "build.properties"));
        supportingFiles.add(new SupportingFile("project/plugins.mustache", "project", "plugins.sbt"));
        supportingFiles.add(new SupportingFile("scalafmt.mustache", "", ".scalafmt.conf"));
        supportingFiles.add(new SupportingFile("dateSerializers.mustache", invokerFolder, "DateSerializers.scala"));
    }

    @Override
    public String getName() {
        return "scala-sttp";
    }

    @Override
    public String getHelp() {
        return "Generates a Scala client library based on Sttp.";
    }

    @Override
    public String encodePath(String input) {
        String path = super.encodePath(input);

        // The parameter names in the URI must be converted to the same case as
        // the method parameter.
        StringBuffer buf = new StringBuffer(path.length());
        Matcher matcher = Pattern.compile("[{](.*?)[}]").matcher(path);
        while (matcher.find()) {
            matcher.appendReplacement(buf, "\\${" + toParamName(matcher.group(0)) + "}");
        }
        matcher.appendTail(buf);
        return buf.toString();
    }

    @Override
    public CodegenOperation fromOperation(String path,
                                          String httpMethod,
                                          Operation operation,
                                          List<Server> servers) {
        CodegenOperation op = super.fromOperation(path, httpMethod, operation, servers);
        op.path = encodePath(path);
        return op;
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String escapeReservedWord(String name) {
        if (this.reservedWordsMappings().containsKey(name)) {
            return this.reservedWordsMappings().get(name);
        }
        return "`" + name + "`";
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        return objs;
    }

    /**
     * Invoked by {@link DefaultGenerator} after all models have been post-processed,
     * allowing for a last pass of codegen-specific model cleanup.
     *
     * @param objs Current state of codegen object model.
     * @return An in-place modified state of the codegen object model.
     */
    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        Map<String, ModelsMap> modelsMap = super.postProcessAllModels(objs);

        Map<String, CodegenModel> allModels = collectAllModels(modelsMap);
        synthesizeOneOfFromDiscriminator(allModels);
        Map<String, Integer> refCounts = countModelReferences(allModels);
        markOneOfTraits(modelsMap, allModels, refCounts);
        removeInlinedModels(modelsMap);

        postProcessUpdateImports(modelsMap);
        return modelsMap;
    }

    /**
     * Collect all CodegenModels by classname for lookup.
     */
    private Map<String, CodegenModel> collectAllModels(Map<String, ModelsMap> modelsMap) {
        return modelsMap.values().stream()
                .flatMap(mm -> mm.getModels().stream())
                .map(ModelMap::getModel)
                .collect(java.util.stream.Collectors.toMap(m -> m.classname, m -> m, (a, b) -> a));
    }

    /**
     * For specs that use allOf+discriminator (children reference parent via allOf, parent has
     * discriminator.mapping but no oneOf), synthesize the oneOf set from the discriminator mapping.
     * This allows the standard oneOf processing logic to handle both patterns uniformly.
     */
    private void synthesizeOneOfFromDiscriminator(Map<String, CodegenModel> allModels) {
        for (CodegenModel model : allModels.values()) {
            if (!model.oneOf.isEmpty() || model.discriminator == null) {
                continue;
            }

            if (model.discriminator.getMappedModels() != null
                    && !model.discriminator.getMappedModels().isEmpty()) {
                for (CodegenDiscriminator.MappedModel mapped : model.discriminator.getMappedModels()) {
                    model.oneOf.add(mapped.getModelName());
                }
            } else if (model.discriminator.getMapping() != null) {
                for (String ref : model.discriminator.getMapping().values()) {
                    String modelName = ref.contains("/") ? ref.substring(ref.lastIndexOf('/') + 1) : ref;
                    if (allModels.containsKey(modelName)) {
                        model.oneOf.add(modelName);
                    }
                }
            }

            if (!model.oneOf.isEmpty()) {
                model.getVendorExtensions().put("x-synthesized-oneOf", true);
            }
        }
    }

    /**
     * Count how many times each model is referenced - both as a oneOf member and as a
     * property type. A child can only be inlined if it's referenced exactly once (by its
     * oneOf parent) and not used as a property type elsewhere.
     */
    private Map<String, Integer> countModelReferences(Map<String, CodegenModel> allModels) {
        Map<String, Integer> counts = new HashMap<>();

        // Count oneOf parent references
        allModels.values().stream()
                .flatMap(m -> m.oneOf.stream())
                .forEach(name -> counts.merge(name, 1, Integer::sum));

        // Count property-type references (prevents inlining models used as field types).
        // Check both dataType and complexType
        allModels.values().stream()
                .flatMap(m -> m.vars.stream())
                .forEach(prop -> {
                    if (prop.dataType != null && allModels.containsKey(prop.dataType)) {
                        counts.merge(prop.dataType, 1, Integer::sum);
                    }
                    if (prop.complexType != null && allModels.containsKey(prop.complexType)) {
                        counts.merge(prop.complexType, 1, Integer::sum);
                    }
                });

        return counts;
    }

    /**
     * Mark oneOf parents as sealed/regular traits with discriminator vendor extensions,
     * and configure child models for inlining.
     */
    private void markOneOfTraits(
        Map<String, ModelsMap> modelsMap,
        Map<String, CodegenModel> allModels,
        Map<String, Integer> refCounts) {
        for (ModelsMap mm : modelsMap.values()) {
            for (ModelMap modelMap : mm.getModels()) {
                CodegenModel model = modelMap.getModel();

                if (!model.oneOf.isEmpty()) {
                    configureOneOfModel(model, allModels, refCounts);
                }

                if (model.discriminator != null) {
                    model.getVendorExtensions().put("x-use-discr", true);
                    if (model.discriminator.getMapping() != null) {
                        model.getVendorExtensions().put("x-use-discr-mapping", true);
                    }
                }
            }
        }
    }

    private void configureOneOfModel(
        CodegenModel parent,
        Map<String, CodegenModel> allModels,
        Map<String, Integer> refCounts) {
        List<CodegenModel> inlineableMembers = new ArrayList<>();
        Set<String> childImports = new HashSet<>();

        for (String childName : parent.oneOf) {
            CodegenModel child = allModels.get(childName);
            if (child == null) continue;

            // All children extend the parent trait
            child.getVendorExtensions().put("x-oneOfParent", parent.classname);
            if (parent.discriminator != null) {
                child.getVendorExtensions().put("x-parentDiscriminatorName",
                        parent.discriminator.getPropertyName());
            }

            if (isInlineable(child, refCounts)) {
                child.getVendorExtensions().put("x-isOneOfMember", true);
                inlineableMembers.add(child);
                if (child.imports != null) {
                    childImports.addAll(child.imports);
                }
            }
        }

        buildDiscriminatorEntries(parent, allModels);

        if (!inlineableMembers.isEmpty() && inlineableMembers.size() == parent.oneOf.size()) {
            markAsSealedTrait(parent, inlineableMembers, childImports);
        } else {
            markAsRegularTrait(parent, inlineableMembers);
        }
    }

    private boolean isInlineable(CodegenModel child, Map<String, Integer> refCounts) {
        return (child.oneOf == null || child.oneOf.isEmpty())
                && refCounts.getOrDefault(child.classname, 0) == 1;
    }

    private void buildDiscriminatorEntries(CodegenModel parent, Map<String, CodegenModel> allModels) {
        List<Map<String, String>> entries = parent.oneOf.stream()
                .map(allModels::get)
                .filter(Objects::nonNull)
                .map(child -> Map.of("classname", child.classname, "schemaName", child.name))
                .collect(java.util.stream.Collectors.toList());
        parent.getVendorExtensions().put("x-discriminator-entries", entries);
    }

    private void markAsSealedTrait(
        CodegenModel parent,
        List<CodegenModel> members,
        Set<String> childImports) {
        parent.getVendorExtensions().put("x-isSealedTrait", true);
        parent.getVendorExtensions().put("x-oneOfMembers", members);

        if (parent.getVendorExtensions().containsKey("x-synthesized-oneOf")
                && parent.vars != null && !parent.vars.isEmpty()) {
            parent.getVendorExtensions().put("x-hasOwnVars", true);
        }

        mergeChildImports(parent, childImports);
    }

    private void markAsRegularTrait(CodegenModel parent, List<CodegenModel> partialMembers) {
        parent.getVendorExtensions().put("x-isRegularTrait", true);
        for (CodegenModel member : partialMembers) {
            member.getVendorExtensions().remove("x-isOneOfMember");
        }
    }

    private void mergeChildImports(CodegenModel parent, Set<String> childImports) {
        if (childImports.isEmpty()) return;
        Set<String> existing = parent.imports != null ? new HashSet<>(parent.imports) : new HashSet<>();
        childImports.removeAll(existing);
        if (!childImports.isEmpty()) {
            if (parent.imports == null) {
                parent.imports = new HashSet<>();
            }
            parent.imports.addAll(childImports);
        }
    }

    /**
     * Remove models that were inlined into their parent sealed trait -
     * they don't need separate files.
     */
    private void removeInlinedModels(Map<String, ModelsMap> modelsMap) {
        modelsMap.entrySet().removeIf(entry ->
            entry.getValue().getModels().stream()
                .anyMatch(m -> m.getModel().getVendorExtensions().containsKey("x-isOneOfMember"))
        );
    }

    /**
     * Update/clean up model imports
     * <p>
     * append '._" if the import is a Enum class, otherwise
     * remove model imports to avoid warnings for importing class in the same package in Scala
     *
     * @param models processed models to be further processed
     */
    @SuppressWarnings("unchecked")
    private void postProcessUpdateImports(final Map<String, ModelsMap> models) {
        final String prefix = modelPackage() + ".";

        enumRefs = getEnumRefs(models);

        for (String openAPIName : models.keySet()) {
            CodegenModel model = ModelUtils.getModelByName(openAPIName, models);
            if (model == null) {
                LOGGER.warn("Expected to retrieve model {} by name, but no model was found. Check your -Dmodels inclusions.", openAPIName);
                continue;
            }

            ModelsMap objs = models.get(openAPIName);
            List<Map<String, String>> imports = objs.getImports();
            if (imports == null || imports.isEmpty()) {
                continue;
            }
            List<Map<String, String>> newImports = new ArrayList<>();
            Iterator<Map<String, String>> iterator = imports.iterator();
            while (iterator.hasNext()) {
                String importPath = iterator.next().get("import");
                Map<String, String> item = new HashMap<>();
                if (importPath.startsWith(prefix)) {
                    if (isEnumClass(importPath, enumRefs)) {
                        item.put("import", importPath.concat("._"));
                        newImports.add(item);
                    }
                } else {
                    item.put("import", importPath);
                    newImports.add(item);
                }
            }
            // reset imports
            objs.setImports(newImports);
        }
    }

    private Map<String, ModelsMap> getEnumRefs(final Map<String, ModelsMap> models) {
        Map<String, ModelsMap> enums = new HashMap<>();
        for (String key : models.keySet()) {
            CodegenModel model = ModelUtils.getModelByName(key, models);
            if (model.isEnum) {
                ModelsMap objs = models.get(key);
                enums.put(key, objs);
            }
        }
        return enums;
    }

    private boolean isEnumClass(final String importPath, final Map<String, ModelsMap> enumModels) {
        if (enumModels == null || enumModels.isEmpty()) {
            return false;
        }
        for (ModelsMap objs : enumModels.values()) {
            List<ModelMap> models = objs.getModels();
            if (models == null || models.isEmpty()) {
                continue;
            }
            for (final Map<String, Object> model : models) {
                String enumImportPath = (String) model.get("importPath");
                if (enumImportPath != null && enumImportPath.equals(importPath)) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        if (registerNonStandardStatusCodes) {
            try {
                OperationMap opsMap = objs.getOperations();
                HashSet<Integer> unknownCodes = new HashSet<>();
                for (CodegenOperation operation : opsMap.getOperation()) {
                    for (CodegenResponse response : operation.responses) {
                        if ("default".equals(response.code)) {
                            continue;
                        }
                        try {
                            int code = Integer.parseInt(response.code);
                            if (code >= 600) {
                                unknownCodes.add(code);
                            }
                        } catch (NumberFormatException e) {
                            LOGGER.error("Status code is not an integer : response.code", e);
                        }
                    }
                }
                if (!unknownCodes.isEmpty()) {
                    additionalProperties.put("unknownStatusCodes", unknownCodes);
                }
            } catch (Exception e) {
                LOGGER.error("Unable to find operations List", e);
            }
        }

        // update imports for enum class
        List<Map<String, String>> newImports = new ArrayList<>();
        List<Map<String, String>> imports = objs.getImports();
        if (imports != null && !imports.isEmpty()) {
            Iterator<Map<String, String>> iterator = imports.iterator();
            while (iterator.hasNext()) {
                String importPath = iterator.next().get("import");
                Map<String, String> item = new HashMap<>();
                if (isEnumClass(importPath, enumRefs)) {
                    item.put("import", importPath.concat("._"));
                } else {
                    item.put("import", importPath);
                }
                newImports.add(item);
            }
        }
        objs.setImports(newImports);

        return super.postProcessOperationsWithModels(objs, allModels);
    }

    @Override
    public List<CodegenSecurity> fromSecurity(Map<String, SecurityScheme> schemes) {
        final List<CodegenSecurity> codegenSecurities = super.fromSecurity(schemes);
        if (!removeOAuthSecurities) {
            return codegenSecurities;
        }

        // Remove OAuth securities
        codegenSecurities.removeIf(security -> security.isOAuth);
        if (codegenSecurities.isEmpty()) {
            return null;
        }
        return codegenSecurities;
    }

    @Override
    public String toParamName(String name) {
        // obtain the name from parameterNameMapping directly if provided
        if (parameterNameMapping.containsKey(name)) {
            return parameterNameMapping.get(name);
        }

        return formatIdentifier(name, false);
    }

    @Override
    public String toEnumName(CodegenProperty property) {
        return formatIdentifier(property.baseName, true);
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (p.getRequired() != null && p.getRequired().contains(p.getName())) {
            return "None";
        }

        if (ModelUtils.isBooleanSchema(p)) {
            return null;
        } else if (ModelUtils.isDateSchema(p)) {
            return null;
        } else if (ModelUtils.isDateTimeSchema(p)) {
            return null;
        } else if (ModelUtils.isNumberSchema(p)) {
            return null;
        } else if (ModelUtils.isIntegerSchema(p)) {
            return null;
        } else if (ModelUtils.isMapSchema(p)) {
            String inner = getSchemaType(ModelUtils.getAdditionalProperties(p));
            return "Map[String, " + inner + "].empty ";
        } else if (ModelUtils.isArraySchema(p)) {
            String inner = getSchemaType(ModelUtils.getSchemaItems(p));
            if (ModelUtils.isSet(p)) {
                return "Set[" + inner + "].empty ";
            }
            return "Seq[" + inner + "].empty ";
        } else if (ModelUtils.isStringSchema(p)) {
            return null;
        } else {
            return null;
        }
    }

    /**
     * Update datatypeWithEnum for array container
     *
     * @param property Codegen property
     */
    @Override
    protected void updateDataTypeWithEnumForArray(CodegenProperty property) {
        CodegenProperty baseItem = property.items;
        while (baseItem != null && (Boolean.TRUE.equals(baseItem.isMap)
                || Boolean.TRUE.equals(baseItem.isArray))) {
            baseItem = baseItem.items;
        }
        if (baseItem != null) {
            // set datetypeWithEnum as only the inner type is enum
            property.datatypeWithEnum = toEnumName(baseItem);
            // naming the enum with respect to the language enum naming convention
            // e.g. remove [], {} from array/map of enum
            property.enumName = toEnumName(property);
            property._enum = baseItem._enum;

            updateCodegenPropertyEnum(property);
        }
    }

    public static abstract class Property<T> {
        final String name;
        final String description;
        final T defaultValue;

        public Property(String name, String description, T defaultValue) {
            this.name = name;
            this.description = description;
            this.defaultValue = defaultValue;
        }

        public abstract List<CliOption> toCliOptions();

        public abstract void updateAdditionalProperties(Map<String, Object> additionalProperties);

        public abstract T getValue(Map<String, Object> additionalProperties);

        public void setValue(Map<String, Object> additionalProperties, T value) {
            additionalProperties.put(name, value);
        }
    }

    public static class StringProperty extends Property<String> {
        public StringProperty(String name, String description, String defaultValue) {
            super(name, description, defaultValue);
        }

        @Override
        public List<CliOption> toCliOptions() {
            return Collections.singletonList(CliOption.newString(name, description).defaultValue(defaultValue));
        }

        @Override
        public void updateAdditionalProperties(Map<String, Object> additionalProperties) {
            if (!additionalProperties.containsKey(name)) {
                additionalProperties.put(name, defaultValue);
            }
        }

        @Override
        public String getValue(Map<String, Object> additionalProperties) {
            return additionalProperties.getOrDefault(name, defaultValue).toString();
        }
    }

    public static class BooleanProperty extends Property<Boolean> {
        public BooleanProperty(String name, String description, Boolean defaultValue) {
            super(name, description, defaultValue);
        }

        @Override
        public List<CliOption> toCliOptions() {
            return Collections.singletonList(CliOption.newBoolean(name, description, defaultValue));
        }

        @Override
        public void updateAdditionalProperties(Map<String, Object> additionalProperties) {
            Boolean value = getValue(additionalProperties);
            additionalProperties.put(name, value);
        }

        @Override
        public Boolean getValue(Map<String, Object> additionalProperties) {
            return Boolean.valueOf(additionalProperties.getOrDefault(name, defaultValue.toString()).toString());
        }
    }

    public static class JsonLibraryProperty extends StringProperty {
        private static final String JSON4S = "json4s";
        private static final String CIRCE = "circe";

        public JsonLibraryProperty() {
            super("jsonLibrary", "Json library to use. Possible values are: json4s and circe.", JSON4S);
        }

        @Override
        public void updateAdditionalProperties(Map<String, Object> additionalProperties) {
            String value = getValue(additionalProperties);
            if (CIRCE.equals(value) || JSON4S.equals(value)) {
                additionalProperties.put(CIRCE, CIRCE.equals(value));
                additionalProperties.put(JSON4S, JSON4S.equals(value));
            } else {
                IllegalArgumentException exception =
                        new IllegalArgumentException("Invalid json library: " + value + ". Must be " + CIRCE + " " +
                                "or " + JSON4S);
                throw exception;
            }
        }
    }

    public static class PackageProperty extends StringProperty {

        public PackageProperty() {
            super("mainPackage", "Top-level package name, which defines 'apiPackage', 'modelPackage', " +
                    "'invokerPackage'", DEFAULT_PACKAGE_NAME);
        }

        @Override
        public void updateAdditionalProperties(Map<String, Object> additionalProperties) {
            String mainPackage = getValue(additionalProperties);
            if (!additionalProperties.containsKey(CodegenConstants.API_PACKAGE)) {
                String apiPackage = mainPackage + ".api";
                additionalProperties.put(CodegenConstants.API_PACKAGE, apiPackage);
            }
            if (!additionalProperties.containsKey(CodegenConstants.MODEL_PACKAGE)) {
                String modelPackage = mainPackage + ".model";
                additionalProperties.put(CodegenConstants.MODEL_PACKAGE, modelPackage);
            }
            if (!additionalProperties.containsKey(CodegenConstants.INVOKER_PACKAGE)) {
                String invokerPackage = mainPackage + ".core";
                additionalProperties.put(CodegenConstants.INVOKER_PACKAGE, invokerPackage);
            }
        }

        public String getApiPackage(Map<String, Object> additionalProperties) {
            return additionalProperties.getOrDefault(CodegenConstants.API_PACKAGE, DEFAULT_PACKAGE_NAME + ".api").toString();
        }

        public String getModelPackage(Map<String, Object> additionalProperties) {
            return additionalProperties.getOrDefault(CodegenConstants.MODEL_PACKAGE, DEFAULT_PACKAGE_NAME + ".model").toString();
        }

        public String getInvokerPackage(Map<String, Object> additionalProperties) {
            return additionalProperties.getOrDefault(CodegenConstants.INVOKER_PACKAGE, DEFAULT_PACKAGE_NAME + ".core").toString();
        }
    }

    private static class JavadocLambda extends CustomLambda {
        @Override
        public String formatFragment(String fragment) {
            final String[] lines = fragment.split("\\r?\\n");
            final StringBuilder sb = new StringBuilder();
            sb.append("  /**\n");
            for (String line : lines) {
                sb.append("   * ").append(line).append("\n");
            }
            sb.append("   */\n");
            return sb.toString();
        }
    }

    private static class CapitalizeLambda extends CustomLambda {
        @Override
        public String formatFragment(String fragment) {
            return StringUtils.capitalize(fragment);
        }
    }

    private class EnumEntryLambda extends CustomLambda {
        @Override
        public String formatFragment(String fragment) {
            return formatIdentifier(fragment, true);
        }
    }

}
