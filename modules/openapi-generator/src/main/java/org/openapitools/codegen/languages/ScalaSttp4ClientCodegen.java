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
 * <p>Mustache templates are located in {@code src/main/resources/scala-sttp4/}.
 */
public class ScalaSttp4ClientCodegen extends AbstractScalaCodegen implements CodegenConfig {
    private static final StringProperty STTP_CLIENT_VERSION = new StringProperty("sttpClientVersion", "The version of " +
            "sttp client", "4.0.15");
    private static final BooleanProperty USE_SEPARATE_ERROR_CHANNEL = new BooleanProperty("separateErrorChannel",
            "Whether to return response as " +
                    "F[Either[ResponseError[ErrorType], ReturnType]]] or to flatten " +
                    "response's error raising them through enclosing monad (F[ReturnType]).", true);
    private static final StringProperty JODA_TIME_VERSION = new StringProperty("jodaTimeVersion", "The version of " +
            "joda-time library", "2.10.13");
    private static final StringProperty JSON4S_VERSION = new StringProperty("json4sVersion", "The version of json4s " +
            "library", "4.0.6");
    private static final StringProperty CIRCE_VERSION = new StringProperty("circeVersion", "The version of circe " +
            "library", "0.14.15");
    private static final StringProperty CIRCE_EXTRAS_VERSION = new StringProperty("circeExtrasVersion",
      "The version of circe-generic-extras library", "0.14.4");

    private static final JsonLibraryProperty JSON_LIBRARY_PROPERTY = new JsonLibraryProperty();

    public static final String DEFAULT_PACKAGE_NAME = "org.openapitools.client";
    private static final PackageProperty PACKAGE_PROPERTY = new PackageProperty();

    private static final List<Property<?>> properties = Arrays.asList(
            STTP_CLIENT_VERSION, USE_SEPARATE_ERROR_CHANNEL, JODA_TIME_VERSION,
            JSON4S_VERSION, CIRCE_VERSION, CIRCE_EXTRAS_VERSION, JSON_LIBRARY_PROPERTY, PACKAGE_PROPERTY);

    private final Logger LOGGER = LoggerFactory.getLogger(ScalaSttp4ClientCodegen.class);

    protected String groupId = "org.openapitools";
    protected String artifactId = "openapi-client";
    protected String artifactVersion = "1.0.0";
    protected boolean registerNonStandardStatusCodes = true;
    protected boolean renderJavadoc = true;
    protected boolean removeOAuthSecurities = true;

    public ScalaSttp4ClientCodegen() {
        super();
        generatorMetadata = GeneratorMetadata.newBuilder(generatorMetadata)
                .stability(Stability.BETA)
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
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
                .includeClientModificationFeatures(
                        ClientModificationFeature.BasePath,
                        ClientModificationFeature.UserAgent
                )
        );

        // Enable oneOf interface generation
        useOneOfInterfaces = true;
        supportsMultipleInheritance = true;
        supportsInheritance = true;
        addOneOfInterfaceImports = true;

        outputFolder = "generated-code/scala-sttp4";
        modelTemplateFiles.put("model.mustache", ".scala");
        apiTemplateFiles.put("api.mustache", ".scala");
        embeddedTemplateDir = templateDir = "scala-sttp4";

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
        typeMapping.put("file", "File");
        typeMapping.put("binary", "File");
        typeMapping.put("number", "Double");
        typeMapping.put("decimal", "BigDecimal");
        typeMapping.put("ByteArray", "Array[Byte]");
        // AnyType and object mapping will be set in processOpts() based on jsonLibrary

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

        // Set AnyType and object mapping based on jsonLibrary
        String jsonLibrary = JSON_LIBRARY_PROPERTY.getValue(additionalProperties);
        if ("circe".equals(jsonLibrary)) {
            typeMapping.put("AnyType", "io.circe.Json");
            typeMapping.put("object", "io.circe.JsonObject");
            importMapping.put("io.circe.Json", "io.circe.Json");
            importMapping.put("io.circe.JsonObject", "io.circe.JsonObject");
        } else {
            typeMapping.put("AnyType", "org.json4s.JValue");
            typeMapping.put("object", "org.json4s.JObject");
            importMapping.put("org.json4s.JValue", "org.json4s.JValue");
            importMapping.put("org.json4s.JObject", "org.json4s.JObject");
        }

        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("build.sbt.mustache", "", "build.sbt"));
        final String invokerFolder = (sourceFolder + File.separator + invokerPackage).replace(".", File.separator);
        supportingFiles.add(new SupportingFile("jsonSupport.mustache", invokerFolder, "JsonSupport.scala"));
        supportingFiles.add(new SupportingFile("additionalTypeSerializers.mustache", invokerFolder, "AdditionalTypeSerializers.scala"));
        supportingFiles.add(new SupportingFile("project/build.properties.mustache", "project", "build.properties"));
        supportingFiles.add(new SupportingFile("dateSerializers.mustache", invokerFolder, "DateSerializers.scala"));
        supportingFiles.add(new SupportingFile("project/plugins.mustache", "project", "plugins.sbt"));
        supportingFiles.add(new SupportingFile("scalafmt.mustache", "", ".scalafmt.conf"));
    }

    @Override
    public String getName() {
        return "scala-sttp4";
    }

    @Override
    public String getHelp() {
        return "Generates a Scala client library (beta) based on Sttp4.";
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

    private void setParameterDefaults(CodegenParameter param) {
        // Set default values for optional parameters
        // Template will handle Option[] wrapping, so all defaults should be None
        if (!param.required) {
            param.defaultValue = "None";
        }
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
        final Map<String, ModelsMap> processed = super.postProcessAllModels(objs);

        // Pre-pass: fix aliased oneOf children.
        // When a oneOf child schema has no type and no properties (e.g. just title + description),
        // DefaultCodegen's unaliasSchema treats it as a type alias and replaces the model name
        // in cModel.oneOf with the language type (e.g. "io.circe.Json" instead of "CancellationInfoHotel").
        // We fix this by looking at the original OpenAPI schema's oneOf $refs to get the real model names.
        Map<String, Schema> allSchemas = this.openAPI.getComponents() != null
                ? this.openAPI.getComponents().getSchemas()
                : Collections.emptyMap();

        for (ModelsMap mm : processed.values()) {
            for (ModelMap model : mm.getModels()) {
                CodegenModel cModel = model.getModel();
                if (!cModel.oneOf.isEmpty() && allSchemas != null) {
                    Schema parentSchema = allSchemas.get(cModel.name);
                    if (parentSchema != null && parentSchema.getOneOf() != null) {
                        Set<String> resolvedOneOf = new LinkedHashSet<>();
                        for (Object o : parentSchema.getOneOf()) {
                            Schema childSchema = (Schema) o;
                            if (childSchema.get$ref() != null) {
                                String refName = toModelName(org.openapitools.codegen.utils.ModelUtils.getSimpleRef(childSchema.get$ref()));
                                resolvedOneOf.add(refName);
                            }
                        }
                        if (!resolvedOneOf.isEmpty()) {
                            cModel.oneOf.clear();
                            cModel.oneOf.addAll(resolvedOneOf);
                        }
                    }
                }
            }
        }

        // First pass: count how many oneOf parents each model has
        Map<String, Integer> oneOfMemberCount = new HashMap<>();
        for (ModelsMap mm : processed.values()) {
            for (ModelMap model : mm.getModels()) {
                CodegenModel cModel = model.getModel();
                if (!cModel.oneOf.isEmpty()) {
                    for (String childName : cModel.oneOf) {
                        oneOfMemberCount.put(childName, oneOfMemberCount.getOrDefault(childName, 0) + 1);
                    }
                }
            }
        }

        // Second pass: process models
        for (ModelsMap mm : processed.values()) {
            for (ModelMap model : mm.getModels()) {
                CodegenModel cModel = model.getModel();

                if (!cModel.oneOf.isEmpty()) {
                    cModel.getVendorExtensions().put("x-isSealedTrait", true);

                    // Identify the parent schema's own properties (not inherited from oneOf children).
                    // These need to be propagated to all children and rendered as abstract members on the sealed trait.
                    List<CodegenProperty> parentOwnProps = getParentOwnProperties(cModel, allSchemas);
                    if (!parentOwnProps.isEmpty()) {
                        cModel.getVendorExtensions().put("x-parentProps", parentOwnProps);
                        cModel.getVendorExtensions().put("x-hasParentProps", true);
                    }

                    // Collect child models for inline generation
                    // Only inline if they are used exclusively by this oneOf parent
                    List<CodegenModel> childModels = new ArrayList<>();
                    // Collect shared child models that need wrapper composition
                    List<Map<String, Object>> wrappedMembers = new ArrayList<>();

                    for (String childName : cModel.oneOf) {
                        CodegenModel childModel = ModelUtils.getModelByName(childName, processed);
                        if (childModel != null && oneOfMemberCount.getOrDefault(childName, 0) == 1) {
                            // This child is only used by this parent - can be inlined
                            childModel.getVendorExtensions().put("x-isOneOfMember", true);
                            childModel.getVendorExtensions().put("x-oneOfParent", cModel.classname);

                            // Propagate parent's own properties to child's allVars
                            // so they appear in the generated case class fields
                            propagateParentProperties(childModel, parentOwnProps);

                            // Add discriminator mapping value if present
                            if (cModel.discriminator != null) {
                                String discriminatorName = cModel.discriminator.getPropertyName();
                                
                                // Find the mapping value for this child model
                                String discriminatorValue = null;
                                if (cModel.discriminator.getMappedModels() != null) {
                                    for (CodegenDiscriminator.MappedModel mappedModel : cModel.discriminator.getMappedModels()) {
                                        if (mappedModel.getModelName().equals(childName)) {
                                            discriminatorValue = mappedModel.getMappingName();
                                            break;
                                        }
                                    }
                                }
                                
                                if (discriminatorValue != null) {
                                    childModel.getVendorExtensions().put("x-discriminator-value", discriminatorValue);
                                }
                                
                                // Remove discriminator field from child
                                // (circe-generic-extras adds it automatically)
                                childModel.vars.removeIf(prop -> prop.baseName.equals(discriminatorName));
                                childModel.allVars.removeIf(prop -> prop.baseName.equals(discriminatorName));
                                childModel.requiredVars.removeIf(prop -> prop.baseName.equals(discriminatorName));
                                childModel.optionalVars.removeIf(prop -> prop.baseName.equals(discriminatorName));
                            }

                            childModels.add(childModel);
                        } else if (childModel != null) {
                            // This child is shared across multiple oneOf parents.
                            // Use wrapper composition: generate a case class wrapper inside the parent.
                            Map<String, Object> wrappedMember = new HashMap<>();
                            wrappedMember.put("classname", childName);
                            wrappedMember.put("wrapperClassname", childName + cModel.classname);
                            wrappedMember.put("parentClassname", cModel.classname);

                            // Resolve discriminator value if applicable
                            if (cModel.discriminator != null) {
                                String discriminatorValue = childName; // default: class name
                                if (cModel.discriminator.getMappedModels() != null) {
                                    for (CodegenDiscriminator.MappedModel mappedModel : cModel.discriminator.getMappedModels()) {
                                        if (mappedModel.getModelName().equals(childName)) {
                                            discriminatorValue = mappedModel.getMappingName();
                                            break;
                                        }
                                    }
                                }
                                wrappedMember.put("discriminatorValue", discriminatorValue);
                            }

                            wrappedMembers.add(wrappedMember);
                        }
                    }
                    cModel.getVendorExtensions().put("x-oneOfMembers", childModels);
                    cModel.getVendorExtensions().put("x-wrappedOneOfMembers", wrappedMembers);
                    if (!wrappedMembers.isEmpty()) {
                        cModel.getVendorExtensions().put("x-hasWrappedOneOfMembers", true);
                    }
                } else if (cModel.isEnum) {
                    cModel.getVendorExtensions().put("x-isEnum", true);
                } else {
                    cModel.getVendorExtensions().put("x-isRegularModel", true);
                }

                if (cModel.discriminator != null) {
                    cModel.getVendorExtensions().put("x-use-discr", true);

                    if (cModel.discriminator.getMapping() != null) {
                        cModel.getVendorExtensions().put("x-use-discr-mapping", true);
                    }
                }

                // Remove discriminator property from models that extend a oneOf parent
                // (circe-generic-extras adds it automatically)
                if (cModel.parent != null && cModel.parentModel != null && cModel.parentModel.discriminator != null) {
                    String discriminatorName = cModel.parentModel.discriminator.getPropertyName();
                    cModel.vars.removeIf(prop -> prop.baseName.equals(discriminatorName));
                    cModel.allVars.removeIf(prop -> prop.baseName.equals(discriminatorName));
                    cModel.requiredVars.removeIf(prop -> prop.baseName.equals(discriminatorName));
                    cModel.optionalVars.removeIf(prop -> prop.baseName.equals(discriminatorName));
                }
            }
        }

        // Third pass: remove oneOf members from the map to skip file generation
        // (they are already inlined in their parent sealed trait)
        processed.entrySet().removeIf(entry -> {
            ModelsMap mm = entry.getValue();
            return mm.getModels().stream()
                    .anyMatch(model -> model.getModel().getVendorExtensions().containsKey("x-isOneOfMember"));
        });

        postProcessUpdateImports(processed);
        return processed;
    }

    /**
     * Identifies properties that are defined directly on a oneOf parent schema
     * (not inherited from oneOf children). These are the parent's "own" properties
     * that need to be propagated to all children.
     */
    private List<CodegenProperty> getParentOwnProperties(CodegenModel parentModel, Map<String, Schema> allSchemas) {
        List<CodegenProperty> parentOwnProps = new ArrayList<>();
        if (allSchemas == null) return parentOwnProps;

        Schema parentSchema = allSchemas.get(parentModel.name);
        if (parentSchema == null || parentSchema.getProperties() == null) return parentOwnProps;

        Set<String> parentPropNames = parentSchema.getProperties().keySet();
        Set<String> parentRequired = parentSchema.getRequired() != null
                ? new HashSet<>(parentSchema.getRequired())
                : Collections.emptySet();

        for (String propName : parentPropNames) {
            // Find matching CodegenProperty in the parent model's vars
            for (CodegenProperty cp : parentModel.vars) {
                if (cp.baseName.equals(propName)) {
                    CodegenProperty cloned = cp.clone();
                    // Ensure required flag reflects the parent schema's required list
                    cloned.required = parentRequired.contains(propName);
                    parentOwnProps.add(cloned);
                    break;
                }
            }
        }
        return parentOwnProps;
    }

    /**
     * Propagates parent-owned properties to a child model's allVars.
     * Parent properties are prepended so they appear first in the generated case class.
     * Properties already present on the child (by baseName) are not duplicated.
     */
    private void propagateParentProperties(CodegenModel childModel, List<CodegenProperty> parentOwnProps) {
        if (parentOwnProps.isEmpty()) return;

        Set<String> existingPropNames = new HashSet<>();
        for (CodegenProperty cp : childModel.allVars) {
            existingPropNames.add(cp.baseName);
        }

        List<CodegenProperty> toAdd = new ArrayList<>();
        for (CodegenProperty parentProp : parentOwnProps) {
            if (!existingPropNames.contains(parentProp.baseName)) {
                toAdd.add(parentProp.clone());
            }
        }

        if (!toAdd.isEmpty()) {
            toAdd.addAll(childModel.allVars);
            childModel.allVars = toAdd;
        }
    }

    /**
     * Update/clean up model imports
     * <p>
     * remove model imports to avoid warnings for importing class in the same package in Scala
     *
     * @param models processed models to be further processed
     */
    @SuppressWarnings("unchecked")
    private void postProcessUpdateImports(final Map<String, ModelsMap> models) {
        final String prefix = modelPackage() + ".";

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
                // Same-package imports are dropped: sttp4 enums are sealed traits +
                // case objects (not Enumeration), so unlike scala-sttp there is no
                // `MyEnum._` wildcard needed to bring a type alias into scope.
                if (!importPath.startsWith(prefix)) {
                    Map<String, String> item = new HashMap<>();
                    item.put("import", importPath);
                    newImports.add(item);
                }
            }
            // reset imports
            objs.setImports(newImports);
        }
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
                // sttp4 enums are sealed traits + case objects (not Enumeration):
                // import the type itself, not `MyEnum._` wildcard members, which
                // would not bring the type into scope.
                item.put("import", importPath);
                newImports.add(item);
            }
        }
        objs.setImports(newImports);

        // Fix parameter types and defaults
        OperationMap opsMap = objs.getOperations();
        for (CodegenOperation operation : opsMap.getOperation()) {
            for (CodegenParameter param : operation.allParams) {
                setParameterDefaults(param);
            }
        }

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
            // Use simple Seq.empty for cleaner code
            if (ModelUtils.isSet(p)) {
                return "Set.empty";
            }
            return "Seq.empty";
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
