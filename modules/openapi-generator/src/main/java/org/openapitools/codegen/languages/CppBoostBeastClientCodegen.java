package org.openapitools.codegen.languages;


import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.*;

import java.io.File;
import java.util.*;
import java.util.Map;
import java.util.HashMap;
import java.util.stream.Collectors;

import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.openapitools.codegen.utils.StringUtils.camelize;

public class CppBoostBeastClientCodegen extends AbstractCppCodegen {

    public static final String DEFAULT_PACKAGE_NAME = "CppBoostBeastOpenAPIClient";
    private static final String X_CODEGEN_DEFAULT_RESPONSE_IS_RETURN_COMPATIBLE =
            "x-codegen-default-response-is-return-compatible";
    private static final String X_CODEGEN_EMPTY_BODY_TOLERANT = "x-codegen-empty-body-tolerant";
    private static final String X_CODEGEN_HAS_DEFAULT_RESPONSE = "x-codegen-has-default-response";
    private static final String X_CODEGEN_IS_RAW_BODY = "x-codegen-is-raw-body";
    private static final String X_CODEGEN_IS_OPTIONAL_QUERY_PARAMETER =
            "x-codegen-is-optional-query-parameter";
    private static final String X_CODEGEN_QUERY_COLLECTION_DELIMITER =
            "x-codegen-query-collection-delimiter";
    private static final String X_CODEGEN_QUERY_COLLECTION_MULTI =
            "x-codegen-query-collection-multi";
    private static final String X_CODEGEN_QUERY_MAP_EXPLODED =
            "x-codegen-query-map-exploded";
    private static final String X_CODEGEN_QUERY_MAP_DEEP_OBJECT =
            "x-codegen-query-map-deep-object";
    private static final String X_CODEGEN_RESPONSE_RANGE = "x-codegen-response-range";
    private static final String X_CODEGEN_RESPONSE_IS_ONE_OF = "x-codegen-response-is-oneof";
    private static final String X_CODEGEN_STREAM_IS_ONE_OF = "x-codegen-stream-is-oneof";
    private static final String X_CODEGEN_DUAL_STREAM_IS_ONE_OF = "x-codegen-dual-stream-is-oneof";
    private final Logger LOGGER = LoggerFactory.getLogger(CppBoostBeastClientCodegen.class);
    /** Tracks model names resolved as oneOf/anyOf variant types for shared_ptr exclusion. */
    private final Set<String> variantModels = new HashSet<>();
    /** Caches resolved C++ types for composed models, keyed by model name.
     *  Populated during Phase 1 of postProcessModels and used by Phase 1b to
     *  transitively resolve $ref chains through model aliases (e.g., ModelIds
     *  referencing ModelIdsShared, both ultimately std::string). */
    private final Map<String, String> resolvedAliasTypes = new HashMap<>();
    /** Retains composition semantics after named schemas are lowered to C++ aliases. */
    private final Map<String, String> composedKeywordsByModel = new HashMap<>();
    protected String packageName = DEFAULT_PACKAGE_NAME;

    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    public String getName() {
        return "cpp-boost-beast-client";
    }

    public String getHelp() {
        return "Generates a cpp-boost-beast client.";
    }

    @Override
    public void preprocessOpenAPI(OpenAPI openAPI) {
        super.preprocessOpenAPI(openAPI);
        // Populate variantModels before model processing begins so that
        // getTypeDeclaration can resolve $ref to composed models as value types
        // (without shared_ptr wrapping) regardless of processing order.
        Map<String, Schema> schemas = openAPI.getComponents() != null
                ? openAPI.getComponents().getSchemas() : null;
        if (schemas != null) {
            for (Map.Entry<String, Schema> entry : schemas.entrySet()) {
                String name = entry.getKey();
                Schema schema = entry.getValue();
                if ((schema.getOneOf() != null && !schema.getOneOf().isEmpty())
                        || (schema.getAnyOf() != null && !schema.getAnyOf().isEmpty())) {
                    variantModels.add(name);
                }
            }
        }
    }

    public CppBoostBeastClientCodegen() {
        super();
        openapiNormalizer.put("NORMALIZER_CLASS", CppBoostBeastOpenAPINormalizer.class.getName());
        modifyFeatureSet(features -> features
                .includeDocumentationFeatures(DocumentationFeature.Readme)
                .securityFeatures(EnumSet.noneOf(SecurityFeature.class))
                .excludeGlobalFeatures(
                        GlobalFeature.XMLStructureDefinitions,
                        GlobalFeature.Callbacks,
                        GlobalFeature.LinkObjects,
                        GlobalFeature.ParameterStyling,
                        GlobalFeature.MultiServer
                )
                .includeSchemaSupportFeatures(
                        SchemaSupportFeature.Polymorphism,
                        SchemaSupportFeature.Composite,
                        SchemaSupportFeature.oneOf,
                        SchemaSupportFeature.anyOf,
                        SchemaSupportFeature.allOf,
                        SchemaSupportFeature.Union
                )
                .includeDataTypeFeatures(
                        DataTypeFeature.AnyType,
                        DataTypeFeature.Null
                )
                .excludeParameterFeatures(
                        ParameterFeature.Cookie
                )
        );

        outputFolder = "generated-code" + File.separator + "cpp-boost-beast";
        modelTemplateFiles.put("model-header.mustache", ".h");
        modelTemplateFiles.put("model-source.mustache", ".cpp");
        apiTemplateFiles.put("api-header.mustache", ".h");
        apiTemplateFiles.put("api-source.mustache", ".cpp");

        embeddedTemplateDir = templateDir = "cpp-boost-beast-client";

        modelPackage = "org.openapitools.client.model";
        apiPackage = "org.openapitools.client.api";

        cliOptions.clear();

        // CLI options
        addOption(CodegenConstants.PACKAGE_NAME, "C++ package and library name.", DEFAULT_PACKAGE_NAME);
        addOption(CodegenConstants.MODEL_PACKAGE, "C++ namespace for models (convention: name.space.model).",
                this.modelPackage);
        addOption(CodegenConstants.API_PACKAGE, "C++ namespace for apis (convention: name.space.api).",
                this.apiPackage);


        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("CMakeLists.txt.mustache", "", "CMakeLists.txt"));
        supportingFiles.add(new SupportingFile("http-client-header.mustache", "api", "HttpClient.h"));
        supportingFiles.add(new SupportingFile("http-client-impl-header.mustache", "api", "HttpClientImpl.h"));
        supportingFiles.add(new SupportingFile("http-client-impl-source.mustache", "api", "HttpClientImpl.cpp"));
        supportingFiles.add(new SupportingFile("anytype-header.mustache", "model", "AnyType.h"));

        languageSpecificPrimitives = new HashSet<String>(
                Arrays.asList("int", "char", "bool", "long", "float", "double", "int32_t", "int64_t"));

        super.typeMapping = new HashMap<String, String>();
        typeMapping.put("date", "std::string");
        typeMapping.put("DateTime", "std::string");
        typeMapping.put("string", "std::string");
        typeMapping.put("integer", "int32_t");
        typeMapping.put("long", "int64_t");
        typeMapping.put("boolean", "bool");
        typeMapping.put("array", "std::vector");
        typeMapping.put("map", "std::map");
        typeMapping.put("file", "std::string");
        typeMapping.put("object", "boost::json::value");
        typeMapping.put("number", "double");
        typeMapping.put("UUID", "std::string");
        typeMapping.put("URI", "std::string");
        typeMapping.put("ByteArray", "std::string");
        
        super.importMapping = new HashMap<String, String>();
        importMapping.put("std::vector", "#include <vector>");
        importMapping.put("std::map", "#include <map>");
        importMapping.put("std::string", "#include <string>");
        importMapping.put("int32_t", "#include <cstdint>");
        importMapping.put("int64_t", "#include <cstdint>");
        importMapping.put("boost::json::value", "#include <boost/json.hpp>");
        importMapping.put("std::nullptr_t", "#include <cstddef>");
        importMapping.put("Null", "#include <cstddef>");
        importMapping.put("std::optional", "#include <optional>");
        importMapping.put("std::variant", "#include <variant>");
        importMapping.put("std::monostate", "#include <variant>");
        importMapping.put("std::shared_ptr", "#include <memory>");
        importMapping.put("AnyType", "#include \"AnyType.h\"");
    }

    /** Retains [Model, null] unions while preserving default normalization elsewhere. */
    public static final class CppBoostBeastOpenAPINormalizer extends OpenAPINormalizer {
        public CppBoostBeastOpenAPINormalizer(OpenAPI openAPI, Map<String, String> inputRules) {
            super(openAPI, inputRules);
        }

        @Override
        protected Schema processSimplifyAnyOf(Schema schema) {
            return nullableModelRef(schema.getAnyOf()) == null
                    ? super.processSimplifyAnyOf(schema) : schema;
        }

        @Override
        protected Schema processSimplifyOneOf(Schema schema) {
            return nullableModelRef(schema.getOneOf()) == null
                    ? super.processSimplifyOneOf(schema) : schema;
        }

        private String nullableModelRef(List<Schema> branches) {
            if (branches == null || branches.size() != 2) {
                return null;
            }
            String referencedModel = null;
            int nullBranches = 0;
            for (Schema branch : branches) {
                if (ModelUtils.isNullTypeSchema(openAPI, branch)) {
                    nullBranches++;
                } else if (branch.get$ref() != null) {
                    referencedModel = ModelUtils.getSimpleRef(branch.get$ref());
                } else {
                    return null;
                }
            }
            return nullBranches == 1 ? referencedModel : null;
        }
    }


    @Override
    public Map<String, ModelsMap> updateAllModels(Map<String, ModelsMap> objs)  {
        // Index all CodegenModels by model name.
        Map<String, CodegenModel> allModels = getAllModels(objs);

        // Clean interfaces of ambiguity
        for (Map.Entry<String, CodegenModel> cm : allModels.entrySet()) {
            if (cm.getValue().interfaces != null && !cm.getValue().interfaces.isEmpty()) {
                List<String> newIntf = new ArrayList<>(cm.getValue().interfaces);

                for (String intf : allModels.get(cm.getKey()).interfaces) {
                    if (allModels.get(intf).interfaces != null && !allModels.get(intf).interfaces.isEmpty()) {
                        for (String intfInner : allModels.get(intf).interfaces) {
                            newIntf.remove(intfInner);
                        }
                    }
                }
                cm.getValue().interfaces = newIntf;
            }
        }

        // --- Critical: Normalize shared_ptr types for cycle detection ---
        // DefaultCodegen.setCircularReferences compares property dataType strings
        // to model names literally. Since getTypeDeclaration wraps refs in
        // "std::shared_ptr<X>", the comparison "std::shared_ptr<Node>" != "Node"
        // never matches — cycles go undetected. This causes the shared_ptr stripping
        // phase below to strip ALL wrappers, including cycle edges, producing invalid
        // C++ with value self-refs.
        //
        // Fix: Temporarily strip std::shared_ptr<> wrappers from all property
        // dataTypes BEFORE super.updateAllModels runs (which calls setCircularReferences),
        // then restore them after. This ensures setCircularReferences sees bare model
        // names and correctly identifies cycles.
        Map<String, Map<String, String>> savedSharedPtr = new HashMap<>();
        for (CodegenModel cm : allModels.values()) {
            Map<String, String> modelSaves = new HashMap<>();
            for (CodegenProperty var : allVarsOf(cm)) {
                if (var == null) continue;
                checkAndSaveSharedPtr(var, cm.classname, modelSaves);
                if (var.isContainer && var.items != null) {
                    checkAndSaveSharedPtr(var.items, cm.classname, modelSaves);
                }
            }
            if (!modelSaves.isEmpty()) {
                savedSharedPtr.put(cm.classname, modelSaves);
            }
        }

        objs = super.updateAllModels(objs);

        // Restore shared_ptr wrappers stripped above.
        // isCircularReference flags are now correctly set by setCircularReferences
        // because it compared bare model names.
        for (CodegenModel cm : allModels.values()) {
            Map<String, String> modelSaves = savedSharedPtr.get(cm.classname);
            if (modelSaves == null) continue;
            for (CodegenProperty var : allVarsOf(cm)) {
                if (var == null) continue;
                restoreSavedSharedPtr(var, cm.classname, modelSaves);
                if (var.isContainer && var.items != null) {
                    restoreSavedSharedPtr(var.items, cm.classname + ".items", modelSaves);
                }
            }
        }

        // Phase: Strip std::shared_ptr<X> from non-cyclic object refs.
        // super.updateAllModels → DefaultCodegen.updateAllModels → setCircularReferences
        // has now run, setting isCircularReference flags on properties correctly.
        // Non-cyclic edges should use value semantics (plain X) rather than
        // std::shared_ptr<X> to avoid unnecessary heap allocation.
        for (CodegenModel cm : allModels.values()) {
            for (CodegenProperty var : allVarsOf(cm)) {
                if (var == null) continue;
                stripNonCyclicSharedPtr(var);
                if (var.isContainer && var.items != null) {
                    stripNonCyclicSharedPtr(var.items);
                }
            }
        }

        return objs;
    }

    /**
     * Returns all property lists of a model for iteration.
     */
    private static List<CodegenProperty> allVarsOf(CodegenModel cm) {
        List<CodegenProperty> combined = new ArrayList<>();
        if (cm.vars != null) combined.addAll(cm.vars);
        if (cm.allVars != null) combined.addAll(cm.allVars);
        if (cm.requiredVars != null) combined.addAll(cm.requiredVars);
        if (cm.optionalVars != null) combined.addAll(cm.optionalVars);
        if (cm.readOnlyVars != null) combined.addAll(cm.readOnlyVars);
        if (cm.readWriteVars != null) combined.addAll(cm.readWriteVars);
        if (cm.parentVars != null) combined.addAll(cm.parentVars);
        return combined;
    }

    /**
     * If a property has a dataType wrapped in std::shared_ptr<>, strips the
     * wrapper and saves the original under a compound key (modelName.baseName)
     * so it can be restored after setCircularReferences runs.
     */
    private static void checkAndSaveSharedPtr(CodegenProperty var, String modelName,
                                               Map<String, String> saves) {
        if (var.dataType != null && var.dataType.startsWith("std::shared_ptr<")) {
            String key = modelName + "." + var.baseName;
            if (!saves.containsKey(key)) {
                saves.put(key, var.dataType);
            }
            var.dataType = var.dataType.substring(16, var.dataType.length() - 1);
        }
    }

    /**
     * Restores a previously saved shared_ptr-wrapped dataType onto a property.
     */
    private static void restoreSavedSharedPtr(CodegenProperty var, String modelName,
                                               Map<String, String> saves) {
        String key = modelName + "." + var.baseName;
        String saved = saves.get(key);
        if (saved != null) {
            var.dataType = saved;
        }
    }

    /**
     * Strips std::shared_ptr<X> from a non-cyclic property, replacing it with
     * bare value type X. Cyclic properties retain the shared_ptr wrapper.
     */
    private static void stripNonCyclicSharedPtr(CodegenProperty var) {
        if (var.dataType != null && var.dataType.startsWith("std::shared_ptr<")
                && !var.isCircularReference) {
            String innerType = var.dataType.substring(16, var.dataType.length() - 1);
            var.dataType = innerType;
            var.defaultValue = null;
        }
    }

    @Override
    public ModelsMap postProcessModels(ModelsMap objs) {
        // Clear parent for non-inheriting array/map models (inherited from AbstractCppCodegen)
        for (ModelMap mo : objs.getModels()) {
            CodegenModel cm = mo.getModel();
            if ((cm.isArray || cm.isMap) && (cm.parentModel == null)) {
                cm.parent = null;
            }
        }

        ModelsMap result = postProcessModelsEnum(objs);

        // Phase 1: Apply type lowering to oneOf/anyOf models
        for (ModelMap mo : result.getModels()) {
            processComposedModel(mo.getModel());
        }

        // Phase 2: Tag models with alias/variant flags for template dispatch.
        // Mustache templates use these flags to choose between emitting a using
        // alias (with to_json/from_json overloads for variants) vs. the existing
        // object model class template (with properties).
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            if (cm.vendorExtensions.containsKey("x-cpp-type")) {
                cm.vendorExtensions.put("x-cpp-is-alias", true);
                String resolvedType = (String) cm.vendorExtensions.get("x-cpp-type");
                // Resolve non-std:: types through the alias chain to detect
                // models that alias to a variant (e.g., ParentServerEvent →
                // StreamEventUnion → std::variant<...>).
                String ultimateType = resolveThroughAliases(resolvedType);
                if (ultimateType != null && ultimateType.startsWith("std::variant<")) {
                    cm.vendorExtensions.put("x-cpp-is-variant", true);
                    cm.vendorExtensions.putIfAbsent("x-cpp-composed-keyword", "oneOf");
                }
            } else if (cm.parent != null && !cm.parent.isEmpty()
                    && resolvedAliasTypes.containsKey(cm.parent)) {
                // (e.g., ParentServerEvent : public StreamEventUnion) but where
                // the parent is a resolved variant/alias. Since inheritance from a
                // variant alias is invalid C++, treat this model as an alias too.
                // Example: ParentServerEvent has anyOf: [StreamEventUnion] where
                // StreamEventUnion = std::variant<...>.
                String parentAlias = cm.parent;
                cm.vendorExtensions.put("x-cpp-type", parentAlias);
                cm.vendorExtensions.put("x-cpp-is-alias", true);
                cm.dataType = parentAlias;
                resolvedAliasTypes.put(cm.classname, parentAlias);
                String parentResolvedType = resolvedAliasTypes.get(parentAlias);
                if (parentResolvedType != null && parentResolvedType.startsWith("std::variant<")) {
                    cm.vendorExtensions.put("x-cpp-is-variant", true);
                    // Non-variant alias source template (Path B) only generates
                    // stubs. For variant aliases (Path A), we need the composed
                    // keyword to generate fromJsonValue_/toJsonValue_ functions.
                    // Default to oneOf (conservative: exactly-one enforcement).
                    cm.vendorExtensions.putIfAbsent("x-cpp-composed-keyword", "oneOf");
                }
            }
        }

        // Fallback: Detect models whose composedSchemas were consumed by fromModel
        // before processComposedModel had a chance to run. This happens when the
        // default codegen pipeline collapses a bare oneOf/anyOf (without type:object)
        // into a flat dataType. These models have no vars and a dataType that differs
        // from their classname (e.g., SingleBranchTest → std::string).
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            if (cm.vendorExtensions.containsKey("x-cpp-is-alias")) {
                continue;
            }
            if (cm.vars != null && !cm.vars.isEmpty()) {
                continue;
            }
            if (cm.isArray || cm.isMap) {
                continue;
            }
            if (cm.dataType != null
                    && !cm.dataType.equals(cm.classname)
                    && (cm.dataType.startsWith("std::") || "boost::json::value".equals(cm.dataType)
                            || resolvedAliasTypes.containsKey(cm.dataType))) {
                cm.vendorExtensions.put("x-cpp-type", cm.dataType);
                cm.vendorExtensions.put("x-cpp-is-alias", true);
                resolvedAliasTypes.put(cm.classname, cm.dataType);
                if (cm.dataType.startsWith("std::variant<")) {
                    cm.vendorExtensions.put("x-cpp-is-variant", true);
                }
                // Determine composed keyword from the CodegenModel's anyOf/oneOf sets
                // for fallback paths that bypassed processComposedModel. For variant
                // types, oneOf is the conservative default (enables exactly-one checking
                // in fromJsonValue).
                String fallbackKeyword = null;
                if (cm.oneOf != null && !cm.oneOf.isEmpty()) {
                    fallbackKeyword = "oneOf";
                } else if (cm.anyOf != null && !cm.anyOf.isEmpty()) {
                    fallbackKeyword = "anyOf";
                }
                if (fallbackKeyword == null) {
                    fallbackKeyword = "oneOf";
                }
                cm.vendorExtensions.put("x-cpp-composed-keyword", fallbackKeyword);
                composedKeywordsByModel.put(cm.classname, fallbackKeyword);
            }
        }

        // Degenerate fallback: Models like AllNullTest whose composed schemas
        // (anyOf [null, null]) were entirely consumed by the default codegen
        // without leaving usable branches or dataType. These models have no vars,
        // are not arrays/maps, and have `isAnyType = true` (no explicit `type` field
        // on the OpenAPI schema). Treat as boost::json::value alias.
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            if (cm.vendorExtensions.containsKey("x-cpp-is-alias")) {
                continue;
            }
            if (cm.vars != null && !cm.vars.isEmpty()) {
                continue;
            }
            if (cm.isArray || cm.isMap) {
                continue;
            }
            if (cm.getIsAnyType()) {
                cm.vendorExtensions.put("x-cpp-type", "boost::json::value");
                resolvedAliasTypes.put(cm.classname, "boost::json::value");
                cm.vendorExtensions.put("x-cpp-is-alias", true);
                // Even for boost::json::value fallbacks, set the keyword so
                // template code referencing vendorExtensions.x-cpp-composed-keyword
                // does not encounter an undefined variable.
                cm.vendorExtensions.put("x-cpp-composed-keyword", "oneOf");
                composedKeywordsByModel.put(cm.classname, "oneOf");
            }
        }

        // Phase 3b: Tag properties whose types already embed optional semantics
        // (e.g., std::optional<T>) so the template skips the redundant IsSet flag.
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            for (CodegenProperty var : allVarsOf(cm)) {
                if (var.dataType != null && var.dataType.startsWith("std::optional<")) {
                    var.vendorExtensions.put("x-cpp-no-is-set", true);
                }
            }
        }

        // Phase 3c: Tag properties is deferred to postProcessAllModels (which runs
        // once with the full model map) because postProcessModels is called per-model,
        // so a cross-model lookup of variant aliases is not possible here.

        // Phase 4: Emit complete includes for resolved alias/variant types.
        // Scan x-cpp-type and x-cpp-branches for known standard types and add
        // corresponding #include directives to the model's imports.
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            if (!cm.vendorExtensions.containsKey("x-cpp-is-alias")) {
                continue;
            }
            String resolvedType = (String) cm.vendorExtensions.get("x-cpp-type");
            List<String> branchTypes = (List<String>) cm.vendorExtensions.get("x-cpp-branches");
            collectImportsForType(resolvedType, cm);
            if (branchTypes != null) {
                for (String branchType : branchTypes) {
                    collectImportsForType(branchType, cm);
                }
            }
            // Remove self-includes that were added by the branch/type scan.
            // A variant like std::variant<std::string, TracingConfiguration> referencing
            // itself as a branch causes the model to include its own header.
            cm.imports.removeIf(imp -> imp.equals("#include \"" + cm.classname + ".h\""));
        }

        return result;
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        Map<String, ModelsMap> processed = super.postProcessAllModels(objs);
        // Build model index for enum lookup in Phase 1b.
        Map<String, CodegenModel> allModels = getAllModels(processed);

        // Phase 1b (global): Transitive resolution for model-reference branches.
        // Runs once with ALL models available (unlike Phase 1b in postProcessModels
        // which is per-batch and cannot see models processed in other batches).
        // Resolves $ref chains like ModelIdsResponses → ModelIdsShared → std::string.
        // Multiple passes needed for deep chains (A→B→C→string).
        boolean typeChanged = true;
        int phase1bPass = 0;
        while (typeChanged && phase1bPass < 10) {
            typeChanged = false;
            phase1bPass++;
            for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
                for (ModelMap mo : entry.getValue().getModels()) {
                    CodegenModel cm = mo.getModel();
                    if (!cm.vendorExtensions.containsKey("x-cpp-type")) {
                        continue;
                    }
                    String composedKeyword = (String) cm.vendorExtensions.get("x-cpp-composed-keyword");
                    if (composedKeyword == null) {
                        continue;
                    }
                    List<String> branchTypes = (List<String>) cm.vendorExtensions.get("x-cpp-branches");
                    if (branchTypes == null) {
                        continue;
                    }
                    List<String> resolved = branchTypes.stream()
                            .map(this::resolveThroughAliases)
                            .collect(Collectors.toList());
                    if (resolved.equals(branchTypes)) {
                        continue;
                    }
                    String currentType = (String) cm.vendorExtensions.get("x-cpp-type");
                    String newType;
                    try {
                        // Reconstruct ComposedBranch objects using resolved C++ type
                        // strings and per-branch isEnum metadata.  Without isEnum, a
                        // oneOf [open-string, string-enum] whose branches resolve to
                        // ["std::string", "std::string"] through the alias chain would
                        // collapse to plain std::string (Rule 7), losing the oneOf overlap
                        // detection (Rule 6) that correctly type-erases to boost::json::value.
                        //
                        // Branch isEnum comes from two sources:
                        //   1. For branches whose original type is a model name (not a C++
                        //      type string), look up the CodegenModel to check isEnum.
                        //   2. Fall back to stored x-cpp-branch-is-enum metadata from the
                        //      first lowering pass (handles inline enum schemas where the
                        //      CodegenProperty.isEnum flag was set directly).
                        @SuppressWarnings("unchecked")
                        List<Boolean> storedIsEnum = (List<Boolean>) cm.vendorExtensions.get("x-cpp-branch-is-enum");
                        List<ComposedBranch> branchesWithMeta = new ArrayList<>();
                        for (int i = 0; i < resolved.size(); i++) {
                            boolean isEnum = false;
                            if ("std::string".equals(resolved.get(i))) {
                                // Source 1: Look up the original branch model for enum status.
                                String originalType = branchTypes.get(i);
                                CodegenModel branchModel = allModels.get(originalType);
                                isEnum = branchModel != null && branchModel.isEnum;
                                // Source 2: Fall back to stored metadata from first pass.
                                if (!isEnum && storedIsEnum != null && i < storedIsEnum.size()) {
                                    isEnum = storedIsEnum.get(i);
                                }
                            }
                            boolean isStringLike = "std::string".equals(resolved.get(i));
                            branchesWithMeta.add(new ComposedBranch(resolved.get(i), isEnum, isStringLike));
                        }
                        newType = lowerComposedTypes(branchesWithMeta, composedKeyword);
                    } catch (RuntimeException e) {
                        LOGGER.warn("Failed to re-lower composed types for '{}': {} — keeping current type '{}'",
                                cm.classname, e.getMessage(), currentType);
                        continue;
                    }
                    if (!newType.equals(currentType)) {
                        cm.vendorExtensions.put("x-cpp-type", newType);
                        // Keep original x-cpp-branches for import resolution.
                        cm.dataType = newType;
                        resolvedAliasTypes.put(cm.classname, newType);
                        // Refresh discriminator resolved type — Phase 4b uses this
                        // to filter self-referential mappings and it must reflect
                        // the final post-collapse type, not the pre-collapse value
                        // cached during Phase 1a (updateAllModels).
                        if (cm.discriminator != null) {
                            cm.vendorExtensions.put("x-discriminator-resolved-type", newType);
                        }
                        typeChanged = true;
                    }
                }
            }
        }

        // Refresh alias/variant flags after Phase 1b resolution. Phase 2 in
        // postProcessModels may have set x-cpp-is-variant = true for models whose
        // types were later collapsed to plain types (e.g., std::string) by Phase 1b.
        // Use transitive alias resolution so models aliased to a variant type
        // (e.g., ParentServerEvent → StreamEventUnion → std::variant<...>)
        // also get the variant flag.
        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            for (ModelMap mo : entry.getValue().getModels()) {
                CodegenModel cm = mo.getModel();
                if (cm.vendorExtensions.containsKey("x-cpp-is-alias")) {
                    String resolvedType = (String) cm.vendorExtensions.get("x-cpp-type");
                    String ultimateType = resolveThroughAliases(resolvedType);
                    if (ultimateType != null && ultimateType.startsWith("std::variant<")) {
                        cm.vendorExtensions.put("x-cpp-is-variant", true);
                        cm.vendorExtensions.putIfAbsent("x-cpp-composed-keyword", "oneOf");
                    } else {
                        cm.vendorExtensions.remove("x-cpp-is-variant");
                    }
                }
            }
        }

        // Type-erased oneOf aliases still need to validate the original branch
        // constraints before accepting the JSON value.
        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            for (ModelMap modelMap : entry.getValue().getModels()) {
                CodegenModel codegenModel = modelMap.getModel();
                if ("oneOf".equals(codegenModel.vendorExtensions.get("x-cpp-composed-keyword"))
                        && "boost::json::value".equals(codegenModel.vendorExtensions.get("x-cpp-type"))
                        && codegenModel.getComposedSchemas() != null
                        && codegenModel.getComposedSchemas().getOneOf() != null
                        && !codegenModel.getComposedSchemas().getOneOf().isEmpty()) {
                    codegenModel.vendorExtensions.put(
                            "x-cpp-type-erased-oneof-branches",
                            buildTypeErasedOneOfBranches(codegenModel, allModels));
                    codegenModel.vendorExtensions.put("x-cpp-type-erased-oneof", true);
                }
            }
        }

        // Phase 4b: Filter discriminator mappings to remove self-referential entries.
        // After Phase 1b, all resolvedAliasTypes are final. A discriminator mapping
        // like "ParentServerEvent" → ParentServerEvent where ParentServerEvent
        // resolves to the same type as the current model (e.g., StreamEventUnion =
        // std::variant<...>) would cause compile errors (constructing variant from self)
        // and infinite recursion in fromJsonValue.
        // The template uses discriminator.mappedModels (built-in CodegenModel field),
        // NOT x-discriminator-mapping. We modify the actual CodegenModel discriminator.
        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            for (ModelMap mo : entry.getValue().getModels()) {
                CodegenModel cm = mo.getModel();
                if (cm.discriminator == null) continue;
                String resolvedType = (String) cm.vendorExtensions.get("x-discriminator-resolved-type");
                if (resolvedType == null) continue;
                Set<CodegenDiscriminator.MappedModel> mappedModels = cm.discriminator.getMappedModels();
                if (mappedModels == null || mappedModels.isEmpty()) continue;
                Set<CodegenDiscriminator.MappedModel> filtered = new TreeSet<>();
                for (CodegenDiscriminator.MappedModel mm : mappedModels) {
                    if (mm.getModelName() != null) {
                        String resolvedTarget = resolveThroughAliases(mm.getModelName());
                        if (resolvedTarget.equals(resolvedType)) {
                            continue; // skip self-referential mapping
                        }
                    }
                    CodegenDiscriminator.MappedModel escapedMapping =
                            new CodegenDiscriminator.MappedModel(
                                    escapeCppStringContent(mm.getMappingName()),
                                    mm.getModelName(),
                                    mm.getSchemaName(),
                                    mm.isExplicitMapping());
                    escapedMapping.setModel(mm.getModel());
                    filtered.add(escapedMapping);
                }
                cm.discriminator.setMappedModels(filtered);
            }
        }

        // Phase 5: Tag properties referencing a variant alias model so the template
        // dispatches via fromJsonValue_/toJsonValue_ free functions (which respect the
        // composed keyword — oneOf vs anyOf semantics) instead of the generic
        // JsonValueConverter<std::variant<Ts...>> (which always enforces exactly-one
        // oneOf semantics, even for anyOf properties).
        //
        // This must run in postProcessAllModels (not postProcessModels) because the
        // latter is called per-model, not globally. We need access to all models to
        // look up whether a property's dataType refers to a variant alias model.
        //
        // Only true std::variant aliases (x-cpp-is-variant = true) have the
        // toJsonValue_/fromJsonValue_ free functions. Non-variant aliases (e.g.,
        // ModelIdsResponses = std::string) must NOT be tagged.
        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            for (ModelMap mo : entry.getValue().getModels()) {
                CodegenModel cm = mo.getModel();
                for (CodegenProperty var : allVarsOf(cm)) {
                    if (var.dataType != null) {
                        // Look up the model that this property's dataType refers to
                        ModelsMap targetEntry = processed.get(var.dataType);
                        if (targetEntry != null) {
                            for (ModelMap targetMo : targetEntry.getModels()) {
                                CodegenModel targetModel = targetMo.getModel();
                                if (Boolean.TRUE.equals(targetModel.vendorExtensions.get("x-cpp-is-variant"))) {
                                    var.vendorExtensions.put("x-cpp-variant-alias", true);
                                    var.vendorExtensions.put("x-cpp-variant-alias-name", var.dataType);
                                }
                            }
                        }
                    }
                }
            }
        }

        // Phase 6: Add includes for discriminator-mapped models to variant
        // alias headers/sources. The discriminator dispatch in the variant alias
        // template calls fromJsonValue_{{modelName}}(value) or toJsonValue_{{modelName}}.
        // Without the include, the compiler sees an undeclared identifier.
        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            for (ModelMap mo : entry.getValue().getModels()) {
                CodegenModel cm = mo.getModel();
                @SuppressWarnings("unchecked")
                Map<String, String> mapping = (Map<String, String>)
                        cm.vendorExtensions.get("x-discriminator-mapping");
                if (mapping == null) continue;
                for (String modelName : mapping.values()) {
                    if (modelName != null) {
                        collectImportsForType(modelName, cm);
                    }
                }
            }
        }

        return processed;
    }

    /**
     * Scans a type string for known standard types and adds corresponding
     * #include directives to the model's import set. Types that look like
     * model names (start with an uppercase letter and are not otherwise
     * mapped) are resolved via toModelImport.
     */
    private void collectImportsForType(String type, CodegenModel cm) {
        if (type == null) {
            return;
        }
        boolean matchedImportMapping = false;
        for (Map.Entry<String, String> entry : importMapping.entrySet()) {
            String mappedKey = entry.getKey();
            String mappedInclude = entry.getValue();
            if (type.contains(mappedKey)) {
                cm.imports.add(mappedInclude);
                if (type.equals(mappedKey) || type.startsWith(mappedKey + "<")) {
                    matchedImportMapping = true;
                }
            }
        }
        // If the type was not matched by importMapping and looks like a model
        // name (starts with uppercase), treat it as a model include.
            if (!matchedImportMapping && !type.isEmpty() && Character.isUpperCase(type.charAt(0))) {
            String modelInclude = toModelImport(type);
            if (modelInclude != null && !modelInclude.isEmpty()) {
                cm.imports.add(modelInclude);
            }
        }
    }

    /**
     * Maps OpenAPI type names (from composed branch properties) to C++ types.
     * Composed properties created by DefaultCodegen.fromProperty use OpenAPI
     * type names (e.g., "null", "integer", "string") rather than mapped C++ types.
     */

    private String resolveOpenApiTypeName(String type) {
        if (type == null) {
            return null;
        }
        // Check typeMapping first for known OpenAPI type names
        if ("null".equals(type)) {
            return "std::nullptr_t";
        }
        // Check if it's already a C++ type (starts with std:: or boost:: or is a model name)
        if (type.startsWith("std::") || type.startsWith("boost::") || type.contains("<")) {
            return type;
        }
        // Map through typeMapping for OpenAPI primitive type names
        String mapped = typeMapping.get(type);
        if (mapped != null) {
            return mapped;
        }
        // If it has underscores or uppercase letters, assume it's already a model name
        return type;
    }

    /**
     * Applies the ordered type lowering rules to a composed (oneOf/anyOf) model.
     * Sets vendor extensions consumed by templates and records the model as a variant type.
     *
     * NOTE: When a schema uses <b>both</b> allOf and oneOf/anyOf at the same root level,
     * the allOf branches are merged into properties while the oneOf/anyOf branches are
     * lowered to variant types. This can produce a model with both concrete properties
     * AND a variant type, which may generate conflicting C++ declarations. Avoid such
     * mixed-schema patterns; prefer separate allOf-only or oneOf-only schemas.
     */
    private void processComposedModel(CodegenModel cm) {
        if (cm.getComposedSchemas() == null) {
            return;
        }

        List<CodegenProperty> branches = null;
        String composedKeyword = null;

        if (cm.getComposedSchemas().getOneOf() != null && !cm.getComposedSchemas().getOneOf().isEmpty()) {
            branches = cm.getComposedSchemas().getOneOf();
            composedKeyword = "oneOf";
        } else if (cm.getComposedSchemas().getAnyOf() != null && !cm.getComposedSchemas().getAnyOf().isEmpty()) {
            branches = cm.getComposedSchemas().getAnyOf();
            composedKeyword = "anyOf";
        }

        if (branches == null) {
            return;
        }

            // Collect C++ branch types (strip shared_ptr wrappers for variant members).
            // Map OpenAPI type names (e.g., "null", "integer", "string") to C++ types
            // because composed properties from fromProperty use OpenAPI type names as-is.
            // Self-referencing branches (a variant containing itself) are excluded
            // because they would create an illegal recursive type alias in C++.
            // Binary branches (format: binary) are mapped to std::vector<std::uint8_t>
            // so the multipart addVariantFormParameter helper can dispatch them as
            // file parts via compile-time type checking.
            // NOTE: Deduplication is deferred to lowerComposedTypes (step 5) so that
            // oneOf semantics can be preserved when duplicate types would otherwise
            // cause silent single-branch collapse.
            List<ComposedBranch> composedBranches = new ArrayList<>();
            for (CodegenProperty b : branches) {
                String cppType;
                if (b.isBinary || b.isFile) {
                    cppType = "std::vector<std::uint8_t>";
                } else {
                    cppType = resolveOpenApiTypeName(stripSharedPtr(b.dataType));
                }
                if (cppType.equals(cm.classname)) {
                    continue;
                }
                boolean isStringLike = b.isString || "std::string".equals(cppType)
                        || "string".equals(b.dataType);
                composedBranches.add(new ComposedBranch(cppType, b.isEnum, isStringLike));
            }
            List<String> branchTypes = composedBranches.stream()
                    .map(cb -> cb.cppType)
                    .collect(Collectors.toList());

            String resolvedType;
            try {
                resolvedType = lowerComposedTypes(composedBranches, composedKeyword);
            } catch (RuntimeException e) {
                // Fallback: if lowering fails (e.g., indistinguishable oneOf branches),
                // treat as boost::json::value and log the warning rather than crashing
                // the entire generation pipeline.
                LOGGER.warn("Failed to lower composed types for '{}': {} — falling back to boost::json::value",
                        cm.classname, e.getMessage());
                resolvedType = "boost::json::value";
            }

        // Cache the resolved type for transitive resolution in Phase 1b
        resolvedAliasTypes.put(cm.classname, resolvedType);

        // Record as variant model for getTypeDeclaration shared_ptr exclusion
        variantModels.add(cm.classname);

        // Emit vendor extensions consumed by Mustache templates
        cm.vendorExtensions.put("x-cpp-type", resolvedType);
        cm.vendorExtensions.put("x-cpp-branches", branchTypes);
        cm.vendorExtensions.put("x-cpp-composed-keyword", composedKeyword);
        composedKeywordsByModel.put(cm.classname, composedKeyword);

        // Store per-branch isEnum metadata for Phase 1b re-lowering.
        // Phase 1b resolves model-name branch types through aliases to
        // C++ type strings but the isEnum flag (used by Rule 6 for oneOf
        // open-string + string-enum overlap detection) is not derivable
        // from C++ type strings alone — both open strings and string enums
        // produce "std::string".
        List<Boolean> branchIsEnumFlags = composedBranches.stream()
                .map(cb -> cb.isEnum)
                .collect(Collectors.toList());
        cm.vendorExtensions.put("x-cpp-branch-is-enum", branchIsEnumFlags);

        if (cm.discriminator != null) {
            cm.vendorExtensions.put("x-has-discriminator", true);
            cm.vendorExtensions.put("x-discriminator-property", cm.discriminator.getPropertyBaseName());
            cm.vendorExtensions.put("x-discriminator-mapping", cm.discriminator.getMapping());
            // Self-referential discriminator entries are filtered in Phase 1b
            // (postProcessAllModels) after all resolvedAliasTypes are populated.
            // We store the resolved type so Phase 1b can check for self-refs.
            cm.vendorExtensions.put("x-discriminator-resolved-type", resolvedType);
        }

        // Update data type so templates and references use the resolved type
        cm.dataType = resolvedType;
    }

    /** Branch metadata used by ordered composition lowering. */
    private static final class ComposedBranch {
        final String cppType;
        final boolean isEnum;
        final boolean isStringLike;

        ComposedBranch(String cppType, boolean isEnum, boolean isStringLike) {
            this.cppType = cppType;
            this.isEnum = isEnum;
            this.isStringLike = isStringLike;
        }
    }

    private List<Map<String, Object>> buildTypeErasedOneOfBranches(
            CodegenModel codegenModel, Map<String, CodegenModel> allModels) {
        List<Map<String, Object>> validationBranches = new ArrayList<>();
        for (CodegenProperty branch : codegenModel.getComposedSchemas().getOneOf()) {
            String originalType = stripSharedPtr(branch.dataType);
            CodegenModel referencedModel = allModels.get(originalType);
            String resolvedType = resolveThroughAliases(originalType);
            if (referencedModel != null && referencedModel.dataType != null) {
                resolvedType = resolveThroughAliases(stripSharedPtr(referencedModel.dataType));
            }
            resolvedType = resolveOpenApiTypeName(resolvedType);

            Map<String, Object> validationBranch = new LinkedHashMap<>();
            if ("std::string".equals(resolvedType)) {
                validationBranch.put("is-string", true);
                List<Object> enumValues = getEnumValues(branch, referencedModel);
                if (!enumValues.isEmpty()) {
                    validationBranch.put("has-enum-values", true);
                    List<Map<String, String>> escapedValues = new ArrayList<>();
                    for (Object enumValue : enumValues) {
                        escapedValues.add(Collections.singletonMap(
                                "literal", escapeCppStringContent(String.valueOf(enumValue))));
                    }
                    validationBranch.put("enum-values", escapedValues);
                }
            } else if ("bool".equals(resolvedType)) {
                validationBranch.put("is-boolean", true);
            } else if ("std::int32_t".equals(resolvedType) || "int32_t".equals(resolvedType)) {
                validationBranch.put("is-int32", true);
            } else if ("std::int64_t".equals(resolvedType) || "int64_t".equals(resolvedType)) {
                validationBranch.put("is-integer", true);
            } else if ("double".equals(resolvedType) || "float".equals(resolvedType)) {
                validationBranch.put("is-number", true);
            } else if ("std::nullptr_t".equals(resolvedType)) {
                validationBranch.put("is-null", true);
            } else if (resolvedType != null && resolvedType.startsWith("std::vector<")) {
                validationBranch.put("is-array", true);
            } else if (resolvedType != null
                    && (resolvedType.startsWith("std::map<")
                    || (!resolvedType.startsWith("std::")
                    && !resolvedType.startsWith("boost::")))) {
                validationBranch.put("is-object", true);
            } else {
                validationBranch.put("is-any", true);
            }
            validationBranches.add(validationBranch);
        }
        return validationBranches;
    }

    @SuppressWarnings("unchecked")
    private static List<Object> getEnumValues(
            CodegenProperty branch, CodegenModel referencedModel) {
        Map<String, Object> allowableValues = branch.allowableValues;
        if ((allowableValues == null || allowableValues.get("values") == null)
                && referencedModel != null) {
            allowableValues = referencedModel.allowableValues;
        }
        if (allowableValues == null || !(allowableValues.get("values") instanceof List)) {
            return Collections.emptyList();
        }
        return (List<Object>) allowableValues.get("values");
    }

    private static String escapeCppStringContent(String value) {
        if (value == null) {
            return "";
        }
        StringBuilder escaped = new StringBuilder(value.length());
        for (int index = 0; index < value.length(); index++) {
            char character = value.charAt(index);
            switch (character) {
                case '\\':
                    escaped.append("\\\\");
                    break;
                case '"':
                    escaped.append("\\\"");
                    break;
                case '\n':
                    escaped.append("\\n");
                    break;
                case '\r':
                    escaped.append("\\r");
                    break;
                case '\t':
                    escaped.append("\\t");
                    break;
                case '\b':
                    escaped.append("\\b");
                    break;
                case '\f':
                    escaped.append("\\f");
                    break;
                default:
                    if (character < 0x20 || character == 0x7f) {
                        escaped.append(String.format(Locale.ROOT, "\\%03o", (int) character));
                    } else {
                        escaped.append(character);
                    }
                    break;
            }
        }
        return escaped.toString();
    }

    /**
     * Ordered lowering rules for composed types (OAS-first):
     * 1. anyOf/oneOf: [T, null] → std::optional&lt;T&gt;
     * 2. anyOf only: all strings/string-enums → std::string
     * 3. Remove null branches
     * 4. Single non-null branch → that branch's type
     * 5. Deduplicate identical branch types
     * 6. oneOf open-string + string-enum (type-erased) → boost::json::value
     *    (do not pretend exclusivity after both erase to std::string)
     * 7. oneOf multi-branch → single identical C++ type (alias collapse) → that type
     * 8. Emit std::variant&lt;Branches...&gt; or boost::json::value
     */
    private String lowerComposedTypes(List<ComposedBranch> branches, String composedKeyword) {
        if (branches == null || branches.isEmpty()) {
            return "boost::json::value";
        }
        List<String> branchTypes = branches.stream()
                .map(b -> b.cppType)
                .collect(Collectors.toList());

        // Rule 1: anyOf/oneOf: [T, null] → std::optional<T>
        int nullCount = (int) branchTypes.stream().filter("std::nullptr_t"::equals).count();
        if (nullCount == 1 && branchTypes.size() == 2) {
            String nonNullBranch = branchTypes.stream()
                    .filter(bt -> !"std::nullptr_t".equals(bt))
                    .findFirst().orElse(null);
            if (nonNullBranch != null) {
                return "std::optional<" + nonNullBranch + ">";
            }
        }

        // Rule 2: anyOf-only collapse of all-string (including string-enum) to std::string.
        // Do NOT apply this collapse to oneOf — exclusive semantics differ.
        if ("anyOf".equals(composedKeyword) && branchTypes.stream().allMatch("std::string"::equals)) {
            return "std::string";
        }

        // Rule 3: Remove null branches for further processing.
        List<ComposedBranch> nonNullMeta = branches.stream()
                .filter(b -> !"std::nullptr_t".equals(b.cppType))
                .collect(Collectors.toList());
        List<String> nonNullBranches = nonNullMeta.stream()
                .map(b -> b.cppType)
                .collect(Collectors.toList());

        // Rule 3b: Flatten nested variants
        List<String> flattened = new ArrayList<>();
        for (String bt : nonNullBranches) {
            if (bt.startsWith("std::variant<") && bt.endsWith(">")) {
                String inner = bt.substring(13, bt.length() - 1);
                int depth = 0;
                int start = 0;
                for (int i = 0; i < inner.length(); i++) {
                    char c = inner.charAt(i);
                    if (c == '<') depth++;
                    else if (c == '>') depth--;
                    else if (c == ',' && depth == 0) {
                        flattened.add(inner.substring(start, i).trim());
                        start = i + 1;
                    }
                }
                if (start < inner.length()) {
                    flattened.add(inner.substring(start).trim());
                }
            } else {
                flattened.add(bt);
            }
        }

        // Rule 4: All-null or empty → boost::json::value
        if (flattened.isEmpty()) {
            return "boost::json::value";
        }

        // Rule 5: Deduplicate identical branch types.
        List<String> deduped = flattened.stream()
                .distinct()
                .collect(Collectors.toList());

        // Rule 6: oneOf string branches that lose exclusivity after type lowering.
        // Branches [open-string, string-enum] or [string-enum-A, string-enum-B] all
        // collapse to std::string after type lowering, so every string value matches
        // every original string-like branch. Under JSON Schema oneOf, this means
        // values matching multiple original branches cannot be detected (count is
        // artificially 1 instead of 2+), causing false acceptance of invalid oneOf
        // inputs. Type-erase to boost::json::value when multiple string-like branches
        // collapse and at least one has enum constraints (the constraint is the only
        // thing that distinguishes otherwise-identical branches). anyOf keeps the
        // string collapse (rule 2) since first-match is correct behavior.
        if ("oneOf".equals(composedKeyword) && nonNullMeta.size() > 1) {
            long preDedupStringCount = nonNullMeta.stream()
                    .filter(b -> b.isStringLike)
                    .count();
            long postDedupStringCount = deduped.stream()
                    .filter("std::string"::equals)
                    .count();
            boolean hasStringEnum = nonNullMeta.stream()
                    .anyMatch(b -> b.isStringLike && b.isEnum);
            if (preDedupStringCount > postDedupStringCount && hasStringEnum) {
                LOGGER.warn(
                        "oneOf string branches erase to std::string; "
                                + "emitting boost::json::value to avoid false exclusive-union fidelity");
                return "boost::json::value";
            }
        }

        // Rule 7: Single branch after dedup (including oneOf alias chains that
        // resolve to the same underlying C++ type without enum/open-string mix).
        if (deduped.size() == 1) {
            return deduped.get(0);
        }

        // Rule 8: Emit std::variant<Branches...>
        List<String> variantBranches = new ArrayList<>(deduped);
        boolean hasNull = branchTypes.stream().anyMatch("std::nullptr_t"::equals);
        if (hasNull && deduped.size() > 1) {
            variantBranches.add("std::nullptr_t");
        }
        return "std::variant<" + String.join(", ", variantBranches) + ">";
    }

    /** Convenience overload for callers that only have C++ type strings. */
    private String lowerComposedTypesFromCppTypes(List<String> branchTypes, String composedKeyword) {
        List<ComposedBranch> branches = new ArrayList<>();
        for (String t : branchTypes) {
            boolean isString = "std::string".equals(t);
            branches.add(new ComposedBranch(t, false, isString));
        }
        return lowerComposedTypes(branches, composedKeyword);
    }

    /**
     * Resolves a type name transitively through the resolvedAliasTypes map.
     * For example, if ModelIdsResponses → std::string and ModelIdsShared → std::string,
     * then resolveThroughAliases("ModelIdsResponses") returns "std::string".
     * <p>
     * Cycles are prevented via a visited set. Returns the typeName unmodified
     * when no alias resolution applies.
     */
    private String resolveThroughAliases(String typeName) {
        if (typeName == null) {
            return null;
        }
        Set<String> visited = new HashSet<>();
        String current = typeName;
        int maxDepth = 20;
        for (int depth = 0; depth < maxDepth; depth++) {
            String resolved = resolvedAliasTypes.get(current);
            if (resolved == null || resolved.equals(current)) {
                break;
            }
            if (!visited.add(current)) {
                break;  // cycle detected
            }
            current = resolved;
        }
        return current;
    }

    /**
     * Detects whether a schema is a null union (anyOf/oneOf with [T, null] or [null, T])
     * that should lower to std::optional&lt;T&gt;. Returns the lowered type string,
     * or null if the schema is not a simple null union.
     */
    private String detectNullUnion(Schema schema, String className) {
        // Use raw List and cast explicitly because Schema is unparameterized.
        List anyOfRaw = schema.getAnyOf();
        List oneOfRaw = schema.getOneOf();
        List<Schema> branches = null;
        if (anyOfRaw != null && !anyOfRaw.isEmpty()) {
            branches = anyOfRaw;
        } else if (oneOfRaw != null && !oneOfRaw.isEmpty()) {
            branches = oneOfRaw;
        }
        if (branches == null) {
            return null;
        }
        if (branches.size() != 2) {
            return null;
        }

        // Find the non-null branch using ModelUtils for correct null-type detection
        // (handles both OAS 3.0 nullable and OAS 3.1 type: "null")
        Schema nonNullBranch = null;
        for (Object brObj : branches) {
            Schema branch = (Schema) brObj;
            if (!ModelUtils.isNullType(branch)) {
                nonNullBranch = branch;
            }
        }
        if (nonNullBranch == null) {
            return null; // Both branches are null
        }
        // Verify exactly one null branch exists
        long nullBranchCount = 0;
        for (Object brObj : branches) {
            if (ModelUtils.isNullType((Schema) brObj)) nullBranchCount++;
        }
        if (nullBranchCount != 1) {
            return null;
        }

        // Resolve the non-null branch type. For $ref schemas, resolve to model name.
        String nonNullType;
        if (nonNullBranch.get$ref() != null) {
            nonNullType = ModelUtils.getSimpleRef(nonNullBranch.get$ref());
        } else {
            nonNullType = getTypeDeclaration(nonNullBranch);
        }

        // Avoid self-referencing optional (optional of the model itself)
        if (nonNullType.equals(className)) {
            return "boost::json::value";
        }

        return "std::optional<" + nonNullType + ">";
    }

    /**
     * Recursively strips {@code std::shared_ptr<X>} wrappers from a type string.
     * <ul>
     *   <li>{@code std::shared_ptr<Foo>} → {@code Foo}</li>
     *   <li>{@code std::vector<std::shared_ptr<Foo>>} → {@code std::vector<Foo>}</li>
     *   <li>{@code std::map<std::string, std::shared_ptr<Foo>>} → {@code std::map<std::string, Foo>}</li>
     *   <li>{@code std::string} → {@code std::string} (unchanged)</li>
     * </ul>
     */
    private static String stripSharedPtr(String type) {
        if (type == null) {
            return null;
        }
        // Direct std::shared_ptr<X> wrapper — extract inner type and recurse.
        if (type.startsWith("std::shared_ptr<") && type.endsWith(">")) {
            return stripSharedPtr(type.substring(16, type.length() - 1));
        }
        // Check for template arguments (contains '<' and '>').
        int firstLt = type.indexOf('<');
        int lastGt = type.lastIndexOf('>');
        if (firstLt > 0 && lastGt > firstLt) {
            // Split arguments at commas at depth 0 (not inside nested angle brackets).
            String prefix = type.substring(0, firstLt);
            String argsSection = type.substring(firstLt + 1, lastGt);
            List<String> args = splitTemplateArgs(argsSection);
            for (int i = 0; i < args.size(); i++) {
                args.set(i, stripSharedPtr(args.get(i).trim()));
            }
            return prefix + "<" + String.join(", ", args) + ">";
        }
        return type;
    }

    /**
     * Splits a comma-separated template argument list, respecting nested angle brackets.
     * {@code "std::string, std::shared_ptr<Foo>"} → {@code ["std::string", "std::shared_ptr<Foo>"]}
     */
    private static List<String> splitTemplateArgs(String args) {
        List<String> result = new ArrayList<>();
        int depth = 0;
        int start = 0;
        for (int i = 0; i < args.length(); i++) {
            char c = args.charAt(i);
            if (c == '<') {
                depth++;
            } else if (c == '>') {
                depth--;
            } else if (c == ',' && depth == 0) {
                result.add(args.substring(start, i));
                start = i + 1;
            }
        }
        result.add(args.substring(start));
        return result;
    }

    /**
     * Camelize the method name of the getter and setter, but keep underscores at the front
     *
     * @param name string to be camelized
     * @return Camelized string
     */
    @Override
    public String getterAndSetterCapitalize(String name) {
        if (name == null || name.length() == 0) {
            return name;
        }

        name = toVarName(name);

        if (name.startsWith("_")) {
            return "_" + camelize(name);
        }

        return camelize(name);
    }

    @Override
    public void processOpts() {
        super.processOpts();
        packageName = additionalProperties.getOrDefault(
                CodegenConstants.PACKAGE_NAME, DEFAULT_PACKAGE_NAME).toString();
        if (StringUtils.isBlank(packageName)) {
            throw new IllegalArgumentException("packageName must not be blank");
        }
        additionalProperties.put(CodegenConstants.PACKAGE_NAME, packageName);
        additionalProperties.put("modelNamespaceDeclarations", modelPackage.split("\\."));
        additionalProperties.put("modelNamespace", modelPackage.replaceAll("\\.", "::"));
        additionalProperties.put("apiNamespaceDeclarations", apiPackage.split("\\."));
        additionalProperties.put("apiNamespace", apiPackage.replaceAll("\\.", "::"));
    }

    /**
     * Location to write model files. You can use the modelPackage() as defined
     * when the class is instantiated
     */
    @Override
    public String modelFileFolder() {
        return (outputFolder + "/model").replace("/", File.separator);
    }

    /**
     * Location to write api files. You can use the apiPackage() as defined when
     * the class is instantiated
     */
    @Override
    public String apiFileFolder() {
        return (outputFolder + "/api").replace("/", File.separator);
    }

    @Override
    public String toModelImport(String name) {
        if (importMapping.containsKey(name)) {
            return importMapping.get(name);
        } else {
            return "#include \"" + name + ".h\"";
        }
    }

    @Override
    public CodegenModel fromModel(String name, Schema model) {
        // Pre-check: allOf scalar type conflicts — detect before the default
        // pipeline consumes the composed schemas.
        if (model != null && model.getAllOf() != null && !model.getAllOf().isEmpty()) {
            List<String> allOfPrimitiveTypes = new ArrayList<>();
            // Also track property-level conflicts: same property name with
            // incompatible types across allOf branches.
            Map<String, String> allOfPropertyTypes = new HashMap<>();
            for (Object allOfItem : model.getAllOf()) {
                if (allOfItem instanceof Schema) {
                    Schema itemSchema = (Schema) allOfItem;
                    String ref = itemSchema.get$ref();
                    String resolvedType = null;
                    Schema resolvedRef = null;
                    // For $ref schemas, resolve to the target's type
                    if (ref != null) {
                        resolvedRef = (openAPI != null)
                            ? ModelUtils.getReferencedSchema(openAPI, itemSchema)
                            : null;
                        if (resolvedRef != null) {
                            resolvedType = resolvedRef.getType();
                        }
                    } else {
                        resolvedType = itemSchema.getType();
                        resolvedRef = itemSchema;
                    }
                    if (resolvedType != null && !"object".equals(resolvedType)) {
                        allOfPrimitiveTypes.add(resolvedType);
                    }
                    // Check property-level conflicts: scan each allOf branch's
                    // properties for name collisions with incompatible types.
                    Schema propsSource = ref != null ? resolvedRef : itemSchema;
                    if (propsSource != null && propsSource.getProperties() != null) {
                        for (Object propEntryObj : propsSource.getProperties().entrySet()) {
                            Map.Entry<String, Schema> propEntry = (Map.Entry<String, Schema>) propEntryObj;
                            String propName = propEntry.getKey();
                            Schema propSchema = propEntry.getValue();
                            String propType = propSchema.getType();
                            if (propType == null) {
                                // For $ref properties, resolve the type
                                if (propSchema.get$ref() != null && openAPI != null) {
                                    Schema refProp = ModelUtils.getReferencedSchema(openAPI, propSchema);
                                    if (refProp != null) {
                                        propType = refProp.getType();
                                    }
                                }
                                if (propType == null) {
                                    propType = "unknown";
                                }
                            }
                            if (allOfPropertyTypes.containsKey(propName)) {
                                String existingType = allOfPropertyTypes.get(propName);
                                if (!existingType.equals(propType)
                                        && !"object".equals(propType)
                                        && !"object".equals(existingType)
                                        && !"unknown".equals(propType)
                                        && !"unknown".equals(existingType)) {
                                    throw new RuntimeException(
                                        "allOf property type conflict in schema '" + name
                                        + "': property '" + propName + "' has incompatible types ["
                                        + existingType + ", " + propType
                                        + "]. allOf with conflicting property types is not supported.");
                                }
                            } else {
                                allOfPropertyTypes.put(propName, propType);
                            }
                        }
                    }
                }
            }
            if (allOfPrimitiveTypes.size() >= 2) {
                String firstType = allOfPrimitiveTypes.get(0);
                for (int i = 1; i < allOfPrimitiveTypes.size(); i++) {
                    if (!allOfPrimitiveTypes.get(i).equals(firstType)) {
                        throw new RuntimeException(
                            "allOf type conflict in schema '" + name
                            + "': branches have incompatible types ["
                            + String.join(", ", allOfPrimitiveTypes)
                            + "]. allOf with scalar type conflicts is not supported.");
                    }
                }
            }
        }

        // Pre-check: The OpenAPI 3.1 parser converts anyOf [T, null] into
        // {type: T, nullable: true} or {$ref: X, nullable: true}, consuming
        // the anyOf list.  Detect these nullable schemas and produce the
        // correct std::optional<T> type.
        //
        // For $ref schemas (normalised anyOf/oneOf [T, null] where T was a
        // $ref), getTypeDeclaration resolves the target and returns the
        // correct C++ type.  For arrays, getTypeDeclaration returns the
        // container type (e.g. std::vector<...>) without optional wrapping,
        // so we wrap it here.  Inline object schemas (type=object, no $ref)
        // are full class models — they stay out of the alias precomputation
        // because getTypeDeclaration would return the raw OAS type name
        // "object" instead of the model name.  They are handled separately
        // below via variant model registration.
        boolean isNullableSchema = model != null
            && Boolean.TRUE.equals(model.getNullable())
            && (model.get$ref() != null
                || (model.getType() != null && !"object".equals(model.getType())));
        String preComputedNullUnionType = null;
        if (isNullableSchema) {
            // Resolve the type to its C++ type and wrap in std::optional
            String innerType = getTypeDeclaration(model);
            // getTypeDeclaration already returns std::optional<T> for nullable.
            // Use it directly if it starts with std::optional<.
            if (innerType.startsWith("std::optional<")) {
                preComputedNullUnionType = innerType;
            } else {
                preComputedNullUnionType = "std::optional<" + innerType + ">";
            }
        } else if (model != null) {
            // Also try the anyOf/oneOf path for cases where the parser
            // preserved the composed schema structure.
            preComputedNullUnionType = detectNullUnion(model, name);
        }

        CodegenModel codegenModel = super.fromModel(name, model);
        if (codegenModel == null) {
            return null;
        }

        // Post-check: Apply the pre-computed null union type if the default
        // pipeline consumed the composed schemas.
        if (preComputedNullUnionType != null) {
            codegenModel.dataType = preComputedNullUnionType;
            codegenModel.vendorExtensions.put("x-cpp-type", preComputedNullUnionType);
            codegenModel.vendorExtensions.put("x-cpp-composed-keyword",
                model.getAnyOf() != null ? "anyOf" : "oneOf");
            codegenModel.vendorExtensions.put("x-cpp-is-alias", true);
            codegenModel.vendorExtensions.put("x-cpp-is-optional", true);
            // Force a model header/source so Gate A inventory and $ref users get
            // `using NullableString = std::optional<std::string>;`. DefaultCodegen
            // marks plain nullable primitives as isAlias and skips file emission.
            codegenModel.isAlias = false;
            resolvedAliasTypes.put(name, preComputedNullUnionType);
            variantModels.add(name);
        }

        // Post-check: Inline nullable object schemas (type=object, nullable=true,
        // no $ref) are full class models with properties — they cannot use the
        // alias path. Register them as variant models so $ref references use value
        // semantics (std::shared_ptr<NullableObject> → NullableObject) and tag
        // the model as optional for correct null-value representation.
        if (model != null && model.get$ref() == null
                && "object".equals(model.getType())
                && Boolean.TRUE.equals(model.getNullable())) {
            variantModels.add(name);
            codegenModel.vendorExtensions.put("x-cpp-is-optional", true);
        }

        Set<String> oldImports = codegenModel.imports;
        codegenModel.imports = new HashSet<>();
        for (String imp : oldImports) {
            String newImp = toModelImport(imp);
            if (!newImp.isEmpty()) {
                codegenModel.imports.add(newImp);
            }
        }
        // Every model header declares vector conversion helpers.
        codegenModel.imports.add("#include <vector>");

        // Fixed-const properties: OAS 3.1 `const`, single-value `enum`, or optional
        // vendor extension `x-stainless-const`. Portable path is OAS `const` / single enum —
        // vendor extensions are never required for correct encode/decode.
        if (codegenModel.vars != null) {
            Map<String, Schema> allProps = new LinkedHashMap<>();
            if (model.getProperties() != null) {
                allProps.putAll(model.getProperties());
            }
            if (model.getAllOf() != null && openAPI != null) {
                for (Object parentObj : model.getAllOf()) {
                    if (parentObj instanceof Schema) {
                        Schema parentSchema = ModelUtils.getReferencedSchema(openAPI, (Schema) parentObj);
                        if (parentSchema != null && parentSchema.getProperties() != null) {
                            allProps.putAll(parentSchema.getProperties());
                        }
                    }
                }
            }
            for (CodegenProperty var : codegenModel.vars) {
                Object rawProp = allProps.get(var.baseName);
                if (!(rawProp instanceof Schema)) {
                    continue;
                }
                Schema varSchema = (Schema) rawProp;
                boolean hasOasConst = varSchema.getConst() != null;
                boolean hasSingleValueEnum = varSchema.getEnum() != null
                        && varSchema.getEnum().size() == 1;
                boolean hasStainlessConst = varSchema.getExtensions() != null
                        && Boolean.TRUE.equals(varSchema.getExtensions().get("x-stainless-const"));
                if (!(hasOasConst || hasSingleValueEnum || hasStainlessConst)) {
                    continue;
                }
                String constRawValue = null;
                if (varSchema.getConst() != null) {
                    constRawValue = varSchema.getConst().toString();
                } else if (varSchema.getEnum() != null && !varSchema.getEnum().isEmpty()) {
                    constRawValue = varSchema.getEnum().get(0).toString();
                }
                if (constRawValue == null && var.example != null) {
                    constRawValue = var.example;
                }
                if (constRawValue == null) {
                    constRawValue = "std::string".equals(var.dataType) ? "" : "0";
                }
                String inlineValue;
                boolean isStringConst = "std::string".equals(var.dataType)
                        || "std::optional<std::string>".equals(var.dataType)
                        || (var.isString && !var.isInteger && !var.isLong && !var.isNumber
                        && !var.isBoolean);
                if ("std::optional<std::string>".equals(var.dataType)) {
                    inlineValue = "std::optional<std::string>{\"" + constRawValue + "\"}";
                } else if (isStringConst || "std::string".equals(var.dataType)) {
                    inlineValue = "\"" + constRawValue + "\"";
                } else {
                    inlineValue = constRawValue;
                }
                // Neutral OAS-first flag used by templates.
                var.vendorExtensions.put("x-cpp-const", true);
                var.vendorExtensions.put("x-cpp-const-value", constRawValue);
                var.vendorExtensions.put("x-cpp-const-inline-value", inlineValue);
                // Mustache is truthy on key presence — only set when string-typed.
                if (isStringConst || "std::string".equals(var.dataType)
                        || "std::optional<std::string>".equals(var.dataType)) {
                    var.vendorExtensions.put("x-cpp-const-is-string", true);
                } else if (var.isBoolean || "bool".equals(var.dataType)
                        || "std::optional<bool>".equals(var.dataType)) {
                    var.vendorExtensions.put("x-cpp-const-is-boolean", true);
                }
                // Keep stainless keys as aliases so older template forks still work.
                var.vendorExtensions.put("x-stainless-const", true);
                var.vendorExtensions.put("x-stainless-const-value", constRawValue);
                var.vendorExtensions.put("x-stainless-const-inline-value", inlineValue);
            }
        }

        addContainerPropertyNames(codegenModel.vars);
        return codegenModel;
    }

    @Override
    public CodegenParameter fromParameter(Parameter parameter, Set<String> imports) {
        CodegenParameter codegenParameter = super.fromParameter(parameter, imports);
        if (!codegenParameter.isQueryParam) {
            return codegenParameter;
        }

        if (!codegenParameter.required) {
            codegenParameter.vendorExtensions.put(X_CODEGEN_IS_OPTIONAL_QUERY_PARAMETER, true);
        }
        if (!codegenParameter.isArray && !codegenParameter.isMap) {
            return codegenParameter;
        }

        // OAS 3 query parameters default to form/explode=true. DefaultCodegen
        // currently represents an omitted style as CSV, so normalize it here.
        boolean usesExplodedFormStyle = !Boolean.FALSE.equals(parameter.getExplode())
                && (parameter.getStyle() == null || parameter.getStyle() == Parameter.StyleEnum.FORM);
        if (codegenParameter.isMap) {
            if (parameter.getStyle() == Parameter.StyleEnum.DEEPOBJECT) {
                codegenParameter.vendorExtensions.put(X_CODEGEN_QUERY_MAP_DEEP_OBJECT, true);
            } else if (usesExplodedFormStyle) {
                codegenParameter.vendorExtensions.put(X_CODEGEN_QUERY_MAP_EXPLODED, true);
            } else {
                codegenParameter.vendorExtensions.put(
                        X_CODEGEN_QUERY_COLLECTION_DELIMITER,
                        queryCollectionDelimiter(parameter.getStyle()));
            }
            return codegenParameter;
        }

        boolean isMulti = codegenParameter.isCollectionFormatMulti || usesExplodedFormStyle;
        if (isMulti) {
            codegenParameter.isCollectionFormatMulti = true;
            codegenParameter.collectionFormat = "multi";
            codegenParameter.vendorExtensions.put(X_CODEGEN_QUERY_COLLECTION_MULTI, true);
            return codegenParameter;
        }

        String collectionDelimiter;
        switch (codegenParameter.collectionFormat) {
            case "csv":
                collectionDelimiter = ",";
                break;
            case "ssv":
                collectionDelimiter = "%20";
                break;
            case "tsv":
                collectionDelimiter = "%09";
                break;
            case "pipes":
                collectionDelimiter = "%7C";
                break;
            default:
                throw new IllegalArgumentException(
                        "Unsupported query collection format: " + codegenParameter.collectionFormat);
        }
        codegenParameter.vendorExtensions.put(
                X_CODEGEN_QUERY_COLLECTION_DELIMITER, collectionDelimiter);
        return codegenParameter;
    }

    private String queryCollectionDelimiter(Parameter.StyleEnum style) {
        if (style == Parameter.StyleEnum.SPACEDELIMITED) {
            return "%20";
        }
        if (style == Parameter.StyleEnum.PIPEDELIMITED) {
            return "%7C";
        }
        return ",";
    }

    private void addContainerPropertyNames(List<CodegenProperty> properties) {
        for (CodegenProperty property : properties) {
            CodegenProperty item = property.items;
            while (item != null) {
                item.vendorExtensions.put("x-container-property-name", property.name);
                item = item.items;
            }
        }
    }

    @Override
    public String toModelFilename(String name) {
        return toModelName(name);
    }

    @Override
    public String toApiFilename(String name) {
        return toApiName(name);
    }

    @SuppressWarnings("unchecked")
    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        Map<String, Object> operations = (Map<String, Object>) objs.get("operations");
        List<CodegenOperation> operationList = (List<CodegenOperation>) operations.get("operation");
        List<CodegenOperation> newOpList = new ArrayList<>();

        for (CodegenOperation op : operationList) {
            addApiResponseMetadata(op);
            String path = op.path;

            String[] items = path.split("/", -1);
            String resourceNameCamelCase = "";
            for (String item : items) {
                if (item.length() > 1) {
                    if (item.matches("^\\{(.*)\\}$")) {
                        String tmpResourceName = item.substring(1, item.length() - 1);
                        resourceNameCamelCase += Character.toUpperCase(tmpResourceName.charAt(0)) + tmpResourceName.substring(1);
                    } else {
                        resourceNameCamelCase += Character.toUpperCase(item.charAt(0)) + item.substring(1);
                    }
                } else if (item.length() == 1) {
                    resourceNameCamelCase += Character.toUpperCase(item.charAt(0));
                }
            }
            op.path = path.replaceFirst("/$", "");

            op.vendorExtensions.put("x-codegen-resource-name", resourceNameCamelCase);

            boolean foundInNewList = false;
            for (CodegenOperation op1 : newOpList) {
                if (!foundInNewList) {
                    if (op1.path.equals(op.path)) {
                        foundInNewList = true;
                        final String X_CODEGEN_OTHER_METHODS = "x-codegen-other-methods";
                        List<CodegenOperation> currentOtherMethodList = (List<CodegenOperation>) op1.vendorExtensions.get(X_CODEGEN_OTHER_METHODS);
                        if (currentOtherMethodList == null) {
                            currentOtherMethodList = new ArrayList<>();
                        }
                        op.operationIdCamelCase = op1.operationIdCamelCase;
                        currentOtherMethodList.add(op);
                        op1.vendorExtensions.put(X_CODEGEN_OTHER_METHODS, currentOtherMethodList);
                    }
                }
            }
            if (!foundInNewList) {
                newOpList.add(op);
            }
        }
        operations.put("operation", newOpList);
        return objs;
    }

    private void addApiResponseMetadata(CodegenOperation operation) {
        boolean hasDefaultResponse = false;
        for (CodegenResponse response : operation.responses) {
            response.vendorExtensions.put("x-codegen-return-compatible",
                    Objects.equals(operation.returnType, response.dataType));
            response.vendorExtensions.put(X_CODEGEN_RESPONSE_IS_ONE_OF,
                    isOneOfResponse(response));
            response.vendorExtensions.put(X_CODEGEN_EMPTY_BODY_TOLERANT,
                    response.isMap || response.isFreeFormObject || response.isAnyType);
            if (response.isRange()) {
                response.vendorExtensions.put(
                        X_CODEGEN_RESPONSE_RANGE, response.code.substring(0, 1));
            }

            if (response.isDefault) {
                hasDefaultResponse = true;
                response.vendorExtensions.put(X_CODEGEN_DEFAULT_RESPONSE_IS_RETURN_COMPATIBLE,
                        operation.returnType != null && Objects.equals(operation.returnType, response.dataType));
            }
        }
        operation.vendorExtensions.put(X_CODEGEN_HAS_DEFAULT_RESPONSE, hasDefaultResponse);

        // Detect text/event-stream produces for SSE streaming responses.
        // When an operation produces ONLY text/event-stream, it is a pure
        // SSE endpoint and the operation-level flag is set, causing the
        // return type to be wrapped in std::vector<...>.
        // For dual-content ops (JSON + SSE), the operation-level flag is NOT
        // set (return type stays JSON). Instead, a dedicated stream method is
        // generated ({operationId}Stream) that sets Accept to text/event-stream
        // and returns std::vector<EventType> via incremental event conversion.
        // Note: Dual-content detection is driven by produces media types, not
        // by the presence of a "stream" query parameter (the parameter is
        // a client-side convention for choosing between JSON and SSE).
        if (operation.produces != null && !operation.produces.isEmpty()) {
            boolean hasEventStream = false;
            boolean hasJsonStream = false;
            for (Map<String, String> produce : operation.produces) {
                String mediaType = produce.get("mediaType");
                if ("text/event-stream".equalsIgnoreCase(mediaType)) {
                    hasEventStream = true;
                } else if (mediaType != null && mediaType.contains("json")) {
                    hasJsonStream = true;
                }
            }
            boolean isPureSse = hasEventStream && !hasJsonStream;
            boolean isDualContent = hasEventStream && hasJsonStream;
            operation.vendorExtensions.put("x-codegen-streaming-response", isPureSse);
                // For pure SSE ops, flag all 2xx responses as streaming and
                // set the stripped element type (without shared_ptr) for use in
                // the event vector element and converter name.
            // For dual-content ops, mark SSE responses (different datatype from returnType)
            // as streaming so the stream method template can identify them.
            // Also mark each response with x-codegen-return-compatible so the normal
            // method template can skip responses whose dataType doesn't match the
            // operation return type (avoids type mismatch in deserializedResponse).
            for (CodegenResponse response : operation.responses) {
                    if (isPureSse) {
                    response.vendorExtensions.put("x-codegen-streaming-response", true);
                    if (isOneOfResponse(response)
                            || isOneOfMediaType(response, "text/event-stream")) {
                        operation.vendorExtensions.put(X_CODEGEN_STREAM_IS_ONE_OF, true);
                    }
                    if (response.dataType != null) {
                        String streamElementType = stripSharedPtr(response.dataType);
                        // Only set element type for model types (uppercase first char).
                        // Primitives like std::string don't have fromJsonValue_ free
                        // functions and would produce invalid identifiers like
                        // fromJsonValue_std::string.
                        if (!streamElementType.startsWith("std::") && !streamElementType.startsWith("boost::")
                                && Character.isUpperCase(streamElementType.charAt(0))) {
                            response.vendorExtensions.put("x-codegen-stream-element-type",
                                    streamElementType);
                            // Propagate to operation level for use outside {{#responses}} scope
                            operation.vendorExtensions.put("x-codegen-stream-element-type",
                                    streamElementType);
                        }
                    }
                } else if (isDualContent && response.is2xx && response.dataType != null
                        && !response.dataType.equals(operation.returnType)) {
                    response.vendorExtensions.put("x-codegen-streaming-response", true);
                    if (response.dataType != null) {
                        String streamElementType = stripSharedPtr(response.dataType);
                        response.vendorExtensions.put("x-codegen-stream-element-type",
                                streamElementType);
                    }
                }
            }
            // If a pure SSE operation has no response schema (no data type
            // on any 2xx response), returnType will be null and the
            // mustache template would produce std::vector<void>, which
            // is invalid C++. Clear the streaming flag so the normal
            // non-streaming void path is used instead.
            if (isPureSse && operation.returnType == null) {
                operation.vendorExtensions.put("x-codegen-streaming-response", false);
                for (CodegenResponse r : operation.responses) {
                    r.vendorExtensions.put("x-codegen-streaming-response", false);
                }
            }
            // Dual-content: generate stream method
            // Only emit the stream method if we can resolve a concrete SSE
            // element type from the response content. Without it, the template
            // would produce an invalid std::vector<> with an empty parameter.
            if (isDualContent) {
                // Resolve SSE response type from the response content media-type map.
                // Specs may expose a single 200 with both application/json and
                // text/event-stream. Look for text/event-stream in any 2xx response.
                String sseReturnType = null;
                String sseBaseModelName = null;
                for (CodegenResponse response : operation.responses) {
                    if (!response.is2xx || response.getContent() == null) continue;
                    CodegenMediaType sseMediaType = response.getContent().get("text/event-stream");
                    if (sseMediaType != null && sseMediaType.getSchema() != null) {
                        CodegenProperty sseSchema = sseMediaType.getSchema();
                        String rawType = sseSchema.dataType;
                        if (rawType != null) {
                            sseReturnType = rawType;
                            // Derive a valid C++ identifier for the fromJsonValue_ converter.
                            // Strip std::shared_ptr<X> wrapper down to just X.
                            sseBaseModelName = stripSharedPtr(rawType);
                            if (isOneOfSchema(sseSchema)) {
                                operation.vendorExtensions.put(X_CODEGEN_DUAL_STREAM_IS_ONE_OF, true);
                            }
                            break;
                        }
                    }
                }
                // Fallback: use response dataType (works for split-status fixtures)
                if (sseReturnType == null) {
                    for (CodegenResponse response : operation.responses) {
                        if (response.is2xx && response.dataType != null
                                && !response.dataType.equals(operation.returnType)) {
                            sseReturnType = response.dataType;
                            sseBaseModelName = stripSharedPtr(response.dataType);
                            break;
                        }
                    }
                }
                if (sseReturnType == null) {
                    // Final fallback: first 2xx response
                    for (CodegenResponse response : operation.responses) {
                        if (response.is2xx && response.dataType != null) {
                            sseReturnType = response.dataType;
                            sseBaseModelName = stripSharedPtr(response.dataType);
                            break;
                        }
                    }
                }
                if (sseReturnType != null && sseBaseModelName != null) {
                    if (isOneOfType(sseReturnType)) {
                        operation.vendorExtensions.put(X_CODEGEN_DUAL_STREAM_IS_ONE_OF, true);
                    }
                    operation.vendorExtensions.put("x-codegen-dual-content", true);
                    // Full C++ type for the vector element (may contain std::shared_ptr<...>)
                    operation.vendorExtensions.put("x-codegen-dual-stream-return-type", sseReturnType);
                    // Stripped base name (valid C++ identifier) for fromJsonValue_ converter
                    operation.vendorExtensions.put("x-codegen-dual-stream-base-name", sseBaseModelName);
                    // Stripped element type for event conversion and the vector element
                    // (same as base name since both strip shared_ptr, but semantically distinct)
                    String dualStreamElementType = stripSharedPtr(sseReturnType);
                    operation.vendorExtensions.put("x-codegen-dual-stream-element-type", dualStreamElementType);
                    // Also propagate to each response so the template can access it
                    // from within the {{#responses}} context scope.
                    for (CodegenResponse response : operation.responses) {
                        response.vendorExtensions.put("x-codegen-dual-stream-return-type", sseReturnType);
                        response.vendorExtensions.put("x-codegen-dual-stream-base-name", sseBaseModelName);
                        response.vendorExtensions.put("x-codegen-dual-stream-element-type", dualStreamElementType);
                    }
                }
            }
        }
    }

    private boolean isOneOfResponse(CodegenResponse response) {
        if (response.getContent() != null) {
            for (Map.Entry<String, CodegenMediaType> contentEntry : response.getContent().entrySet()) {
                String mediaType = contentEntry.getKey();
                CodegenMediaType codegenMediaType = contentEntry.getValue();
                if (mediaType != null && mediaType.toLowerCase(Locale.ROOT).contains("json")
                        && codegenMediaType != null && isOneOfSchema(codegenMediaType.getSchema())) {
                    return true;
                }
            }
        }
        return isOneOfType(response.dataType);
    }

    private boolean isOneOfMediaType(CodegenResponse response, String mediaType) {
        if (response.getContent() == null) {
            return false;
        }
        CodegenMediaType codegenMediaType = response.getContent().get(mediaType);
        return codegenMediaType != null && isOneOfSchema(codegenMediaType.getSchema());
    }

    private boolean isOneOfSchema(CodegenProperty schema) {
        return schema != null
                && (Boolean.TRUE.equals(schema.vendorExtensions.get("x-cpp-is-oneof"))
                || isOneOfType(schema.dataType));
    }

    private boolean isOneOfType(String dataType) {
        String unwrappedType = stripSharedPtr(dataType);
        return "oneOf".equals(composedKeywordsByModel.get(unwrappedType));
    }

    /**
     * Optional - type declaration. This is a String which is used by the
     * templates to instantiate your types. There is typically special handling
     * for different property types
     *
     * @return a string value used as the `dataType` field for model templates,
     * `returnType` for api templates
     */
    @Override
    public String getTypeDeclaration(Schema p) {
        // Handle inline oneOf/anyOf composed schemas (apply lowering rules directly)
        if (ModelUtils.isComposedSchema(p) && (p.getOneOf() != null || p.getAnyOf() != null)) {
            return lowerInlineComposedSchema(p);
        }

        String openAPIType = getSchemaType(p);

        if (ModelUtils.isArraySchema(p)) {
            // Use getItems() directly to handle both OpenAPI 3.0 and 3.1
            Schema inner = p.getItems();
            String arrayType;
            if (inner != null) {
                arrayType = getSchemaType(p) + "<" + getTypeDeclaration(inner) + ">";
            } else {
                arrayType = "std::vector<boost::json::value>";
            }
            // Nullable arrays must be wrapped in std::optional so null JSON
            // values are representable. The array branch returns before the
            // nullable fallback checks at the end of this method.
            if (ModelUtils.isNullable(p)) {
                return "std::optional<" + arrayType + ">";
            }
            return arrayType;
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            String innerType = inner == null ? "boost::json::value" : getTypeDeclaration(inner);
            String mapType = getSchemaType(p) + "<std::string, " + innerType + ">";
            // Nullable maps must be wrapped in std::optional so null JSON
            // values are representable. The map branch returns before the
            // nullable fallback checks at the end of this method.
            if (ModelUtils.isNullable(p)) {
                return "std::optional<" + mapType + ">";
            }
            return mapType;
        } else if (ModelUtils.isByteArraySchema(p)) {
            return "std::string";
        } else if (ModelUtils.isStringSchema(p)
                || ModelUtils.isDateSchema(p)
                || ModelUtils.isDateTimeSchema(p) || ModelUtils.isFileSchema(p)
                || languageSpecificPrimitives.contains(openAPIType)) {
            String resolved = toModelName(openAPIType);
            // OAS 3.0 nullable: true → std::optional<T>
            if (ModelUtils.isNullable(p)) {
                return "std::optional<" + resolved + ">";
            }
            return resolved;
        } else if (ModelUtils.isNullType(p)) {
            // Handle OpenAPI 3.1 null type
            return "std::nullptr_t";
        } else if (ModelUtils.isAnyType(p) || ModelUtils.isFreeFormObject(p, openAPI)) {
            return "boost::json::value";
        }

        // OAS 3.0 nullable: true → std::optional<T>
        if (ModelUtils.isNullable(p)) {
            return "std::optional<" + openAPIType + ">";
        }

        // Variant models use value semantics (no shared_ptr wrapping)
        if (variantModels.contains(openAPIType)) {
            return openAPIType;
        }

        // Fallback: wrap in shared_ptr for all other model refs.
        // NOTE (Phase 1 scope): "shared_ptr only for cycles" is a planned follow-up.
        // Determining cycle safety requires circular-reference analysis (setCircularReferences)
        // which runs too late in the pipeline for getTypeDeclaration. Variant models (oneOf/anyOf)
        // are treated as value types via variantModels. For regular object refs, we conservatively
        // use shared_ptr — this will be narrowed to cycle-only in a later phase.
        return "std::shared_ptr<" + openAPIType + ">";
    }

    /**
     * Resolves an inline oneOf/anyOf schema to its lowered C++ type by computing
     * branch types and applying the same ordered lowering rules as model-level types.
     */
    private String lowerInlineComposedSchema(Schema p) {
        String composedKeyword;
        List<Schema> children;
        if (p.getOneOf() != null) {
            children = p.getOneOf();
            composedKeyword = "oneOf";
        } else {
            children = p.getAnyOf();
            composedKeyword = "anyOf";
        }

        List<ComposedBranch> composedBranches = new ArrayList<>();
        for (Schema child : children) {
            // Compute the branch type using the full type declaration pipeline
            // but strip shared_ptr for variant members (value semantics).
            String childType = stripSharedPtr(getTypeDeclaration(child));
            // Resolve $ref targets that are aliased to primitive types at the
            // declaration point, before resolvedAliasTypes is available (it is
            // populated during postProcessModels, which runs later). This handles
            // inline schemas like CreateAssistantRequest_model = oneOf [string,
            // $ref AssistantSupportedModels] where the target is anyOf [string,
            // string-enum] → std::string, collapsing to just std::string.
            Schema resolvedChild = child;
            if (!childType.startsWith("std::") && !childType.startsWith("boost::")
                    && !childType.startsWith("std::shared_ptr<")) {
                Schema resolvedTarget = child.get$ref() != null && openAPI != null
                        ? ModelUtils.getReferencedSchema(openAPI, child) : null;
                if (resolvedTarget != null) {
                    resolvedChild = resolvedTarget;
                    String resolved = getTypeDeclaration(resolvedTarget);
                    String stripped = stripSharedPtr(resolved);
                    if (!stripped.equals(childType)) {
                        childType = stripped;
                    }
                }
            }
            boolean isEnum = resolvedChild.getEnum() != null && !resolvedChild.getEnum().isEmpty();
            boolean isStringLike = ModelUtils.isStringSchema(resolvedChild)
                    || "std::string".equals(childType);
            composedBranches.add(new ComposedBranch(childType, isEnum, isStringLike));
        }

        // Deduplication is deferred to lowerComposedTypes (step 5) so that
        // oneOf semantics can be preserved when duplicate types would otherwise
        // cause silent single-branch collapse.
        return lowerComposedTypes(composedBranches, composedKeyword);
    }

    @Override
    public CodegenProperty fromProperty(String name, Schema p, boolean required,
                                        boolean schemaIsFromAdditionalProperties) {
        CodegenProperty prop = super.fromProperty(name, p, required, schemaIsFromAdditionalProperties);
        if (prop == null || p == null) {
            return prop;
        }
        // Tag inline composed properties so templates can honor oneOf vs anyOf
        // decode rules (exactly-one vs first-match) instead of always using
        // the generic JsonValueConverter exactly-one path.
        if (p.getOneOf() != null && !p.getOneOf().isEmpty()) {
            prop.vendorExtensions.put("x-cpp-composed-keyword", "oneOf");
            prop.vendorExtensions.put("x-cpp-is-oneof", true);
        } else if (p.getAnyOf() != null && !p.getAnyOf().isEmpty()) {
            prop.vendorExtensions.put("x-cpp-composed-keyword", "anyOf");
            prop.vendorExtensions.put("x-cpp-is-anyof", true);
        }
        return prop;
    }

    @Override
    public String toDefaultValue(Schema p) {
        if (ModelUtils.isStringSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            } else {
                return "\"\"";
            }
        } else if (ModelUtils.isBooleanSchema(p)) {
            if (p.getDefault() != null) {
                return p.getDefault().toString();
            } else {
                return "false";
            }
        } else if (ModelUtils.isDateSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            } else {
                return "\"\"";
            }
        } else if (ModelUtils.isDateTimeSchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            } else {
                return "\"\"";
            }
        } else if (ModelUtils.isNumberSchema(p)) {
            if (ModelUtils.isFloatSchema(p)) { // float
                if (p.getDefault() != null) {
                    return p.getDefault().toString() + "f";
                } else {
                    return "0.0f";
                }
            } else { // double
                if (p.getDefault() != null) {
                    return p.getDefault().toString();
                } else {
                    return "0.0";
                }
            }
        } else if (ModelUtils.isIntegerSchema(p)) {
            if (ModelUtils.isLongSchema(p)) { // long
                if (p.getDefault() != null) {
                    return p.getDefault().toString() + "L";
                } else {
                    return "0L";
                }
            } else { // integer
                if (p.getDefault() != null) {
                    return p.getDefault().toString();
                } else {
                    return "0";
                }
            }
        } else if (ModelUtils.isByteArraySchema(p)) {
            if (p.getDefault() != null) {
                return "\"" + p.getDefault().toString() + "\"";
            } else {
                return "\"\"";
            }
        } else if (ModelUtils.isMapSchema(p)) {
            Schema inner = ModelUtils.getAdditionalProperties(p);
            String innerType = inner == null ? "boost::json::value" : getTypeDeclaration(inner);
            return "std::map<std::string, " + innerType + ">()";
        } else if (ModelUtils.isArraySchema(p)) {
            // Use getItems() directly to handle OpenAPI 3.1 JsonSchema
            Schema inner = p.getItems();
            String innerType = inner != null ? getTypeDeclaration(inner) : "boost::json::value";
            return "std::vector<" + innerType + ">()";
        } else if (!StringUtils.isEmpty(p.get$ref())) {
            String refName = toModelName(ModelUtils.getSimpleRef(p.get$ref()));
            if (variantModels.contains(refName)) {
                return refName + "()";
            }
            return "std::make_shared<" + refName + ">()";
        } else if (ModelUtils.isNullType(p)) {
            return "nullptr";
        } else if (ModelUtils.isAnyType(p) || ModelUtils.isFreeFormObject(p, openAPI)) {
            return "boost::json::value()";
        }

        return "nullptr";
    }
    
    @Override
    public String toDefaultValue(CodegenProperty codegenProperty, Schema schema) {
        if (codegenProperty != null) {
            if (codegenProperty.dataType != null && codegenProperty.dataType.startsWith("std::shared_ptr<")) {
                return "nullptr";
            }
            if ("boost::json::value".equals(codegenProperty.dataType)) {
                return "boost::json::value()";
            }
        }
        return super.toDefaultValue(codegenProperty, schema);
    }

    @Override
    public void postProcessParameter(CodegenParameter parameter) {
        super.postProcessParameter(parameter);

        boolean isPrimitiveType = parameter.isPrimitiveType == Boolean.TRUE;
        boolean isArray = parameter.isArray == Boolean.TRUE;
        boolean isMap = parameter.isMap == Boolean.TRUE;
        boolean isString = parameter.isString == Boolean.TRUE;
        parameter.vendorExtensions.put(X_CODEGEN_IS_RAW_BODY,
                isPrimitiveType || isString || parameter.isByteArray || parameter.isBinary
                        || "std::string".equals(parameter.dataType));

        if (!isPrimitiveType && !isArray && !isMap && !isString && !parameter.dataType.startsWith("std::shared_ptr")
                && !"boost::json::value".equals(parameter.dataType)
                && !"std::nullptr_t".equals(parameter.dataType)
                && !parameter.dataType.startsWith("std::variant<")
                && !parameter.dataType.startsWith("std::optional<")
                && !"std::monostate".equals(parameter.dataType)) {
            // Wrap non-primitive types in shared_ptr, unless:
            // - The type is a variant/optional model (value semantics)
            // - The type is a known variant model name from composed schemas
            if (!variantModels.contains(parameter.dataType)) {
                parameter.dataType = "std::shared_ptr<" + parameter.dataType + ">";
                parameter.defaultValue = "std::make_shared<" + parameter.dataType + ">()";
            }
        }

        // Post-hoc unwrap: if the type ended up as std::shared_ptr<VariantModel>,
        // strip the shared_ptr wrapper (value semantics for variant types).
        if (parameter.dataType != null && parameter.dataType.startsWith("std::shared_ptr<")
                && parameter.dataType.endsWith(">")) {
            String innerType = parameter.dataType.substring(16, parameter.dataType.length() - 1);
            if (variantModels.contains(innerType)) {
                parameter.dataType = innerType;
                parameter.defaultValue = null;
            }
        }

        // Tag variant form params for branch-aware multipart serialization.
        // When a form parameter's type is a variant, the template uses
        // addVariantFormParameter to dispatch binary branches as file parts
        // and object branches as JSON parts.
        // Only set for actual std::variant types, not for models that alias
        // to primitive types (e.g., VideoModel → std::string), which would
        // cause instantiation of addVariantFormParameter<std::string> and
        // an invalid std::visit call on a non-variant type.
        boolean isVariantParam = false;
        if (parameter.isFormParam && parameter.dataType != null) {
            if (parameter.dataType.startsWith("std::variant<")) {
                isVariantParam = true;
            } else if (variantModels.contains(parameter.dataType)) {
                String resolved = resolveThroughAliases(parameter.dataType);
                if (resolved != null && resolved.startsWith("std::variant<")) {
                    isVariantParam = true;
                }
            }
        }
        if (isVariantParam) {
            parameter.vendorExtensions.put("x-codegen-is-variant-form-param", true);
        }
    }

    /**
     * Optional - OpenAPI type conversion. This is used to map OpenAPI types in
     * a `Schema` into either language specific types via `typeMapping` or
     * into complex models if there is not a mapping.
     *
     * @return a string value of the type or complex model for this property
     */
    @Override
    public String getSchemaType(Schema p) {
        // Non-standard format (NOT core OAS vocabulary). Documented generator
        // convenience for corpora that use Unix-epoch integer timestamps.
        // Disable by not using format: unixtime in the source document.
        if (p != null && "unixtime".equals(p.getFormat())) {
            return "int64_t";
        }
        String openAPIType = super.getSchemaType(p);
        String type = null;
        String modelName;
        if (typeMapping.containsKey(openAPIType)) {
            type = typeMapping.get(openAPIType);
        } else {
            type = openAPIType;
        }

        modelName = toModelName(type);
        return modelName;
    }

    @Override
    public void updateCodegenPropertyEnum(CodegenProperty var) {
        // Remove prefix added by DefaultCodegen
        String originalDefaultValue = var.defaultValue;
        super.updateCodegenPropertyEnum(var);
        var.defaultValue = originalDefaultValue;
    }
}
