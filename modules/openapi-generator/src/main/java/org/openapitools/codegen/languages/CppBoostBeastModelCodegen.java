/*
 * Copyright 2026 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.languages;

import com.fasterxml.jackson.databind.JsonNode;
import com.google.common.collect.ImmutableMap;
import com.samskivert.mustache.Mustache.Lambda;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.media.MediaType;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.responses.ApiResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.openapitools.codegen.*;
import org.openapitools.codegen.languages.Oas31CompositionLowering.AllOfIntersection;
import org.openapitools.codegen.languages.Oas31CompositionLowering.CompositionBranchDescriptor;
import org.openapitools.codegen.languages.Oas31CompositionLowering.CompositionDescriptor;
import org.openapitools.codegen.languages.Oas31CompositionLowering.DiscriminatorDescriptor;
import org.openapitools.codegen.meta.features.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.utils.ModelUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.util.*;
import java.util.Map;
import java.util.HashMap;
import java.util.stream.Collectors;

import static org.openapitools.codegen.utils.StringUtils.camelize;

/**
 * Model-lowering and composition post-processing shared by the Boost.Beast
 * generator. Operation assembly and output configuration stay on the client
 * generator subclass.
 */
public abstract class CppBoostBeastModelCodegen extends AbstractCppCodegen {
    protected static final String X_CPP_EXPLICIT_DEFAULT_SCALAR =
            "x-cpp-explicit-default-scalar";
    protected static final String X_CPP_TOLERATE_NONNULLABLE_NULL =
            "x-cpp-tolerate-nonnullable-null";
    private static final String SHARED_PTR_PREFIX = "std::shared_ptr<";
    /** Compatibility mode for server responses that send undeclared nulls. */
    protected boolean tolerateNonNullableNulls = true;
    protected final Logger LOGGER = LoggerFactory.getLogger(CppBoostBeastClientCodegen.class);
    /** Tracks model names resolved as oneOf/anyOf variant types for shared_ptr exclusion. */
    protected Set<String> variantModels = new HashSet<>();
    /** Caches resolved C++ types for composed models so postProcessModels can
     *  transitively resolve $ref chains through model aliases (for example,
     *  ModelIds referencing ModelIdsShared, both ultimately std::string). */
    protected Map<String, String> resolvedAliasTypes = new HashMap<>();
    /** Retains composition semantics after named schemas are lowered to C++ aliases. */
    protected Map<String, String> composedKeywordsByModel = new HashMap<>();
    /** Descriptor index mapping schema name to composition descriptor, populated
     *  in preprocessOpenAPI after inline model flattening. Replaces raw schema
     *  inspection as the semantic source for branch lowering. */
    protected Map<String, CompositionDescriptor> compositionDescriptors = new LinkedHashMap<>();
    /** All descriptors indexed for schemas that combine composition keywords. */
    protected Map<String, List<CompositionDescriptor>> compositionDescriptorSets =
            new LinkedHashMap<>();
    /** OpenAPI document retained for operation and model post-processing. */
    protected OpenAPI sourceOpenApi;
    /** Whether the source document explicitly declares its root servers field. */
    protected boolean hasExplicitRootServers;

    /** Preserved inbound-only webhook metadata. Webhooks are removed from
     *  outbound API generation so upstream folding cannot replace path APIs. */
    protected List<String> webhookPreservation = new ArrayList<>();

    public List<String> getWebhookPreservation() {
        return new ArrayList<>(webhookPreservation);
    }

    protected static String idOf(io.swagger.v3.oas.models.Operation op) {
        return op.getOperationId() == null ? "(no operationId)" : op.getOperationId();
    }

    /** Callback and response-link names keyed by path and method. */
    protected Map<String, List<String>> operationCallbacks = new HashMap<>();
    protected Map<String, List<String>> operationLinks = new HashMap<>();

    protected void captureOperationMetadata(OpenAPI openAPI) {
        operationCallbacks.clear();
        operationLinks.clear();
        if (openAPI == null || openAPI.getPaths() == null) {
            return;
        }
        for (Map.Entry<String, PathItem> pathEntry : openAPI.getPaths().entrySet()) {
            PathItem pathItem = pathEntry.getValue();
            if (pathItem == null || pathItem.readOperationsMap() == null) {
                continue;
            }
            for (Map.Entry<PathItem.HttpMethod, Operation> operationEntry
                    : pathItem.readOperationsMap().entrySet()) {
                Operation operation = operationEntry.getValue();
                if (operation == null) {
                    continue;
                }
                String key = pathEntry.getKey() + '\0' + operationEntry.getKey().name();
                List<String> callbackNames = operation.getCallbacks() == null
                        ? Collections.emptyList()
                        : new ArrayList<>(operation.getCallbacks().keySet());
                operationCallbacks.put(key, callbackNames);

                Set<String> linkNames = new LinkedHashSet<>();
                if (operation.getResponses() != null) {
                    for (ApiResponse candidate : operation.getResponses().values()) {
                        if (candidate == null) {
                            continue;
                        }
                        if (candidate.getLinks() != null) {
                            linkNames.addAll(candidate.getLinks().keySet());
                        }
                        ApiResponse resolved = ModelUtils.getReferencedApiResponse(
                                openAPI, candidate);
                        if (resolved != null && resolved.getLinks() != null) {
                            linkNames.addAll(resolved.getLinks().keySet());
                        }
                    }
                }
                operationLinks.put(key, new ArrayList<>(linkNames));
            }
        }
    }
    /** Cached allOf intersections keyed by model name. Populated during
     *  preprocessOpenAPI and consumed by fromModel to build synthetic schemas. */
    protected Map<String, AllOfIntersection> allOfIntersections = new LinkedHashMap<>();
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
        // never matches, so cycle edges would lose shared_ptr wrappers and emit
        // invalid value self-references.
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
                    restoreSavedSharedPtr(var.items, cm.classname, modelSaves);
                }
            }
        }

        // Phase: Strip std::shared_ptr<X> from non-cyclic object refs.
        // DefaultCodegen only records the immediate container item type, while
        // nested containers can contain a recursive model reference farther down.
        // Build the full model-reference graph before changing any C++ types.
        Map<String, Set<String>> cyclicModelReferences = findCyclicModelReferences(allModels);
        for (CodegenModel cm : allModels.values()) {
            Set<String> cyclicTargets = cyclicModelReferences.getOrDefault(
                    cm.classname, Collections.emptySet());
            for (CodegenProperty var : allVarsOf(cm)) {
                stripNonCyclicSharedPtrs(var, cyclicTargets);
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
     * Finds model-reference edges that participate in a cycle, including model
     * references nested inside array and map C++ types.
     */
    private static Map<String, Set<String>> findCyclicModelReferences(
            Map<String, CodegenModel> allModels) {
        Map<String, Set<String>> dependencies = new LinkedHashMap<>();
        for (CodegenModel model : allModels.values()) {
            dependencies.putIfAbsent(model.classname, new LinkedHashSet<>());
        }

        Set<String> modelNames = dependencies.keySet();
        for (CodegenModel model : allModels.values()) {
            Set<String> references = dependencies.get(model.classname);
            for (CodegenProperty property : allVarsOf(model)) {
                collectModelReferences(property == null ? null : property.dataType,
                        modelNames, references);
            }
        }

        Map<String, Set<String>> cyclicReferences = new LinkedHashMap<>();
        for (Map.Entry<String, Set<String>> entry : dependencies.entrySet()) {
            Set<String> cyclicTargets = new LinkedHashSet<>();
            for (String target : entry.getValue()) {
                if (hasModelPath(target, entry.getKey(), dependencies)) {
                    cyclicTargets.add(target);
                }
            }
            cyclicReferences.put(entry.getKey(), cyclicTargets);
        }
        return cyclicReferences;
    }

    private static void collectModelReferences(String dataType, Set<String> modelNames,
                                               Set<String> references) {
        if (dataType == null) {
            return;
        }
        int cursor = 0;
        while (cursor < dataType.length()) {
            int pointerStart = dataType.indexOf(SHARED_PTR_PREFIX, cursor);
            if (pointerStart < 0) {
                return;
            }
            int contentStart = pointerStart + SHARED_PTR_PREFIX.length();
            int pointerEnd = matchingTemplateEnd(dataType, contentStart);
            if (pointerEnd < 0) {
                return;
            }
            String pointedType = dataType.substring(contentStart, pointerEnd);
            if (modelNames.contains(pointedType)) {
                references.add(pointedType);
            }
            collectModelReferences(pointedType, modelNames, references);
            cursor = pointerEnd + 1;
        }
    }

    private static boolean hasModelPath(String start, String target,
                                        Map<String, Set<String>> dependencies) {
        Deque<String> pending = new ArrayDeque<>();
        Set<String> visited = new HashSet<>();
        pending.add(start);
        while (!pending.isEmpty()) {
            String current = pending.removeFirst();
            if (!visited.add(current)) {
                continue;
            }
            if (target.equals(current)) {
                return true;
            }
            pending.addAll(dependencies.getOrDefault(current, Collections.emptySet()));
        }
        return false;
    }

    /**
     * Removes every shared_ptr wrapper whose referenced model is not on a
     * recursive edge. The parser tracks nested container structure, but the
     * rendered C++ dataType is authoritative for replacing every level.
     */
    private static void stripNonCyclicSharedPtrs(CodegenProperty property,
                                                 Set<String> cyclicTargets) {
        if (property == null) {
            return;
        }
        String strippedDataType = stripNonCyclicSharedPtrType(property.dataType, cyclicTargets);
        if (!Objects.equals(property.dataType, strippedDataType)) {
            property.dataType = strippedDataType;
            property.defaultValue = null;
        }
        stripNonCyclicSharedPtrs(property.items, cyclicTargets);
    }

    private static String stripNonCyclicSharedPtrType(String dataType,
                                                       Set<String> cyclicTargets) {
        if (dataType == null) {
            return null;
        }

        StringBuilder result = new StringBuilder(dataType.length());
        int cursor = 0;
        while (cursor < dataType.length()) {
            int pointerStart = dataType.indexOf(SHARED_PTR_PREFIX, cursor);
            if (pointerStart < 0) {
                result.append(dataType, cursor, dataType.length());
                break;
            }
            result.append(dataType, cursor, pointerStart);
            int contentStart = pointerStart + SHARED_PTR_PREFIX.length();
            int pointerEnd = matchingTemplateEnd(dataType, contentStart);
            if (pointerEnd < 0) {
                return dataType;
            }

            String pointedType = dataType.substring(contentStart, pointerEnd);
            String strippedPointedType = stripNonCyclicSharedPtrType(pointedType, cyclicTargets);
            if (cyclicTargets.contains(pointedType)) {
                result.append(SHARED_PTR_PREFIX).append(strippedPointedType).append('>');
            } else {
                result.append(strippedPointedType);
            }
            cursor = pointerEnd + 1;
        }
        return result.toString();
    }

    private static int matchingTemplateEnd(String dataType, int contentStart) {
        int depth = 1;
        for (int index = contentStart; index < dataType.length(); index++) {
            char character = dataType.charAt(index);
            if (character == '<') {
                depth++;
            } else if (character == '>' && --depth == 0) {
                return index;
            }
        }
        return -1;
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

        // Lower oneOf/anyOf models before template-dispatch metadata is derived.
        for (ModelMap mo : result.getModels()) {
            processComposedModel(mo.getModel());
        }

        // Tag models with alias/variant flags for template dispatch.
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
        // A descriptor, when present, is the semantic source rather than dataType.
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            if (cm.vendorExtensions.containsKey("x-cpp-is-alias")) {
                continue;
            }
            if (compositionDescriptors.containsKey(cm.classname)) {
                continue; // descriptor provides semantics; skip dataType heuristic
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
        // Apply only when a composition descriptor establishes the schema semantics.
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            if (cm.vendorExtensions.containsKey("x-cpp-is-alias")) {
                continue;
            }
            if (compositionDescriptors.containsKey(cm.classname)) {
                continue; // descriptor provides semantics; skip dataType heuristic
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

        // Recover all-null oneOf/anyOf models that still reach model processing
        // as one std::nullptr_t branch. Preserve the authored cardinality with
        // tagged alternatives and add matching descriptors so the schema IR
        // owns every generated branch validator.
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            String checkType = (String) cm.vendorExtensions.get("x-cpp-type");
            if (checkType == null && cm.isAlias) {
                checkType = cm.dataType;
            }
            if ("std::nullptr_t".equals(checkType)
                    && !cm.vendorExtensions.containsKey("x-cpp-is-variant")) {
                CompositionDescriptor descriptor = compositionDescriptors.get(cm.classname);
                int branchCount = descriptor != null && descriptor.getBranches().size() > 1
                        ? descriptor.getBranches().size() : 2;
                String keyword = descriptor == null ? "oneOf" : descriptor.getKeyword();
                boolean isNullComposition = branchCount > 1
                        && ("oneOf".equals(keyword) || "anyOf".equals(keyword));
                if (isNullComposition) {
                    String rawSchemaName = cm.schemaName != null && !cm.schemaName.isEmpty()
                            ? cm.schemaName : cm.classname;
                    if (descriptor == null || descriptor.getBranches().size() != branchCount) {
                        List<CompositionBranchDescriptor> nullBranches = new ArrayList<>();
                        String validatorPrefix = toValidIdentifier(rawSchemaName);
                        for (int bi = 0; bi < branchCount; bi++) {
                            String storageType =
                                    "CompositionBranchValue<" + bi + ", std::nullptr_t>";
                            Map<String, Object> validateParams = new LinkedHashMap<>();
                            validateParams.put("validation-type", "null");
                            nullBranches.add(new CompositionBranchDescriptor(
                                    bi, null, "null", storageType,
                                    validatorPrefix + "_branch_" + bi,
                                    CompositionBranchDescriptor.NullCapability.ALWAYS,
                                    List.of("type"), Collections.emptyList(), validateParams));
                        }
                        String schemaLocation = descriptor != null
                                ? descriptor.getSchemaLocation()
                                : "#/components/schemas/"
                                        + rawSchemaName.replace("~", "~0")
                                                .replace("/", "~1");
                        descriptor = new CompositionDescriptor(
                                rawSchemaName, schemaLocation, keyword, nullBranches, null);
                        compositionDescriptors.put(cm.classname, descriptor);
                    }

                    List<String> tagged = new ArrayList<>();
                    for (int bi = 0; bi < branchCount; bi++) {
                        tagged.add("CompositionBranchValue<" + bi + ", std::nullptr_t>");
                    }
                    String variantType = "std::variant<" + String.join(", ", tagged) + ">";
                    cm.vendorExtensions.put("x-cpp-type", variantType);
                    cm.dataType = variantType;
                    resolvedAliasTypes.put(cm.classname, variantType);
                    variantModels.add(cm.classname);
                    cm.vendorExtensions.put("x-cpp-is-variant", true);
                    cm.vendorExtensions.put("x-cpp-is-alias", true);
                    cm.vendorExtensions.put("x-cpp-has-duplicate-types", true);
                    cm.vendorExtensions.put("x-cpp-composed-keyword", keyword);
                    composedKeywordsByModel.put(cm.classname, keyword);
                    cm.vendorExtensions.put("x-cpp-branches",
                            new ArrayList<>(Collections.nCopies(
                                    branchCount, "std::nullptr_t")));

                    Map<String, Object> templateMap = descriptor.toTemplateMap();
                    templateMap.put("has-duplicate-types", true);
                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> branchMaps =
                            (List<Map<String, Object>>) templateMap.get("branches");
                    for (int bi = 0; bi < branchMaps.size(); bi++) {
                        branchMaps.get(bi).put("storage-cpp-type",
                                "CompositionBranchValue<" + bi + ", std::nullptr_t>");
                        branchMaps.get(bi).put("inner-cpp-type", "std::nullptr_t");
                    }
                    cm.vendorExtensions.put("x-cpp-composition-branches", templateMap);
                }
            }
        }

        // Tag properties whose types already embed optional semantics so the
        // template skips redundant IsSet state.
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            for (CodegenProperty var : allVarsOf(cm)) {
                if (var.dataType != null && var.dataType.startsWith("std::optional<")) {
                    var.vendorExtensions.put("x-cpp-no-is-set", true);
                }
            }
        }


        // Cross-model property tagging runs in postProcessAllModels, where the
        // complete model index is available.

        // Tag optional-impossible properties from allOf intersections.
        // These properties have an empty intersection (e.g., string ∩ integer).
        // The generated decode validation rejects the property when present
        // in JSON but accepts the object when the property is absent. The
        // getter/setter and member are still emitted (non-empty shell).
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            @SuppressWarnings("unchecked")
            List<String> optImpProps = (List<String>) cm.vendorExtensions
                    .remove("x-cpp-optional-impossible-properties");
            if (optImpProps == null || optImpProps.isEmpty()) continue;
            for (CodegenProperty var : allVarsOf(cm)) {
                if (optImpProps.contains(var.baseName)) {
                    var.vendorExtensions.put("x-cpp-optional-impossible", true);
                    var.vendorExtensions.put("x-cpp-reject-if-present", true);
                }
            }
        }

        // Emit complete includes for resolved alias and variant types.
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

        // Phase: Emit x-cpp-composition-branches for allOf models that were
        // processed by fromModel (not by processComposedModel). These models
        // have descriptors but were bypassed by the oneOf/anyOf lowering loop.
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            if (cm.vendorExtensions.containsKey("x-cpp-composition-branches")) {
                continue;
            }
            CompositionDescriptor desc = compositionDescriptors.get(cm.classname);
            if (desc != null && "allOf".equals(desc.getKeyword())) {
                cm.vendorExtensions.put("x-cpp-composition-branches", desc.toTemplateMap());
            }
        }

        // Phase: Convert allOf models with scalar-type intersection (e.g.,
        // allOf of two string enums, allOf of a scalar type and an object)
        // to type aliases when the merged properties are empty. These models
        // have an AllOfIntersection with a rootScalarType but no object
        // properties, so they should emit "using Name = std::string;" rather
        // than an empty class shell.
        for (ModelMap mo : result.getModels()) {
            CodegenModel cm = mo.getModel();
            if (cm.vendorExtensions.containsKey("x-cpp-is-alias")) {
                continue;
            }
            AllOfIntersection intersection = allOfIntersections.get(cm.classname);
            if (intersection == null) {
                continue;
            }
            if (intersection.getRootScalarType() == null) {
                continue;
            }
            // Only convert to alias when the merged properties are empty
            // (no object properties from allOf contributors). Models with
            // both a root scalar and properties need a class.
            if (!intersection.getProperties().isEmpty()) {
                continue;
            }
            if (!intersection.isSatisfiable()) {
                continue;
            }
            // Resolve the root scalar type to its C++ type
            String resolvedType = resolveOpenApiTypeName(intersection.getRootScalarType());
            // Apply intersected root-level enum values: if the allOf produces
            // an enum intersection (e.g., [a,b] ∩ [b,c] = [b]), keep the type
            // as std::string (not an enum class), since the intersection may
            // be narrower than the full enum set.
            cm.vendorExtensions.put("x-cpp-type", resolvedType);
            cm.vendorExtensions.put("x-cpp-is-alias", true);
            cm.dataType = resolvedType;
            resolvedAliasTypes.put(cm.classname, resolvedType);
            cm.vendorExtensions.put("x-cpp-composed-keyword", "allOf");
            composedKeywordsByModel.put(cm.classname, "allOf");
            // Propagate intersected enum values to vendor extensions so the
            // alias fromJsonValue template can generate enum validation.
            // Enum values are stored as List<String> for Mustache iteration.
            if (intersection.getRootEnumValues() != null
                    && !intersection.getRootEnumValues().isEmpty()) {
                List<String> intersectedEnum = new ArrayList<>();
                for (Object ev : intersection.getRootEnumValues()) {
                    if (ev != null) {
                        intersectedEnum.add(escapeCppStringContent(ev.toString()));
                    }
                }
                cm.vendorExtensions.put("x-cpp-allof-intersected-enum-values",
                        intersectedEnum);
                cm.vendorExtensions.put("x-cpp-allof-intersected-enum", true);
            }
        }

        return result;
    }

    private static boolean hasTaggedCompositionBranches(String resolvedType) {
        // Duplicate lowering wraps every outer alternative. A nested variant may
        // contain the same tag text without changing the outer storage contract.
        return resolvedType != null
                && resolvedType.startsWith("std::variant<CompositionBranchValue<");
    }

    @SuppressWarnings("unchecked")
    private void refreshCompositionStorageMetadata(
            CodegenModel model, List<ComposedBranch> branches, String resolvedType) {
        Object metadataObject = model.vendorExtensions.get("x-cpp-composition-branches");
        if (!(metadataObject instanceof Map)) {
            return;
        }
        Map<String, Object> metadata = (Map<String, Object>) metadataObject;
        Object branchMapsObject = metadata.get("branches");
        if (!(branchMapsObject instanceof List)) {
            return;
        }
        List<Map<String, Object>> branchMaps = (List<Map<String, Object>>) branchMapsObject;
        boolean wrapped = hasTaggedCompositionBranches(resolvedType);

        for (ComposedBranch branch : branches) {
            int index = branch.originalBranchIndex;
            if (index < 0 || index >= branchMaps.size()) {
                continue;
            }
            Map<String, Object> branchMap = branchMaps.get(index);
            if (wrapped) {
                branchMap.put("storage-cpp-type",
                        "CompositionBranchValue<" + index + ", " + branch.cppType + ">");
                branchMap.put("inner-cpp-type", branch.cppType);
            } else {
                branchMap.put("storage-cpp-type", branch.cppType);
                branchMap.remove("inner-cpp-type");
            }
        }

        metadata.put("has-duplicate-types", wrapped);
        if (wrapped) {
            model.vendorExtensions.put("x-cpp-has-duplicate-types", true);
        } else {
            model.vendorExtensions.remove("x-cpp-has-duplicate-types");
        }
    }


    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        Map<String, ModelsMap> processed = super.postProcessAllModels(objs);
        Map<String, CodegenModel> allModels = getAllModels(processed);
        // Resolve composed aliases to a fixed point. An acyclic dependency graph
        // must converge within one pass per model.
        int maxAliasResolutionPasses = processed.values().stream()
                .mapToInt(models -> models.getModels().size())
                .sum() + 1;
        boolean typeChanged = true;
        int aliasResolutionPass = 0;
        while (typeChanged && aliasResolutionPass < maxAliasResolutionPasses) {
            typeChanged = false;
            aliasResolutionPass++;
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
                    List<ComposedBranch> branchesWithMeta = new ArrayList<>();
                    try {
                        // Reconstruct ComposedBranch objects using resolved C++ type
                        // strings and per-branch isEnum metadata.  Without isEnum, a
                        // oneOf [open-string, string-enum] whose branches resolve to
                        // ["std::string", "std::string"] through the alias chain would
                        // collapse to plain std::string and lose the oneOf overlap
                        // detection that correctly type-erases to boost::json::value.
                        //
                        // Branch isEnum comes from two sources:
                        //   1. For branches whose original type is a model name (not a C++
                        //      type string), look up the CodegenModel to check isEnum.
                        //   2. Fall back to stored x-cpp-branch-is-enum metadata from the
                        //      first lowering pass (handles inline enum schemas where the
                        //      CodegenProperty.isEnum flag was set directly).
                        //
                        // Preserve the original descriptor index after
                        // self-referencing branches are filtered.
                        @SuppressWarnings("unchecked")
                        List<Boolean> storedIsEnum = (List<Boolean>) cm.vendorExtensions.get("x-cpp-branch-is-enum");
                        @SuppressWarnings("unchecked")
                        List<Integer> storedOriginalIndices = (List<Integer>) cm.vendorExtensions
                                .get("x-cpp-branch-original-index");
                        for (int i = 0; i < resolved.size(); i++) {
                            int descIndex = (storedOriginalIndices != null && i < storedOriginalIndices.size())
                                    ? storedOriginalIndices.get(i) : i;
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
                            branchesWithMeta.add(new ComposedBranch(resolved.get(i), isEnum, isStringLike, descIndex));
                        }
                        CompositionDescriptor descriptor =
                                compositionDescriptors.get(cm.classname);
                        newType = Oas31CompositionLowering.lowerComposedTypes(
                                branchesWithMeta, composedKeyword, descriptor, LOGGER::warn);
                    } catch (RuntimeException e) {
                        throw new IllegalStateException(
                                "Failed to resolve composed aliases for '" + cm.classname + "'", e);
                    }
                    if (!newType.equals(currentType)) {
                        cm.vendorExtensions.put("x-cpp-type", newType);
                        // Keep original x-cpp-branches for import resolution.
                        cm.dataType = newType;
                        resolvedAliasTypes.put(cm.classname, newType);
                        refreshCompositionStorageMetadata(cm, branchesWithMeta, newType);
                        // Self-reference filtering needs the final post-collapse type,
                        // not the value cached during the first lowering pass.
                        if (cm.discriminator != null) {
                            cm.vendorExtensions.put("x-discriminator-resolved-type", newType);
                        }
                        typeChanged = true;
                    }
                }
            }
        }
        if (typeChanged) {
            throw new IllegalStateException("Composed alias resolution did not converge");
        }

        // Recompute alias and variant flags after transitive type resolution so
        // aliases of variant types inherit variant serialization behavior.
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

        // Remove discriminator mappings that resolve to the current model type;
        // retaining one would recurse indefinitely and try to construct a variant
        // from itself. Update the CodegenDiscriminator consumed by templates.
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

        // Finalize nullable storage only after updateAllModels has identified
        // cycles and removed shared_ptr from non-cyclic model references.
        for (ModelsMap modelsMap : processed.values()) {
            for (ModelMap modelMap : modelsMap.getModels()) {
                CodegenModel cm = modelMap.getModel();
                boolean needsNullableFieldInclude = false;
                for (CodegenProperty var : allVarsOf(cm)) {
                    if (tolerateNonNullableNulls && !var.isNullable) {
                        var.vendorExtensions.put(X_CPP_TOLERATE_NONNULLABLE_NULL, true);
                    }
                    if (!var.isNullable || var.dataType == null
                            || Boolean.TRUE.equals(var.vendorExtensions.get("x-cpp-nullable-field"))) {
                        continue;
                    }
                    String innerType = extractOptionalInnerType(var.dataType);
                    if (innerType == null && !Boolean.TRUE.equals(var.vendorExtensions
                            .get(Oas31RawSpecRecovery.LEGACY_NULLABLE_EXT))) {
                        continue;
                    }
                    if (var.isEnum) {
                        var.vendorExtensions.put(
                                "x-cpp-enum-value-type",
                                innerType == null ? var.dataType : innerType);
                    }
                    if (var.required) {
                        if (innerType == null) {
                            var.dataType = "std::optional<" + var.dataType + ">";
                            cm.imports.add("#include <optional>");
                            var.vendorExtensions.put("x-cpp-no-is-set", true);
                        }
                        continue;
                    }
                    if (innerType == null) {
                        innerType = var.dataType;
                        var.vendorExtensions.put("x-cpp-no-is-set", true);
                    }
                    if (Boolean.TRUE.equals(var.vendorExtensions.get(
                            "x-cpp-has-explicit-default"))) {
                        if (Boolean.TRUE.equals(var.vendorExtensions.get(
                                "x-cpp-default-is-null"))) {
                            var.defaultValue = "NullableField<" + innerType
                                    + ">::makeDefaultNull()";
                        } else {
                            var.defaultValue = "NullableField<" + innerType
                                    + ">::makeDefaultValue(" + var.defaultValue + ")";
                        }
                        var.vendorExtensions.put("x-cpp-member-default", true);
                    } else {
                        // DefaultCodegen seeds primitive placeholders even when the
                        // schema has no default; they are not NullableField values.
                        var.defaultValue = null;
                    }
                    var.dataType = "NullableField<" + innerType + ">";
                    var.vendorExtensions.put("x-cpp-nullable-field", true);
                    var.vendorExtensions.put("x-cpp-nullable-field-inner-type", innerType);
                    needsNullableFieldInclude = true;
                }
                if (needsNullableFieldInclude) {
                    cm.imports.add("#include \"NullableField.h\"");
                }
            }
        }

        // Tag properties that refer to variant aliases so templates use the
        // keyword-aware free conversion functions rather than the generic variant
        // converter, which always enforces oneOf semantics. This global pass can
        // inspect every model and unwrap NullableField before alias lookup.
        for (Map.Entry<String, ModelsMap> entry : processed.entrySet()) {
            for (ModelMap mo : entry.getValue().getModels()) {
                CodegenModel cm = mo.getModel();
                for (CodegenProperty var : allVarsOf(cm)) {
                    if (var.dataType != null) {
                        // Strip NullableField wrapper when present: use inner type
                        // for alias lookup.
                        String lookupType;
                        if (Boolean.TRUE.equals(var.vendorExtensions.get("x-cpp-nullable-field"))) {
                            lookupType = (String) var.vendorExtensions.get("x-cpp-nullable-field-inner-type");
                        } else {
                            lookupType = var.dataType;
                        }
                        if (lookupType == null) {
                            continue;
                        }
                        ModelsMap targetEntry = processed.get(lookupType);
                        if (targetEntry != null) {
                            for (ModelMap targetMo : targetEntry.getModels()) {
                                CodegenModel targetModel = targetMo.getModel();
                                if (Boolean.TRUE.equals(targetModel.vendorExtensions.get("x-cpp-is-variant"))) {
                                    var.vendorExtensions.put("x-cpp-variant-alias", true);
                                    var.vendorExtensions.put("x-cpp-variant-alias-name", lookupType);
                                    rewriteVariantAliasDefault(var, lookupType);
                                }
                            }
                        }
                    }
                }
            }
        }

        // Include discriminator-mapped models used by generated variant dispatch.
        // Without these includes, the conversion functions are undeclared.
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

    private static void rewriteVariantAliasDefault(
            CodegenProperty property, String aliasName) {
        Object scalarDefault = property.vendorExtensions.get(
                X_CPP_EXPLICIT_DEFAULT_SCALAR);
        if (!(scalarDefault instanceof String)
                || Boolean.TRUE.equals(property.vendorExtensions.get(
                "x-cpp-default-is-null"))) {
            return;
        }

        String decodedDefault = "fromJsonValue_" + aliasName
                + "(boost::json::value(" + scalarDefault + "))";
        Object nullableInner = property.vendorExtensions.get(
                "x-cpp-nullable-field-inner-type");
        if (nullableInner != null) {
            decodedDefault = "NullableField<" + nullableInner
                    + ">::makeDefaultValue(" + decodedDefault + ")";
        }
        property.defaultValue = decodedDefault;
        property.vendorExtensions.put("x-cpp-member-default", true);
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
            // Descriptor-complete path: when composedSchemas were consumed by
            // fromModel before we could access them, use the CompositionDescriptor
            // built in preprocessOpenAPI to reconstruct branch metadata and
            // perform lowering.
            CompositionDescriptor desc = compositionDescriptors.get(cm.classname);
            if (desc == null || "allOf".equals(desc.getKeyword())) {
                return; // allOf models handled separately in postProcessModels
            }
            processComposedModelFromDescriptor(cm, desc);
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
            // Fall through to descriptor path when oneOf/anyOf branches were
            // consumed by the default pipeline but a composition descriptor
            // still exists (e.g., all branches were self-references or the
            // schema uses composedSchemas for allOf only).
            CompositionDescriptor desc = compositionDescriptors.get(cm.classname);
            if (desc != null && !"allOf".equals(desc.getKeyword())) {
                processComposedModelFromDescriptor(cm, desc);
            }
            return;
        }

        // Look up the composition descriptor as the semantic source for lowering.
        // When available, descriptor metadata (null capability, assertions, keyword)
        // is used by lowerComposedTypes instead of inferring semantics from C++ type
        // strings alone.
        CompositionDescriptor descriptor = compositionDescriptors.get(cm.classname);

        // Collect C++ branch types (strip shared_ptr wrappers for variant members).
        // Map OpenAPI type names (e.g., "null", "integer", "string") to C++ types
        // because composed properties from fromProperty use OpenAPI type names as-is.
        // Self-referencing branches (a variant containing itself) are excluded
        // because they would create an illegal recursive type alias in C++.
        // Binary branches (format: binary) are mapped to std::vector<std::uint8_t>
        // so the multipart addVariantFormParameter helper can dispatch them as
        // file parts via compile-time type checking.
        // Deduplicate in lowerComposedTypes so oneOf retains branch identity when
        // identical C++ types represent distinct schemas.
        //
        // Track originalBranchIndex (bi) for descriptor alignment after
        // self-referencing branches are filtered out.
        List<ComposedBranch> composedBranches = new ArrayList<>();
        for (int bi = 0; bi < branches.size(); bi++) {
            CodegenProperty b = branches.get(bi);
            String cppType;
            if (b.isBinary || b.isFile) {
                cppType = "std::vector<std::uint8_t>";
            } else {
                String rawType = stripSharedPtr(b.dataType);
                if (rawType == null || "null".equals(rawType)) {
                    cppType = "std::nullptr_t";
                } else {
                    cppType = resolveOpenApiTypeName(rawType);
                }
            }
            if (cppType != null && cppType.equals(cm.classname)) {
                continue;
            }
            boolean isStringLike = b.isString || "std::string".equals(cppType)
                    || "string".equals(b.dataType);
            composedBranches.add(new ComposedBranch(cppType, b.isEnum, isStringLike, bi));
        }
        List<String> branchTypes = composedBranches.stream()
                .map(cb -> cb.cppType)
                .collect(Collectors.toList());

        String resolvedType;
        try {
            resolvedType = Oas31CompositionLowering.lowerComposedTypes(
                    composedBranches, composedKeyword, descriptor, LOGGER::warn);
        } catch (RuntimeException e) {
            throw new IllegalStateException(
                    "Failed to lower composed model '" + cm.classname + "'", e);
        }

        // Cache the resolved type for transitive alias resolution.
        resolvedAliasTypes.put(cm.classname, resolvedType);

        // Record as variant model for getTypeDeclaration shared_ptr exclusion
        variantModels.add(cm.classname);

        // Emit vendor extensions consumed by Mustache templates
        cm.vendorExtensions.put("x-cpp-type", resolvedType);
        cm.vendorExtensions.put("x-cpp-branches", branchTypes);
        cm.vendorExtensions.put("x-cpp-composed-keyword", composedKeyword);
        composedKeywordsByModel.put(cm.classname, composedKeyword);

        // Populate each descriptor branch's storage type and expose duplicate
        // alternatives so templates generate CompositionBranchValue accessors.
        boolean hasDuplicateTypes = hasTaggedCompositionBranches(resolvedType);
        if (descriptor != null) {
            Map<String, Object> templateMap = descriptor.toTemplateMap();
            @SuppressWarnings("unchecked")
            var templateBranches = (List<Map<String, Object>>) templateMap.get("branches");
            for (int bi = 0; bi < composedBranches.size(); bi++) {
                ComposedBranch cb = composedBranches.get(bi);
                int descIdx = cb.originalBranchIndex;
                if (descIdx >= 0 && descIdx < templateBranches.size()) {
                    Map<String, Object> tBranch = templateBranches.get(descIdx);
                    String storageType;
                    if (hasDuplicateTypes) {
                        storageType = "CompositionBranchValue<" + descIdx
                                + ", " + cb.cppType + ">";
                        tBranch.put("inner-cpp-type", cb.cppType);
                    } else {
                        storageType = cb.cppType;
                    }
                    tBranch.put("storage-cpp-type", storageType);
                }
            }
            templateMap.put("has-duplicate-types", hasDuplicateTypes);
            cm.vendorExtensions.put("x-cpp-composition-branches", templateMap);
            if (hasDuplicateTypes) {
                cm.vendorExtensions.put("x-cpp-has-duplicate-types", true);
            }
        } else {
            // Fallback: build branch maps from the composed branches when no
            // precomputed descriptor exists (e.g., inline schemas not in the
            // component schema index).
            List<Map<String, Object>> fallbackBranches = new ArrayList<>();
            for (int bi = 0; bi < composedBranches.size(); bi++) {
                ComposedBranch cb = composedBranches.get(bi);
                Map<String, Object> branchMap = new LinkedHashMap<>();
                branchMap.put("branch-index", bi);
                branchMap.put("source-schema-ref", null);
                branchMap.put("resolved-schema-name", cb.cppType);
                String storageType = hasDuplicateTypes
                        ? "CompositionBranchValue<" + bi + ", " + cb.cppType + ">"
                        : cb.cppType;
                branchMap.put("storage-cpp-type", storageType);
                if (hasDuplicateTypes) {
                    branchMap.put("inner-cpp-type", cb.cppType);
                }
                branchMap.put("validator-id", null);
                branchMap.put("null-capability",
                        "std::nullptr_t".equals(cb.cppType) ? "always" : "never");
                fallbackBranches.add(branchMap);
            }
            Map<String, Object> fallbackMap = new LinkedHashMap<>();
            fallbackMap.put("schema-name", cm.classname);
            fallbackMap.put("schema-location", null);
            fallbackMap.put("keyword", composedKeyword);
            fallbackMap.put("branches", fallbackBranches);
            fallbackMap.put("has-duplicate-types", hasDuplicateTypes);
            cm.vendorExtensions.put("x-cpp-composition-branches", fallbackMap);
            if (hasDuplicateTypes) {
                cm.vendorExtensions.put("x-cpp-has-duplicate-types", true);
            }
        }

        // Preserve enum identity for the later alias-resolution pass: open strings
        // and string enums both lower to std::string, so the C++ type alone cannot
        // detect overlap.
        List<Boolean> branchIsEnumFlags = composedBranches.stream()
                .map(cb -> cb.isEnum)
                .collect(Collectors.toList());
        cm.vendorExtensions.put("x-cpp-branch-is-enum", branchIsEnumFlags);
        // Preserve descriptor indices when self-referential branches are filtered.
        List<Integer> branchOriginalIndices = composedBranches.stream()
                .map(cb -> cb.originalBranchIndex)
                .collect(Collectors.toList());
        cm.vendorExtensions.put("x-cpp-branch-original-index", branchOriginalIndices);

        if (cm.discriminator != null) {
            cm.vendorExtensions.put("x-has-discriminator", true);
            cm.vendorExtensions.put("x-discriminator-property", cm.discriminator.getPropertyBaseName());
            cm.vendorExtensions.put("x-discriminator-mapping", cm.discriminator.getMapping());
            // Store the resolved type until all aliases are available for
            // discriminator self-reference filtering.
            cm.vendorExtensions.put("x-discriminator-resolved-type", resolvedType);

            // Build discriminator-value to branch-index metadata for diagnostic
            // ordering. Self-referential mappings are omitted.
            if (cm.discriminator != null && cm.discriminator.getMappedModels() != null
                    && !cm.discriminator.getMappedModels().isEmpty()
                    && descriptor != null) {
                // Filter out self-referential MappedModel entries
                Set<CodegenDiscriminator.MappedModel> filtered = new LinkedHashSet<>();
                for (CodegenDiscriminator.MappedModel mm : cm.discriminator.getMappedModels()) {
                    if (mm.getModelName() == null || !mm.getModelName().equals(cm.classname)) {
                        filtered.add(mm);
                    }
                }
                if (!filtered.isEmpty()) {
                    List<Map<String, Object>> discBranchIndex =
                            Oas31CompositionLowering.buildDiscriminatorBranchIndex(
                            filtered, descriptor.getBranches());
                    if (!discBranchIndex.isEmpty()) {
                        cm.vendorExtensions.put("x-discriminator-branch-index", discBranchIndex);
                        cm.vendorExtensions.put("x-has-discriminator-branch-index", true);
                    }
                }
            } else if (descriptor != null && descriptor.hasDiscriminator()) {
                // Fallback: use explicit descriptor mapping when MappedModel unavailable
                List<Map<String, Object>> discBranchIndex =
                        Oas31CompositionLowering.buildDiscriminatorBranchIndex(
                        descriptor.getDiscriminator().getMapping(),
                        descriptor.getBranches());
                if (!discBranchIndex.isEmpty()) {
                    cm.vendorExtensions.put("x-discriminator-branch-index", discBranchIndex);
                    cm.vendorExtensions.put("x-has-discriminator-branch-index", true);
                }
            }
        }

        // Update data type so templates and references use the resolved type
        cm.dataType = resolvedType;
    }

    /**
     * Descriptor-complete path: process a composed model whose composedSchemas
     * were consumed by fromModel, using only the descriptor metadata.
     * Reconstructs ComposedBranch entries from descriptor branch schema names,
     * resolves C++ types, then runs the same lowering/emission pipeline as
     * the normal composedSchemas path.
     */
    private void processComposedModelFromDescriptor(CodegenModel cm,
                                                     CompositionDescriptor desc) {
        List<ComposedBranch> composedBranches = new ArrayList<>();
        List<CompositionBranchDescriptor> descBranches = desc.getBranches();

        for (int bi = 0; bi < descBranches.size(); bi++) {
            CompositionBranchDescriptor db = descBranches.get(bi);
            String resolvedSchemaName = db.getResolvedSchemaName();
            String cppType = resolveOpenApiTypeName(resolvedSchemaName);

            // Skip self-referencing branches
            if (cppType != null && cppType.equals(cm.classname)) {
                continue;
            }
            if (cppType == null) {
                cppType = resolvedSchemaName;
            }
            // Skip self-referencing after fallback
            if (cppType.equals(cm.classname)) {
                continue;
            }

            // Determine isEnum from descriptor assertion metadata
            boolean isEnum = db.getSupportedAssertions().contains("enum");
            boolean isStringLike = "std::string".equals(cppType);
            composedBranches.add(new ComposedBranch(cppType, isEnum, isStringLike, bi));
        }

        List<String> branchTypes = composedBranches.stream()
                .map(cb -> cb.cppType)
                .collect(Collectors.toList());

        String resolvedType;
        try {
            resolvedType = Oas31CompositionLowering.lowerComposedTypes(
                    composedBranches, desc.getKeyword(), desc, LOGGER::warn);
        } catch (RuntimeException e) {
            throw new IllegalStateException(
                    "Failed to lower descriptor-backed model '" + cm.classname + "'", e);
        }

        // Cache the resolved type
        resolvedAliasTypes.put(cm.classname, resolvedType);
        variantModels.add(cm.classname);

        // Populate descriptor storage types, including duplicate-type wrappers.
        boolean hasDuplicateTypes = hasTaggedCompositionBranches(resolvedType);
        Map<String, Object> descTemplateMap = desc.toTemplateMap();
        {
            @SuppressWarnings("unchecked")
            var templateBranches = (List<Map<String, Object>>) descTemplateMap.get("branches");
            // When hasDuplicateTypes, all branches (including null) get
            // CompositionBranchValue wrapping — match shortcut behavior.
            for (int bi = 0; bi < composedBranches.size(); bi++) {
                ComposedBranch cb = composedBranches.get(bi);
                int descIdx = cb.originalBranchIndex;
                if (descIdx >= 0 && descIdx < templateBranches.size()) {
                    Map<String, Object> tBranch = templateBranches.get(descIdx);
                    String storageType;
                    if (hasDuplicateTypes) {
                        storageType = "CompositionBranchValue<" + descIdx
                                + ", " + cb.cppType + ">";
                        tBranch.put("inner-cpp-type", cb.cppType);
                    } else {
                        storageType = cb.cppType;
                    }
                    tBranch.put("storage-cpp-type", storageType);
                }
            }
        }
        descTemplateMap.put("has-duplicate-types", hasDuplicateTypes);

        // Emit vendor extensions
        cm.vendorExtensions.put("x-cpp-type", resolvedType);
        cm.vendorExtensions.put("x-cpp-branches", branchTypes);
        cm.vendorExtensions.put("x-cpp-composed-keyword", desc.getKeyword());
        composedKeywordsByModel.put(cm.classname, desc.getKeyword());
        cm.vendorExtensions.put("x-cpp-composition-branches", descTemplateMap);
        if (hasDuplicateTypes) {
            cm.vendorExtensions.put("x-cpp-has-duplicate-types", true);
        }

        // Preserve branch metadata for transitive alias resolution.
        List<Boolean> branchIsEnumFlags = composedBranches.stream()
                .map(cb -> cb.isEnum)
                .collect(Collectors.toList());
        cm.vendorExtensions.put("x-cpp-branch-is-enum", branchIsEnumFlags);
        List<Integer> branchOriginalIndices = composedBranches.stream()
                .map(cb -> cb.originalBranchIndex)
                .collect(Collectors.toList());
        cm.vendorExtensions.put("x-cpp-branch-original-index", branchOriginalIndices);

        if (desc.hasDiscriminator()) {
            cm.vendorExtensions.put("x-has-discriminator", true);
            cm.vendorExtensions.put("x-discriminator-property",
                    desc.getDiscriminator().getPropertyName());
            cm.vendorExtensions.put("x-discriminator-mapping",
                    desc.getDiscriminator().getMapping());
            cm.vendorExtensions.put("x-discriminator-resolved-type", resolvedType);

            // Prefer complete mapped-model metadata for discriminator ordering and
            // fall back to explicit descriptor mappings. Omit self-references.
            if (cm.discriminator != null && cm.discriminator.getMappedModels() != null
                    && !cm.discriminator.getMappedModels().isEmpty()) {
                // Filter out self-referential MappedModel entries
                Set<CodegenDiscriminator.MappedModel> filtered = new LinkedHashSet<>();
                for (CodegenDiscriminator.MappedModel mm : cm.discriminator.getMappedModels()) {
                    if (mm.getModelName() == null || !mm.getModelName().equals(cm.classname)) {
                        filtered.add(mm);
                    }
                }
                if (!filtered.isEmpty()) {
                    List<Map<String, Object>> discBranchIndex =
                            Oas31CompositionLowering.buildDiscriminatorBranchIndex(
                            filtered, descBranches);
                    if (!discBranchIndex.isEmpty()) {
                        cm.vendorExtensions.put("x-discriminator-branch-index", discBranchIndex);
                        cm.vendorExtensions.put("x-has-discriminator-branch-index", true);
                    }
                }
            } else if (desc.hasDiscriminator()) {
                // Fallback: use explicit descriptor mapping when MappedModel unavailable
                List<Map<String, Object>> discBranchIndex =
                        Oas31CompositionLowering.buildDiscriminatorBranchIndex(
                        desc.getDiscriminator().getMapping(),
                        descBranches);
                if (!discBranchIndex.isEmpty()) {
                    cm.vendorExtensions.put("x-discriminator-branch-index", discBranchIndex);
                    cm.vendorExtensions.put("x-has-discriminator-branch-index", true);
                }
            }
        }

        cm.dataType = resolvedType;
    }

    /** Branch metadata used by ordered composition lowering. */
    static final class ComposedBranch {
        final String cppType;
        final boolean isEnum;
        final boolean isStringLike;
        /** Index into the CompositionDescriptor branch list.
         *  -1 means no descriptor alignment (fallback path). */
        final int originalBranchIndex;

        ComposedBranch(String cppType, boolean isEnum, boolean isStringLike,
                       int originalBranchIndex) {
            this.cppType = cppType;
            this.isEnum = isEnum;
            this.isStringLike = isStringLike;
            this.originalBranchIndex = originalBranchIndex;
        }
    }

    private List<Map<String, Object>> buildTypeErasedOneOfBranches(
            CodegenModel codegenModel, Map<String, CodegenModel> allModels) {
        List<Map<String, Object>> validationBranches = new ArrayList<>();
        Object branchMetadata = codegenModel.vendorExtensions.get(
                "x-cpp-composition-branches");
        Object templateBranches = branchMetadata instanceof Map
                ? ((Map<?, ?>) branchMetadata).get("branches") : null;
        int branchIndex = 0;
        for (CodegenProperty branch : codegenModel.getComposedSchemas().getOneOf()) {
            String originalType = stripSharedPtr(branch.dataType);
            CodegenModel referencedModel = allModels.get(originalType);
            String resolvedType = resolveThroughAliases(originalType);
            if (referencedModel != null && referencedModel.dataType != null) {
                resolvedType = resolveThroughAliases(stripSharedPtr(referencedModel.dataType));
            }
            resolvedType = resolveOpenApiTypeName(resolvedType);

            Map<String, Object> validationBranch = new LinkedHashMap<>();
            String validatorId = validatorIdAt(templateBranches, branchIndex);
            if (validatorId != null) {
                // Type erasure is safe only when the original branch validator
                // remains available to distinguish its full assertion surface.
                validationBranch.put("validator-id", validatorId);
                validationBranch.put("has-validator-id", true);
            }
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
            branchIndex++;
        }
        return validationBranches;
    }

    private static String validatorIdAt(Object templateBranches, int branchIndex) {
        if (!(templateBranches instanceof List) || branchIndex < 0
                || branchIndex >= ((List<?>) templateBranches).size()) {
            return null;
        }
        Object branch = ((List<?>) templateBranches).get(branchIndex);
        if (!(branch instanceof Map)) {
            return null;
        }
        Object validatorId = ((Map<?, ?>) branch).get("validator-id");
        return validatorId instanceof String && !((String) validatorId).isEmpty()
                ? (String) validatorId : null;
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

    static String escapeCppStringContent(String value) {
        if (value == null) {
            return "";
        }
        for (int index = 0; index < value.length(); ++index) {
            char constUnit = value.charAt(index);
            if (Character.isHighSurrogate(constUnit)) {
                if (index + 1 >= value.length()
                        || !Character.isLowSurrogate(value.charAt(index + 1))) {
                    throw new IllegalArgumentException(
                            "Cannot emit an unpaired UTF-16 high surrogate");
                }
                ++index;
            } else if (Character.isLowSurrogate(constUnit)) {
                throw new IllegalArgumentException(
                        "Cannot emit an unpaired UTF-16 low surrogate");
            }
        }

        byte[] utf8 = value.getBytes(java.nio.charset.StandardCharsets.UTF_8);
        StringBuilder escaped = new StringBuilder(utf8.length);
        for (byte encoded : utf8) {
            int character = Byte.toUnsignedInt(encoded);
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
                    if (character >= 0x20 && character <= 0x7e) {
                        escaped.append((char) character);
                    } else {
                        // Three-digit octal escapes cannot absorb following
                        // hexadecimal characters and preserve exact UTF-8 bytes.
                        escaped.append('\\')
                                .append((char) ('0' + ((character >>> 6) & 7)))
                                .append((char) ('0' + ((character >>> 3) & 7)))
                                .append((char) ('0' + (character & 7)));
                    }
                    break;
            }
        }
        return escaped.toString();
    }

    protected static String toPreprocessorIdentifier(String value) {
        String sanitized = value.replaceAll("[^A-Za-z0-9_]", "_");
        if (!sanitized.isEmpty() && Character.isDigit(sanitized.charAt(0))) {
            return "_" + sanitized;
        }
        return sanitized.isEmpty() ? "_" : sanitized;
    }

    /**
     * Converts an arbitrary schema name into a valid C++ identifier for use
     * in generated validator function names. Replaces non-alphanumeric
     * characters with underscores and ensures the result starts with a letter.
     */
    static String toValidIdentifier(String name) {
        if (name == null || name.isEmpty()) {
            return "schema";
        }
        StringBuilder sb = new StringBuilder(name.length());
        for (int i = 0; i < name.length(); i++) {
            char c = name.charAt(i);
            if (Character.isLetterOrDigit(c) || c == '_') {
                sb.append(c);
            } else {
                sb.append('_');
            }
        }
        String result = sb.toString();
        if (!result.isEmpty() && !Character.isLetter(result.charAt(0))
                && result.charAt(0) != '_') {
            result = "_" + result;
        }
        return result.isEmpty() ? "schema" : result;
    }

    /** Returns unique schema IR ids for raw component schema names. */
    static Map<String, String> componentSchemaIds(Collection<String> schemaNames) {
        List<String> names = new ArrayList<>(schemaNames);
        Collections.sort(names);
        Map<String, List<String>> namesByBase = new LinkedHashMap<>();
        for (String name : names) {
            namesByBase.computeIfAbsent(toValidIdentifier(name), ignored -> new ArrayList<>())
                    .add(name);
        }

        Map<String, String> ids = new LinkedHashMap<>();
        for (Map.Entry<String, List<String>> entry : namesByBase.entrySet()) {
            String base = entry.getKey() + "_component";
            List<String> collidingNames = entry.getValue();
            if (collidingNames.size() == 1) {
                ids.put(collidingNames.get(0), base);
                continue;
            }
            for (int index = 0; index < collidingNames.size(); index++) {
                ids.put(collidingNames.get(index), base + "_" + (index + 1));
            }
        }
        return ids;
    }

    /** Returns the schema IR id for a raw component schema name. */
    static String componentSchemaId(String schemaName, Map<String, String> ids) {
        String id = ids.get(schemaName);
        return id != null ? id : toValidIdentifier(schemaName) + "_component";
    }

    /**
     * Thrown during generation when a schema branch has assertion keywords that
     * can affect composition membership but no generated validator exists.
     * Carries the schema location, keyword, and remediation guidance.
     */
    public static final class UnsupportedSchemaAssertionException
            extends RuntimeException {
        private final String schemaLocation;
        private final String assertionKeyword;

        public UnsupportedSchemaAssertionException(
                String schemaLocation, String assertionKeyword) {
            super(buildMessage(schemaLocation, assertionKeyword));
            this.schemaLocation = schemaLocation;
            this.assertionKeyword = assertionKeyword;
        }

        public String getSchemaLocation() { return schemaLocation; }
        public String getAssertionKeyword() { return assertionKeyword; }

        private static String buildMessage(
                String schemaLocation, String assertionKeyword) {
            return "Unsupported schema assertion '" + assertionKeyword
                    + "' at " + schemaLocation
                    + ". This keyword can affect composition membership but "
                    + "no generated validator exists. Add support in a later generator "
                    + "version, or restructure the schema to avoid this keyword.";
        }
    }

    /**
     * Exception thrown when an allOf intersection produces an unsatisfiable
     * result on a required property, preventing model generation.
     */
    public static final class AllOfRequiredUnsatisfiableException
            extends RuntimeException {
        private final String schemaName;
        private final String reason;

        public AllOfRequiredUnsatisfiableException(
                String schemaName, String reason) {
            super(buildMessage(schemaName, reason));
            this.schemaName = schemaName;
            this.reason = reason;
        }

        public String getSchemaName() { return schemaName; }
        public String getReason() { return reason; }

        private static String buildMessage(
                String schemaName, String reason) {
            return "Unsatisfiable allOf intersection for schema '"
                    + schemaName + "': " + reason;
        }
    }

    /**
     * Resolves a type name transitively through the resolvedAliasTypes map.
     * For example, if ModelIdsResponses → std::string and ModelIdsShared → std::string,
     * then resolveThroughAliases("ModelIdsResponses") returns "std::string".
     * <p>
     * Cyclic alias maps fail generation; an unmapped type is returned unchanged.
     */
    protected String resolveThroughAliases(String typeName) {
        if (typeName == null) {
            return null;
        }
        Set<String> visited = new HashSet<>();
        String current = typeName;
        while (true) {
            String resolved = resolvedAliasTypes.get(current);
            if (resolved == null || resolved.equals(current)) {
                return current;
            }
            if (!visited.add(current)) {
                throw new IllegalStateException(
                        "Cyclic resolved alias chain starting at '" + typeName + "'");
            }
            current = resolved;
        }
    }

    /**
     * Detects whether a schema is a null union (anyOf/oneOf with [T, null] or [null, T])
     * that should lower to std::optional&lt;T&gt;. Returns the lowered type string,
     * or null if the schema is not a simple null union.
     */
    protected String detectNullUnion(Schema schema, String className) {
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
    protected static String stripSharedPtr(String type) {
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
     * Extracts the inner type from a std::optional<T> type declaration, correctly
     * handling nested angle brackets.
     * <ul>
     *   <li>{@code std::optional<std::string>} → {@code std::string}</li>
     *   <li>{@code std::optional<std::vector<int>>} → {@code std::vector<int>}</li>
     *   <li>{@code std::optional<MyModel>} → {@code MyModel}</li>
     *   <li>{@code std::string} → {@code null}</li>
     * </ul>
     *
     * @return the inner type, or null if the input does not start with "std::optional<"
     */
    private static String extractOptionalInnerType(String type) {
        if (type == null || !type.startsWith("std::optional<")) {
            return null;
        }
        // Strip prefix "std::optional<" (14 chars) and find matching '>'
        int depth = 0;
        int start = 14; // length of "std::optional<"
        for (int i = start; i < type.length(); i++) {
            char c = type.charAt(i);
            if (c == '<') {
                depth++;
            } else if (c == '>') {
                if (depth == 0) {
                    return type.substring(start, i);
                }
                depth--;
            }
        }
        return null;
    }
}
