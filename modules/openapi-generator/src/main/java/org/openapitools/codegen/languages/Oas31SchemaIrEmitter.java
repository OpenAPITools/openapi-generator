package org.openapitools.codegen.languages;

import io.swagger.v3.core.util.Json;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

/**
 * Emits the densified OAS 3.1 schema IR (Oas31SchemaRegistry.h,
 * schema_ir.generated.cpp, and optional schema_ir.generated.chunk*.cpp files)
 * for the cpp-boost-beast client: every composition branch, extracted component,
 * and structural child is densified into a flat SchemaNode registry against which
 * the generated C++ evaluator validates instances exactly (original numeric
 * lexemes, deep JSON enum/const stores, dynamic-scope markers, annotation
 * keywords).
 *
 * <p>All facts were collected earlier into
 * {@link Oas31CompositionLowering.CompositionDescriptor}s (per-branch
 * validateParams) and the post-model-extraction components map; this emitter
 * never re-parses the spec. Main rows (one per branch) use direct
 * {@code SchemaEvaluator} lookup; child and component rows are flattened after
 * them so main-node indices stay stable. The {@code x-oas31-*} recovery
 * extensions of {@link Oas31RawSpecRecovery} are honoured here (count-bound
 * lexemes, pristine enum JSON and type-null markers, dependentRequired maps).
 */
final class Oas31SchemaIrEmitter {
    // Spread large registries across a bounded number of compiler inputs.
    private static final int TARGET_SCHEMA_IR_NODES_PER_SOURCE = 512;
    private static final int MAX_SCHEMA_IR_CHUNKS = 16;


    /** OpenAPI document this emit pass densifies. */
    private final OpenAPI openAPI;
    /** Branch descriptors built by preprocessOpenAPI (template-facing maps
     *  live on the codegen, not here). */
    private final Map<String, Oas31CompositionLowering.CompositionDescriptor> compositionDescriptors;
    /** $dynamicAnchor registrations collected and consumed within this emit pass. */
    private final Map<String, DynamicAnchorReg> dynamicAnchorRegs = new LinkedHashMap<>();
    /** Real component names collected within this emit pass. */
    private final Set<String> oasComponentNames = new HashSet<>();
    /** Codegen model access (reads the oas31BaseUri option). */
    private final Map<String, Object> additionalProperties;
    /** Component validator IDs finalized after inline-model extraction. */
    private final Map<String, String> componentSchemaIds;


    // Component composition snapshot captured after model extraction to select
    // the correct ref-target row form.
    private final Map<String, Boolean> irComponentComposed = new HashMap<>();
    // Synthetic resource IDs whose dialect omits the validation vocabulary.
    private final Set<Integer> vocabInertResources = new TreeSet<>();

    Oas31SchemaIrEmitter(
            OpenAPI openAPI,
            Map<String, Oas31CompositionLowering.CompositionDescriptor> compositionDescriptors,
            Map<String, Object> additionalProperties,
            Map<String, String> componentSchemaIds) {
        this.openAPI = openAPI;
        this.compositionDescriptors = compositionDescriptors;
        this.additionalProperties = additionalProperties;
        this.componentSchemaIds = componentSchemaIds == null
                ? Collections.emptyMap() : componentSchemaIds;
    }

    /** Materializes annotation fields from branch validation parameters. */
    private void readAnnotationVp(
            Map<String, Object> vp, IrNode n) {
        if (vp == null) return;
        n.annTitle = strOf(vp.get("validation-ann-title"));
        n.annDescription = strOf(vp.get("validation-ann-description"));
        n.annDefaultJson = strOf(vp.get("validation-ann-default"));
        n.annExamplesJson = strOf(vp.get("validation-ann-examples"));
        n.annDeprecatedJson = strOf(vp.get("validation-ann-deprecated"));
        n.annReadOnlyJson = strOf(vp.get("validation-ann-readOnly"));
        n.annWriteOnlyJson = strOf(vp.get("validation-ann-writeOnly"));
        n.annFormat = strOf(vp.get("validation-ann-format"));
        n.annContentEncoding = strOf(vp.get("validation-ann-contentEncoding"));
        n.annContentMediaType = strOf(vp.get("validation-ann-contentMediaType"));
        n.annContentSchemaJson = strOf(vp.get("validation-ann-contentSchema"));
        n.annComment = strOf(vp.get("validation-ann-comment"));
        if ("TRUE".equals(String.valueOf(vp.get(
                "validation-ann-comment-shape-violation")))) {
            n.annCommentShapeViolation = true;
        }
        for (Map.Entry<?, ?> e : vp.entrySet()) {
            if (String.valueOf(e.getKey()).startsWith("validation-ann-extra:")) {
                n.annExtras.add(new java.util.AbstractMap.SimpleImmutableEntry<>(
                        String.valueOf(e.getKey())
                                .substring("validation-ann-extra:".length()),
                        String.valueOf(e.getValue())));
            }
        }
    }

    private static String strOf(Object o) {
        return o == null ? "" : String.valueOf(o);
    }

    /** Reads annotations directly from a structural or component schema row. */
    private void readAnnotationRaw(
            io.swagger.v3.oas.models.media.Schema schema, IrNode n) {
        if (schema == null) return;
        final IrNode node = n;
        Oas31SchemaSurfaceAssertionScanner.readAnnotationKeywords(schema, (key, value) -> {
            switch (key) {
                case "title":
                    node.annTitle = String.valueOf(value);
                    break;
                case "description":
                    node.annDescription = String.valueOf(value);
                    break;
                case "default":
                    node.annDefaultJson = String.valueOf(value);
                    break;
                case "examples":
                    node.annExamplesJson = String.valueOf(value);
                    break;
                case "deprecated":
                    node.annDeprecatedJson = String.valueOf(value);
                    break;
                case "readOnly":
                    node.annReadOnlyJson = String.valueOf(value);
                    break;
                case "writeOnly":
                    node.annWriteOnlyJson = String.valueOf(value);
                    break;
                case "format":
                    node.annFormat = String.valueOf(value);
                    break;
                case "contentEncoding":
                    node.annContentEncoding = String.valueOf(value);
                    break;
                case "contentMediaType":
                    node.annContentMediaType = String.valueOf(value);
                    break;
                case "contentSchema":
                    node.annContentSchemaJson = String.valueOf(value);
                    break;
                case "comment":
                    node.annComment = String.valueOf(value);
                    break;
                case "comment-shape-violation":
                    node.annCommentShapeViolation = true;
                    break;
                default:
                    if (key.startsWith("extra:")) {
                        node.annExtras.add(
                                new java.util.AbstractMap.SimpleImmutableEntry<>(
                                        key.substring("extra:".length()),
                                        String.valueOf(value)));
                    }
            }
        });
    }

    /**
     * Densify the current components map + branch descriptors into the combined
     * SchemaNode registry and its generated header, coordinator, optional source
     * chunks, and validation dispatch.
     */
    Map<String, Object> produce(Map<String, Object> objs) {
        // Snapshot post-extraction components so raw-schema rows can distinguish
        // composed targets from plain extracted targets. InlineModelResolver may
        // replace a schema subtree with a component ref; densifying the current
        // component content restores the moved semantics.
        irComponentComposed.clear();
        if (openAPI != null && openAPI.getComponents() != null
                && openAPI.getComponents().getSchemas() != null) {
            for (String name : openAPI.getComponents().getSchemas().keySet()) {
                Schema compSchema = openAPI.getComponents().getSchemas().get(name);
                if (compSchema == null) continue;
                boolean composed = (compSchema.getOneOf() != null && !compSchema.getOneOf().isEmpty())
                        || (compSchema.getAnyOf() != null && !compSchema.getAnyOf().isEmpty())
                        || (compSchema.getAllOf() != null && !compSchema.getAllOf().isEmpty());
                irComponentComposed.put(name, composed);
            }
            // Dynamic-ref decoding must recognize real specification component
            // names while main rows are built. Otherwise wrapper refs are decoded
            // against an empty component set and degrade to static refs.
            oasComponentNames.addAll(
                    openAPI.getComponents().getSchemas().keySet());
        }

        List<IrNode> mainNodes = new ArrayList<>();
        for (Oas31CompositionLowering.CompositionDescriptor desc : compositionDescriptors.values()) {
            if (desc == null || desc.getBranches() == null) {
                continue;
            }
            for (Oas31CompositionLowering.CompositionBranchDescriptor branch : desc.getBranches()) {
                IrNode node = irNodeFromBranch(branch);
                if (node != null) {
                    assignSchemaPaths(node, "#/components/schemas/"
                            + jsonPointerToken(desc.getSchemaName()) + "/"
                            + desc.getKeyword() + "/" + branch.getBranchIndex());
                    mainNodes.add(node);
                }
            }
        }
        // Deterministic ordering by validate_<id> so output is stable across runs.
        mainNodes.sort(Comparator.comparing(n -> n.validatorId));

        // Flatten every structural child into extra registry rows after the main
        // validator rows, keeping main indices stable. Child rows have no direct
        // validate_<id> dispatch and are reached only through SchemaNode fields.
        // Breadth-first identity deduplication keeps row ordering deterministic.
        List<IrNode> extraNodes = new ArrayList<>();
        java.util.ArrayDeque<IrNode> queue = new java.util.ArrayDeque<>();
        java.util.Set<IrNode> visitedChildren = java.util.Collections.newSetFromMap(
                new java.util.IdentityHashMap<IrNode, Boolean>());

        // Inline-model extraction moves schema subtrees into components and leaves
        // refs at their original sites. Densify those components after the main
        // and structural-child rows so every ref resolves without disturbing main
        // indices. Composed components receive wrapper rows that retain the full
        // applicator rather than aliasing the first branch.
        List<IrNode> componentRows = new ArrayList<>();
        if (openAPI != null && openAPI.getComponents() != null
                && openAPI.getComponents().getSchemas() != null) {
            java.util.List<String> names = new ArrayList<>(
                    openAPI.getComponents().getSchemas().keySet());
            java.util.Collections.sort(names);
            for (String name : names) {
                // Only authored component names can carry the normalized
                // dynamic-reference wrapper convention.
                oasComponentNames.add(name);
                Schema compSchema = openAPI.getComponents().getSchemas().get(name);
                if (compSchema == null) continue;
                IrNode row = irNodeFromRawSchema(
                        compSchema, CppBoostBeastModelCodegen.componentSchemaId(
                                name, componentSchemaIds));
                if (row != null) {
                    assignSchemaPaths(row, "#/components/schemas/" + jsonPointerToken(name));
                    componentRows.add(row);
                }
            }
        }

        // Seed flattening from main nodes and component rows so all structural
        // children are present before references are resolved.
        java.util.List<IrNode> seeds = new ArrayList<>(mainNodes);
        seeds.addAll(componentRows);
        for (IrNode seed : seeds) {
            for (IrNode c : structuralChildren(seed)) {
                if (visitedChildren.add(c)) queue.add(c);
            }
        }
        while (!queue.isEmpty()) {
            IrNode c = queue.poll();
            extraNodes.add(c);
            for (IrNode g : structuralChildren(c)) {
                if (visitedChildren.add(g)) queue.add(g);
            }
        }
        visitedChildren.addAll(componentRows);
        visitedChildren.addAll(mainNodes);

        List<IrNode> allRows = new ArrayList<>(mainNodes);
        allRows.addAll(extraNodes);
        allRows.addAll(componentRows);

        // Identity-keyed index map over the COMBINED registry rows.
        java.util.Map<IrNode, Integer> indexOf = new java.util.IdentityHashMap<>();
        for (int i = 0; i < allRows.size(); i++) {
            indexOf.put(allRows.get(i), i);
        }
        // Resolve refs against main, structural-child, and component rows.
        java.util.Map<String, Integer> idIndex = new java.util.HashMap<>();
        for (int i = 0; i < allRows.size(); i++) {
            String vid = allRows.get(i).validatorId;
            if (vid != null) idIndex.putIfAbsent(vid, i);
        }

        // Resolve child and reference indices after every row is numbered.
        for (IrNode n : allRows) {
            if (n.notChild != null) {
                Integer idx = indexOf.get(n.notChild);
                if (idx != null) n.notSchemaIndex = idx;
            }
            if (n.additionalSchemaChild != null) {
                Integer idx = indexOf.get(n.additionalSchemaChild);
                if (idx != null) n.additionalSchemaIndex = idx;
            }
            if (n.itemsChild != null) {
                Integer idx = indexOf.get(n.itemsChild);
                if (idx != null) n.itemsIndex = idx;
            }
            if (n.unevaluatedSchemaChild != null) {
                Integer idx = indexOf.get(n.unevaluatedSchemaChild);
                if (idx != null) n.unevaluatedSchemaIndex = idx;
            }
            if (n.propertyNamesChild != null) {
                Integer idx = indexOf.get(n.propertyNamesChild);
                if (idx != null) n.propertyNamesIndex = idx;
            }
            for (IrNode.PatternSchema pb : n.patternProperties) {
                if (pb.child != null) {
                    Integer idx = indexOf.get(pb.child);
                    if (idx != null) pb.index = idx;
                }
            }
            for (IrNode.PropertySchema pb : n.properties) {
                if (pb.child != null) {
                    Integer idx = indexOf.get(pb.child);
                    if (idx != null) pb.index = idx;
                }
            }
            for (int i = 0; i < n.prefixItems.size(); i++) {
                Integer idx = indexOf.get(n.prefixItems.get(i));
                if (idx != null) n.prefixItemIndices.add(idx);
                else n.prefixItemIndices.add(-1);
            }
            for (int i = 0; i < n.applicatorChildren.size(); i++) {
                Integer idx = indexOf.get(n.applicatorChildren.get(i));
                if (idx != null) n.applicatorChildIndices.add(idx);
                else n.applicatorChildIndices.add(-1);
            }
            resolveChildList(n.allOfChildren, n.allOfChildIndices, indexOf);
            resolveChildList(n.anyOfChildren, n.anyOfChildIndices, indexOf);
            resolveChildList(n.oneOfChildren, n.oneOfChildIndices, indexOf);
            if (n.unevaluatedItemsSchemaChild != null) {
                Integer idx = indexOf.get(n.unevaluatedItemsSchemaChild);
                if (idx != null) n.unevaluatedItemsSchemaIndex = idx;
            }
            if (n.containsChild != null) {
                Integer idx = indexOf.get(n.containsChild);
                if (idx != null) n.containsIndex = idx;
            }
            if (n.ifChild != null) {
                Integer idx = indexOf.get(n.ifChild);
                if (idx != null) n.ifIndex = idx;
            }
            if (n.thenChild != null) {
                Integer idx = indexOf.get(n.thenChild);
                if (idx != null) n.thenIndex = idx;
            }
            if (n.elseChild != null) {
                Integer idx = indexOf.get(n.elseChild);
                if (idx != null) n.elseIndex = idx;
            }
            for (IrNode.DependentSchema d : n.dependentSchemas) {
                if (d.child != null) {
                    Integer idx = indexOf.get(d.child);
                    if (idx != null) d.index = idx;
                }
            }
            if (n.isRef && n.refTargetId != null) {
                Integer idx = idIndex.get(n.refTargetId);
                if (idx != null) {
                    n.refTargetIndex = idx;
                } else if (n.selfRef) {
                    n.refTargetIndex = indexOf.get(n).intValue();
                } else {
                    throw new CppBoostBeastClientCodegen.UnsupportedSchemaAssertionException(
                            n.validatorId, "unresolved $ref target '" + n.refTargetId + "'");
                }
            }
        }
        // Resolve dynamic-anchor registrations after row numbering is final.
        // Wrapper registrations target their first composed child.
        for (DynamicAnchorReg reg : dynamicAnchorRegs.values()) {
            if (reg.self) {
                Integer idx = indexOf.get(reg.node);
                reg.row = idx != null ? idx.intValue() : -1;
            } else {
                reg.row = (reg.node.oneOfChildIndices != null
                        && reg.node.oneOfChildIndices.size() > 0)
                        ? reg.node.oneOfChildIndices.get(0).intValue() : -1;
            }
        }

        vocabInertResources.clear();
        for (IrNode node : allRows) {
            if (node.dialectValidationInert) {
                vocabInertResources.add(node.dynamicResource);
            }
        }

        Oas31SchemaIrRenderer renderer = new Oas31SchemaIrRenderer(
                openAPI, additionalProperties, dynamicAnchorRegs, vocabInertResources);

        int chunkCount = schemaIrChunkCount(allRows.size());
        java.util.List<java.util.Map<String, String>> chunkFiles = new ArrayList<>();
        if (chunkCount == 0) {
            objs.put("oas31SchemaIrSource",
                    renderer.buildSchemaIrSource(allRows, mainNodes.size(), 0, allRows.size(), -1));
        } else {
            int chunkSize = (allRows.size() + chunkCount - 1) / chunkCount;
            for (int chunk = 0; chunk < chunkCount; chunk++) {
                int start = chunk * chunkSize;
                int end = Math.min(allRows.size(), start + chunkSize);
                objs.put("oas31SchemaIrChunk" + chunk + "Source",
                        renderer.buildSchemaIrSource(allRows, mainNodes.size(), start, end, chunk));
                java.util.Map<String, String> chunkFile = new LinkedHashMap<>();
                chunkFile.put("filename", schemaIrChunkFilename(chunk));
                chunkFiles.add(chunkFile);
            }
            objs.put("oas31SchemaIrSource",
                    renderer.buildSchemaIrCoordinatorSource(allRows, mainNodes.size(), chunkCount));
        }
        objs.put("oas31SchemaIrChunkCount", chunkCount);
        objs.put("oas31SchemaIrChunkFiles", chunkFiles);
        objs.put("oas31SchemaIrHeader", renderer.buildSchemaIrHeader(allRows));
        return objs;
    }

    private static int schemaIrChunkCount(int nodeCount) {
        if (nodeCount <= TARGET_SCHEMA_IR_NODES_PER_SOURCE) {
            return 0;
        }
        int required = (nodeCount + TARGET_SCHEMA_IR_NODES_PER_SOURCE - 1)
                / TARGET_SCHEMA_IR_NODES_PER_SOURCE;
        return Math.min(required, MAX_SCHEMA_IR_CHUNKS);
    }

    static String schemaIrChunkFilename(int chunk) {
        return "schema_ir.generated.chunk" + chunk + ".cpp";
    }

    static String schemaIrChunkTemplate(int chunk) {
        return "oas31_schema_ir_chunk" + chunk + ".mustache";
    }

    /** Ordered structural children of a node (BFS source, no duplicates). */
    private static java.util.List<IrNode> structuralChildren(IrNode n) {
        java.util.List<IrNode> out = new ArrayList<>();
        if (n.notChild != null) out.add(n.notChild);
        if (n.additionalSchemaChild != null) out.add(n.additionalSchemaChild);
        if (n.itemsChild != null) out.add(n.itemsChild);
        if (n.unevaluatedSchemaChild != null) out.add(n.unevaluatedSchemaChild);
        if (n.unevaluatedItemsSchemaChild != null) out.add(n.unevaluatedItemsSchemaChild);
        if (n.containsChild != null) out.add(n.containsChild);
        if (n.ifChild != null) out.add(n.ifChild);
        if (n.thenChild != null) out.add(n.thenChild);
        if (n.elseChild != null) out.add(n.elseChild);
        for (IrNode.DependentSchema d : n.dependentSchemas) {
            if (d.child != null) out.add(d.child);
        }
        if (n.propertyNamesChild != null) out.add(n.propertyNamesChild);
        for (IrNode.PatternSchema pb : n.patternProperties) {
            if (pb.child != null) out.add(pb.child);
        }
        out.addAll(n.prefixItems);
        out.addAll(n.applicatorChildren);
        out.addAll(n.allOfChildren);
        out.addAll(n.anyOfChildren);
        out.addAll(n.oneOfChildren);
        for (IrNode.PropertySchema pb : n.properties) {
            if (pb.child != null) out.add(pb.child);
        }
        return out;
    }

    /** Stamps each structural row with its authored JSON Pointer location. */
    private static void assignSchemaPaths(IrNode root, String rootPath) {
        java.util.Set<IrNode> visited = java.util.Collections.newSetFromMap(
                new java.util.IdentityHashMap<IrNode, Boolean>());
        assignSchemaPaths(root, rootPath, visited);
    }

    private static void assignSchemaPaths(
            IrNode node, String path, java.util.Set<IrNode> visited) {
        if (node == null || !visited.add(node)) {
            return;
        }
        node.schemaPath = path;
        assignSchemaPaths(node.notChild, path + "/not", visited);
        assignSchemaPaths(node.additionalSchemaChild,
                path + "/additionalProperties", visited);
        assignSchemaPaths(node.itemsChild, path + "/items", visited);
        assignSchemaPaths(node.unevaluatedSchemaChild,
                path + "/unevaluatedProperties", visited);
        assignSchemaPaths(node.unevaluatedItemsSchemaChild,
                path + "/unevaluatedItems", visited);
        assignSchemaPaths(node.containsChild, path + "/contains", visited);
        assignSchemaPaths(node.propertyNamesChild, path + "/propertyNames", visited);
        assignSchemaPaths(node.ifChild, path + "/if", visited);
        assignSchemaPaths(node.thenChild, path + "/then", visited);
        assignSchemaPaths(node.elseChild, path + "/else", visited);
        for (IrNode.PropertySchema child : node.properties) {
            assignSchemaPaths(child.child,
                    path + "/properties/" + jsonPointerToken(child.name), visited);
        }
        for (IrNode.PatternSchema child : node.patternProperties) {
            assignSchemaPaths(child.child,
                    path + "/patternProperties/" + jsonPointerToken(child.regex), visited);
        }
        for (int i = 0; i < node.prefixItems.size(); ++i) {
            assignSchemaPaths(node.prefixItems.get(i), path + "/prefixItems/" + i, visited);
        }
        assignIndexedSchemaPaths(node.allOfChildren, path + "/allOf", visited);
        assignIndexedSchemaPaths(node.anyOfChildren, path + "/anyOf", visited);
        assignIndexedSchemaPaths(node.oneOfChildren, path + "/oneOf", visited);
        for (IrNode.DependentSchema child : node.dependentSchemas) {
            assignSchemaPaths(child.child,
                    path + "/dependentSchemas/" + jsonPointerToken(child.name), visited);
        }
    }

    private static void assignIndexedSchemaPaths(
            java.util.List<IrNode> children,
            String path,
            java.util.Set<IrNode> visited) {
        for (int i = 0; i < children.size(); ++i) {
            assignSchemaPaths(children.get(i), path + "/" + i, visited);
        }
    }

    private static String jsonPointerToken(String value) {
        return value.replace("~", "~0").replace("/", "~1");
    }

    /** Resolve an IrNode child list to its combined-registry row indices. */
    private static void resolveChildList(java.util.List<IrNode> children,
                                         java.util.List<Integer> indices,
                                         java.util.Map<IrNode, Integer> indexOf) {
        for (IrNode c : children) {
            Integer idx = indexOf.get(c);
            indices.add(idx != null ? idx : -1);
        }
    }

    /**
     * A row of the $dynamicAnchor registration map. Entries are filled during
     * IR node building and resolved to concrete registry rows once numbering
     * is final (self=false entries target the wrapper's FIRST composed child
     * — the runner single-branch oneOf content).
     */
    static final class DynamicAnchorReg {
        final IrNode node;
        final int    resource;
        final String name;
        final boolean self;
        int row = -1;
        DynamicAnchorReg(IrNode node, int resource, String name, boolean self) {
            this.node = node; this.resource = resource; this.name = name; this.self = self;
        }
    }

    /** A single densified SchemaNode to emit, from one composition branch. */
    static final class IrNode {
        String validatorId;
        String resolvedName;
        int typeFlags = 0;
        boolean hasType = false;
        BooleanValueKind booleanValue = BooleanValueKind.NOT_BOOLEAN;
        String minimum = null;
        String maximum = null;
        String exclusiveMinimum = null;
        String exclusiveMaximum = null;
        String multipleOf = null;
        java.util.List<String> enumNumbers = new ArrayList<>();
        java.util.List<String> enumStrings = new ArrayList<>();
        java.util.List<String> enumBooleans = new ArrayList<>();
        String constNumber = null;
        String constString = null;
        Boolean constBool = null;
        boolean hasConst = false;

        // -- Exact equality, not, uniqueItems, and reference state --
        String constJson = null;        // serialized JSON literal for the FULL const value
        String enumJson = null;         // serialized JSON array literal for ALL enum members
        boolean hasUniqueItems = false;
        boolean uniqueItemsSeen = false;  // keyword PRESENT (true OR false) — false is a no-op
        IrNode  notChild = null;        // child row for a `not` subschema
        int     notSchemaIndex = -1;    // resolved combined-registry index of notChild
        boolean isRef = false;          // this row references another component
        String  refTargetId = null;     // validatorId of the ref target
        int     refTargetIndex = -1;    // resolved combined-registry index; -1 => unresolved (inline)
        // -- Dynamic scope ($dynamicRef / $dynamicAnchor) --
        // Normalized resource markers assign scope identities and static fallbacks.
        // A resource-root row pushes a scope frame during validation.
        int dynamicResource = 0;
        boolean resourceRoot = false;
        String dynamicAnchorName = null;
        String dynamicRefAnchor = null;
        // Validation keywords are inert in resources whose dialect omits the
        // validation vocabulary.
        boolean dialectValidationInert = false;

        // -- Object structure --
        static final class PropertySchema {
            String name;
            IrNode  child;
            int     index = -1;   // resolved registry row of child
        }
        boolean hasObjectSchema = false;
        java.util.List<PropertySchema>  properties = new ArrayList<>();
        java.util.List<String>          required = new ArrayList<>();
        String            additionalPropertiesKind = "absent";  // absent|allowed|reject|schema
        IrNode            additionalSchemaChild = null;
        int               additionalSchemaIndex = -1;
        String            minPropertiesLexeme = null;   boolean minPropertiesPresent = false;
        String            maxPropertiesLexeme = null;   boolean maxPropertiesPresent = false;

        // -- String constraints --
        String minLengthLexeme = null;   boolean minLengthPresent = false;
        String maxLengthLexeme = null;   boolean maxLengthPresent = false;
        String patternLexeme = null;     boolean patternPresent = false;

        // -- patternProperties / propertyNames --
        static final class PatternSchema {
            String regex;
            IrNode  child;
            int     index = -1;
        }
        java.util.List<PatternSchema>  patternProperties = new ArrayList<>();
        IrNode  propertyNamesChild = null;
        int     propertyNamesIndex = -1;

        // -- Array structure --
        java.util.List<IrNode>  prefixItems = new ArrayList<>();
        java.util.List<Integer> prefixItemIndices = new ArrayList<>();
        IrNode                  itemsChild = null;
        int                     itemsIndex = -1;
        String                  minItemsLexeme = null;  boolean minItemsPresent = false;
        String                  maxItemsLexeme = null;  boolean maxItemsPresent = false;

        // -- Composition applicators --
        String                   applicatorKind = null;   // legacy single-keyword hint
        java.util.List<IrNode>   applicatorChildren = new ArrayList<>();
        java.util.List<Integer>  applicatorChildIndices = new ArrayList<>();
        // allOf, anyOf, and oneOf may coexist on one schema.
        java.util.List<IrNode>   allOfChildren = new ArrayList<>();
        java.util.List<Integer>  allOfChildIndices = new ArrayList<>();
        java.util.List<IrNode>   anyOfChildren = new ArrayList<>();
        java.util.List<Integer>  anyOfChildIndices = new ArrayList<>();
        java.util.List<IrNode>   oneOfChildren = new ArrayList<>();
        java.util.List<Integer>  oneOfChildIndices = new ArrayList<>();

        // -- unevaluatedProperties --
        boolean unevaluatedPropertiesPresent = false;
        boolean unevaluatedPropertiesRejects = false;
        IrNode  unevaluatedSchemaChild = null;
        int     unevaluatedSchemaIndex = -1;

        // -- unevaluatedItems --
        boolean unevaluatedItemsPresent = false;
        boolean unevaluatedItemsRejects = false;
        IrNode  unevaluatedItemsSchemaChild = null;
        int     unevaluatedItemsSchemaIndex = -1;

        // -- contains family --
        IrNode  containsChild = null;    // `contains` subschema row
        int     containsIndex = -1;
        String  minContainsLexeme = null;  boolean minContainsPresent = false;
        String  maxContainsLexeme = null;  boolean maxContainsPresent = false;

        // -- if / then / else --
        IrNode  ifChild   = null;
        int     ifIndex   = -1;
        IrNode  thenChild = null;
        int     thenIndex = -1;
        IrNode  elseChild = null;
        int     elseIndex = -1;

        // -- dependentSchemas --
        static final class DependentSchema {
            String name;
            IrNode  child;
            int     index = -1;   // resolved registry row of child
        }
        java.util.List<DependentSchema> dependentSchemas = new ArrayList<>();
        // -- dependentRequired property prerequisites --
        static final class DependentRequiredEntry {
            String name;
            java.util.List<String> required = new ArrayList<>();
        }
        java.util.List<DependentRequiredEntry> dependentRequired = new ArrayList<>();
        boolean selfRef = false;   // $ref resolves to THIS node (self/root ref)

        // -- Annotation keywords (JSON-text values; empty means absent) --
        // $comment is shape-checked but never produces annotation output.
        String schemaPath = "";
        String annTitle = "";
        String annDescription = "";
        String annDefaultJson = "";
        String annExamplesJson = "";
        String annDeprecatedJson = "";
        String annReadOnlyJson = "";
        String annWriteOnlyJson = "";
        String annFormat = "";
        String annContentEncoding = "";
        String annContentMediaType = "";
        String annContentSchemaJson = "";
        String annComment = "";
        boolean annCommentShapeViolation = false;
        java.util.List<java.util.Map.Entry<String, String>> annExtras =
                new ArrayList<>();

        /** Deterministic child-row id suffix counter (per node). */
        private int childCounter = 0;

        /** Build a deterministic child validatorId under this node. */
        String childId(String tag) {
            childCounter += 1;
            return validatorId + "_" + tag + childCounter;
        }
    }

    enum BooleanValueKind {
        NOT_BOOLEAN, TRUE, FALSE
    }

    /**
     * Builds an IR node from one branch's validateParams; null when nothing
     * to emit.
     */
    private IrNode irNodeFromBranch(Oas31CompositionLowering.CompositionBranchDescriptor branch) {
        IrNode n = new IrNode();
        n.validatorId = branch.getValidatorId();
        n.resolvedName = branch.getResolvedSchemaName() != null
                ? branch.getResolvedSchemaName() : "schema";
        if (n.validatorId == null || n.validatorId.isEmpty()) {
            return null;
        }
        Map<String, Object> vp = branch.getValidateParams();
        if (vp == null) {
            vp = Collections.emptyMap();
        }

        // type / type-array -> typeFlags
        Object otype = vp.get("validation-type");
        if (otype != null) {
            n.hasType = true;
            if ("type-array".equals(otype)) {
                Object arr = vp.get("validation-type-array");
                if (arr instanceof java.util.List) {
                    for (Object t : (java.util.List<?>) arr) {
                        n.typeFlags |= jsonTypeBit(String.valueOf(t));
                    }
                }
            } else {
                n.typeFlags |= jsonTypeBit(String.valueOf(otype));
            }
        }

        // boolean value-schema
        Object obool = vp.get("validation-boolean-value");
        if (obool != null) {
            n.booleanValue = Boolean.TRUE.equals(obool)
                    ? BooleanValueKind.TRUE : BooleanValueKind.FALSE;
        }

        n.minimum = lexemeOf(vp.get("validation-min"));
        n.maximum = lexemeOf(vp.get("validation-max"));
        n.exclusiveMinimum = lexemeOf(vp.get("validation-exclusive-min"));
        n.exclusiveMaximum = lexemeOf(vp.get("validation-exclusive-max"));
        n.multipleOf = lexemeOf(vp.get("validation-multiple-of"));

        // enum (partitioned by predominant kind, mirroring the hand template)
        if (vp.containsKey("has-validation-enum")) {
            Object kind = vp.get("validation-enum-kind");
            Object vals = vp.get("validation-enum-values");
            if (vals instanceof java.util.List) {
                for (Object v : (java.util.List<?>) vals) {
                    String sv = String.valueOf(v); // already escaped for strings
                    if ("integer".equals(kind) || "number".equals(kind)) {
                        n.enumNumbers.add(sv);
                    } else if ("bool".equals(kind)) {
                        n.enumBooleans.add(sv);
                    } else {
                        n.enumStrings.add(sv);
                    }
                }
            }
        }

        // const (partitioned by kind)
        if (vp.containsKey("has-validation-const")) {
            String ctype = String.valueOf(vp.get("validation-const-type"));
            Object cval = vp.get("validation-const-value");
            if ("number".equals(ctype)) {
                n.constNumber = lexemeOf(cval);
            } else if ("boolean".equals(ctype)) {
                n.constBool = Boolean.valueOf(String.valueOf(cval));
            } else {
                n.constString = cval != null ? String.valueOf(cval) : null;
            }
        }

        // Preserve full JSON const and enum values captured by the assertion scan
        // so deep equality works across every JSON kind, including explicit null.
        Object pristineConstJson = vp.get("validation-const-json");
        if (pristineConstJson instanceof String) {
            n.hasConst = true;
            n.constJson = (String) pristineConstJson;
        } else {
            Object constRaw = vp.get("validation-const-raw");
            // Non-number consts use the deep JSON store. Numbers stay on the exact
            // scalar path so values beyond uint64/double never lose precision.
            if (constRaw != null && !(constRaw instanceof Number)) {
                n.hasConst = true;
                n.constJson = toJsonLiteral(constRaw);
            }
        }
        Object enumRaw = vp.get("validation-enum-raw");
        Object pristineEnumJson = vp.get("validation-enum-json");
        if (pristineEnumJson instanceof String) {
            n.enumJson = (String) pristineEnumJson;
            // The pristine deep store is authoritative and preserves every kind
            // and numeric lexeme, so discard parser-inferred scalar buckets.
            n.enumNumbers = new ArrayList<>();
            n.enumStrings = new ArrayList<>();
            n.enumBooleans = new ArrayList<>();
        } else if (enumRaw instanceof java.util.List) {
            java.util.List<?> list = (java.util.List<?>) enumRaw;
            if (!list.isEmpty()) {
                n.enumJson = toJsonLiteral(list);
            } else {
                // An empty enum is a reject-all constraint, so keep an explicit
                // empty deep store rather than treating the keyword as absent.
                n.enumJson = "[]";
            }
            // Rebuild every scalar bucket from each member's actual JSON kind.
            // The descriptor's predominant-kind view exists for legacy template
            // metadata and cannot represent legal mixed-kind JSON Schema enums.
            n.enumNumbers = new ArrayList<>();
            n.enumStrings = new ArrayList<>();
            n.enumBooleans = new ArrayList<>();
            for (Object m : list) {
                if (m instanceof Number) {
                    n.enumNumbers.add(m.toString());
                } else if (m instanceof Boolean) {
                    n.enumBooleans.add(m.toString());
                } else if (m instanceof String) {
                    n.enumStrings.add(CppBoostBeastClientCodegen
                            .escapeCppStringContent((String) m));
                } else if (m instanceof com.fasterxml.jackson.databind.JsonNode) {
                    com.fasterxml.jackson.databind.JsonNode jsonMember =
                            (com.fasterxml.jackson.databind.JsonNode) m;
                    if (jsonMember.isNumber()) {
                        n.enumNumbers.add(jsonMember.asText());
                    } else if (jsonMember.isBoolean()) {
                        n.enumBooleans.add(Boolean.toString(jsonMember.asBoolean()));
                    } else if (jsonMember.isTextual()) {
                        n.enumStrings.add(CppBoostBeastClientCodegen
                                .escapeCppStringContent(jsonMember.asText()));
                    }
                }
            }
        }

        // uniqueItems is presence-sensitive; false is an explicit no-op.
        if (vp.containsKey("validation-unique-items")) {
            n.uniqueItemsSeen = true;
            n.hasUniqueItems = Boolean.TRUE.equals(vp.get("validation-unique-items"));
        }

        // Build the `not` child now and resolve its row index later.
        Object notSchemaObj = vp.get("validation-not-schema");
        if (notSchemaObj instanceof Schema) {
            n.notChild = irNodeFromRawSchema((Schema) notSchemaObj, n.validatorId + "_not");
        }

        // Resolve $ref targets after all component rows exist.
        Object refObj = vp.get("validation-ref");
        if (refObj != null) {
            n.isRef = true;
            n.refTargetId = refTargetIdOf(String.valueOf(refObj));
            // Dynamic-ref anchor identity can ride in the rewritten target name;
            // parser normalization may drop sibling extension markers.
            String dynAnchor = dynamicRefAnchorOf(String.valueOf(refObj));
            if (dynAnchor != null && n.dynamicRefAnchor == null) {
                n.dynamicRefAnchor = dynAnchor;
            }
        }

        // Dynamic-scope fields collected during branch scanning.
        Object dynRes = vp.get("validation-dynamic-resource");
        if (dynRes instanceof Number) {
            n.dynamicResource = ((Number) dynRes).intValue();
        }
        if (Boolean.TRUE.equals(vp.get("validation-resource-root"))) {
            n.resourceRoot = true;
        }
        if (Boolean.TRUE.equals(vp.get("validation-vocab-inert"))) {
            n.dialectValidationInert = true;
        }
        Object dynRef = vp.get("validation-dynamic-ref-anchor");
        if (dynRef != null) {
            n.dynamicRefAnchor = String.valueOf(dynRef);
            if (!n.isRef && n.refTargetId == null) {
                // a $dynamicRef whose static target failed to rewrite stays a
                // pure marker node (still materialised + fail-closed later).
                n.isRef = true;
                n.refTargetId = "__unresolved_dynamic_ref";
            }
        }
        Object dynAnchor = vp.get("validation-dynamic-anchor");
        if (dynAnchor != null) {
            n.dynamicAnchorName = String.valueOf(dynAnchor);
            // Self-registration: the anchor decl sits ON this row. Later
            // wrapper-level registrations (x-oas31-dyanchor) override it.
            dynamicAnchorRegs.put(n.dynamicResource + "\u0000" + n.dynamicAnchorName,
                    new DynamicAnchorReg(n, n.dynamicResource, n.dynamicAnchorName, true));
        }

        // Structural assertions from the restricted branch scan are densified
        // into child registry rows through irNodeFromRawSchema.
        Object propsObj = vp.get("validation-properties");
        if (propsObj instanceof java.util.Map && !((java.util.Map<?, ?>) propsObj).isEmpty()) {
            n.hasObjectSchema = true;
            java.util.Map<?, ?> pm = (java.util.Map<?, ?>) propsObj;
            java.util.List<String> names = new ArrayList<>();
            for (Object k : pm.keySet()) names.add(String.valueOf(k));
            java.util.Collections.sort(names);   // deterministic emission order
            for (String name : names) {
                Object ps = pm.get(name);
                if (ps instanceof Schema) {
                    IrNode.PropertySchema pb = new IrNode.PropertySchema();
                    pb.name = name;
                    pb.child = irNodeFromRawSchema((Schema) ps, n.childId("prop"));
                    n.properties.add(pb);
                }
            }
        }
        Object reqObj = vp.get("validation-required");
        if (reqObj instanceof java.util.List) {
            for (Object r : (java.util.List<?>) reqObj) {
                n.required.add(String.valueOf(r));
            }
            if (!n.required.isEmpty()) n.hasObjectSchema = true;
        }
        String apKind = (String) vp.get("validation-additional-properties-kind");
        if (apKind != null && !"absent".equals(apKind)) {
            n.additionalPropertiesKind = apKind;
            n.hasObjectSchema = true;
            if ("schema".equals(apKind)) {
                Object s = vp.get("validation-additional-properties-schema");
                if (s instanceof Schema) {
                    n.additionalSchemaChild = irNodeFromRawSchema(
                            (Schema) s, n.childId("addprops"));
                }
            }
        }
        if (vp.containsKey("validation-min-properties")) {
            n.minPropertiesLexeme = lexemeOf(vp.get("validation-min-properties"));
            n.minPropertiesPresent = n.minPropertiesLexeme != null;
            n.hasObjectSchema = true;
        }
        if (vp.containsKey("validation-max-properties")) {
            n.maxPropertiesLexeme = lexemeOf(vp.get("validation-max-properties"));
            n.maxPropertiesPresent = n.maxPropertiesLexeme != null;
            n.hasObjectSchema = true;
        }
        Object piObj = vp.get("validation-prefix-items");
        if (piObj instanceof java.util.List) {
            for (Object s : (java.util.List<?>) piObj) {
                if (s instanceof Schema) {
                    n.prefixItems.add(irNodeFromRawSchema((Schema) s, n.childId("pi")));
                } else if (s instanceof Boolean) {
                    n.prefixItems.add(booleanValueSchema((Boolean) s, n.childId("pib")));
                }
            }
        }
        Object itemsObj = vp.get("validation-items");
        if (itemsObj instanceof Schema) {
            n.itemsChild = irNodeFromRawSchema((Schema) itemsObj, n.childId("items"));
        } else if (itemsObj instanceof Boolean) {
            n.itemsChild = booleanValueSchema((Boolean) itemsObj, n.childId("items"));
        }
        if (vp.containsKey("validation-min-items")) {
            n.minItemsLexeme = lexemeOf(vp.get("validation-min-items"));
            n.minItemsPresent = n.minItemsLexeme != null;
        }
        if (vp.containsKey("validation-max-items")) {
            n.maxItemsLexeme = lexemeOf(vp.get("validation-max-items"));
            n.maxItemsPresent = n.maxItemsLexeme != null;
        }
        // String constraints; branch scanning already escaped the pattern.
        if (vp.containsKey("validation-min-length")) {
            n.minLengthLexeme = lexemeOf(vp.get("validation-min-length"));
            n.minLengthPresent = n.minLengthLexeme != null;
        }
        if (vp.containsKey("validation-max-length")) {
            n.maxLengthLexeme = lexemeOf(vp.get("validation-max-length"));
            n.maxLengthPresent = n.maxLengthLexeme != null;
        }
        if (vp.containsKey("validation-pattern")) {
            n.patternLexeme = String.valueOf(vp.get("validation-pattern"));
            n.patternPresent = n.patternLexeme != null && !n.patternLexeme.isEmpty();
        }
        Object ppObj = vp.get("validation-pattern-properties");
        if (ppObj instanceof java.util.Map) {
            java.util.Map<?, ?> ppm = (java.util.Map<?, ?>) ppObj;
            java.util.List<String> ppNames = new ArrayList<>();
            for (Object k : ppm.keySet()) ppNames.add(String.valueOf(k));
            java.util.Collections.sort(ppNames);
            for (String ppName : ppNames) {
                Object ps = ppm.get(ppName);
                if (ps instanceof Schema) {
                    IrNode.PatternSchema pb = new IrNode.PatternSchema();
                    pb.regex = ppName;
                    pb.child = irNodeFromRawSchema((Schema) ps, n.childId("pp"));
                    n.patternProperties.add(pb);
                }
            }
        }
        Object pnObj = vp.get("validation-property-names");
        if (pnObj instanceof Schema) {
            n.propertyNamesChild = irNodeFromRawSchema((Schema) pnObj, n.childId("pn"));
        } else if (pnObj instanceof Boolean) {
            n.propertyNamesChild = booleanValueSchema((Boolean) pnObj, n.childId("pn"));
        }
        // allOf, anyOf, and oneOf applicators may coexist.
        Object allOfList = vp.get("validation-allof-schemas");
        if (allOfList instanceof java.util.List) {
            n.applicatorKind = "allOf";
            for (Object s : (java.util.List<?>) allOfList) {
                if (s instanceof Schema) {
                    n.allOfChildren.add(
                            irNodeFromRawSchema((Schema) s, n.childId("app")));
                } else if (s instanceof Boolean) {
                    n.allOfChildren.add(
                            booleanValueSchema((Boolean) s, n.childId("app")));
                }
            }
        }
        Object anyOfList = vp.get("validation-anyof-schemas");
        if (anyOfList instanceof java.util.List) {
            if (n.applicatorKind == null) n.applicatorKind = "anyOf";
            for (Object s : (java.util.List<?>) anyOfList) {
                if (s instanceof Schema) {
                    n.anyOfChildren.add(
                            irNodeFromRawSchema((Schema) s, n.childId("app")));
                } else if (s instanceof Boolean) {
                    n.anyOfChildren.add(
                            booleanValueSchema((Boolean) s, n.childId("app")));
                }
            }
        }
        Object oneOfList = vp.get("validation-oneof-schemas");
        if (oneOfList instanceof java.util.List) {
            if (n.applicatorKind == null) n.applicatorKind = "oneOf";
            for (Object s : (java.util.List<?>) oneOfList) {
                if (s instanceof Schema) {
                    n.oneOfChildren.add(
                            irNodeFromRawSchema((Schema) s, n.childId("app")));
                } else if (s instanceof Boolean) {
                    n.oneOfChildren.add(
                            booleanValueSchema((Boolean) s, n.childId("app")));
                }
            }
        }
        Object unevalItemsObj = vp.get("validation-unevaluated-items");
        if (unevalItemsObj != null) {
            n.unevaluatedItemsPresent = true;
            if (unevalItemsObj instanceof Schema) {
                Schema us = (Schema) unevalItemsObj;
                Boolean bv = us.getBooleanSchemaValue();
                if (bv != null) {
                    n.unevaluatedItemsRejects = !Boolean.TRUE.equals(bv);
                } else {
                    n.unevaluatedItemsSchemaChild =
                            irNodeFromRawSchema(us, n.childId("uneval"));
                }
            } else if (unevalItemsObj instanceof Boolean) {
                n.unevaluatedItemsRejects =
                        !Boolean.TRUE.equals(unevalItemsObj);
            }
        }
        Object ifObj = vp.get("validation-if");
        if (ifObj instanceof Schema) {
            n.ifChild = irNodeFromRawSchema((Schema) ifObj, n.childId("if"));
        }
        Object thenObj = vp.get("validation-then");
        if (thenObj instanceof Schema) {
            n.thenChild = irNodeFromRawSchema((Schema) thenObj, n.childId("then"));
        }
        Object elseObj = vp.get("validation-else");
        if (elseObj instanceof Schema) {
            n.elseChild = irNodeFromRawSchema((Schema) elseObj, n.childId("else"));
        }
        Object depObj = vp.get("validation-dependent-schemas");
        if (depObj instanceof java.util.Map) {
            for (java.util.Map.Entry<?, ?> e
                    : ((java.util.Map<?, ?>) depObj).entrySet()) {
                if (!(e.getValue() instanceof Schema)) continue;
                IrNode.DependentSchema d = new IrNode.DependentSchema();
                d.name = String.valueOf(e.getKey());
                d.child = irNodeFromRawSchema((Schema) e.getValue(),
                        n.childId("dep_" + n.dependentSchemas.size()));
                n.dependentSchemas.add(d);
            }
        }
        Object unevalObj = vp.get("validation-unevaluated-properties");
        if (unevalObj != null) {
            n.unevaluatedPropertiesPresent = true;
            if (unevalObj instanceof Schema) {
                Schema us = (Schema) unevalObj;
                Boolean bv = us.getBooleanSchemaValue();
                if (bv != null) {
                    n.unevaluatedPropertiesRejects = !Boolean.TRUE.equals(bv);
                } else {
                    n.unevaluatedSchemaChild =
                            irNodeFromRawSchema(us, n.childId("uneval"));
                }
            } else if (unevalObj instanceof Boolean) {
                n.unevaluatedPropertiesRejects = !Boolean.TRUE.equals(unevalObj);
            }
        }

        // contains and its exact count bounds.
        Object containsObj = vp.get("validation-contains-schema");
        if (containsObj instanceof Schema) {
            n.containsChild = irNodeFromRawSchema((Schema) containsObj,
                    n.childId("contains"));
        }
        n.minContainsLexeme = lexemeOf(vp.get("validation-min-contains"));
        n.maxContainsLexeme = lexemeOf(vp.get("validation-max-contains"));
        if (n.minContainsLexeme != null) n.minContainsPresent = true;
        if (n.maxContainsLexeme != null) n.maxContainsPresent = true;

        // dependentRequired maps a present property to its prerequisites. Raw
        // recovery corrects swagger-parser's merged multi-entry representation.
        Object depReqObj = vp.get("validation-dependent-required");
        if (depReqObj instanceof java.util.Map) {
            for (java.util.Map.Entry<?, ?> e
                    : ((java.util.Map<?, ?>) depReqObj).entrySet()) {
                if (!(e.getValue() instanceof java.util.List)) continue;
                IrNode.DependentRequiredEntry de = new IrNode.DependentRequiredEntry();
                de.name = String.valueOf(e.getKey());
                for (Object r : (java.util.List<?>) e.getValue()) {
                    de.required.add(String.valueOf(r));
                }
                n.dependentRequired.add(de);
            }
        }

        // Annotation keywords collected by scanSurfaceAssertions.
        readAnnotationVp(vp, n);

        boolean hasKeyword = n.hasType
                || n.booleanValue != BooleanValueKind.NOT_BOOLEAN
                || n.minimum != null || n.maximum != null
                || n.exclusiveMinimum != null || n.exclusiveMaximum != null
                || n.multipleOf != null
                || !n.enumNumbers.isEmpty() || !n.enumStrings.isEmpty()
                || !n.enumBooleans.isEmpty()
                || n.constNumber != null || n.constString != null || n.constBool != null
                || n.constJson != null || n.enumJson != null
                || n.hasUniqueItems || n.uniqueItemsSeen
                || n.notChild != null || n.isRef
                || n.hasObjectSchema || n.required != null && !n.required.isEmpty()
                || "absent" != n.additionalPropertiesKind && !"absent".equals(n.additionalPropertiesKind)
                || n.minPropertiesPresent || n.maxPropertiesPresent
                || !n.prefixItems.isEmpty() || n.itemsChild != null
                || n.minItemsPresent || n.maxItemsPresent
                || n.minLengthPresent || n.maxLengthPresent || n.patternPresent
                || !n.patternProperties.isEmpty() || n.propertyNamesChild != null
                || n.applicatorKind != null
                || !n.allOfChildren.isEmpty() || !n.anyOfChildren.isEmpty()
                || !n.oneOfChildren.isEmpty()
                || n.unevaluatedPropertiesPresent
                || n.unevaluatedItemsPresent
                || n.containsChild != null || n.minContainsPresent || n.maxContainsPresent
                || n.ifChild != null || n.thenChild != null || n.elseChild != null
                || !n.dependentSchemas.isEmpty()
                || !n.dependentRequired.isEmpty()
                || n.resourceRoot || n.dynamicRefAnchor != null
                || n.dynamicAnchorName != null;
        return hasKeyword ? n : null;
    }

    /**
     * Builds an IR node directly from a raw schema. This path handles `not`,
     * properties, array items, applicators, and unevaluated schemas that branch
     * lowering does not visit.
     */
    private IrNode irNodeFromRawSchema(Schema schema, String validatorId) {
        IrNode n = new IrNode();
        n.validatorId = validatorId;
        n.resolvedName = validatorId;
        if (schema == null) {
            return n;
        }
        if (schema.get$ref() != null) {
            // Local ref: resolve against the combined registry later. Siblings
            // are still densified (2020-12: $ref and siblings BOTH apply).
            n.isRef = true;
            n.refTargetId = refTargetIdOf(schema.get$ref());
            // A normalized dynamic-reference wrapper encodes its anchor in the
            // target component name because parser normalization can discard
            // sibling extensions on $ref schemas.
            String dynAnchor = dynamicRefAnchorOf(schema.get$ref());
            if (dynAnchor != null && n.dynamicRefAnchor == null) {
                n.dynamicRefAnchor = dynAnchor;
            }
        }

        // Dynamic-scope facts carried by the normalization extensions.
        {
            java.util.Map ext = schema.getExtensions();
            if (ext != null) {
                Object dynres = ext.get("x-oas31-res");
                if (dynres instanceof Number) {
                    n.dynamicResource = ((Number) dynres).intValue();
                }
                Object dynroot = ext.get("x-oas31-res-root");
                if (Boolean.TRUE.equals(dynroot) || (dynroot instanceof Number
                        && ((Number) dynroot).intValue() != 0)) {
                    n.resourceRoot = true;
                }
                Object vinert = ext.get("x-oas31-vocab-inert");
                if (Boolean.TRUE.equals(vinert)) {
                    n.dialectValidationInert = true;
                }
                Object dynref = ext.get("x-oas31-dynref");
                if (dynref != null) {
                    n.dynamicRefAnchor = String.valueOf(dynref);
                    if (!n.isRef && n.refTargetId == null) {
                        n.isRef = true;
                        n.refTargetId = "__unresolved_dynamic_ref";
                    }
                }
                Object dynanch = ext.get("x-oas31-dyanchor");
                if (dynanch != null) {
                    // Hoisted anchor component wrapper: the anchor target row is
                    // this wrapper's FIRST composed child (the runner single-
                    // branch oneOf). Overrides any earlier self-registration of
                    // the same (resource, name) pair.
                    n.dynamicAnchorName = String.valueOf(dynanch);
                    dynamicAnchorRegs.put(
                            n.dynamicResource + "\u0000" + n.dynamicAnchorName,
                            new DynamicAnchorReg(n, n.dynamicResource,
                                    n.dynamicAnchorName, false));
                }
            }
            if (n.dynamicAnchorName == null && schema.get$dynamicAnchor() != null
                    && !schema.get$dynamicAnchor().isEmpty()) {
                n.dynamicAnchorName = schema.get$dynamicAnchor();
                dynamicAnchorRegs.put(n.dynamicResource + "\u0000" + n.dynamicAnchorName,
                        new DynamicAnchorReg(n, n.dynamicResource,
                                n.dynamicAnchorName, true));
            }
        }
        if (schema.getType() != null) {
            n.hasType = true;
            n.typeFlags |= jsonTypeBit(String.valueOf(schema.getType()));
        }
        if (schema.getTypes() != null && !schema.getTypes().isEmpty()) {
            n.hasType = true;
            for (Object t : schema.getTypes()) {
                n.typeFlags |= jsonTypeBit(String.valueOf(t));
            }
        }
        if (schema.getBooleanSchemaValue() != null) {
            n.booleanValue = Boolean.TRUE.equals(schema.getBooleanSchemaValue())
                    ? BooleanValueKind.TRUE : BooleanValueKind.FALSE;
        }
        String pristineConstJson = Oas31RawSpecRecovery.constJsonOf(schema);
        if (pristineConstJson != null) {
            n.hasConst = true;
            n.constJson = pristineConstJson;
        } else if (schema.getConst() != null) {
            n.hasConst = true;
            n.constJson = toJsonLiteral(schema.getConst());
        }
        String pristineEnumJson = Oas31RawSpecRecovery.enumJsonOf(schema);
        if (pristineEnumJson != null) {
            n.enumJson = pristineEnumJson;
        } else if (schema.getEnum() != null) {
            // An EMPTY enum (enum: []) is a valid reject-all schema.
            n.enumJson = toJsonLiteral(schema.getEnum());
        }
        if (schema.getMinimum() != null) {
            n.minimum = String.valueOf(schema.getMinimum());
        }
        if (schema.getMaximum() != null) {
            n.maximum = String.valueOf(schema.getMaximum());
        }
        // Use the *Value* accessor (Number); getExclusiveMinimum() is a Boolean
        // presence marker in OAS 3.0 and not a numeric bound.
        if (schema.getExclusiveMinimumValue() != null) {
            n.exclusiveMinimum = String.valueOf(schema.getExclusiveMinimumValue());
        }
        if (schema.getExclusiveMaximumValue() != null) {
            n.exclusiveMaximum = String.valueOf(schema.getExclusiveMaximumValue());
        }
        if (schema.getMultipleOf() != null) {
            n.multipleOf = String.valueOf(schema.getMultipleOf());
        }
        if (schema.getUniqueItems() != null) {
            n.uniqueItemsSeen = true;
            n.hasUniqueItems = Boolean.TRUE.equals(schema.getUniqueItems());
        }
        if (schema.getNot() != null) {
            n.notChild = irNodeFromRawSchema(schema.getNot(), n.childId("not"));
        }

        // ---- Object structure ----
        if (schema.getProperties() != null && !schema.getProperties().isEmpty()) {
            n.hasObjectSchema = true;
            java.util.List<String> names = new ArrayList<>(schema.getProperties().keySet());
            java.util.Collections.sort(names);
            for (String name : names) {
                Schema ps = (Schema) schema.getProperties().get(name);
                if (ps == null) continue;
                IrNode.PropertySchema pb = new IrNode.PropertySchema();
                pb.name = name;
                pb.child = irNodeFromRawSchema(ps, n.childId("prop"));
                n.properties.add(pb);
            }
        }
        if (schema.getRequired() != null && !schema.getRequired().isEmpty()) {
            n.required.addAll(schema.getRequired());
            n.hasObjectSchema = true;
        }
        Object addProps = schema.getAdditionalProperties();
        if (addProps != null) {
            if (addProps instanceof Boolean) {
                n.additionalPropertiesKind =
                        Boolean.TRUE.equals(addProps) ? "allowed" : "reject";
                n.hasObjectSchema = true;
            } else if (addProps instanceof Schema) {
                Schema as = (Schema) addProps;
                Boolean bv = as.getBooleanSchemaValue();
                if (bv != null) {
                    n.additionalPropertiesKind =
                            Boolean.TRUE.equals(bv) ? "allowed" : "reject";
                    n.hasObjectSchema = true;
                } else if (as.getProperties() == null && as.getType() == null
                        && as.getEnum() == null && as.getItems() == null
                        && as.getPrefixItems() == null && as.getConst() == null
                        && as.getNot() == null && as.get$ref() == null) {
                    // additionalProperties: {} — unrestricted (allowed).
                    n.additionalPropertiesKind = "allowed";
                    n.hasObjectSchema = true;
                } else {
                    n.additionalPropertiesKind = "schema";
                    n.hasObjectSchema = true;
                    n.additionalSchemaChild =
                            irNodeFromRawSchema(as, n.childId("addprops"));
                }
            }
        }
        String minPropertiesLexeme = Oas31RawSpecRecovery.countBoundLexemeOf(
                schema, "minProperties");
        if (minPropertiesLexeme != null || schema.getMinProperties() != null) {
            n.minPropertiesLexeme = minPropertiesLexeme != null
                    ? minPropertiesLexeme : String.valueOf(schema.getMinProperties());
            n.minPropertiesPresent = true;
            n.hasObjectSchema = true;
        }
        String maxPropertiesLexeme = Oas31RawSpecRecovery.countBoundLexemeOf(
                schema, "maxProperties");
        if (maxPropertiesLexeme != null || schema.getMaxProperties() != null) {
            n.maxPropertiesLexeme = maxPropertiesLexeme != null
                    ? maxPropertiesLexeme : String.valueOf(schema.getMaxProperties());
            n.maxPropertiesPresent = true;
            n.hasObjectSchema = true;
        }

        // ---- Array structure ----
        if (schema.getPrefixItems() != null) {
            for (Object o : schema.getPrefixItems()) {
                Schema s = (Schema) o;
                if (s == null) continue;
                if (s.getBooleanSchemaValue() != null) {
                    n.prefixItems.add(booleanValueSchema(
                            s.getBooleanSchemaValue(), n.childId("pi")));
                } else {
                    n.prefixItems.add(irNodeFromRawSchema(s, n.childId("pi")));
                }
            }
        }
        if (schema.getItems() != null) {
            Schema its = schema.getItems();
            if (its.getBooleanSchemaValue() != null) {
                n.itemsChild = booleanValueSchema(
                        its.getBooleanSchemaValue(), n.childId("items"));
            } else {
                n.itemsChild = irNodeFromRawSchema(its, n.childId("items"));
            }
        }
        String mibl = Oas31RawSpecRecovery.countBoundLexemeOf(schema, "minItems");
        if (mibl != null || schema.getMinItems() != null) {
            n.minItemsLexeme = mibl != null
                    ? mibl : String.valueOf(schema.getMinItems());
            n.minItemsPresent = true;
        }
        String maxbl = Oas31RawSpecRecovery.countBoundLexemeOf(schema, "maxItems");
        if (maxbl != null || schema.getMaxItems() != null) {
            n.maxItemsLexeme = maxbl != null
                    ? maxbl : String.valueOf(schema.getMaxItems());
            n.maxItemsPresent = true;
        }

        // ---- String constraints ----
        String minll = Oas31RawSpecRecovery.countBoundLexemeOf(schema, "minLength");
        if (minll != null || schema.getMinLength() != null) {
            n.minLengthLexeme = minll != null
                    ? minll : String.valueOf(schema.getMinLength());
            n.minLengthPresent = true;
        }
        String maxll = Oas31RawSpecRecovery.countBoundLexemeOf(schema, "maxLength");
        if (maxll != null || schema.getMaxLength() != null) {
            n.maxLengthLexeme = maxll != null
                    ? maxll : String.valueOf(schema.getMaxLength());
            n.maxLengthPresent = true;
        }
        if (schema.getPattern() != null) {
            n.patternLexeme = CppBoostBeastClientCodegen.escapeCppStringContent(schema.getPattern());
            n.patternPresent = true;
        }

        // ---- patternProperties / propertyNames ----
        if (schema.getPatternProperties() != null
                && !schema.getPatternProperties().isEmpty()) {
            java.util.List<String> ppNames = new ArrayList<>(
                    schema.getPatternProperties().keySet());
            java.util.Collections.sort(ppNames);
            for (String ppName : ppNames) {
                Schema ps = (Schema) schema.getPatternProperties().get(ppName);
                if (ps == null) continue;
                IrNode.PatternSchema pb = new IrNode.PatternSchema();
                pb.regex = ppName;
                pb.child = irNodeFromRawSchema(ps, n.childId("pp"));
                n.patternProperties.add(pb);
            }
        }
        if (schema.getPropertyNames() != null) {
            Schema pns = schema.getPropertyNames();
            if (pns.getBooleanSchemaValue() != null) {
                n.propertyNamesChild = booleanValueSchema(
                        pns.getBooleanSchemaValue(), n.childId("pn"));
            } else {
                n.propertyNamesChild = irNodeFromRawSchema(pns, n.childId("pn"));
            }
        }

        // ---- Coexisting allOf / anyOf / oneOf applicators ----
        {
            java.util.List<?> allMembers = schema.getAllOf();
            if (allMembers != null && !allMembers.isEmpty()) {
                n.applicatorKind = "allOf";
                for (Object mo : allMembers) {
                    Schema s = (Schema) mo;
                    if (s == null) continue;
                    if (s.getBooleanSchemaValue() != null) {
                        n.allOfChildren.add(booleanValueSchema(
                                s.getBooleanSchemaValue(), n.childId("app")));
                    } else {
                        n.allOfChildren.add(irNodeFromRawSchema(s, n.childId("app")));
                    }
                }
            }
            java.util.List<?> anyMembers = schema.getAnyOf();
            if (anyMembers != null && !anyMembers.isEmpty()) {
                if (n.applicatorKind == null) n.applicatorKind = "anyOf";
                for (Object mo : anyMembers) {
                    Schema s = (Schema) mo;
                    if (s == null) continue;
                    if (s.getBooleanSchemaValue() != null) {
                        n.anyOfChildren.add(booleanValueSchema(
                                s.getBooleanSchemaValue(), n.childId("app")));
                    } else {
                        n.anyOfChildren.add(irNodeFromRawSchema(s, n.childId("app")));
                    }
                }
            }
            java.util.List<?> oneMembers = schema.getOneOf();
            if (oneMembers != null && !oneMembers.isEmpty()) {
                if (n.applicatorKind == null) n.applicatorKind = "oneOf";
                for (Object mo : oneMembers) {
                    Schema s = (Schema) mo;
                    if (s == null) continue;
                    if (s.getBooleanSchemaValue() != null) {
                        n.oneOfChildren.add(booleanValueSchema(
                                s.getBooleanSchemaValue(), n.childId("app")));
                    } else {
                        n.oneOfChildren.add(irNodeFromRawSchema(s, n.childId("app")));
                    }
                }
            }
        }

        // ---- unevaluatedProperties ----
        if (schema.getUnevaluatedProperties() != null) {
            n.unevaluatedPropertiesPresent = true;
            Schema us = schema.getUnevaluatedProperties();
            Boolean bv = us.getBooleanSchemaValue();
            if (bv != null) {
                n.unevaluatedPropertiesRejects = !Boolean.TRUE.equals(bv);
            } else {
                n.unevaluatedSchemaChild = irNodeFromRawSchema(us, n.childId("uneval"));
            }
        }

        // ---- unevaluatedItems ----
        if (schema.getUnevaluatedItems() != null) {
            n.unevaluatedItemsPresent = true;
            Schema us = schema.getUnevaluatedItems();
            Boolean bv = us.getBooleanSchemaValue();
            if (bv != null) {
                n.unevaluatedItemsRejects = !Boolean.TRUE.equals(bv);
            } else {
                n.unevaluatedItemsSchemaChild =
                        irNodeFromRawSchema(us, n.childId("uneval"));
            }
        }

        // ---- if / then / else ----
        if (schema.getIf() != null) {
            n.ifChild = irNodeFromRawSchema(schema.getIf(), n.childId("if"));
        }
        if (schema.getThen() != null) {
            n.thenChild = irNodeFromRawSchema(schema.getThen(), n.childId("then"));
        }
        if (schema.getElse() != null) {
            n.elseChild = irNodeFromRawSchema(schema.getElse(), n.childId("else"));
        }

        // ---- dependentSchemas ----
        java.util.Map<String, Schema> depMap = schema.getDependentSchemas();
        if (depMap != null && !depMap.isEmpty()) {
            for (java.util.Map.Entry<String, Schema> e : depMap.entrySet()) {
                if (e.getValue() == null) continue;
                IrNode.DependentSchema d = new IrNode.DependentSchema();
                d.name = e.getKey();
                d.child = irNodeFromRawSchema(e.getValue(),
                        n.childId("dep_" + n.dependentSchemas.size()));
                n.dependentSchemas.add(d);
            }
        }

        // ---- contains family ----
        if (schema.getContains() != null) {
            n.containsChild = irNodeFromRawSchema(schema.getContains(),
                    n.childId("contains"));
        }
        String minCLex = Oas31RawSpecRecovery.countBoundLexemeOf(
                schema, "minContains");
        if (minCLex == null && schema.getMinContains() != null) {
            minCLex = String.valueOf(schema.getMinContains());
        }
        if (minCLex != null) {
            n.minContainsLexeme = minCLex;
            n.minContainsPresent = true;
        }
        String maxCLex = Oas31RawSpecRecovery.countBoundLexemeOf(
                schema, "maxContains");
        if (maxCLex == null && schema.getMaxContains() != null) {
            maxCLex = String.valueOf(schema.getMaxContains());
        }
        if (maxCLex != null) {
            n.maxContainsLexeme = maxCLex;
            n.maxContainsPresent = true;
        }

        // ---- dependentRequired ----
        // The parser merges multi-entry maps (see (c)); the recovered literal
        // extension is authoritative when present.
        java.util.Map<String, java.util.List<String>> depReqMap =
                schema.getDependentRequired();
        if (depReqMap != null && schema.getExtensions() != null
                && schema.getExtensions().containsKey(
                        "x-oas31-dependent-required")) {
            Object ext = schema.getExtensions()
                    .get("x-oas31-dependent-required");
            if (ext instanceof java.util.Map) {
                depReqMap = (java.util.Map<String, java.util.List<String>>) ext;
            }
        }
        if (depReqMap != null && !depReqMap.isEmpty()) {
            for (java.util.Map.Entry<String, java.util.List<String>> e
                    : depReqMap.entrySet()) {
                if (e.getValue() == null || e.getValue().isEmpty()) continue;
                IrNode.DependentRequiredEntry de = new IrNode.DependentRequiredEntry();
                de.name = e.getKey();
                de.required.addAll(e.getValue());
                n.dependentRequired.add(de);
            }
        }
        // Annotation keywords read directly from the raw schema.
        readAnnotationRaw(schema, n);
        return n;
    }

    /** The applicator keyword of a schema, null when it has none. */
    private static String applicatorOf(Schema schema) {
        if (schema.getOneOf() != null && !schema.getOneOf().isEmpty()) return "oneOf";
        if (schema.getAnyOf() != null && !schema.getAnyOf().isEmpty()) return "anyOf";
        if (schema.getAllOf() != null && !schema.getAllOf().isEmpty()) return "allOf";
        return null;
    }

    /** The applicator member list for the schema's (single) applicator. */
    private static java.util.List<?> applicatorMembers(Schema schema) {
        if (schema.getOneOf() != null && !schema.getOneOf().isEmpty()) return schema.getOneOf();
        if (schema.getAnyOf() != null && !schema.getAnyOf().isEmpty()) return schema.getAnyOf();
        if (schema.getAllOf() != null && !schema.getAllOf().isEmpty()) return schema.getAllOf();
        return java.util.Collections.emptyList();
    }


    /** Builds a boolean value-schema node (OAS 3.1 true/false literal). */
    private IrNode booleanValueSchema(Boolean b, String validatorId) {
        IrNode n = new IrNode();
        n.validatorId = validatorId;
        n.resolvedName = validatorId;
        n.booleanValue = Boolean.TRUE.equals(b) ? BooleanValueKind.TRUE : BooleanValueKind.FALSE;
        return n;
    }

    /**
     * Maps a component reference to its complete densified wrapper row.
     * Unsupported targets intentionally remain unresolved and fail generation.
     */
    private String refTargetIdOf(String refStr) {
        String name = refSimpleName(refStr);
        // Every component has a wrapper row, so refs resolve to the complete
        // composition rather than an accidental first-branch alias.
        return CppBoostBeastModelCodegen.componentSchemaId(name, componentSchemaIds);
    }

    /** Extracts the referenced component or final URI-path name. */
    private static String refSimpleName(String ref) {
        if (ref == null) return "";
        String r = ref.trim();
        if (r.startsWith("#/components/schemas/")) {
            return r.substring("#/components/schemas/".length());
        }
        if (r.startsWith("#/$defs/")) {
            return r.substring("#/$defs/".length());
        }
        int hash = r.indexOf('#');
        String base = hash >= 0 ? r.substring(0, hash) : r;
        int slash = base.lastIndexOf('/');
        String tail = slash >= 0 ? base.substring(slash + 1) : base;
        return tail.isEmpty() ? base : tail;
    }

    /**
     * Decodes the anchor from a normalized {@code __dynref_<resource>_<anchor>}
     * component wrapper. Plain references return null.
     */
    private String dynamicRefAnchorOf(String refStr) {
        String name = refSimpleName(refStr);
        if (name == null || !name.startsWith("__dynref_")) {
            return null;
        }
        // Ignore synthetic model-layer names that were not authored normalized
        // wrapper components.
        if (!oasComponentNames.contains(name)) {
            return null;
        }
        String rest = name.substring("__dynref_".length());
        int cut = rest.indexOf('_');
        if (cut <= 0) {
            return null;
        }
        String resDigits = rest.substring(0, cut);
        for (int i = 0; i < resDigits.length(); i++) {
            if (!Character.isDigit(resDigits.charAt(i))) {
                return null;
            }
        }
        String anchor = rest.substring(cut + 1);
        return anchor.isEmpty() ? null : anchor;
    }

    /** Serialize one arbitrary Swagger/Jackson value as strict JSON. */
    private static String toJsonLiteral(Object value) {
        try {
            return Json.mapper().writeValueAsString(value);
        } catch (com.fasterxml.jackson.core.JsonProcessingException ex) {
            throw new IllegalArgumentException("Unable to serialize a schema JSON value", ex);
        }
    }


    /** Original numeric lexeme, or null when absent (BigDecimal.toString()). */
    private static String lexemeOf(Object value) {
        if (value == null) {
            return null;
        }
        String s = String.valueOf(value);
        return s.isEmpty() ? null : s;
    }

    // JsonType bit positions must match JsonType in Oas31SchemaIr.h.
    private static final int JSONTYPE_BIT_NULL = 1 << 0;
    private static final int JSONTYPE_BIT_BOOLEAN = 1 << 1;
    private static final int JSONTYPE_BIT_NUMBER = 1 << 2;
    private static final int JSONTYPE_BIT_STRING = 1 << 3;
    private static final int JSONTYPE_BIT_ARRAY = 1 << 4;
    private static final int JSONTYPE_BIT_OBJECT = 1 << 5;
    private static final int JSONTYPE_BIT_INTEGER = 1 << 6; // schema-level only

    /** Maps an OAS 3.1 type name to a JsonType bit. */
    private static int jsonTypeBit(String type) {
        switch (type) {
            case "null": return JSONTYPE_BIT_NULL;
            case "boolean": return JSONTYPE_BIT_BOOLEAN;
            case "number": return JSONTYPE_BIT_NUMBER;
            case "string": return JSONTYPE_BIT_STRING;
            case "array": return JSONTYPE_BIT_ARRAY;
            case "object": return JSONTYPE_BIT_OBJECT;
            case "integer": return JSONTYPE_BIT_INTEGER;
            default: return 0;
        }
    }

}