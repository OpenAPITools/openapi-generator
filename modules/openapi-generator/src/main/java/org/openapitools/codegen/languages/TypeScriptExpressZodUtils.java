package org.openapitools.codegen.languages;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.openapitools.codegen.CodegenDiscriminator;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.utils.ModelUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

import static org.openapitools.codegen.utils.StringUtils.camelize;

/**
 * Shared helpers for the {@code typescript-express-zod-server} generator and the
 * {@code express-zod} framework of the unified {@code typescript} generator
 * ({@link TypeScriptClientCodegen}).
 *
 * <p>The server generator and the client framework live in different classes, so the
 * recursive Zod builder, the model annotation pass, the topological model sort, and the
 * array/map/primitive DTO resolution would otherwise be duplicated and free to diverge.
 * Keeping that logic here — the highest-risk, must-stay-identical part — is the single
 * source of truth. Client-only post-processing lives in
 * {@link TypeScriptExpressZodClientUtils}.
 */
final class TypeScriptExpressZodUtils {
    private TypeScriptExpressZodUtils() {
    }

    /** Direction of a DTO mapping: domain-&gt;wire ({@code mapTo*Dto}) or wire-&gt;domain ({@code mapFrom*Dto}). */
    enum Direction {
        TO_WIRE, FROM_WIRE
    }

    /** Resolution of a body/response shape into its Zod schema, wire (DTO) type, and mapper. */
    static final class Resolved {
        /** Zod schema expression, e.g. {@code PetSchema.array()}. */
        final String zod;
        /** Over-the-wire TypeScript type, e.g. {@code dtos.PetDto[]}. */
        final String dtoGeneric;
        /** Domain TypeScript type qualified for use outside {@code types.ts}, e.g. {@code types.Pet[]}. */
        final String domainGeneric;
        /** Function expression converting between domain and wire, or {@code null} for identity/passthrough. */
        final String mapFn;

        Resolved(String zod, String dtoGeneric, String domainGeneric, String mapFn) {
            this.zod = zod;
            this.dtoGeneric = dtoGeneric;
            this.domainGeneric = domainGeneric;
            this.mapFn = mapFn;
        }
    }

    // ---------------------------------------------------------------------
    // Recursive Zod expression builder
    // ---------------------------------------------------------------------

    /**
     * Build a Zod type expression for a model/response property. Handles enums, arrays,
     * maps ({@code z.record}), $refs, and primitives (with numeric/length constraints).
     */
    static String zodExpr(CodegenProperty prop) {
        if (prop == null) {
            return "z.any()";
        }

        // Enum properties with allowable values.
        if (prop.isEnum && prop.allowableValues != null) {
            List<?> values = (List<?>) prop.allowableValues.get("values");
            if (values != null && values.size() == 1) {
                // Single-value enum = literal.
                return "z.literal(\"" + values.get(0) + "\")";
            }
            if (prop.complexType != null) {
                // Multi-value enum referenced via $ref — reference the enum schema.
                return camelize(prop.complexType) + "Schema";
            }
            if (values != null) {
                StringBuilder sb = new StringBuilder("z.enum([");
                for (int i = 0; i < values.size(); i++) {
                    if (i > 0) sb.append(", ");
                    sb.append("\"").append(values.get(i)).append("\"");
                }
                sb.append("])");
                return sb.toString();
            }
        }

        // Arrays (before the complexType check, to handle Array<ModelType>).
        // Nullable items get `.nullable()` to match the TS type (`Array<X | null>`
        // from getTypeDeclaration); top-level nullability of model properties is
        // applied externally via x-zod-full.
        if (prop.isArray && prop.items != null) {
            String item = zodExpr(prop.items);
            if (prop.items.isNullable) item += ".nullable()";
            return item + ".array()";
        }

        // Maps / free-form objects with a typed value (additionalProperties). Zod v4
        // requires the two-arg z.record(keyType, valueType) signature.
        if (prop.isMap && prop.additionalProperties != null) {
            String value = zodExpr(prop.additionalProperties);
            if (prop.additionalProperties.isNullable) value += ".nullable()";
            return "z.record(z.string(), " + value + ")";
        }

        // $ref to another model or enum type.
        if (prop.complexType != null) {
            return camelize(prop.complexType) + "Schema";
        }

        // Primitive types.
        if (prop.isInteger || prop.isLong) {
            StringBuilder sb = new StringBuilder("z.coerce.number().int()");
            if (prop.minimum != null) sb.append(".gte(").append(prop.minimum).append(")");
            if (prop.maximum != null) sb.append(".lte(").append(prop.maximum).append(")");
            return sb.toString();
        }
        if (prop.isNumber || prop.isFloat || prop.isDouble) {
            StringBuilder sb = new StringBuilder("z.number()");
            if (prop.minimum != null) sb.append(".gte(").append(prop.minimum).append(")");
            if (prop.maximum != null) sb.append(".lte(").append(prop.maximum).append(")");
            return sb.toString();
        }
        if (prop.isBoolean) {
            return "z.boolean()";
        }
        if (prop.isString) {
            StringBuilder sb = new StringBuilder("z.string()");
            if (prop.maxLength != null) sb.append(".max(").append(prop.maxLength).append(")");
            if (prop.minLength != null) sb.append(".min(").append(prop.minLength).append(")");
            return sb.toString();
        }

        // Fallback.
        return "z.any()";
    }

    // ---------------------------------------------------------------------
    // Shared model post-processing
    // ---------------------------------------------------------------------

    /**
     * Annotate all models with the vendor extensions the express-zod templates rely on:
     * per-property Zod expressions ({@code x-zod-type}/{@code x-zod-full}), discriminated
     * unions ({@code x-is-discriminated-union}/{@code x-mapped-models}), non-discriminated
     * oneOf unions ({@code x-is-union}/{@code x-union-members}), allOf intersections
     * ({@code x-is-intersection}/{@code x-intersection-parents}/{@code x-has-own-properties}),
     * enum/object $ref flags, DTO/schema names, and single-value enum literals.
     *
     * <p>{@code openAPI} and {@code toModelName} are passed in because this class has no
     * access to the codegen's protected state; callers use {@code this.openAPI} and
     * {@code this::toModelName}.
     */
    static void annotateModels(Map<String, ModelsMap> result, OpenAPI openAPI,
                               Function<String, String> toModelName) {
        // Collect enum model names (enums don't need DTO mappers)
        Set<String> enumModelNames = new HashSet<>();
        for (ModelsMap modEntry : result.values()) {
            for (ModelMap innerMo : modEntry.getModels()) {
                CodegenModel innerCm = innerMo.getModel();
                if (innerCm.isEnum) {
                    enumModelNames.add(innerCm.classname);
                }
            }
        }

        for (ModelsMap entry : result.values()) {
            for (ModelMap mo : entry.getModels()) {
                CodegenModel cm = mo.getModel();

                // Add Zod vendor extensions to each property
                for (CodegenProperty prop : cm.vars) {
                    addZodVendorExtensions(prop);
                }

                // Look up original schema from OAS spec (shared by oneOf and allOf detection)
                Map<String, Schema> allSchemas = ModelUtils.getSchemas(openAPI);
                Schema origSchema = null;
                for (Map.Entry<String, Schema> schemaEntry : allSchemas.entrySet()) {
                    if (toModelName.apply(schemaEntry.getKey()).equals(cm.classname)) {
                        origSchema = schemaEntry.getValue();
                        break;
                    }
                }

                // Determine if this is a discriminated union
                if (cm.discriminator != null && cm.oneOf != null && !cm.oneOf.isEmpty()) {
                    cm.vendorExtensions.put("x-is-discriminated-union", true);
                    cm.vendorExtensions.put("x-discriminator-property", cm.discriminator.getPropertyBaseName());

                    List<Map<String, String>> mappedTypes = new ArrayList<>();

                    if (cm.discriminator.getMappedModels() != null && !cm.discriminator.getMappedModels().isEmpty()) {
                        // Use explicit mappings from the discriminator
                        for (CodegenDiscriminator.MappedModel mm : cm.discriminator.getMappedModels()) {
                            Map<String, String> mapped = new HashMap<>();
                            mapped.put("modelName", mm.getModelName());
                            mapped.put("mappingName", mm.getMappingName());
                            mappedTypes.add(mapped);
                        }
                    } else {
                        // Build mappings from oneOf members by inspecting their discriminator property
                        for (String memberName : cm.oneOf) {
                            Map<String, String> mapped = new HashMap<>();
                            mapped.put("modelName", memberName);
                            // Try to find the discriminator value from the member model's literal property
                            String discValue = findDiscriminatorValue(memberName, cm.discriminator.getPropertyBaseName(), result);
                            mapped.put("mappingName", discValue != null ? discValue : memberName);
                            mappedTypes.add(mapped);
                        }
                    }
                    cm.vendorExtensions.put("x-mapped-models", mappedTypes);
                }

                // Detect non-discriminated oneOf unions from the original OAS spec
                if (cm.discriminator == null && !cm.vendorExtensions.containsKey("x-is-discriminated-union")) {
                    if (origSchema != null && origSchema.getOneOf() != null && !origSchema.getOneOf().isEmpty()) {
                        cm.vendorExtensions.put("x-is-union", true);

                        // Collect member classnames
                        List<String> memberNames = new ArrayList<>();
                        for (Object memberObj : origSchema.getOneOf()) {
                            Schema memberSchema = (Schema) memberObj;
                            String ref = memberSchema.get$ref();
                            if (ref != null) {
                                memberNames.add(toModelName.apply(ModelUtils.getSimpleRef(ref)));
                            }
                        }

                        // Collect property sets per member to find distinguishing properties
                        Map<String, Set<String>> memberPropSets = new LinkedHashMap<>();
                        for (String memberName : memberNames) {
                            Set<String> props = new LinkedHashSet<>();
                            ModelsMap memberModels = result.get(memberName);
                            if (memberModels != null) {
                                for (ModelMap mm : memberModels.getModels()) {
                                    for (CodegenProperty prop : mm.getModel().vars) {
                                        props.add(prop.name);
                                    }
                                }
                            }
                            memberPropSets.put(memberName, props);
                        }

                        // Build x-union-members list with distinguishing properties
                        List<Map<String, Object>> members = new ArrayList<>();
                        for (int i = 0; i < memberNames.size(); i++) {
                            String memberName = memberNames.get(i);
                            Map<String, Object> info = new HashMap<>();
                            info.put("modelName", memberName);

                            if (i < memberNames.size() - 1) {
                                // Find a property unique to this member
                                Set<String> myProps = memberPropSets.getOrDefault(memberName, Collections.emptySet());
                                for (String prop : myProps) {
                                    boolean unique = true;
                                    for (Map.Entry<String, Set<String>> other : memberPropSets.entrySet()) {
                                        if (!other.getKey().equals(memberName) && other.getValue().contains(prop)) {
                                            unique = false;
                                            break;
                                        }
                                    }
                                    if (unique) {
                                        info.put("hasDistinguishingProperty", true);
                                        info.put("distinguishingProperty", prop);
                                        break;
                                    }
                                }
                            }
                            // Last member is always the fallback (no distinguishing property needed)
                            members.add(info);
                        }

                        cm.vendorExtensions.put("x-union-members", members);
                    }
                }

                // Detect allOf intersection types
                if (origSchema != null && origSchema.getAllOf() != null && !origSchema.getAllOf().isEmpty()) {
                    List<String> parentNames = new ArrayList<>();
                    for (Object member : origSchema.getAllOf()) {
                        Schema memberSchema = (Schema) member;
                        if (memberSchema.get$ref() != null) {
                            parentNames.add(toModelName.apply(ModelUtils.getSimpleRef(memberSchema.get$ref())));
                        }
                    }

                    if (!parentNames.isEmpty()) {
                        cm.vendorExtensions.put("x-is-intersection", true);

                        List<Map<String, String>> parents = new ArrayList<>();
                        Set<String> parentPropertyNames = new HashSet<>();
                        for (String parentName : parentNames) {
                            Map<String, String> parentInfo = new HashMap<>();
                            parentInfo.put("modelName", parentName);
                            parents.add(parentInfo);

                            ModelsMap parentModels = result.get(parentName);
                            if (parentModels != null) {
                                for (ModelMap pmm : parentModels.getModels()) {
                                    for (CodegenProperty prop : pmm.getModel().vars) {
                                        parentPropertyNames.add(prop.name);
                                    }
                                }
                            }
                        }
                        cm.vendorExtensions.put("x-intersection-parents", parents);

                        boolean hasOwnProperties = false;
                        for (CodegenProperty prop : cm.vars) {
                            if (!parentPropertyNames.contains(prop.name)) {
                                prop.vendorExtensions.put("x-is-own-property", true);
                                hasOwnProperties = true;
                            }
                        }
                        cm.vendorExtensions.put("x-has-own-properties", hasOwnProperties);
                    }
                }

                // Check if model has properties that reference other object models
                // (not enums - enums are identity-mapped and don't need DTO mappers)
                for (CodegenProperty prop : cm.vars) {
                    if (prop.complexType != null && !prop.isArray) {
                        if (enumModelNames.contains(prop.complexType)) {
                            // Reference to an enum type - mark for DTO template to use types. prefix
                            prop.vendorExtensions.put("x-is-enum-ref", true);
                            prop.vendorExtensions.put("x-enum-type", prop.complexType);
                        } else {
                            // Reference to an object type - needs DTO mapper
                            prop.vendorExtensions.put("x-is-ref", true);
                            prop.vendorExtensions.put("x-ref-type", prop.complexType);
                        }
                    } else if (prop.isArray && prop.items != null && prop.items.complexType != null) {
                        if (enumModelNames.contains(prop.items.complexType)) {
                            prop.vendorExtensions.put("x-is-enum-ref", true);
                            prop.vendorExtensions.put("x-enum-type", prop.items.complexType);
                        } else {
                            prop.vendorExtensions.put("x-is-ref", true);
                            prop.vendorExtensions.put("x-ref-type", prop.items.complexType);
                        }
                    }
                }

                // Add DTO-related vendor extensions
                cm.vendorExtensions.put("x-dto-name", cm.classname + "Dto");
                cm.vendorExtensions.put("x-schema-name", cm.classname + "Schema");

                // Check if has single-value enum properties (literal types)
                for (CodegenProperty prop : cm.vars) {
                    if (prop.isEnum && prop.allowableValues != null) {
                        List<?> values = (List<?>) prop.allowableValues.get("values");
                        if (values != null && values.size() == 1) {
                            prop.vendorExtensions.put("x-is-literal", true);
                            prop.vendorExtensions.put("x-literal-value", values.get(0).toString());
                        }
                    }
                }
            }
        }
    }

    /**
     * Add Zod-specific vendor extensions to a model property: the bare Zod expression
     * ({@code x-zod-type}) and the full expression including nullability and optionality
     * ({@code x-zod-full}).
     */
    private static void addZodVendorExtensions(CodegenProperty prop) {
        String zodType = zodExpr(prop);
        prop.vendorExtensions.put("x-zod-type", zodType);

        String fullZod = zodType;
        if (prop.isNullable) {
            fullZod = fullZod + ".nullable()";
        }
        if (!prop.required) {
            fullZod = fullZod + ".optional()";
        }
        prop.vendorExtensions.put("x-zod-full", fullZod);
    }

    /**
     * Find the discriminator property's literal value in a member model.
     * E.g., for UserLoginPrincipal with discriminator "kind", returns "USER_LOGIN".
     */
    private static String findDiscriminatorValue(String modelName, String discriminatorProperty,
                                                 Map<String, ModelsMap> allModels) {
        ModelsMap modelsMap = allModels.get(modelName);
        if (modelsMap == null) return null;

        for (ModelMap mo : modelsMap.getModels()) {
            CodegenModel cm = mo.getModel();
            for (CodegenProperty prop : cm.vars) {
                if (prop.baseName.equals(discriminatorProperty) || prop.name.equals(discriminatorProperty)) {
                    if (prop.isEnum && prop.allowableValues != null) {
                        List<?> values = (List<?>) prop.allowableValues.get("values");
                        if (values != null && values.size() == 1) {
                            return values.get(0).toString();
                        }
                    }
                }
            }
        }
        return null;
    }

    /**
     * Topologically sort models so that leaf types (enums, simple objects) come before
     * models that reference them. This ensures correct declaration order in generated schemas.
     */
    @SuppressWarnings("unchecked")
    static List<Object> topologicallySortModels(List<Object> models) {
        // Build maps from classname to model entry and dependencies
        Map<String, Object> modelByName = new LinkedHashMap<>();
        Map<String, Set<String>> dependencies = new LinkedHashMap<>();

        for (Object modelEntry : models) {
            Map<String, Object> entry = (Map<String, Object>) modelEntry;
            CodegenModel cm = (CodegenModel) entry.get("model");
            if (cm == null) continue;

            modelByName.put(cm.classname, modelEntry);

            Set<String> deps = new LinkedHashSet<>();
            for (CodegenProperty prop : cm.vars) {
                if (prop.complexType != null) {
                    deps.add(prop.complexType);
                }
                if (prop.isArray && prop.items != null && prop.items.complexType != null) {
                    deps.add(prop.items.complexType);
                }
            }
            // oneOf dependencies
            if (cm.oneOf != null) {
                deps.addAll(cm.oneOf);
            }
            // Also include non-discriminated union member dependencies
            if (cm.vendorExtensions.containsKey("x-union-members")) {
                List<Map<String, Object>> unionMembers =
                    (List<Map<String, Object>>) cm.vendorExtensions.get("x-union-members");
                for (Map<String, Object> member : unionMembers) {
                    deps.add((String) member.get("modelName"));
                }
            }
            // Also include allOf intersection parent dependencies
            if (cm.vendorExtensions.containsKey("x-intersection-parents")) {
                List<Map<String, String>> intersectionParents =
                    (List<Map<String, String>>) cm.vendorExtensions.get("x-intersection-parents");
                for (Map<String, String> parent : intersectionParents) {
                    deps.add(parent.get("modelName"));
                }
            }
            // Remove self-references
            deps.remove(cm.classname);
            dependencies.put(cm.classname, deps);
        }

        // DFS-based topological sort
        List<Object> sorted = new ArrayList<>();
        Set<String> visited = new LinkedHashSet<>();
        Set<String> visiting = new LinkedHashSet<>();

        for (String name : modelByName.keySet()) {
            if (!visited.contains(name)) {
                topoVisit(name, modelByName, dependencies, visited, visiting, sorted);
            }
        }

        return sorted;
    }

    private static void topoVisit(String name, Map<String, Object> modelByName,
                                  Map<String, Set<String>> dependencies,
                                  Set<String> visited, Set<String> visiting, List<Object> sorted) {
        if (visited.contains(name)) return;
        if (visiting.contains(name)) return; // cycle detected, break it
        visiting.add(name);

        Set<String> deps = dependencies.getOrDefault(name, Collections.emptySet());
        for (String dep : deps) {
            if (modelByName.containsKey(dep)) {
                topoVisit(dep, modelByName, dependencies, visited, visiting, sorted);
            }
        }

        visiting.remove(name);
        visited.add(name);
        if (modelByName.containsKey(name)) {
            sorted.add(modelByName.get(name));
        }
    }

    // ---------------------------------------------------------------------
    // Response / body shape resolution
    // ---------------------------------------------------------------------

    /**
     * Resolve an operation's response ({@code returnType}) into a Zod schema, wire type,
     * and mapper. {@code dir} is {@code TO_WIRE} on the server (domain result -&gt; wire)
     * and {@code FROM_WIRE} on the client (wire json -&gt; domain).
     */
    static Resolved resolveResponse(CodegenOperation op, Direction dir) {
        CodegenProperty rp = op.returnProperty;
        String zod = zodExpr(rp);
        if (rp == null) {
            // DefaultCodegen assigns returnType and returnProperty together, so this is
            // unreachable when callers guard on returnType != null; identity as a defensive
            // fallback rather than fabricating a "<returnType>Dto" symbol.
            return new Resolved(zod, op.returnType, op.returnType, null);
        }
        Shape shape = shapeOf(rp, dir, 0);
        return new Resolved(zod, shape.dtoType, shape.domainType, shape.mapper);
    }

    /**
     * Resolve a request body parameter into a wire type and mapper. {@code zod} is the
     * body's Zod expression (built by the caller's parameter builder). {@code dir} is
     * {@code FROM_WIRE} on the server (req.body -&gt; domain) and {@code TO_WIRE} on the
     * client (domain -&gt; wire before send).
     */
    static Resolved resolveBody(CodegenParameter body, Direction dir, String zod) {
        Shape shape;
        if (body.isArray && body.items != null) {
            shape = arrayShape(body.items, dir, 0);
        } else if (body.isMap && body.additionalProperties != null) {
            shape = mapShape(body.additionalProperties, dir, 0);
        } else if (body.isModel) {
            // For a scalar model body, CodegenParameter.dataType is the classname of the
            // referenced model (e.g. "Pet"). Bodies that dereference to a primitive (e.g.
            // a $ref'd enum handled as string by fromRequestBody) have isModel=false.
            shape = new Shape("dtos." + body.dataType + "Dto", "types." + body.dataType,
                    "mappers." + mapPrefix(dir) + body.dataType + "Dto");
        } else {
            // Bare primitive: domain == wire, identity mapping.
            shape = new Shape(body.dataType, body.dataType, null);
        }
        return new Resolved(zod, shape.dtoType, shape.domainType, shape.mapper);
    }

    /** Wire type, domain type, and mapper (null =&gt; identity) for a resolved shape. */
    private static final class Shape {
        final String dtoType;
        final String domainType;
        final String mapper;

        Shape(String dtoType, String domainType, String mapper) {
            this.dtoType = dtoType;
            this.domainType = domainType;
            this.mapper = mapper;
        }
    }

    /**
     * Recursively resolve a property's shape. Containers recurse into their element/value
     * type (so nested shapes like {@code Array<Array<Pet>>} or map-of-array compose
     * correctly); {@code depth} suffixes lambda parameters to avoid shadowing.
     *
     * <p>Note: container properties carry the innermost ref in {@code complexType}
     * (DefaultCodegen.updatePropertyForArray/Map), so isArray/isMap MUST be checked
     * before the complexType leaf branches.
     */
    private static Shape shapeOf(CodegenProperty p, Direction dir, int depth) {
        if (p.isArray && p.items != null) {
            return arrayShape(p.items, dir, depth);
        }
        if (p.isMap && p.additionalProperties != null) {
            return mapShape(p.additionalProperties, dir, depth);
        }
        if (p.complexType != null && !p.isEnum && !p.isEnumRef) {
            // Object model — has a generated Dto + mappers. $ref'd enums set isEnumRef
            // (not isEnum) and get neither, so they must not take this branch.
            return new Shape("dtos." + p.complexType + "Dto", "types." + p.complexType,
                    "mappers." + mapPrefix(dir) + p.complexType + "Dto");
        }
        if (p.complexType != null) {
            // Enum (inline via isEnum or $ref via isEnumRef) — domain == wire, identity.
            return new Shape("types." + p.complexType, "types." + p.complexType, null);
        }
        // Primitive or free-form object — domain == wire, identity.
        return new Shape(p.dataType, p.dataType, null);
    }

    private static Shape arrayShape(CodegenProperty items, Direction dir, int depth) {
        Shape inner = shapeOf(items, dir, depth + 1);
        String dtoElem = inner.dtoType;
        String domainElem = inner.domainType;
        String elemMapper = inner.mapper;
        if (items.isNullable) {
            // Match getTypeDeclaration's Array<X | null> and skip nulls when mapping.
            dtoElem = "(" + dtoElem + " | null)";
            domainElem = "(" + domainElem + " | null)";
            if (elemMapper != null) {
                elemMapper = nullSafe(elemMapper, depth);
            }
        }
        String mapper = elemMapper == null
                ? null
                : "(xs" + depth + ") => xs" + depth + ".map(" + elemMapper + ")";
        return new Shape(dtoElem + "[]", domainElem + "[]", mapper);
    }

    private static Shape mapShape(CodegenProperty values, Direction dir, int depth) {
        Shape inner = shapeOf(values, dir, depth + 1);
        String dtoVal = inner.dtoType;
        String domainVal = inner.domainType;
        String valMapper = inner.mapper;
        if (values.isNullable) {
            dtoVal = dtoVal + " | null";
            domainVal = domainVal + " | null";
            if (valMapper != null) {
                valMapper = nullSafe(valMapper, depth);
            }
        }
        String mapper = valMapper == null
                ? null
                : "(m" + depth + ") => Object.fromEntries(Object.entries(m" + depth
                    + ").map(([k" + depth + ", v" + depth + "]) => [k" + depth
                    + ", (" + valMapper + ")(v" + depth + ")]))";
        return new Shape("{ [key: string]: " + dtoVal + " }",
                "{ [key: string]: " + domainVal + " }", mapper);
    }

    /** Wrap a mapper so null/undefined elements pass through untouched. */
    private static String nullSafe(String mapper, int depth) {
        String e = "e" + depth;
        return "(" + e + ") => " + e + " == null ? " + e + " : (" + mapper + ")(" + e + ")";
    }

    private static String mapPrefix(Direction dir) {
        return dir == Direction.TO_WIRE ? "mapTo" : "mapFrom";
    }
}
