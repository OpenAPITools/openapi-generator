package org.openapitools.codegen.languages;

import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenParameter;
import org.openapitools.codegen.CodegenProperty;

import java.util.List;

import static org.openapitools.codegen.utils.StringUtils.camelize;

/**
 * Shared helpers for the {@code typescript-express-zod-server} and
 * {@code typescript-express-zod-client} generators.
 *
 * <p>The two generators are siblings (both extend {@code AbstractTypeScriptClientCodegen},
 * neither extends the other), so the recursive Zod builder and the array/map/primitive
 * DTO resolution would otherwise be duplicated and free to diverge. Keeping that logic
 * here — the highest-risk, must-stay-identical part — is the single source of truth.
 *
 * <p>The per-generator {@code postProcess*} methods keep their own vendor-extension wiring
 * (which keys they set, {@code dtos.}/{@code mappers.} imports) but delegate the string
 * computation to these static helpers.
 */
final class TypescriptExpressZodSupport {
    private TypescriptExpressZodSupport() {
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
