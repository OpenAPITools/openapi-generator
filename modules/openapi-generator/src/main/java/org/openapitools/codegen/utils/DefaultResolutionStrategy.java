package org.openapitools.codegen.utils;

/**
 * Defines the strategy used by {@link ModelUtils#resolveDefault(io.swagger.v3.oas.models.OpenAPI,
 * io.swagger.v3.oas.models.media.Schema, DefaultResolutionStrategy)} when an OpenAPI schema
 * composed with {@code allOf} carries {@code default} values in more than one branch.
 *
 * <p>OpenAPI does not specify precedence rules for {@code default} in {@code allOf} compositions,
 * so any single strategy is a generator-level convention rather than a spec guarantee.
 * Callers that need reproducible, portable behaviour should prefer {@link #STRICT} to detect
 * ambiguity early, and fall back to an explicit heuristic mode only when the spec cannot be
 * improved.</p>
 *
 * <h3>How candidates are collected</h3>
 * <p>Internally, a single <em>post-order DFS</em> traversal of the fully-resolved schema tree
 * (including {@code $ref} expansion and recursive {@code allOf} flattening) produces an ordered
 * list of {@link ModelUtils.DefaultCandidate} objects. Each candidate records:
 * <ul>
 *   <li>{@code value} — the raw default value found on that schema node</li>
 *   <li>{@code depth} — 0 for the root schema, 1 for its {@code allOf} items, and so on</li>
 *   <li>{@code visitOrder} — monotonically increasing post-order DFS index; the root schema's
 *       direct {@code default:} (if any) is always appended <em>last</em>, giving it the highest
 *       {@code visitOrder} among all candidates</li>
 * </ul>
 * Each strategy below is then a pure selector over that list.</p>
 */
public enum DefaultResolutionStrategy {

    /**
     * Spec-faithful strict mode: never silently resolve conflicts.
     *
     * <ul>
     *   <li>0 candidates → {@code null}</li>
     *   <li>1 distinct value → return it</li>
     *   <li>2+ distinct values → emit {@code LOGGER.warn} with a human-readable conflict message
     *       and return {@code null}</li>
     * </ul>
     *
     * <p>This is the most honest interpretation of the OpenAPI specification. Use it when
     * consistency across generators is more important than silently choosing a winner.</p>
     */
    STRICT,

    /**
     * Heuristic mode: the schema node <em>closest</em> (shallowest) to the root property
     * definition wins.
     *
     * <p>Selection rule: {@code min(depth), then min(visitOrder)} among all candidates.
     * A direct {@code default:} on the root schema (depth 0) therefore always wins; if the root
     * has no direct default, the leftmost {@code allOf} item at depth 1 is tried next, and so
     * on. This strategy treats "nearest to the consumer" as "most specific".</p>
     *
     * <p>Emits a {@code LOGGER.warn} when more than one distinct value is found.</p>
     */
    NEAREST_WINS,

    /**
     * Heuristic mode (current default behaviour): the last non-null default encountered in the
     * post-order DFS traversal wins.
     *
     * <p>Selection rule: {@code max(visitOrder)}. Because the post-order traversal appends each
     * schema's direct {@code default:} <em>after</em> its children, the root schema's direct
     * default always has the highest {@code visitOrder} and therefore always wins when present.
     * When the root has no direct default, the last {@code allOf} branch (recursively resolved)
     * wins.</p>
     *
     * <p>This is the strategy used by the zero-argument
     * {@link ModelUtils#resolveDefault(io.swagger.v3.oas.models.OpenAPI,
     * io.swagger.v3.oas.models.media.Schema)} overload for backward compatibility.</p>
     */
    LAST_WINS,

    /**
     * Heuristic mode: only the outermost schema's direct {@code default:} counts.
     *
     * <p>Selection rule: filter candidates to {@code depth == 0} and return the first (and only
     * possible) match. No {@code allOf} traversal result is used. Returns {@code null} when the
     * root schema has no direct {@code default:}, regardless of what nested branches define.</p>
     *
     * <p>Emits a {@code LOGGER.debug} when {@code allOf} branches carry defaults that are being
     * ignored.</p>
     */
    ROOT_WINS
}
