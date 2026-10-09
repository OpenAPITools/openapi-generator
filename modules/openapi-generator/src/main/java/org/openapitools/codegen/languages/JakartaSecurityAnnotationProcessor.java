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

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import org.openapitools.codegen.CodegenOperation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Translates an OpenAPI operation's security requirements into Jakarta security
 * vendor extensions on a {@link CodegenOperation} for downstream Mustache templates.
 *
 * <p>The OpenAPI {@code security} array uses OR semantics (any one alternative
 * satisfies the request); the Jakarta annotations are AND-stacked. The two cannot
 * always be reconciled, so this class emits the least restrictive annotation that
 * is still correct for the OR group.
 *
 * <p>Two mutually exclusive vendor extensions carry the emission decision:
 * <ul>
 *   <li>{@code x-jakarta-roles-allowed} = {@code ["**"]} for the any-authenticated-user
 *       case, producing {@code @RolesAllowed({"**"})}.
 *   <li>{@code x-jakarta-roles-allowed} = sorted, deduplicated list of scope names
 *       (e.g. {@code ["admin", "user"]}) when every OR alternative is scoped, producing
 *       {@code @RolesAllowed({"admin","user"})}.
 *   <li>{@code x-jakarta-permit-all} = {@code true} when the operation is unauthenticated
 *       (explicit {@code security: []}, an anonymous {@code - {}} OR alternative, or an
 *       entirely unsecured spec), producing {@code @PermitAll}.
 *   <li>Neither set when the operation does not qualify (mixed-scope AND group,
 *       undefined scheme, etc.) — nothing is emitted and a warning is logged.
 * </ul>
 *
 * <p>The three emissions are mutually exclusive per operation: if any OR alternative
 * qualifies as "any authenticated user", the wildcard wins; otherwise the scoped path
 * is tried; otherwise {@code @PermitAll} is tried.
 */
final class JakartaSecurityAnnotationProcessor {

    static final String VENDOR_X_JAKARTA_ROLES_ALLOWED = "x-jakarta-roles-allowed";
    static final String VENDOR_X_JAKARTA_PERMIT_ALL = "x-jakarta-permit-all";

    private static final List<String> ANY_AUTHENTICATED_ROLE = Collections.singletonList("**");

    private final Logger LOGGER = LoggerFactory.getLogger(JakartaSecurityAnnotationProcessor.class);

    /**
     * Inspects {@code rawOp}'s security requirements (falling back to the global
     * {@code openAPI.security} when the operation does not override) and sets either
     * {@code x-jakarta-roles-allowed} (for {@code @RolesAllowed}) or
     * {@code x-jakarta-permit-all} (for {@code @PermitAll}) on {@code op}.
     */
    void applyTo(CodegenOperation op, Operation rawOp, OpenAPI openAPI) {
        // Use the raw Operation here rather than op.authMethods: by the time postProcessOperationsWithModels
        // runs, DefaultGenerator.filterAuthMethods has flattened all SecurityRequirements into a plain list,
        // losing the AND-group structure needed to evaluate mixed-scope combinations correctly.
        List<SecurityRequirement> requirements = rawOp.getSecurity();
        if (requirements == null) {
            // Fall back to the global security block when the operation does not override it.
            requirements = openAPI.getSecurity();
        }
        Map<String, SecurityScheme> schemes = resolveSchemes(openAPI);

        if (qualifiesForAnyRoles(requirements, schemes)) {
            op.vendorExtensions.put(VENDOR_X_JAKARTA_ROLES_ALLOWED, ANY_AUTHENTICATED_ROLE);
            return; // mutually exclusive -- short-circuit before the scoped path runs
        }
        List<String> scopes = collectRolesAllowedScopes(requirements, schemes);
        if (scopes != null && !scopes.isEmpty()) {
            op.vendorExtensions.put(VENDOR_X_JAKARTA_ROLES_ALLOWED, scopes);
            return;
        }
        if (qualifiesForPermitAll(rawOp, openAPI, requirements)) {
            op.vendorExtensions.put(VENDOR_X_JAKARTA_PERMIT_ALL, Boolean.TRUE);
        }
    }

    /**
     * Returns true when at least one OR alternative fully qualifies for
     * {@code @RolesAllowed({"**"})} and no alternative is anonymous ({@code - {}}).
     *
     * <p>An empty {@link SecurityRequirement} ({@code - {}}) inside the OR list means
     * the operation may also be called unauthenticated. When that is present, the
     * least-restrictive alternative is "no auth required", so emitting
     * {@code @RolesAllowed({"**"})} would force authentication and contradict the
     * spec -- we return false instead and let the future {@code @PermitAll} branch
     * handle that case.
     */
    private boolean qualifiesForAnyRoles(List<SecurityRequirement> requirements,
            Map<String, SecurityScheme> schemes) {
        if (requirements == null || requirements.isEmpty()) {
            return false;
        }
        boolean anyQualifies = false;
        for (SecurityRequirement requirement : requirements) {
            if (requirement.isEmpty()) {
                // Anonymous OR alternative -- least restrictive wins; do not emit @RolesAllowed.
                return false;
            }
            if (andGroupQualifies(requirement, schemes)) {
                anyQualifies = true;
            }
        }
        return anyQualifies;
    }

    /**
     * Returns true when the operation should emit {@code @PermitAll} -- the
     * "no authentication required" cases that {@code qualifiesForAnyRoles} and
     * {@code collectRolesAllowedScopes} deliberately reject.
     *
     * <p>The decision uses raw op-level and global security fields (not the already
     * resolved {@code effectiveRequirements}) so it can distinguish explicit op-level
     * opt-out ({@code security: []}) from global inheritance.
     *
     * <ul>
     *   <li>Op-level {@code security: []} -> always permit-all (overrides any global).
     *   <li>No op-level security AND global {@code security: []} -> inherits empty.
     *   <li>No op-level security AND no global security -> the spec declares the
     *       entire API unauthenticated.
     *   <li>Op-level OR list contains {@code - {}} -> least-restrictive wins.
     * </ul>
     *
     * <p>This method returns false for mixed-scope AND groups, undefined schemes, and
     * other ambiguous cases -- those bail with a warning at the {@code @RolesAllowed}
     * stage and must NOT silently fall through to {@code @PermitAll}.
     */
    private boolean qualifiesForPermitAll(Operation rawOp, OpenAPI openAPI, List<SecurityRequirement> effectiveRequirements) {
        List<SecurityRequirement> opSecurity = rawOp.getSecurity();
        if (opSecurity != null && opSecurity.isEmpty()) {
            // Explicit op-level opt-out wins over any global setting.
            return true;
        }
        if (opSecurity == null) {
            List<SecurityRequirement> globalSecurity = openAPI.getSecurity();
            if (globalSecurity == null) {
                // Spec defines no security at all -- every operation is unauthenticated.
                return true;
            }
            if (globalSecurity.isEmpty()) {
                // Operation inherits the global empty list -- unauthenticated.
                return true;
            }
        }
        if (effectiveRequirements != null) {
            for (SecurityRequirement requirement : effectiveRequirements) {
                if (requirement.isEmpty()) {
                    // Anonymous OR alternative -- least restrictive wins.
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * A single {@link SecurityRequirement} is an AND group: all schemes must be
     * satisfied simultaneously. If any scheme in the group has explicit scopes
     * (e.g. {@code oauth2: [admin:write]}), the combined requirement is more
     * restrictive than "any authenticated user" and does not qualify.
     */
    private boolean andGroupQualifies(SecurityRequirement requirement, Map<String, SecurityScheme> schemes) {
        for (Map.Entry<String, List<String>> entry : requirement.entrySet()) {
            SecurityScheme scheme = schemes.get(entry.getKey());
            if (scheme == null) {
                LOGGER.warn("Security requirement references undefined scheme '{}' -- skipping Jakarta security annotation for this AND group.",
                        entry.getKey());
                return false;
            }
            if (!schemeQualifies(scheme, entry.getValue())) {
                return false;
            }
        }
        return true;
    }

    private boolean schemeQualifies(SecurityScheme scheme, List<String> scopes) {
        if (scheme.getType() == null) {
            LOGGER.warn("Security scheme is missing 'type' -- skipping Jakarta security annotation.");
            return false;
        }
        switch (scheme.getType()) {
            case OAUTH2:
            case OPENIDCONNECT:
                // Empty scope list means the operation requires authentication but no specific role,
                // so @RolesAllowed({"**"}) is correct. Non-empty scopes are handled by collectRolesAllowedScopes.
                return scopes == null || scopes.isEmpty();
            case HTTP:
            case APIKEY:
            case MUTUALTLS:
                // These schemes have no scope concept; any valid credential satisfies them.
                return true;
            default:
                LOGGER.warn("Unrecognised security scheme type '{}' -- skipping Jakarta security annotation.",
                        scheme.getType());
                return false;
        }
    }

    /**
     * Returns the deduplicated, alphabetically sorted union of scope names across every OR
     * alternative, or {@code null} if the requirement set does not qualify (anonymous OR
     * alternative, mixed-scope AND group, undefined scheme, or no requirements at all).
     *
     * <p>A {@code null} return means the scoped {@code @RolesAllowed} annotation must not
     * be emitted for this operation.
     */
    private List<String> collectRolesAllowedScopes(List<SecurityRequirement> requirements,
            Map<String, SecurityScheme> schemes) {
        if (requirements == null || requirements.isEmpty()) {
            return null;
        }
        Set<String> union = new TreeSet<>(); // sorted, deduplicated
        for (SecurityRequirement requirement : requirements) {
            if (requirement.isEmpty()) {
                // Anonymous OR alternative -- defer to @PermitAll (future PR).
                return null;
            }
            List<String> groupScopes = collectAndGroupScopes(requirement, schemes);
            if (groupScopes == null) {
                // Unscopable AND group -- bail the entire operation.
                return null;
            }
            union.addAll(groupScopes);
        }
        return new ArrayList<>(union);
    }

    /**
     * Returns the scope list contributed by a single AND group, or {@code null} if the AND
     * group cannot be expressed as a single Jakarta {@code @RolesAllowed} annotation.
     *
     * <p>At most ONE scheme in the AND group may have non-empty scopes (the "scoped scheme").
     * If two or more schemes carry competing scope sets, Quarkus annotations cannot express
     * the AND-of-different-scope-sets relationship -- we log a warning and return {@code null}.
     *
     * <p>An empty list (not {@code null}) is returned when the AND group is valid but no
     * scheme contributes scopes; the caller treats that as "no scopes from this alternative".
     */
    private List<String> collectAndGroupScopes(SecurityRequirement requirement,
            Map<String, SecurityScheme> schemes) {
        List<String> scopedSchemeScopes = null;
        int scopedSchemeCount = 0;
        for (Map.Entry<String, List<String>> entry : requirement.entrySet()) {
            SecurityScheme scheme = schemes.get(entry.getKey());
            if (scheme == null) {
                LOGGER.warn("Security requirement references undefined scheme '{}' -- skipping Jakarta scoped @RolesAllowed for this operation.",
                        entry.getKey());
                return null;
            }
            if (scheme.getType() == null) {
                LOGGER.warn("Security scheme '{}' is missing 'type' -- skipping Jakarta scoped @RolesAllowed.",
                        entry.getKey());
                return null;
            }
            switch (scheme.getType()) {
                case OAUTH2:
                case OPENIDCONNECT:
                    List<String> scopes = entry.getValue();
                    if (scopes != null && !scopes.isEmpty()) {
                        scopedSchemeCount++;
                        if (scopedSchemeCount > 1) {
                            LOGGER.warn(
                                    "AND-group contains multiple scoped schemes (e.g. '{}'); Jakarta @RolesAllowed cannot express AND of different scope sets -- skipping scoped @RolesAllowed for this operation.",
                                    entry.getKey());
                            return null;
                        }
                        scopedSchemeScopes = scopes;
                    }
                    // Unscoped OAuth2/OIDC contributes nothing to the scope list.
                    break;
                case HTTP:
                case APIKEY:
                case MUTUALTLS:
                    // No scope concept; participates in the AND group but contributes no scopes.
                    break;
                default:
                    LOGGER.warn("Unrecognised security scheme type '{}' -- skipping Jakarta scoped @RolesAllowed.",
                            scheme.getType());
                    return null;
            }
        }
        return scopedSchemeScopes != null ? scopedSchemeScopes : Collections.emptyList();
    }

    private static Map<String, SecurityScheme> resolveSchemes(OpenAPI openAPI) {
        if (openAPI.getComponents() != null && openAPI.getComponents().getSecuritySchemes() != null) {
            return openAPI.getComponents().getSecuritySchemes();
        }
        return Collections.emptyMap();
    }
}
