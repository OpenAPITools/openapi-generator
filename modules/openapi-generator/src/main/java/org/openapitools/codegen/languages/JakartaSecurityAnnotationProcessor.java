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
import java.util.Collections;
import java.util.List;
import java.util.Map;
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
 * <p>A single vendor extension {@code x-jakarta-roles-allowed} carries the value to
 * emit. For the any-authenticated-user case it is set to the singleton list
 * {@code ["**"]}, producing {@code @RolesAllowed({"**"})}. Future PRs will reuse
 * the same extension to emit scoped roles (e.g. {@code ["admin"]}) without needing
 * a second flag or template branch.
 */
final class JakartaSecurityAnnotationProcessor {

    static final String VENDOR_X_JAKARTA_ROLES_ALLOWED = "x-jakarta-roles-allowed";

    private static final List<String> ANY_AUTHENTICATED_ROLE = Collections.singletonList("**");

    private final Logger LOGGER = LoggerFactory.getLogger(JakartaSecurityAnnotationProcessor.class);

    /**
     * Inspects {@code rawOp}'s security requirements (falling back to the global
     * {@code openAPI.security} when the operation does not override) and sets
     * {@code x-jakarta-roles-allowed} on {@code op} when the operation qualifies
     * for {@code @RolesAllowed} emission.
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
                // so @RolesAllowed({"**"}) is correct. Non-empty scopes belong to a future @RolesAllowed({scope}) PR.
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

    private static Map<String, SecurityScheme> resolveSchemes(OpenAPI openAPI) {
        if (openAPI.getComponents() != null && openAPI.getComponents().getSecuritySchemes() != null) {
            return openAPI.getComponents().getSecuritySchemes();
        }
        return Collections.emptyMap();
    }
}
