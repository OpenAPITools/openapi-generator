package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.security.SecurityScheme;
import lombok.Getter;

/**
 * Encapsulates an OAS parameter.
 */
public class SecuritySchemeWrapper {
    OpenAPI specification;
    /**
     * -- GETTER --
     * Return the OAS securityScheme
     *
     * @return the OAS securityScheme
     */
    @Getter private SecurityScheme securityScheme;

    /**
     * Constructs a new instance of {@link SecuritySchemeWrapper}
     *
     * @param specification  The OAS specification
     * @param securityScheme The OAS securityScheme
     */
    SecuritySchemeWrapper(OpenAPI specification, SecurityScheme securityScheme) {
        this.specification = specification;
        this.securityScheme = securityScheme;
    }

    /**
     * Returns the OpenAPI specification.
     *
     * @return The OpenAPI specification.
     */
    public OpenAPI getOpenAPI() {
        return specification;
    }
}
