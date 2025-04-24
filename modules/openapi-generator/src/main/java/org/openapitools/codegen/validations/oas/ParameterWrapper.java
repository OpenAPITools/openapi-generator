package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.parameters.Parameter;
import lombok.Getter;

/**
 * Encapsulates an OAS parameter.
 */
public class ParameterWrapper {
    OpenAPI specification;
    /**
     * -- GETTER --
     * Return the OAS parameter
     *
     * @return the OAS parameter
     */
    @Getter private Parameter parameter;

    /**
     * Constructs a new instance of {@link ParameterWrapper}
     *
     * @param specification The OAS specification
     * @param parameter     The OAS parameter
     */
    ParameterWrapper(OpenAPI specification, Parameter parameter) {
        this.specification = specification;
        this.parameter = parameter;
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
