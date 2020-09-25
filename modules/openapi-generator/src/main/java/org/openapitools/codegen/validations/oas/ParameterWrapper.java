package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.OpenAPI;

/**
 * Encapsulates an OAS parameter.
 */
public class ParameterWrapper {
    OpenAPI specification;
    private Parameter parameter;

    /**
     * Constructs a new instance of {@link ParameterWrapper}
     *
     * @param specification The OAS specification
     * @param parameter The OAS parameter
     */
    ParameterWrapper(OpenAPI specification, Parameter parameter) {
        this.specification = specification;
        this.parameter = parameter;
    }

    /**
     * Return the OAS parameter
     *
     * @return the OAS parameter
     */
    public Parameter getParameter() {
        return parameter;
    }

    /**
     * Returns the OpenAPI specification.
     *
     * @return The the OpenAPI specification.
     */
    public OpenAPI getOpenAPI() {
        return specification;
    }
}
