package org.openapitools.codegen.validations.oas;

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.PathItem;
import io.swagger.v3.oas.models.OpenAPI;

/**
 * Encapsulates an operation with its HTTP Method. In OAS, the {@link PathItem} structure contains more than what we'd
 * want to evaluate for operation-only checks.
 */
public class OperationWrapper {
    OpenAPI specification;
    private Operation operation;
    private PathItem.HttpMethod httpMethod;

    /**
     * Constructs a new instance of {@link OperationWrapper}
     *
     * @param operation The operation instances to wrap
     * @param httpMethod The http method to wrap
     */
    OperationWrapper(OpenAPI specification, Operation operation, PathItem.HttpMethod httpMethod) {
        this.specification = specification;
        this.operation = operation;
        this.httpMethod = httpMethod;
    }

    /**
     * Gets the operation associated with the http method
     *
     * @return An operation instance
     */
    public Operation getOperation() {
        return operation;
    }

    /**
     * Gets the http method associated with the operation
     *
     * @return The http method
     */
    public PathItem.HttpMethod getHttpMethod() {
        return httpMethod;
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
