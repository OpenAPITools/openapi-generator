package io.swagger.codegen;

import io.swagger.models.ExternalDocs;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class CodegenOperation {
    public final List<CodegenProperty> responseHeaders = new ArrayList<CodegenProperty>();
    public Boolean hasAuthMethods, hasConsumes, hasProduces, hasParams, hasOptionalParams,
            returnTypeIsPrimitive, returnSimpleType, subresourceOperation, isMapContainer,
            isListContainer, isMultipart, hasMore = Boolean.TRUE,
            isResponseBinary = Boolean.FALSE, hasReference = Boolean.FALSE;
    public String path, operationId, returnType, httpMethod, returnBaseType,
            returnContainer, summary, notes, baseName, defaultResponse;
    public List<Map<String, String>> consumes, produces;
    public CodegenParameter bodyParam;
    public List<CodegenParameter> allParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> bodyParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> pathParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> queryParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> headerParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> formParams = new ArrayList<CodegenParameter>();
    public List<CodegenSecurity> authMethods;
    public List<String> tags;
    public List<CodegenResponse> responses = new ArrayList<CodegenResponse>();
    public Set<String> imports = new HashSet<String>();
    public List<Map<String, String>> examples;
    public ExternalDocs externalDocs;
    public Map<String, Object> vendorExtensions;
    public String nickname; // legacy support

    /**
     * Check if there's at least one parameter
     *
     * @return true if parameter exists, false otherwise
     */
    private static boolean nonempty(List<CodegenParameter> params) {
        return params != null && params.size() > 0;
    }

    /**
     * Check if there's at least one body parameter
     *
     * @return true if body parameter exists, false otherwise
     */
    public boolean getHasBodyParam() {
        return nonempty(bodyParams);
    }

    /**
     * Check if there's at least one query parameter
     *
     * @return true if query parameter exists, false otherwise
     */
    public boolean getHasQueryParams() {
        return nonempty(queryParams);
    }

    /**
     * Check if there's at least one header parameter
     *
     * @return true if header parameter exists, false otherwise
     */
    public boolean getHasHeaderParams() {
        return nonempty(headerParams);
    }

    /**
     * Check if there's at least one path parameter
     *
     * @return true if path parameter exists, false otherwise
     */
    public boolean getHasPathParams() {
        return nonempty(pathParams);
    }

    /**
     * Check if there's at least one form parameter
     *
     * @return true if any form parameter exists, false otherwise
     */
    public boolean getHasFormParams() {
        return nonempty(formParams);
    }

}
