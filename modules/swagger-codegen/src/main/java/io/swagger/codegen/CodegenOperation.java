package io.swagger.codegen;

import io.swagger.models.ExternalDocs;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Arrays;

public class CodegenOperation {
    public final List<CodegenProperty> responseHeaders = new ArrayList<CodegenProperty>();
    public boolean hasAuthMethods, hasConsumes, hasProduces, hasParams, hasOptionalParams,
            returnTypeIsPrimitive, returnSimpleType, subresourceOperation, isMapContainer,
            isListContainer, isMultipart, hasMore = true,
            isResponseBinary = false, isResponseFile = false, hasReference = false,
            isRestfulIndex, isRestfulShow, isRestfulCreate, isRestfulUpdate, isRestfulDestroy,
            isRestful;
    public String path, operationId, returnType, httpMethod, returnBaseType,
            returnContainer, summary, unescapedNotes, notes, baseName, defaultResponse, discriminator;
    public List<Map<String, String>> consumes, produces, prioritizedContentTypes;
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
    public String operationIdLowerCase; // for mardown documentation
    public String operationIdCamelCase; // for class names
    public String operationIdSnakeCase;

    /**
     * Check if there's at least one parameter
     *
     * @return true if parameter exists, false otherwise
     */
    private static boolean nonempty(List<?> params) {
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

    /**
     * Check if there's at least one example parameter
     *
     * @return true if examples parameter exists, false otherwise
     */
    public boolean getHasExamples() {
        return nonempty(examples);
    }

    /**
     * Check if act as Restful index method
     *
     * @return true if act as Restful index method, false otherwise
     */
    public boolean isRestfulIndex() {
        return "GET".equals(httpMethod) && "".equals(pathWithoutBaseName());
    }

    /**
     * Check if act as Restful show method
     *
     * @return true if act as Restful show method, false otherwise
     */
    public boolean isRestfulShow() {
        return "GET".equals(httpMethod) && isMemberPath();
    }

    /**
     * Check if act as Restful create method
     *
     * @return true if act as Restful create method, false otherwise
     */
    public boolean isRestfulCreate() {
        return "POST".equals(httpMethod) && "".equals(pathWithoutBaseName());
    }

    /**
     * Check if act as Restful update method
     *
     * @return true if act as Restful update method, false otherwise
     */
    public boolean isRestfulUpdate() {
        return Arrays.asList("PUT", "PATCH").contains(httpMethod) && isMemberPath();
    }

    /**
     * Check if act as Restful destroy method
     *
     * @return true if act as Restful destroy method, false otherwise
     */
    public boolean isRestfulDestroy() {
        return "DELETE".equals(httpMethod) && isMemberPath();
    }

    /**
     * Check if Restful-style
     *
     * @return true if Restful-style, false otherwise
     */
    public boolean isRestful() {
        return isRestfulIndex() || isRestfulShow() || isRestfulCreate() || isRestfulUpdate() || isRestfulDestroy();
    }

    /**
     * Get the substring except baseName from path
     *
     * @return the substring
     */
    private String pathWithoutBaseName() {
        return baseName != null ? path.replace("/" + baseName.toLowerCase(), "") : path;
    }

    /**
     * Check if the path match format /xxx/:id
     *
     * @return true if path act as member
     */
    private boolean isMemberPath() {
        if (pathParams.size() != 1) return false;

        String id = pathParams.get(0).baseName;
        return ("/{" + id + "}").equals(pathWithoutBaseName());
    }

    @Override
    public String toString() {
        return String.format("%s(%s)", baseName, path);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CodegenOperation that = (CodegenOperation) o;

        if (responseHeaders != null ? !responseHeaders.equals(that.responseHeaders) : that.responseHeaders != null)
            return false;
        if (hasAuthMethods != that.hasAuthMethods)
            return false;
        if (hasConsumes != that.hasConsumes)
            return false;
        if (hasProduces != that.hasProduces)
            return false;
        if (hasParams != that.hasParams)
            return false;
        if (hasOptionalParams != that.hasOptionalParams)
            return false;
        if (returnTypeIsPrimitive != that.returnTypeIsPrimitive)
            return false;
        if (returnSimpleType != that.returnSimpleType)
            return false;
        if (subresourceOperation != that.subresourceOperation)
            return false;
        if (isMapContainer != that.isMapContainer)
            return false;
        if (isListContainer != that.isListContainer)
            return false;
        if (isMultipart != that.isMultipart)
            return false;
        if (hasMore != that.hasMore)
            return false;
        if (isResponseBinary != that.isResponseBinary)
            return false;
        if (hasReference != that.hasReference)
            return false;
        if (isResponseFile != that.isResponseFile)
            return false;
        if (path != null ? !path.equals(that.path) : that.path != null)
            return false;
        if (operationId != null ? !operationId.equals(that.operationId) : that.operationId != null)
            return false;
        if (returnType != null ? !returnType.equals(that.returnType) : that.returnType != null)
            return false;
        if (httpMethod != null ? !httpMethod.equals(that.httpMethod) : that.httpMethod != null)
            return false;
        if (returnBaseType != null ? !returnBaseType.equals(that.returnBaseType) : that.returnBaseType != null)
            return false;
        if (returnContainer != null ? !returnContainer.equals(that.returnContainer) : that.returnContainer != null)
            return false;
        if (summary != null ? !summary.equals(that.summary) : that.summary != null)
            return false;
        if (unescapedNotes != null ? !unescapedNotes.equals(that.unescapedNotes) : that.unescapedNotes != null)
            return false;
        if (notes != null ? !notes.equals(that.notes) : that.notes != null)
            return false;
        if (baseName != null ? !baseName.equals(that.baseName) : that.baseName != null)
            return false;
        if (defaultResponse != null ? !defaultResponse.equals(that.defaultResponse) : that.defaultResponse != null)
            return false;
        if (discriminator != null ? !discriminator.equals(that.discriminator) : that.discriminator != null)
            return false;
        if (consumes != null ? !consumes.equals(that.consumes) : that.consumes != null)
            return false;
        if (produces != null ? !produces.equals(that.produces) : that.produces != null)
            return false;
        if (bodyParam != null ? !bodyParam.equals(that.bodyParam) : that.bodyParam != null)
            return false;
        if (allParams != null ? !allParams.equals(that.allParams) : that.allParams != null)
            return false;
        if (bodyParams != null ? !bodyParams.equals(that.bodyParams) : that.bodyParams != null)
            return false;
        if (pathParams != null ? !pathParams.equals(that.pathParams) : that.pathParams != null)
            return false;
        if (queryParams != null ? !queryParams.equals(that.queryParams) : that.queryParams != null)
            return false;
        if (headerParams != null ? !headerParams.equals(that.headerParams) : that.headerParams != null)
            return false;
        if (formParams != null ? !formParams.equals(that.formParams) : that.formParams != null)
            return false;
        if (authMethods != null ? !authMethods.equals(that.authMethods) : that.authMethods != null)
            return false;
        if (tags != null ? !tags.equals(that.tags) : that.tags != null)
            return false;
        if (responses != null ? !responses.equals(that.responses) : that.responses != null)
            return false;
        if (imports != null ? !imports.equals(that.imports) : that.imports != null)
            return false;
        if (examples != null ? !examples.equals(that.examples) : that.examples != null)
            return false;
        if (externalDocs != null ? !externalDocs.equals(that.externalDocs) : that.externalDocs != null)
            return false;
        if (vendorExtensions != null ? !vendorExtensions.equals(that.vendorExtensions) : that.vendorExtensions != null)
            return false;
        if (nickname != null ? !nickname.equals(that.nickname) : that.nickname != null)
            return false;
        if ( prioritizedContentTypes != null ? !prioritizedContentTypes.equals(that.prioritizedContentTypes) : that.prioritizedContentTypes != null )
            return false;
        if ( operationIdLowerCase != null ? !operationIdLowerCase.equals(that.operationIdLowerCase) : that.operationIdLowerCase != null )
            return false;
        return operationIdCamelCase != null ? operationIdCamelCase.equals(that.operationIdCamelCase) : that.operationIdCamelCase == null;

    }

    @Override
    public int hashCode() {
        int result = responseHeaders.hashCode();
        result = 31 * result + (hasAuthMethods ? 13:31);
        result = 31 * result + (hasConsumes ? 13:31);
        result = 31 * result + (hasProduces ? 13:31);
        result = 31 * result + (hasParams ? 13:31);
        result = 31 * result + (hasOptionalParams ? 13:31);
        result = 31 * result + (returnTypeIsPrimitive ? 13:31);
        result = 31 * result + (returnSimpleType ? 13:31);
        result = 31 * result + (subresourceOperation ? 13:31);
        result = 31 * result + (isMapContainer ? 13:31);
        result = 31 * result + (isListContainer ? 13:31);
        result = 31 * result + (isMultipart ? 13:31);
        result = 31 * result + (hasMore ? 13:31);
        result = 31 * result + (isResponseBinary ? 13:31);
        result = 31 * result + (isResponseFile ? 13:31);
        result = 31 * result + (hasReference ? 13:31);
        result = 31 * result + (path != null ? path.hashCode() : 0);
        result = 31 * result + (operationId != null ? operationId.hashCode() : 0);
        result = 31 * result + (returnType != null ? returnType.hashCode() : 0);
        result = 31 * result + (httpMethod != null ? httpMethod.hashCode() : 0);
        result = 31 * result + (returnBaseType != null ? returnBaseType.hashCode() : 0);
        result = 31 * result + (returnContainer != null ? returnContainer.hashCode() : 0);
        result = 31 * result + (summary != null ? summary.hashCode() : 0);
        result = 31 * result + (unescapedNotes != null ? unescapedNotes.hashCode() : 0);
        result = 31 * result + (notes != null ? notes.hashCode() : 0);
        result = 31 * result + (baseName != null ? baseName.hashCode() : 0);
        result = 31 * result + (defaultResponse != null ? defaultResponse.hashCode() : 0);
        result = 31 * result + (discriminator != null ? discriminator.hashCode() : 0);
        result = 31 * result + (consumes != null ? consumes.hashCode() : 0);
        result = 31 * result + (produces != null ? produces.hashCode() : 0);
        result = 31 * result + (bodyParam != null ? bodyParam.hashCode() : 0);
        result = 31 * result + (allParams != null ? allParams.hashCode() : 0);
        result = 31 * result + (bodyParams != null ? bodyParams.hashCode() : 0);
        result = 31 * result + (pathParams != null ? pathParams.hashCode() : 0);
        result = 31 * result + (queryParams != null ? queryParams.hashCode() : 0);
        result = 31 * result + (headerParams != null ? headerParams.hashCode() : 0);
        result = 31 * result + (formParams != null ? formParams.hashCode() : 0);
        result = 31 * result + (authMethods != null ? authMethods.hashCode() : 0);
        result = 31 * result + (tags != null ? tags.hashCode() : 0);
        result = 31 * result + (responses != null ? responses.hashCode() : 0);
        result = 31 * result + (imports != null ? imports.hashCode() : 0);
        result = 31 * result + (examples != null ? examples.hashCode() : 0);
        result = 31 * result + (externalDocs != null ? externalDocs.hashCode() : 0);
        result = 31 * result + (vendorExtensions != null ? vendorExtensions.hashCode() : 0);
        result = 31 * result + (nickname != null ? nickname.hashCode() : 0);
        result = 31 * result + (prioritizedContentTypes != null ? prioritizedContentTypes.hashCode() : 0);
        result = 31 * result + (operationIdLowerCase != null ? operationIdLowerCase.hashCode() : 0);
        result = 31 * result + (operationIdCamelCase != null ? operationIdCamelCase.hashCode() : 0);
        return result;
    }
}
