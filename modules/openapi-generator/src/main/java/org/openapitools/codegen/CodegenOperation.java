/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen;

import io.swagger.v3.oas.models.ExternalDocumentation;
import io.swagger.v3.oas.models.tags.Tag;

import java.util.*;

public class CodegenOperation {
    public final List<CodegenProperty> responseHeaders = new ArrayList<CodegenProperty>();
    public boolean hasAuthMethods, hasConsumes, hasProduces, hasParams, hasOptionalParams, hasRequiredParams,
            returnTypeIsPrimitive, returnSimpleType, subresourceOperation, isMapContainer,
            isListContainer, isMultipart, hasMore = true,
            isResponseBinary = false, isResponseFile = false, hasReference = false,
            isRestfulIndex, isRestfulShow, isRestfulCreate, isRestfulUpdate, isRestfulDestroy,
            isRestful, isDeprecated, isCallbackRequest;
    public String path, operationId, returnType, httpMethod, returnBaseType,
            returnContainer, summary, unescapedNotes, notes, baseName, defaultResponse;
    public CodegenDiscriminator discriminator;
    public List<Map<String, String>> consumes, produces, prioritizedContentTypes;
    public List<CodegenServer> servers = new ArrayList<CodegenServer>();
    public CodegenParameter bodyParam;
    public List<CodegenParameter> allParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> bodyParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> pathParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> queryParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> headerParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> formParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> cookieParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> requiredParams = new ArrayList<CodegenParameter>();
    public List<CodegenParameter> optionalParams = new ArrayList<CodegenParameter>();
    public List<CodegenSecurity> authMethods;
    public List<Tag> tags;
    public List<CodegenResponse> responses = new ArrayList<CodegenResponse>();
    public List<CodegenCallback> callbacks = new ArrayList<>();
    public Set<String> imports = new HashSet<String>();
    public List<Map<String, String>> examples;
    public List<Map<String, String>> requestBodyExamples;
    public ExternalDocumentation externalDocs;
    public Map<String, Object> vendorExtensions = new HashMap<String, Object>();
    public String nickname; // legacy support
    public String operationIdOriginal; // for plug-in
    public String operationIdLowerCase; // for markdown documentation
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
     * Check if there's at least one form parameter
     *
     * @return true if any cookie parameter exists, false otherwise
     */
    public boolean getHasCookieParams() {
        return nonempty(cookieParams);
    }

    /**
     * Check if there's at least one optional parameter
     *
     * @return true if any optional parameter exists, false otherwise
     */
    public boolean getHasOptionalParams() {
        return nonempty(optionalParams);
    }

    /**
     * Check if there's at least one required parameter
     *
     * @return true if any optional parameter exists, false otherwise
     */
    public boolean getHasRequiredParams() {
        return nonempty(requiredParams);
    }

    /**
     * Check if there's at least one response header
     *
     * @return true if header response exists, false otherwise
     */
    public boolean getHasResponseHeaders() {
        return nonempty(responseHeaders);
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
        return "GET".equalsIgnoreCase(httpMethod) && "".equals(pathWithoutBaseName());
    }

    /**
     * Check if act as Restful show method
     *
     * @return true if act as Restful show method, false otherwise
     */
    public boolean isRestfulShow() {
        return "GET".equalsIgnoreCase(httpMethod) && isMemberPath();
    }

    /**
     * Check if act as Restful create method
     *
     * @return true if act as Restful create method, false otherwise
     */
    public boolean isRestfulCreate() {
        return "POST".equalsIgnoreCase(httpMethod) && "".equals(pathWithoutBaseName());
    }

    /**
     * Check if act as Restful update method
     *
     * @return true if act as Restful update method, false otherwise
     */
    public boolean isRestfulUpdate() {
        return Arrays.asList("PUT", "PATCH").contains(httpMethod.toUpperCase(Locale.ROOT)) && isMemberPath();
    }

    /**
     * Check if body param is allowed for the request method
     *
     * @return true request method is PUT, PATCH or POST; false otherwise
     */
    public boolean isBodyAllowed() {
        return Arrays.asList("PUT", "PATCH", "POST").contains(httpMethod.toUpperCase(Locale.ROOT));
    }

    /**
     * Check if act as Restful destroy method
     *
     * @return true if act as Restful destroy method, false otherwise
     */
    public boolean isRestfulDestroy() {
        return "DELETE".equalsIgnoreCase(httpMethod) && isMemberPath();
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
        return baseName != null ? path.replace("/" + baseName.toLowerCase(Locale.ROOT), "") : path;
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
        return String.format(Locale.ROOT, "%s(%s)", baseName, path);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        CodegenOperation that = (CodegenOperation) o;

        return Objects.equals(responseHeaders, that.responseHeaders) &&
            Objects.equals(hasAuthMethods, that.hasAuthMethods) &&
            Objects.equals(hasConsumes, that.hasConsumes) &&
            Objects.equals(hasProduces, that.hasProduces) &&
            Objects.equals(hasParams, that.hasParams) &&
            Objects.equals(hasOptionalParams, that.hasOptionalParams) &&
            Objects.equals(returnTypeIsPrimitive, that.returnTypeIsPrimitive) &&
            Objects.equals(returnSimpleType, that.returnSimpleType) &&
            Objects.equals(subresourceOperation, that.subresourceOperation) &&
            Objects.equals(isMapContainer, that.isMapContainer) &&
            Objects.equals(isListContainer, that.isListContainer) &&
            Objects.equals(isMultipart, that.isMultipart) &&
            Objects.equals(hasMore, that.hasMore) &&
            Objects.equals(isResponseBinary, that.isResponseBinary) &&
            Objects.equals(hasReference, that.hasReference) &&
            Objects.equals(isResponseFile, that.isResponseFile) &&
            Objects.equals(isDeprecated, that.isDeprecated) &&
            Objects.equals(isCallbackRequest, that.isCallbackRequest) &&
            Objects.equals(path, that.path) &&
            Objects.equals(operationId, that.operationId) &&
            Objects.equals(returnType, that.returnType) &&
            Objects.equals(httpMethod, that.httpMethod) &&
            Objects.equals(returnBaseType, that.returnBaseType) &&
            Objects.equals(returnContainer, that.returnContainer) &&
            Objects.equals(summary, that.summary) &&
            Objects.equals(unescapedNotes, that.unescapedNotes) &&
            Objects.equals(notes, that.notes) &&
            Objects.equals(baseName, that.baseName) &&
            Objects.equals(defaultResponse, that.defaultResponse) &&
            Objects.equals(discriminator, that.discriminator) &&
            Objects.equals(consumes, that.consumes) &&
            Objects.equals(produces, that.produces) &&
            Objects.equals(servers, that.servers) &&
            Objects.equals(bodyParam, that.bodyParam) &&
            Objects.equals(allParams, that.allParams) &&
            Objects.equals(bodyParams, that.bodyParams) &&
            Objects.equals(pathParams, that.pathParams) &&
            Objects.equals(queryParams, that.queryParams) &&
            Objects.equals(headerParams, that.headerParams) &&
            Objects.equals(formParams, that.formParams) &&
            Objects.equals(cookieParams, that.cookieParams) &&
            Objects.equals(requiredParams, that.requiredParams) &&
            Objects.equals(optionalParams, that.optionalParams) &&
            Objects.equals(authMethods, that.authMethods) &&
            Objects.equals(tags, that.tags) &&
            Objects.equals(responses, that.responses) &&
            Objects.equals(callbacks, that.callbacks) &&
            Objects.equals(imports, that.imports) &&
            Objects.equals(examples, that.examples) &&
            Objects.equals(externalDocs, that.externalDocs) &&
            Objects.equals(vendorExtensions, that.vendorExtensions) &&
            Objects.equals(nickname, that.nickname) &&
            Objects.equals(prioritizedContentTypes, that.prioritizedContentTypes) &&
            Objects.equals(operationIdOriginal, that.operationIdOriginal) &&
            Objects.equals(operationIdLowerCase, that.operationIdLowerCase) &&
            Objects.equals(operationIdCamelCase, that.operationIdCamelCase);
    }

    @Override
    public int hashCode() {
        return Objects.hash(
            responseHeaders,
            hasAuthMethods,
            hasConsumes,
            hasProduces,
            hasParams,
            hasOptionalParams,
            returnTypeIsPrimitive,
            returnSimpleType,
            subresourceOperation,
            isMapContainer,
            isListContainer,
            isMultipart,
            hasMore,
            isResponseBinary,
            isResponseFile,
            hasReference,
            isDeprecated,
            isCallbackRequest,
            path,
            operationId,
            returnType,
            httpMethod,
            returnBaseType,
            returnContainer,
            summary,
            unescapedNotes,
            notes,
            baseName,
            defaultResponse,
            discriminator,
            consumes,
            produces,
            servers,
            bodyParam,
            allParams,
            bodyParams,
            pathParams,
            queryParams,
            headerParams,
            formParams,
            cookieParams,
            requiredParams,
            optionalParams,
            authMethods,
            tags,
            responses,
            callbacks,
            imports,
            examples,
            externalDocs,
            vendorExtensions,
            nickname,
            prioritizedContentTypes,
            operationIdOriginal,
            operationIdLowerCase,
            operationIdCamelCase);
    }


}
