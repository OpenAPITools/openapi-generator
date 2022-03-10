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

package org.openapitools.codegen;

import io.swagger.v3.oas.models.ExternalDocumentation;
import io.swagger.v3.oas.models.tags.Tag;

import java.util.*;

public class CodegenOperation {
    public final List<CodegenProperty> responseHeaders = new ArrayList<CodegenProperty>();
    public boolean hasAuthMethods, hasConsumes, hasProduces, hasParams, hasOptionalParams, hasRequiredParams,
            returnTypeIsPrimitive, returnSimpleType, subresourceOperation, isMap,
            isArray, isMultipart,
            isResponseBinary = false, isResponseFile = false, hasReference = false,
            isRestfulIndex, isRestfulShow, isRestfulCreate, isRestfulUpdate, isRestfulDestroy,
            isRestful, isDeprecated, isCallbackRequest, uniqueItems, hasDefaultResponse = false,
            hasErrorResponseObject; // if 4xx, 5xx responses have at least one error object defined
    public String path, operationId, returnType, returnFormat, httpMethod, returnBaseType,
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
    public List<CodegenParameter> implicitHeadersParams = new ArrayList<CodegenParameter>();
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
    private static boolean nonEmpty(List<?> params) {
        return params != null && params.size() > 0;
    }

    private static boolean nonEmpty(Map<?, ?> params) {
        return params != null && params.size() > 0;
    }

    /**
     * Check if there's at least one body parameter
     *
     * @return true if body parameter exists, false otherwise
     */
    public boolean getHasBodyParam() {
        return nonEmpty(bodyParams);
    }

    /**
     * Check if there's at least one query parameter
     *
     * @return true if query parameter exists, false otherwise
     */
    public boolean getHasQueryParams() {
        return nonEmpty(queryParams);
    }

    /**
     * Check if there's at least one query parameter or passing API keys in query
     *
     * @return true if query parameter exists or passing API keys in query, false otherwise
     */
    public boolean getHasQueryParamsOrAuth() {
        return getHasQueryParams() || (authMethods != null && authMethods.stream().anyMatch(authMethod -> authMethod.isKeyInQuery));
    }

    /**
     * Check if there's at least one header parameter
     *
     * @return true if header parameter exists, false otherwise
     */
    public boolean getHasHeaderParams() {
        return nonEmpty(headerParams);
    }

    /**
     * Check if there's at least one path parameter
     *
     * @return true if path parameter exists, false otherwise
     */
    public boolean getHasPathParams() {
        return nonEmpty(pathParams);
    }

    /**
     * Check if there's at least one form parameter
     *
     * @return true if any form parameter exists, false otherwise
     */
    public boolean getHasFormParams() {
        return nonEmpty(formParams);
    }

    /**
     * Check if there's at least one body parameter or at least one form parameter
     *
     * @return true if body or form parameter exists, false otherwise
     */
    public boolean getHasBodyOrFormParams() {
        return getHasBodyParam() || getHasFormParams();
    }

    /**
     * Check if there's at least one form parameter
     *
     * @return true if any cookie parameter exists, false otherwise
     */
    public boolean getHasCookieParams() {
        return nonEmpty(cookieParams);
    }

    /**
     * Check if there's at least one optional parameter
     *
     * @return true if any optional parameter exists, false otherwise
     */
    public boolean getHasOptionalParams() {
        return nonEmpty(optionalParams);
    }

    /**
     * Check if there's at least one required parameter
     *
     * @return true if any optional parameter exists, false otherwise
     */
    public boolean getHasRequiredParams() {
        return nonEmpty(requiredParams);
    }

    /**
     * Check if there's at least one response header
     *
     * @return true if header response exists, false otherwise
     */
    public boolean getHasResponseHeaders() {
        return nonEmpty(responseHeaders);
    }

    /**
     * Check if there's at least one example parameter
     *
     * @return true if examples parameter exists, false otherwise
     */
    public boolean getHasExamples() {
        return nonEmpty(examples);
    }

    /**
     * Check if there's a default response
     *
     * @return true if responses contain a default response, false otherwise
     */
    public boolean getHasDefaultResponse() {
        return responses.stream().filter(response -> response.isDefault).findFirst().isPresent();
    }

    /**
     * Check if there's at least one vendor extension
     *
     * @return true if vendor extensions exists, false otherwise
     */
    public boolean getHasVendorExtensions() {
        return nonEmpty(vendorExtensions);
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
        final StringBuffer sb = new StringBuffer("CodegenOperation{");
        sb.append("responseHeaders=").append(responseHeaders);
        sb.append(", hasAuthMethods=").append(hasAuthMethods);
        sb.append(", hasConsumes=").append(hasConsumes);
        sb.append(", hasProduces=").append(hasProduces);
        sb.append(", hasParams=").append(hasParams);
        sb.append(", hasOptionalParams=").append(hasOptionalParams);
        sb.append(", hasRequiredParams=").append(hasRequiredParams);
        sb.append(", returnTypeIsPrimitive=").append(returnTypeIsPrimitive);
        sb.append(", returnSimpleType=").append(returnSimpleType);
        sb.append(", subresourceOperation=").append(subresourceOperation);
        sb.append(", isMap=").append(isMap);
        sb.append(", isArray=").append(isArray);
        sb.append(", isMultipart=").append(isMultipart);
        sb.append(", isResponseBinary=").append(isResponseBinary);
        sb.append(", isResponseFile=").append(isResponseFile);
        sb.append(", hasReference=").append(hasReference);
        sb.append(", hasDefaultResponse=").append(hasDefaultResponse);
        sb.append(", hasErrorResponseObject=").append(hasErrorResponseObject);
        sb.append(", isRestfulIndex=").append(isRestfulIndex);
        sb.append(", isRestfulShow=").append(isRestfulShow);
        sb.append(", isRestfulCreate=").append(isRestfulCreate);
        sb.append(", isRestfulUpdate=").append(isRestfulUpdate);
        sb.append(", isRestfulDestroy=").append(isRestfulDestroy);
        sb.append(", isRestful=").append(isRestful);
        sb.append(", isDeprecated=").append(isDeprecated);
        sb.append(", isCallbackRequest=").append(isCallbackRequest);
        sb.append(", uniqueItems='").append(uniqueItems);
        sb.append(", path='").append(path).append('\'');
        sb.append(", operationId='").append(operationId).append('\'');
        sb.append(", returnType='").append(returnType).append('\'');
        sb.append(", httpMethod='").append(httpMethod).append('\'');
        sb.append(", returnBaseType='").append(returnBaseType).append('\'');
        sb.append(", returnContainer='").append(returnContainer).append('\'');
        sb.append(", summary='").append(summary).append('\'');
        sb.append(", unescapedNotes='").append(unescapedNotes).append('\'');
        sb.append(", notes='").append(notes).append('\'');
        sb.append(", baseName='").append(baseName).append('\'');
        sb.append(", defaultResponse='").append(defaultResponse).append('\'');
        sb.append(", discriminator=").append(discriminator);
        sb.append(", consumes=").append(consumes);
        sb.append(", produces=").append(produces);
        sb.append(", prioritizedContentTypes=").append(prioritizedContentTypes);
        sb.append(", servers=").append(servers);
        sb.append(", bodyParam=").append(bodyParam);
        sb.append(", allParams=").append(allParams);
        sb.append(", bodyParams=").append(bodyParams);
        sb.append(", pathParams=").append(pathParams);
        sb.append(", queryParams=").append(queryParams);
        sb.append(", headerParams=").append(headerParams);
        sb.append(", formParams=").append(formParams);
        sb.append(", cookieParams=").append(cookieParams);
        sb.append(", requiredParams=").append(requiredParams);
        sb.append(", optionalParams=").append(optionalParams);
        sb.append(", authMethods=").append(authMethods);
        sb.append(", tags=").append(tags);
        sb.append(", responses=").append(responses);
        sb.append(", callbacks=").append(callbacks);
        sb.append(", imports=").append(imports);
        sb.append(", examples=").append(examples);
        sb.append(", requestBodyExamples=").append(requestBodyExamples);
        sb.append(", externalDocs=").append(externalDocs);
        sb.append(", vendorExtensions=").append(vendorExtensions);
        sb.append(", nickname='").append(nickname).append('\'');
        sb.append(", operationIdOriginal='").append(operationIdOriginal).append('\'');
        sb.append(", operationIdLowerCase='").append(operationIdLowerCase).append('\'');
        sb.append(", operationIdCamelCase='").append(operationIdCamelCase).append('\'');
        sb.append(", operationIdSnakeCase='").append(operationIdSnakeCase).append('\'');
        sb.append('}');
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CodegenOperation that = (CodegenOperation) o;
        return hasAuthMethods == that.hasAuthMethods &&
                hasConsumes == that.hasConsumes &&
                hasProduces == that.hasProduces &&
                hasParams == that.hasParams &&
                hasOptionalParams == that.hasOptionalParams &&
                hasRequiredParams == that.hasRequiredParams &&
                returnTypeIsPrimitive == that.returnTypeIsPrimitive &&
                returnSimpleType == that.returnSimpleType &&
                subresourceOperation == that.subresourceOperation &&
                isMap == that.isMap &&
                isArray == that.isArray &&
                isMultipart == that.isMultipart &&
                isResponseBinary == that.isResponseBinary &&
                isResponseFile == that.isResponseFile &&
                hasReference == that.hasReference &&
                hasDefaultResponse == that.hasDefaultResponse &&
                hasErrorResponseObject == that.hasErrorResponseObject &&
                isRestfulIndex == that.isRestfulIndex &&
                isRestfulShow == that.isRestfulShow &&
                isRestfulCreate == that.isRestfulCreate &&
                isRestfulUpdate == that.isRestfulUpdate &&
                isRestfulDestroy == that.isRestfulDestroy &&
                isRestful == that.isRestful &&
                isDeprecated == that.isDeprecated &&
                isCallbackRequest == that.isCallbackRequest &&
                uniqueItems == that.uniqueItems &&
                Objects.equals(responseHeaders, that.responseHeaders) &&
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
                Objects.equals(prioritizedContentTypes, that.prioritizedContentTypes) &&
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
                Objects.equals(requestBodyExamples, that.requestBodyExamples) &&
                Objects.equals(externalDocs, that.externalDocs) &&
                Objects.equals(vendorExtensions, that.vendorExtensions) &&
                Objects.equals(nickname, that.nickname) &&
                Objects.equals(operationIdOriginal, that.operationIdOriginal) &&
                Objects.equals(operationIdLowerCase, that.operationIdLowerCase) &&
                Objects.equals(operationIdCamelCase, that.operationIdCamelCase) &&
                Objects.equals(operationIdSnakeCase, that.operationIdSnakeCase);
    }

    @Override
    public int hashCode() {

        return Objects.hash(responseHeaders, hasAuthMethods, hasConsumes, hasProduces, hasParams, hasOptionalParams,
                hasRequiredParams, returnTypeIsPrimitive, returnSimpleType, subresourceOperation, isMap,
                isArray, isMultipart, isResponseBinary, isResponseFile, hasReference, hasDefaultResponse, isRestfulIndex,
                isRestfulShow, isRestfulCreate, isRestfulUpdate, isRestfulDestroy, isRestful, isDeprecated,
                isCallbackRequest, uniqueItems, path, operationId, returnType, httpMethod, returnBaseType,
                returnContainer, summary, unescapedNotes, notes, baseName, defaultResponse, discriminator, consumes,
                produces, prioritizedContentTypes, servers, bodyParam, allParams, bodyParams, pathParams, queryParams,
                headerParams, formParams, cookieParams, requiredParams, optionalParams, authMethods, tags,
                responses, callbacks, imports, examples, requestBodyExamples, externalDocs, vendorExtensions,
                nickname, operationIdOriginal, operationIdLowerCase, operationIdCamelCase, operationIdSnakeCase,
                hasErrorResponseObject);
    }
}
