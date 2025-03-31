// TODO: better import syntax?
import {BaseAPIRequestFactory, RequiredError, COLLECTION_FORMATS} from './baseapi';
import {Configuration} from '../configuration';
import {RequestContext, HttpMethod, ResponseContext, HttpFile, HttpInfo} from '../http/http';
import * as FormData from "form-data";
import { URLSearchParams } from 'url';
import {ObjectSerializer} from '../models/ObjectSerializer';
import {ApiException} from './exception';
import {canConsumeForm, isCodeInRange} from '../util';
import {SecurityAuthentication} from '../auth/auth';


import { StringEnumRef } from '../models/StringEnumRef';

/**
 * no description
 */
export class PathApiRequestFactory extends BaseAPIRequestFactory {

    /**
     * Test path parameter(s)
     * Test path parameter(s)
     * @param pathString 
     * @param pathInteger 
     * @param enumNonrefStringPath 
     * @param enumRefStringPath 
     */
    public async testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(pathString: string, pathInteger: number, enumNonrefStringPath: 'success' | 'failure' | 'unclassified', enumRefStringPath: StringEnumRef, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'pathString' is not null or undefined
        if (pathString === null || pathString === undefined) {
            throw new RequiredError("PathApi", "testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath", "pathString");
        }


        // verify required parameter 'pathInteger' is not null or undefined
        if (pathInteger === null || pathInteger === undefined) {
            throw new RequiredError("PathApi", "testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath", "pathInteger");
        }


        // verify required parameter 'enumNonrefStringPath' is not null or undefined
        if (enumNonrefStringPath === null || enumNonrefStringPath === undefined) {
            throw new RequiredError("PathApi", "testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath", "enumNonrefStringPath");
        }


        // verify required parameter 'enumRefStringPath' is not null or undefined
        if (enumRefStringPath === null || enumRefStringPath === undefined) {
            throw new RequiredError("PathApi", "testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath", "enumRefStringPath");
        }


        // Path Params
        const localVarPath = '/path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path}'
            .replace('{' + 'path_string' + '}', encodeURIComponent(String(pathString)))
            .replace('{' + 'path_integer' + '}', encodeURIComponent(String(pathInteger)))
            .replace('{' + 'enum_nonref_string_path' + '}', encodeURIComponent(String(enumNonrefStringPath)))
            .replace('{' + 'enum_ref_string_path' + '}', encodeURIComponent(String(enumRefStringPath)));

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")


        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

}

export class PathApiResponseProcessor {

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathWithHttpInfo(response: ResponseContext): Promise<HttpInfo<string >> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            const body: string = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "string", ""
            ) as string;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: string = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "string", ""
            ) as string;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

}
