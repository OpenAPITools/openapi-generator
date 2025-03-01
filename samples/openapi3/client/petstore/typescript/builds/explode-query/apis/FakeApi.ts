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


import { Client } from '../models/Client';
import { EnumClass } from '../models/EnumClass';
import { FakeBigDecimalMap200Response } from '../models/FakeBigDecimalMap200Response';
import { FileSchemaTestClass } from '../models/FileSchemaTestClass';
import { HealthCheckResult } from '../models/HealthCheckResult';
import { OuterComposite } from '../models/OuterComposite';
import { OuterObjectWithEnumProperty } from '../models/OuterObjectWithEnumProperty';
import { Pet } from '../models/Pet';
import { User } from '../models/User';

/**
 * no description
 */
export class FakeApiRequestFactory extends BaseAPIRequestFactory {

    /**
     * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
     */
    public async fakeBigDecimalMap(_options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // Path Params
        const localVarPath = '/fake/BigDecimalMap';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")


        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * Health check endpoint
     */
    public async fakeHealthGet(_options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // Path Params
        const localVarPath = '/fake/health';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")


        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * test http signature authentication
     * @param pet Pet object that needs to be added to the store
     * @param query1 query parameter
     * @param header1 header parameter
     */
    public async fakeHttpSignatureTest(pet: Pet, query1?: string, header1?: string, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'pet' is not null or undefined
        if (pet === null || pet === undefined) {
            throw new RequiredError("FakeApi", "fakeHttpSignatureTest", "pet");
        }




        // Path Params
        const localVarPath = '/fake/http-signature-test';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Query Params
        if (query1 !== undefined) {
            requestContext.setQueryParam("query_1", ObjectSerializer.serialize(query1, "string", ""));
        }

        // Header Params
        requestContext.setHeaderParam("header_1", ObjectSerializer.serialize(header1, "string", ""));


        // Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([
            "application/json",
        
            "application/xml"
        ]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(pet, "Pet", ""),
            contentType
        );
        requestContext.setBody(serializedBody);

        let authMethod: SecurityAuthentication | undefined;
        // Apply auth methods
        authMethod = _config.authMethods["http_signature_test"]
        if (authMethod?.applySecurityAuthentication) {
            await authMethod?.applySecurityAuthentication(requestContext);
        }
        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * Test serialization of outer boolean types
     * @param body Input boolean as post body
     */
    public async fakeOuterBooleanSerialize(body?: boolean, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;


        // Path Params
        const localVarPath = '/fake/outer/boolean';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")


        // Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([
            "application/json"
        ]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(body, "boolean", ""),
            contentType
        );
        requestContext.setBody(serializedBody);

        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * Test serialization of object with outer number type
     * @param outerComposite Input composite as post body
     */
    public async fakeOuterCompositeSerialize(outerComposite?: OuterComposite, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;


        // Path Params
        const localVarPath = '/fake/outer/composite';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")


        // Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([
            "application/json"
        ]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(outerComposite, "OuterComposite", ""),
            contentType
        );
        requestContext.setBody(serializedBody);

        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * Test serialization of outer number types
     * @param body Input number as post body
     */
    public async fakeOuterNumberSerialize(body?: number, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;


        // Path Params
        const localVarPath = '/fake/outer/number';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")


        // Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([
            "application/json"
        ]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(body, "number", ""),
            contentType
        );
        requestContext.setBody(serializedBody);

        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * Test serialization of outer string types
     * @param body Input string as post body
     */
    public async fakeOuterStringSerialize(body?: string, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;


        // Path Params
        const localVarPath = '/fake/outer/string';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")


        // Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([
            "application/json"
        ]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(body, "string", ""),
            contentType
        );
        requestContext.setBody(serializedBody);

        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * Test serialization of enum (int) properties with examples
     * @param outerObjectWithEnumProperty Input enum (int) as post body
     */
    public async fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty: OuterObjectWithEnumProperty, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'outerObjectWithEnumProperty' is not null or undefined
        if (outerObjectWithEnumProperty === null || outerObjectWithEnumProperty === undefined) {
            throw new RequiredError("FakeApi", "fakePropertyEnumIntegerSerialize", "outerObjectWithEnumProperty");
        }


        // Path Params
        const localVarPath = '/fake/property/enum-int';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")


        // Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([
            "application/json"
        ]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(outerObjectWithEnumProperty, "OuterObjectWithEnumProperty", ""),
            contentType
        );
        requestContext.setBody(serializedBody);

        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * For this test, the body has to be a binary file.
     * @param body image to upload
     */
    public async testBodyWithBinary(body: HttpFile, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'body' is not null or undefined
        if (body === null || body === undefined) {
            throw new RequiredError("FakeApi", "testBodyWithBinary", "body");
        }


        // Path Params
        const localVarPath = '/fake/body-with-binary';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.PUT);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")


        // Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([
            "image/png"
        ]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(body, "HttpFile", ""),
            contentType
        );
        requestContext.setBody(serializedBody);

        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * For this test, the body for this request must reference a schema named `File`.
     * @param fileSchemaTestClass 
     */
    public async testBodyWithFileSchema(fileSchemaTestClass: FileSchemaTestClass, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'fileSchemaTestClass' is not null or undefined
        if (fileSchemaTestClass === null || fileSchemaTestClass === undefined) {
            throw new RequiredError("FakeApi", "testBodyWithFileSchema", "fileSchemaTestClass");
        }


        // Path Params
        const localVarPath = '/fake/body-with-file-schema';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.PUT);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")


        // Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([
            "application/json"
        ]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(fileSchemaTestClass, "FileSchemaTestClass", ""),
            contentType
        );
        requestContext.setBody(serializedBody);

        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * @param query 
     * @param user 
     */
    public async testBodyWithQueryParams(query: string, user: User, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'query' is not null or undefined
        if (query === null || query === undefined) {
            throw new RequiredError("FakeApi", "testBodyWithQueryParams", "query");
        }


        // verify required parameter 'user' is not null or undefined
        if (user === null || user === undefined) {
            throw new RequiredError("FakeApi", "testBodyWithQueryParams", "user");
        }


        // Path Params
        const localVarPath = '/fake/body-with-query-params';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.PUT);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Query Params
        if (query !== undefined) {
            requestContext.setQueryParam("query", ObjectSerializer.serialize(query, "string", ""));
        }


        // Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([
            "application/json"
        ]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(user, "User", ""),
            contentType
        );
        requestContext.setBody(serializedBody);

        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * To test \"client\" model
     * To test \"client\" model
     * @param client client model
     */
    public async testClientModel(client: Client, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'client' is not null or undefined
        if (client === null || client === undefined) {
            throw new RequiredError("FakeApi", "testClientModel", "client");
        }


        // Path Params
        const localVarPath = '/fake';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.PATCH);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")


        // Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([
            "application/json"
        ]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(client, "Client", ""),
            contentType
        );
        requestContext.setBody(serializedBody);

        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * @param number None
     * @param _double None
     * @param patternWithoutDelimiter None
     * @param _byte None
     * @param integer None
     * @param int32 None
     * @param int64 None
     * @param _float None
     * @param string None
     * @param binary None
     * @param date None
     * @param dateTime None
     * @param password None
     * @param callback None
     */
    public async testEndpointParameters(number: number, _double: number, patternWithoutDelimiter: string, _byte: string, integer?: number, int32?: number, int64?: number, _float?: number, string?: string, binary?: HttpFile, date?: string, dateTime?: Date, password?: string, callback?: string, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'number' is not null or undefined
        if (number === null || number === undefined) {
            throw new RequiredError("FakeApi", "testEndpointParameters", "number");
        }


        // verify required parameter '_double' is not null or undefined
        if (_double === null || _double === undefined) {
            throw new RequiredError("FakeApi", "testEndpointParameters", "_double");
        }


        // verify required parameter 'patternWithoutDelimiter' is not null or undefined
        if (patternWithoutDelimiter === null || patternWithoutDelimiter === undefined) {
            throw new RequiredError("FakeApi", "testEndpointParameters", "patternWithoutDelimiter");
        }


        // verify required parameter '_byte' is not null or undefined
        if (_byte === null || _byte === undefined) {
            throw new RequiredError("FakeApi", "testEndpointParameters", "_byte");
        }












        // Path Params
        const localVarPath = '/fake';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Form Params
        const useForm = canConsumeForm([
            'application/x-www-form-urlencoded',
        ]);

        let localVarFormParams
        if (useForm) {
            localVarFormParams = new FormData();
        } else {
            localVarFormParams = new URLSearchParams();
        }

        if (integer !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('integer', integer as any);
        }
        if (int32 !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('int32', int32 as any);
        }
        if (int64 !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('int64', int64 as any);
        }
        if (number !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('number', number as any);
        }
        if (_float !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('float', _float as any);
        }
        if (_double !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('double', _double as any);
        }
        if (string !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('string', string as any);
        }
        if (patternWithoutDelimiter !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('pattern_without_delimiter', patternWithoutDelimiter as any);
        }
        if (_byte !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('byte', _byte as any);
        }
        if (binary !== undefined) {
             // TODO: replace .append with .set
             if (localVarFormParams instanceof FormData) {
                 localVarFormParams.append('binary', binary.data, binary.name);
             }
        }
        if (date !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('date', date as any);
        }
        if (dateTime !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('dateTime', dateTime as any);
        }
        if (password !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('password', password as any);
        }
        if (callback !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('callback', callback as any);
        }

        requestContext.setBody(localVarFormParams);

        if(!useForm) {
            const contentType = ObjectSerializer.getPreferredMediaType([
                "application/x-www-form-urlencoded"
            ]);
            requestContext.setHeaderParam("Content-Type", contentType);
        }

        let authMethod: SecurityAuthentication | undefined;
        // Apply auth methods
        authMethod = _config.authMethods["http_basic_test"]
        if (authMethod?.applySecurityAuthentication) {
            await authMethod?.applySecurityAuthentication(requestContext);
        }
        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * @param enumHeaderStringArray Header parameter enum test (string array)
     * @param enumHeaderString Header parameter enum test (string)
     * @param enumQueryStringArray Query parameter enum test (string array)
     * @param enumQueryString Query parameter enum test (string)
     * @param enumQueryInteger Query parameter enum test (double)
     * @param enumQueryDouble Query parameter enum test (double)
     * @param enumQueryModelArray 
     * @param enumFormStringArray Form parameter enum test (string array)
     * @param enumFormString Form parameter enum test (string)
     */
    public async testEnumParameters(enumHeaderStringArray?: Array<'>' | '$'>, enumHeaderString?: '_abc' | '-efg' | '(xyz)', enumQueryStringArray?: Array<'>' | '$'>, enumQueryString?: '_abc' | '-efg' | '(xyz)', enumQueryInteger?: 1 | -2, enumQueryDouble?: 1.1 | -1.2, enumQueryModelArray?: Array<EnumClass>, enumFormStringArray?: Array<string>, enumFormString?: string, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;










        // Path Params
        const localVarPath = '/fake';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Query Params
        if (enumQueryStringArray !== undefined) {
            const serializedParams = ObjectSerializer.serialize(enumQueryStringArray, "Array<'>' | '$'>", "");
            for (const serializedParam of serializedParams) {
                requestContext.appendQueryParam("enum_query_string_array", serializedParam);
            }
        }

        // Query Params
        if (enumQueryString !== undefined) {
            requestContext.setQueryParam("enum_query_string", ObjectSerializer.serialize(enumQueryString, "'_abc' | '-efg' | '(xyz)'", ""));
        }

        // Query Params
        if (enumQueryInteger !== undefined) {
            requestContext.setQueryParam("enum_query_integer", ObjectSerializer.serialize(enumQueryInteger, "1 | -2", "int32"));
        }

        // Query Params
        if (enumQueryDouble !== undefined) {
            requestContext.setQueryParam("enum_query_double", ObjectSerializer.serialize(enumQueryDouble, "1.1 | -1.2", "double"));
        }

        // Query Params
        if (enumQueryModelArray !== undefined) {
            const serializedParams = ObjectSerializer.serialize(enumQueryModelArray, "Array<EnumClass>", "");
            for (const serializedParam of serializedParams) {
                requestContext.appendQueryParam("enum_query_model_array", serializedParam);
            }
        }

        // Header Params
        requestContext.setHeaderParam("enum_header_string_array", ObjectSerializer.serialize(enumHeaderStringArray, "Array<'>' | '$'>", ""));

        // Header Params
        requestContext.setHeaderParam("enum_header_string", ObjectSerializer.serialize(enumHeaderString, "'_abc' | '-efg' | '(xyz)'", ""));

        // Form Params
        const useForm = canConsumeForm([
            'application/x-www-form-urlencoded',
        ]);

        let localVarFormParams
        if (useForm) {
            localVarFormParams = new FormData();
        } else {
            localVarFormParams = new URLSearchParams();
        }

        if (enumFormStringArray) {
            // TODO: replace .append with .set
            localVarFormParams.append('enum_form_string_array', enumFormStringArray.join(COLLECTION_FORMATS["csv"]));
        }
        if (enumFormString !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('enum_form_string', enumFormString as any);
        }

        requestContext.setBody(localVarFormParams);

        if(!useForm) {
            const contentType = ObjectSerializer.getPreferredMediaType([
                "application/x-www-form-urlencoded"
            ]);
            requestContext.setHeaderParam("Content-Type", contentType);
        }

        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * Fake endpoint to test group parameters (optional)
     * Fake endpoint to test group parameters (optional)
     * @param requiredStringGroup Required String in group parameters
     * @param requiredBooleanGroup Required Boolean in group parameters
     * @param requiredInt64Group Required Integer in group parameters
     * @param stringGroup String in group parameters
     * @param booleanGroup Boolean in group parameters
     * @param int64Group Integer in group parameters
     */
    public async testGroupParameters(requiredStringGroup: number, requiredBooleanGroup: boolean, requiredInt64Group: number, stringGroup?: number, booleanGroup?: boolean, int64Group?: number, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'requiredStringGroup' is not null or undefined
        if (requiredStringGroup === null || requiredStringGroup === undefined) {
            throw new RequiredError("FakeApi", "testGroupParameters", "requiredStringGroup");
        }


        // verify required parameter 'requiredBooleanGroup' is not null or undefined
        if (requiredBooleanGroup === null || requiredBooleanGroup === undefined) {
            throw new RequiredError("FakeApi", "testGroupParameters", "requiredBooleanGroup");
        }


        // verify required parameter 'requiredInt64Group' is not null or undefined
        if (requiredInt64Group === null || requiredInt64Group === undefined) {
            throw new RequiredError("FakeApi", "testGroupParameters", "requiredInt64Group");
        }





        // Path Params
        const localVarPath = '/fake';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.DELETE);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Query Params
        if (requiredStringGroup !== undefined) {
            requestContext.setQueryParam("required_string_group", ObjectSerializer.serialize(requiredStringGroup, "number", ""));
        }

        // Query Params
        if (requiredInt64Group !== undefined) {
            requestContext.setQueryParam("required_int64_group", ObjectSerializer.serialize(requiredInt64Group, "number", "int64"));
        }

        // Query Params
        if (stringGroup !== undefined) {
            requestContext.setQueryParam("string_group", ObjectSerializer.serialize(stringGroup, "number", ""));
        }

        // Query Params
        if (int64Group !== undefined) {
            requestContext.setQueryParam("int64_group", ObjectSerializer.serialize(int64Group, "number", "int64"));
        }

        // Header Params
        requestContext.setHeaderParam("required_boolean_group", ObjectSerializer.serialize(requiredBooleanGroup, "boolean", ""));

        // Header Params
        requestContext.setHeaderParam("boolean_group", ObjectSerializer.serialize(booleanGroup, "boolean", ""));


        let authMethod: SecurityAuthentication | undefined;
        // Apply auth methods
        authMethod = _config.authMethods["bearer_test"]
        if (authMethod?.applySecurityAuthentication) {
            await authMethod?.applySecurityAuthentication(requestContext);
        }
        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * 
     * test inline additionalProperties
     * @param requestBody request body
     */
    public async testInlineAdditionalProperties(requestBody: { [key: string]: string; }, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'requestBody' is not null or undefined
        if (requestBody === null || requestBody === undefined) {
            throw new RequiredError("FakeApi", "testInlineAdditionalProperties", "requestBody");
        }


        // Path Params
        const localVarPath = '/fake/inline-additionalProperties';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.POST);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")


        // Body Params
        const contentType = ObjectSerializer.getPreferredMediaType([
            "application/json"
        ]);
        requestContext.setHeaderParam("Content-Type", contentType);
        const serializedBody = ObjectSerializer.stringify(
            ObjectSerializer.serialize(requestBody, "{ [key: string]: string; }", ""),
            contentType
        );
        requestContext.setBody(serializedBody);

        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * 
     * test json serialization of form data
     * @param param field1
     * @param param2 field2
     */
    public async testJsonFormData(param: string, param2: string, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'param' is not null or undefined
        if (param === null || param === undefined) {
            throw new RequiredError("FakeApi", "testJsonFormData", "param");
        }


        // verify required parameter 'param2' is not null or undefined
        if (param2 === null || param2 === undefined) {
            throw new RequiredError("FakeApi", "testJsonFormData", "param2");
        }


        // Path Params
        const localVarPath = '/fake/jsonFormData';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.GET);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Form Params
        const useForm = canConsumeForm([
            'application/x-www-form-urlencoded',
        ]);

        let localVarFormParams
        if (useForm) {
            localVarFormParams = new FormData();
        } else {
            localVarFormParams = new URLSearchParams();
        }

        if (param !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('param', param as any);
        }
        if (param2 !== undefined) {
             // TODO: replace .append with .set
             localVarFormParams.append('param2', param2 as any);
        }

        requestContext.setBody(localVarFormParams);

        if(!useForm) {
            const contentType = ObjectSerializer.getPreferredMediaType([
                "application/x-www-form-urlencoded"
            ]);
            requestContext.setHeaderParam("Content-Type", contentType);
        }

        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

    /**
     * To test the collection format in query parameters
     * @param pipe 
     * @param ioutil 
     * @param http 
     * @param url 
     * @param context 
     * @param allowEmpty 
     * @param language 
     */
    public async testQueryParameterCollectionFormat(pipe: Array<string>, ioutil: Array<string>, http: Array<string>, url: Array<string>, context: Array<string>, allowEmpty: string, language?: { [key: string]: string; }, _options?: Configuration): Promise<RequestContext> {
        let _config = _options || this.configuration;

        // verify required parameter 'pipe' is not null or undefined
        if (pipe === null || pipe === undefined) {
            throw new RequiredError("FakeApi", "testQueryParameterCollectionFormat", "pipe");
        }


        // verify required parameter 'ioutil' is not null or undefined
        if (ioutil === null || ioutil === undefined) {
            throw new RequiredError("FakeApi", "testQueryParameterCollectionFormat", "ioutil");
        }


        // verify required parameter 'http' is not null or undefined
        if (http === null || http === undefined) {
            throw new RequiredError("FakeApi", "testQueryParameterCollectionFormat", "http");
        }


        // verify required parameter 'url' is not null or undefined
        if (url === null || url === undefined) {
            throw new RequiredError("FakeApi", "testQueryParameterCollectionFormat", "url");
        }


        // verify required parameter 'context' is not null or undefined
        if (context === null || context === undefined) {
            throw new RequiredError("FakeApi", "testQueryParameterCollectionFormat", "context");
        }


        // verify required parameter 'allowEmpty' is not null or undefined
        if (allowEmpty === null || allowEmpty === undefined) {
            throw new RequiredError("FakeApi", "testQueryParameterCollectionFormat", "allowEmpty");
        }



        // Path Params
        const localVarPath = '/fake/test-query-parameters';

        // Make Request Context
        const requestContext = _config.baseServer.makeRequestContext(localVarPath, HttpMethod.PUT);
        requestContext.setHeaderParam("Accept", "application/json, */*;q=0.8")

        // Query Params
        if (pipe !== undefined) {
            requestContext.setQueryParam("pipe", ObjectSerializer.serialize(pipe, "Array<string>", ""));
        }

        // Query Params
        if (ioutil !== undefined) {
            requestContext.setQueryParam("ioutil", ObjectSerializer.serialize(ioutil, "Array<string>", ""));
        }

        // Query Params
        if (http !== undefined) {
            requestContext.setQueryParam("http", ObjectSerializer.serialize(http, "Array<string>", ""));
        }

        // Query Params
        if (url !== undefined) {
            requestContext.setQueryParam("url", ObjectSerializer.serialize(url, "Array<string>", ""));
        }

        // Query Params
        if (context !== undefined) {
            const serializedParams = ObjectSerializer.serialize(context, "Array<string>", "");
            for (const serializedParam of serializedParams) {
                requestContext.appendQueryParam("context", serializedParam);
            }
        }

        // Query Params
        if (language !== undefined) {
            const serializedParams = ObjectSerializer.serialize(language, "{ [key: string]: string; }", "string");
            for (const key of Object.keys(serializedParams)) {
                requestContext.setQueryParam(key, serializedParams[key]);
            }
        }

        // Query Params
        if (allowEmpty !== undefined) {
            requestContext.setQueryParam("allowEmpty", ObjectSerializer.serialize(allowEmpty, "string", ""));
        }


        
        const defaultAuth: SecurityAuthentication | undefined = _config?.authMethods?.default
        if (defaultAuth?.applySecurityAuthentication) {
            await defaultAuth?.applySecurityAuthentication(requestContext);
        }

        return requestContext;
    }

}

export class FakeApiResponseProcessor {

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to fakeBigDecimalMap
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async fakeBigDecimalMapWithHttpInfo(response: ResponseContext): Promise<HttpInfo<FakeBigDecimalMap200Response >> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            const body: FakeBigDecimalMap200Response = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "FakeBigDecimalMap200Response", ""
            ) as FakeBigDecimalMap200Response;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: FakeBigDecimalMap200Response = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "FakeBigDecimalMap200Response", ""
            ) as FakeBigDecimalMap200Response;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to fakeHealthGet
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async fakeHealthGetWithHttpInfo(response: ResponseContext): Promise<HttpInfo<HealthCheckResult >> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            const body: HealthCheckResult = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "HealthCheckResult", ""
            ) as HealthCheckResult;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: HealthCheckResult = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "HealthCheckResult", ""
            ) as HealthCheckResult;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to fakeHttpSignatureTest
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async fakeHttpSignatureTestWithHttpInfo(response: ResponseContext): Promise<HttpInfo<void >> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, undefined);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: void = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "void", ""
            ) as void;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to fakeOuterBooleanSerialize
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async fakeOuterBooleanSerializeWithHttpInfo(response: ResponseContext): Promise<HttpInfo<boolean >> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            const body: boolean = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "boolean", ""
            ) as boolean;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: boolean = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "boolean", ""
            ) as boolean;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to fakeOuterCompositeSerialize
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async fakeOuterCompositeSerializeWithHttpInfo(response: ResponseContext): Promise<HttpInfo<OuterComposite >> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            const body: OuterComposite = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "OuterComposite", ""
            ) as OuterComposite;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: OuterComposite = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "OuterComposite", ""
            ) as OuterComposite;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to fakeOuterNumberSerialize
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async fakeOuterNumberSerializeWithHttpInfo(response: ResponseContext): Promise<HttpInfo<number >> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            const body: number = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "number", ""
            ) as number;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: number = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "number", ""
            ) as number;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to fakeOuterStringSerialize
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async fakeOuterStringSerializeWithHttpInfo(response: ResponseContext): Promise<HttpInfo<string >> {
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

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to fakePropertyEnumIntegerSerialize
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async fakePropertyEnumIntegerSerializeWithHttpInfo(response: ResponseContext): Promise<HttpInfo<OuterObjectWithEnumProperty >> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            const body: OuterObjectWithEnumProperty = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "OuterObjectWithEnumProperty", ""
            ) as OuterObjectWithEnumProperty;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: OuterObjectWithEnumProperty = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "OuterObjectWithEnumProperty", ""
            ) as OuterObjectWithEnumProperty;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to testBodyWithBinary
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async testBodyWithBinaryWithHttpInfo(response: ResponseContext): Promise<HttpInfo<void >> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, undefined);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: void = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "void", ""
            ) as void;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to testBodyWithFileSchema
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async testBodyWithFileSchemaWithHttpInfo(response: ResponseContext): Promise<HttpInfo<void >> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, undefined);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: void = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "void", ""
            ) as void;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to testBodyWithQueryParams
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async testBodyWithQueryParamsWithHttpInfo(response: ResponseContext): Promise<HttpInfo<void >> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, undefined);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: void = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "void", ""
            ) as void;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to testClientModel
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async testClientModelWithHttpInfo(response: ResponseContext): Promise<HttpInfo<Client >> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            const body: Client = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "Client", ""
            ) as Client;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: Client = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "Client", ""
            ) as Client;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to testEndpointParameters
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async testEndpointParametersWithHttpInfo(response: ResponseContext): Promise<HttpInfo< void>> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<undefined>(response.httpStatusCode, "Invalid username supplied", undefined, response.headers);
        }
        if (isCodeInRange("404", response.httpStatusCode)) {
            throw new ApiException<undefined>(response.httpStatusCode, "User not found", undefined, response.headers);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, undefined);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to testEnumParameters
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async testEnumParametersWithHttpInfo(response: ResponseContext): Promise<HttpInfo< void>> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<undefined>(response.httpStatusCode, "Invalid request", undefined, response.headers);
        }
        if (isCodeInRange("404", response.httpStatusCode)) {
            throw new ApiException<undefined>(response.httpStatusCode, "Not found", undefined, response.headers);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, undefined);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to testGroupParameters
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async testGroupParametersWithHttpInfo(response: ResponseContext): Promise<HttpInfo< void>> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("400", response.httpStatusCode)) {
            throw new ApiException<undefined>(response.httpStatusCode, "Something wrong", undefined, response.headers);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, undefined);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to testInlineAdditionalProperties
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async testInlineAdditionalPropertiesWithHttpInfo(response: ResponseContext): Promise<HttpInfo<void >> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, undefined);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: void = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "void", ""
            ) as void;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to testJsonFormData
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async testJsonFormDataWithHttpInfo(response: ResponseContext): Promise<HttpInfo<void >> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, undefined);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: void = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "void", ""
            ) as void;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

    /**
     * Unwraps the actual response sent by the server from the response context and deserializes the response content
     * to the expected objects
     *
     * @params response Response returned by the server for a request to testQueryParameterCollectionFormat
     * @throws ApiException if the response code was not in [200, 299]
     */
     public async testQueryParameterCollectionFormatWithHttpInfo(response: ResponseContext): Promise<HttpInfo<void >> {
        const contentType = ObjectSerializer.normalizeMediaType(response.headers["content-type"]);
        if (isCodeInRange("200", response.httpStatusCode)) {
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, undefined);
        }

        // Work around for missing responses in specification, e.g. for petstore.yaml
        if (response.httpStatusCode >= 200 && response.httpStatusCode <= 299) {
            const body: void = ObjectSerializer.deserialize(
                ObjectSerializer.parse(await response.body.text(), contentType),
                "void", ""
            ) as void;
            return new HttpInfo(response.httpStatusCode, response.headers, response.body, body);
        }

        throw new ApiException<string | Buffer | undefined>(response.httpStatusCode, "Unknown API Status Code!", await response.getBodyAsAny(), response.headers);
    }

}
