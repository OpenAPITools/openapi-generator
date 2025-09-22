import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration, ConfigurationOptions } from '../configuration'
import type { Middleware } from '../middleware';

import { AdditionalPropertiesClass } from '../models/AdditionalPropertiesClass';
import { AllOfWithSingleRef } from '../models/AllOfWithSingleRef';
import { Animal } from '../models/Animal';
import { ApiResponse } from '../models/ApiResponse';
import { ArrayOfArrayOfNumberOnly } from '../models/ArrayOfArrayOfNumberOnly';
import { ArrayOfNumberOnly } from '../models/ArrayOfNumberOnly';
import { ArrayTest } from '../models/ArrayTest';
import { Capitalization } from '../models/Capitalization';
import { Cat } from '../models/Cat';
import { Category } from '../models/Category';
import { ClassModel } from '../models/ClassModel';
import { Client } from '../models/Client';
import { DeprecatedObject } from '../models/DeprecatedObject';
import { Dog } from '../models/Dog';
import { EnumArrays } from '../models/EnumArrays';
import { EnumClass } from '../models/EnumClass';
import { EnumTest } from '../models/EnumTest';
import { FakeBigDecimalMap200Response } from '../models/FakeBigDecimalMap200Response';
import { FileSchemaTestClass } from '../models/FileSchemaTestClass';
import { Foo } from '../models/Foo';
import { FooGetDefaultResponse } from '../models/FooGetDefaultResponse';
import { FormatTest } from '../models/FormatTest';
import { HasOnlyReadOnly } from '../models/HasOnlyReadOnly';
import { HealthCheckResult } from '../models/HealthCheckResult';
import { List } from '../models/List';
import { MapTest } from '../models/MapTest';
import { MixedPropertiesAndAdditionalPropertiesClass } from '../models/MixedPropertiesAndAdditionalPropertiesClass';
import { Model200Response } from '../models/Model200Response';
import { ModelFile } from '../models/ModelFile';
import { Name } from '../models/Name';
import { NullableClass } from '../models/NullableClass';
import { NumberOnly } from '../models/NumberOnly';
import { ObjectWithDeprecatedFields } from '../models/ObjectWithDeprecatedFields';
import { Order } from '../models/Order';
import { OuterComposite } from '../models/OuterComposite';
import { OuterEnum } from '../models/OuterEnum';
import { OuterEnumDefaultValue } from '../models/OuterEnumDefaultValue';
import { OuterEnumInteger } from '../models/OuterEnumInteger';
import { OuterEnumIntegerDefaultValue } from '../models/OuterEnumIntegerDefaultValue';
import { OuterObjectWithEnumProperty } from '../models/OuterObjectWithEnumProperty';
import { Pet } from '../models/Pet';
import { ReadOnlyFirst } from '../models/ReadOnlyFirst';
import { Return } from '../models/Return';
import { SingleRefType } from '../models/SingleRefType';
import { SpecialModelName } from '../models/SpecialModelName';
import { Tag } from '../models/Tag';
import { User } from '../models/User';

import { ObservableAnotherFakeApi } from "./ObservableAPI";
import { AnotherFakeApiRequestFactory, AnotherFakeApiResponseProcessor} from "../apis/AnotherFakeApi";

export interface AnotherFakeApi123testSpecialTagsRequest {
    /**
     * client model
     * @type Client
     * @memberof AnotherFakeApi_123testSpecialTags
     */
    client: Client
}

export class ObjectAnotherFakeApi {
    private api: ObservableAnotherFakeApi

    public constructor(configuration: Configuration, requestFactory?: AnotherFakeApiRequestFactory, responseProcessor?: AnotherFakeApiResponseProcessor) {
        this.api = new ObservableAnotherFakeApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * To test special tags and operation ID starting with number
     * To test special tags
     * @param param the request object
     */
    public _123testSpecialTagsWithHttpInfo(param: AnotherFakeApi123testSpecialTagsRequest, options?: ConfigurationOptions): Promise<HttpInfo<Client>> {
        return this.api._123testSpecialTagsWithHttpInfo(param.client,  options).toPromise();
    }

    /**
     * To test special tags and operation ID starting with number
     * To test special tags
     * @param param the request object
     */
    public _123testSpecialTags(param: AnotherFakeApi123testSpecialTagsRequest, options?: ConfigurationOptions): Promise<Client> {
        return this.api._123testSpecialTags(param.client,  options).toPromise();
    }

}

import { ObservableDefaultApi } from "./ObservableAPI";
import { DefaultApiRequestFactory, DefaultApiResponseProcessor} from "../apis/DefaultApi";

export interface DefaultApiFooGetRequest {
}

export class ObjectDefaultApi {
    private api: ObservableDefaultApi

    public constructor(configuration: Configuration, requestFactory?: DefaultApiRequestFactory, responseProcessor?: DefaultApiResponseProcessor) {
        this.api = new ObservableDefaultApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * @param param the request object
     */
    public fooGetWithHttpInfo(param: DefaultApiFooGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.fooGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public fooGet(param: DefaultApiFooGetRequest = {}, options?: ConfigurationOptions): Promise<void> {
        return this.api.fooGet( options).toPromise();
    }

}

import { ObservableFakeApi } from "./ObservableAPI";
import { FakeApiRequestFactory, FakeApiResponseProcessor} from "../apis/FakeApi";

export interface FakeApiFakeBigDecimalMapRequest {
}

export interface FakeApiFakeHealthGetRequest {
}

export interface FakeApiFakeHttpSignatureTestRequest {
    /**
     * Pet object that needs to be added to the store
     * @type Pet
     * @memberof FakeApifakeHttpSignatureTest
     */
    pet: Pet
    /**
     * query parameter
     * Defaults to: undefined
     * @type string
     * @memberof FakeApifakeHttpSignatureTest
     */
    query1?: string
    /**
     * header parameter
     * Defaults to: undefined
     * @type string
     * @memberof FakeApifakeHttpSignatureTest
     */
    header1?: string
}

export interface FakeApiFakeOuterBooleanSerializeRequest {
    /**
     * Input boolean as post body
     * @type boolean
     * @memberof FakeApifakeOuterBooleanSerialize
     */
    body?: boolean
}

export interface FakeApiFakeOuterCompositeSerializeRequest {
    /**
     * Input composite as post body
     * @type OuterComposite
     * @memberof FakeApifakeOuterCompositeSerialize
     */
    outerComposite?: OuterComposite
}

export interface FakeApiFakeOuterNumberSerializeRequest {
    /**
     * Input number as post body
     * @type number
     * @memberof FakeApifakeOuterNumberSerialize
     */
    body?: number
}

export interface FakeApiFakeOuterStringSerializeRequest {
    /**
     * Input string as post body
     * @type string
     * @memberof FakeApifakeOuterStringSerialize
     */
    body?: string
}

export interface FakeApiFakePropertyEnumIntegerSerializeRequest {
    /**
     * Input enum (int) as post body
     * @type OuterObjectWithEnumProperty
     * @memberof FakeApifakePropertyEnumIntegerSerialize
     */
    outerObjectWithEnumProperty: OuterObjectWithEnumProperty
}

export interface FakeApiTestBodyWithBinaryRequest {
    /**
     * image to upload
     * @type HttpFile
     * @memberof FakeApitestBodyWithBinary
     */
    body: HttpFile
}

export interface FakeApiTestBodyWithFileSchemaRequest {
    /**
     * 
     * @type FileSchemaTestClass
     * @memberof FakeApitestBodyWithFileSchema
     */
    fileSchemaTestClass: FileSchemaTestClass
}

export interface FakeApiTestBodyWithQueryParamsRequest {
    /**
     * 
     * Defaults to: undefined
     * @type string
     * @memberof FakeApitestBodyWithQueryParams
     */
    query: string
    /**
     * 
     * @type User
     * @memberof FakeApitestBodyWithQueryParams
     */
    user: User
}

export interface FakeApiTestClientModelRequest {
    /**
     * client model
     * @type Client
     * @memberof FakeApitestClientModel
     */
    client: Client
}

export interface FakeApiTestEndpointParametersRequest {
    /**
     * None
     * Minimum: 32.1
     * Maximum: 543.2
     * Defaults to: undefined
     * @type number
     * @memberof FakeApitestEndpointParameters
     */
    number: number
    /**
     * None
     * Minimum: 67.8
     * Maximum: 123.4
     * Defaults to: undefined
     * @type number
     * @memberof FakeApitestEndpointParameters
     */
    _double: number
    /**
     * None
     * Defaults to: undefined
     * @type string
     * @memberof FakeApitestEndpointParameters
     */
    patternWithoutDelimiter: string
    /**
     * None
     * Defaults to: undefined
     * @type string
     * @memberof FakeApitestEndpointParameters
     */
    _byte: string
    /**
     * None
     * Minimum: 10
     * Maximum: 100
     * Defaults to: undefined
     * @type number
     * @memberof FakeApitestEndpointParameters
     */
    integer?: number
    /**
     * None
     * Minimum: 20
     * Maximum: 200
     * Defaults to: undefined
     * @type number
     * @memberof FakeApitestEndpointParameters
     */
    int32?: number
    /**
     * None
     * Defaults to: undefined
     * @type number
     * @memberof FakeApitestEndpointParameters
     */
    int64?: number
    /**
     * None
     * Maximum: 987.6
     * Defaults to: undefined
     * @type number
     * @memberof FakeApitestEndpointParameters
     */
    _float?: number
    /**
     * None
     * Defaults to: undefined
     * @type string
     * @memberof FakeApitestEndpointParameters
     */
    string?: string
    /**
     * None
     * Defaults to: undefined
     * @type HttpFile
     * @memberof FakeApitestEndpointParameters
     */
    binary?: HttpFile
    /**
     * None
     * Defaults to: undefined
     * @type string
     * @memberof FakeApitestEndpointParameters
     */
    date?: string
    /**
     * None
     * Defaults to: undefined
     * @type Date
     * @memberof FakeApitestEndpointParameters
     */
    dateTime?: Date
    /**
     * None
     * Defaults to: undefined
     * @type string
     * @memberof FakeApitestEndpointParameters
     */
    password?: string
    /**
     * None
     * Defaults to: undefined
     * @type string
     * @memberof FakeApitestEndpointParameters
     */
    callback?: string
}

export interface FakeApiTestEnumParametersRequest {
    /**
     * Header parameter enum test (string array)
     * Defaults to: undefined
     * @type Array&lt;&#39;&gt;&#39; | &#39;$&#39;&gt;
     * @memberof FakeApitestEnumParameters
     */
    enumHeaderStringArray?: Array<'>' | '$'>
    /**
     * Header parameter enum test (string)
     * Defaults to: &#39;-efg&#39;
     * @type &#39;_abc&#39; | &#39;-efg&#39; | &#39;(xyz)&#39;
     * @memberof FakeApitestEnumParameters
     */
    enumHeaderString?: '_abc' | '-efg' | '(xyz)'
    /**
     * Query parameter enum test (string array)
     * Defaults to: undefined
     * @type Array&lt;&#39;&gt;&#39; | &#39;$&#39;&gt;
     * @memberof FakeApitestEnumParameters
     */
    enumQueryStringArray?: Array<'>' | '$'>
    /**
     * Query parameter enum test (string)
     * Defaults to: &#39;-efg&#39;
     * @type &#39;_abc&#39; | &#39;-efg&#39; | &#39;(xyz)&#39;
     * @memberof FakeApitestEnumParameters
     */
    enumQueryString?: '_abc' | '-efg' | '(xyz)'
    /**
     * Query parameter enum test (double)
     * Defaults to: undefined
     * @type 1 | -2
     * @memberof FakeApitestEnumParameters
     */
    enumQueryInteger?: 1 | -2
    /**
     * Query parameter enum test (double)
     * Defaults to: undefined
     * @type 1.1 | -1.2
     * @memberof FakeApitestEnumParameters
     */
    enumQueryDouble?: 1.1 | -1.2
    /**
     * 
     * Defaults to: undefined
     * @type Array&lt;EnumClass&gt;
     * @memberof FakeApitestEnumParameters
     */
    enumQueryModelArray?: Array<EnumClass>
    /**
     * Form parameter enum test (string array)
     * Defaults to: &#39;$&#39;
     * @type Array&lt;string&gt;
     * @memberof FakeApitestEnumParameters
     */
    enumFormStringArray?: Array<string>
    /**
     * Form parameter enum test (string)
     * Defaults to: &#39;-efg&#39;
     * @type string
     * @memberof FakeApitestEnumParameters
     */
    enumFormString?: string
}

export interface FakeApiTestGroupParametersRequest {
    /**
     * Required String in group parameters
     * Defaults to: undefined
     * @type number
     * @memberof FakeApitestGroupParameters
     */
    requiredStringGroup: number
    /**
     * Required Boolean in group parameters
     * Defaults to: undefined
     * @type boolean
     * @memberof FakeApitestGroupParameters
     */
    requiredBooleanGroup: boolean
    /**
     * Required Integer in group parameters
     * Defaults to: undefined
     * @type number
     * @memberof FakeApitestGroupParameters
     */
    requiredInt64Group: number
    /**
     * String in group parameters
     * Defaults to: undefined
     * @type number
     * @memberof FakeApitestGroupParameters
     */
    stringGroup?: number
    /**
     * Boolean in group parameters
     * Defaults to: undefined
     * @type boolean
     * @memberof FakeApitestGroupParameters
     */
    booleanGroup?: boolean
    /**
     * Integer in group parameters
     * Defaults to: undefined
     * @type number
     * @memberof FakeApitestGroupParameters
     */
    int64Group?: number
}

export interface FakeApiTestInlineAdditionalPropertiesRequest {
    /**
     * request body
     * @type { [key: string]: string; }
     * @memberof FakeApitestInlineAdditionalProperties
     */
    requestBody: { [key: string]: string; }
}

export interface FakeApiTestJsonFormDataRequest {
    /**
     * field1
     * Defaults to: undefined
     * @type string
     * @memberof FakeApitestJsonFormData
     */
    param: string
    /**
     * field2
     * Defaults to: undefined
     * @type string
     * @memberof FakeApitestJsonFormData
     */
    param2: string
}

export interface FakeApiTestQueryParameterCollectionFormatRequest {
    /**
     * 
     * Defaults to: undefined
     * @type Array&lt;string&gt;
     * @memberof FakeApitestQueryParameterCollectionFormat
     */
    pipe: Array<string>
    /**
     * 
     * Defaults to: undefined
     * @type Array&lt;string&gt;
     * @memberof FakeApitestQueryParameterCollectionFormat
     */
    ioutil: Array<string>
    /**
     * 
     * Defaults to: undefined
     * @type Array&lt;string&gt;
     * @memberof FakeApitestQueryParameterCollectionFormat
     */
    http: Array<string>
    /**
     * 
     * Defaults to: undefined
     * @type Array&lt;string&gt;
     * @memberof FakeApitestQueryParameterCollectionFormat
     */
    url: Array<string>
    /**
     * 
     * Defaults to: undefined
     * @type Array&lt;string&gt;
     * @memberof FakeApitestQueryParameterCollectionFormat
     */
    context: Array<string>
    /**
     * 
     * Defaults to: undefined
     * @type string
     * @memberof FakeApitestQueryParameterCollectionFormat
     */
    allowEmpty: string
    /**
     * 
     * Defaults to: undefined
     * @type { [key: string]: string; }
     * @memberof FakeApitestQueryParameterCollectionFormat
     */
    language?: { [key: string]: string; }
}

export class ObjectFakeApi {
    private api: ObservableFakeApi

    public constructor(configuration: Configuration, requestFactory?: FakeApiRequestFactory, responseProcessor?: FakeApiResponseProcessor) {
        this.api = new ObservableFakeApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
     * @param param the request object
     */
    public fakeBigDecimalMapWithHttpInfo(param: FakeApiFakeBigDecimalMapRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<FakeBigDecimalMap200Response>> {
        return this.api.fakeBigDecimalMapWithHttpInfo( options).toPromise();
    }

    /**
     * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
     * @param param the request object
     */
    public fakeBigDecimalMap(param: FakeApiFakeBigDecimalMapRequest = {}, options?: ConfigurationOptions): Promise<FakeBigDecimalMap200Response> {
        return this.api.fakeBigDecimalMap( options).toPromise();
    }

    /**
     * Health check endpoint
     * @param param the request object
     */
    public fakeHealthGetWithHttpInfo(param: FakeApiFakeHealthGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<HealthCheckResult>> {
        return this.api.fakeHealthGetWithHttpInfo( options).toPromise();
    }

    /**
     * Health check endpoint
     * @param param the request object
     */
    public fakeHealthGet(param: FakeApiFakeHealthGetRequest = {}, options?: ConfigurationOptions): Promise<HealthCheckResult> {
        return this.api.fakeHealthGet( options).toPromise();
    }

    /**
     * test http signature authentication
     * @param param the request object
     */
    public fakeHttpSignatureTestWithHttpInfo(param: FakeApiFakeHttpSignatureTestRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.fakeHttpSignatureTestWithHttpInfo(param.pet, param.query1, param.header1,  options).toPromise();
    }

    /**
     * test http signature authentication
     * @param param the request object
     */
    public fakeHttpSignatureTest(param: FakeApiFakeHttpSignatureTestRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.fakeHttpSignatureTest(param.pet, param.query1, param.header1,  options).toPromise();
    }

    /**
     * Test serialization of outer boolean types
     * @param param the request object
     */
    public fakeOuterBooleanSerializeWithHttpInfo(param: FakeApiFakeOuterBooleanSerializeRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<boolean>> {
        return this.api.fakeOuterBooleanSerializeWithHttpInfo(param.body,  options).toPromise();
    }

    /**
     * Test serialization of outer boolean types
     * @param param the request object
     */
    public fakeOuterBooleanSerialize(param: FakeApiFakeOuterBooleanSerializeRequest = {}, options?: ConfigurationOptions): Promise<boolean> {
        return this.api.fakeOuterBooleanSerialize(param.body,  options).toPromise();
    }

    /**
     * Test serialization of object with outer number type
     * @param param the request object
     */
    public fakeOuterCompositeSerializeWithHttpInfo(param: FakeApiFakeOuterCompositeSerializeRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<OuterComposite>> {
        return this.api.fakeOuterCompositeSerializeWithHttpInfo(param.outerComposite,  options).toPromise();
    }

    /**
     * Test serialization of object with outer number type
     * @param param the request object
     */
    public fakeOuterCompositeSerialize(param: FakeApiFakeOuterCompositeSerializeRequest = {}, options?: ConfigurationOptions): Promise<OuterComposite> {
        return this.api.fakeOuterCompositeSerialize(param.outerComposite,  options).toPromise();
    }

    /**
     * Test serialization of outer number types
     * @param param the request object
     */
    public fakeOuterNumberSerializeWithHttpInfo(param: FakeApiFakeOuterNumberSerializeRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<number>> {
        return this.api.fakeOuterNumberSerializeWithHttpInfo(param.body,  options).toPromise();
    }

    /**
     * Test serialization of outer number types
     * @param param the request object
     */
    public fakeOuterNumberSerialize(param: FakeApiFakeOuterNumberSerializeRequest = {}, options?: ConfigurationOptions): Promise<number> {
        return this.api.fakeOuterNumberSerialize(param.body,  options).toPromise();
    }

    /**
     * Test serialization of outer string types
     * @param param the request object
     */
    public fakeOuterStringSerializeWithHttpInfo(param: FakeApiFakeOuterStringSerializeRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.fakeOuterStringSerializeWithHttpInfo(param.body,  options).toPromise();
    }

    /**
     * Test serialization of outer string types
     * @param param the request object
     */
    public fakeOuterStringSerialize(param: FakeApiFakeOuterStringSerializeRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.fakeOuterStringSerialize(param.body,  options).toPromise();
    }

    /**
     * Test serialization of enum (int) properties with examples
     * @param param the request object
     */
    public fakePropertyEnumIntegerSerializeWithHttpInfo(param: FakeApiFakePropertyEnumIntegerSerializeRequest, options?: ConfigurationOptions): Promise<HttpInfo<OuterObjectWithEnumProperty>> {
        return this.api.fakePropertyEnumIntegerSerializeWithHttpInfo(param.outerObjectWithEnumProperty,  options).toPromise();
    }

    /**
     * Test serialization of enum (int) properties with examples
     * @param param the request object
     */
    public fakePropertyEnumIntegerSerialize(param: FakeApiFakePropertyEnumIntegerSerializeRequest, options?: ConfigurationOptions): Promise<OuterObjectWithEnumProperty> {
        return this.api.fakePropertyEnumIntegerSerialize(param.outerObjectWithEnumProperty,  options).toPromise();
    }

    /**
     * For this test, the body has to be a binary file.
     * @param param the request object
     */
    public testBodyWithBinaryWithHttpInfo(param: FakeApiTestBodyWithBinaryRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testBodyWithBinaryWithHttpInfo(param.body,  options).toPromise();
    }

    /**
     * For this test, the body has to be a binary file.
     * @param param the request object
     */
    public testBodyWithBinary(param: FakeApiTestBodyWithBinaryRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testBodyWithBinary(param.body,  options).toPromise();
    }

    /**
     * For this test, the body for this request must reference a schema named `File`.
     * @param param the request object
     */
    public testBodyWithFileSchemaWithHttpInfo(param: FakeApiTestBodyWithFileSchemaRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testBodyWithFileSchemaWithHttpInfo(param.fileSchemaTestClass,  options).toPromise();
    }

    /**
     * For this test, the body for this request must reference a schema named `File`.
     * @param param the request object
     */
    public testBodyWithFileSchema(param: FakeApiTestBodyWithFileSchemaRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testBodyWithFileSchema(param.fileSchemaTestClass,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testBodyWithQueryParamsWithHttpInfo(param: FakeApiTestBodyWithQueryParamsRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testBodyWithQueryParamsWithHttpInfo(param.query, param.user,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testBodyWithQueryParams(param: FakeApiTestBodyWithQueryParamsRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testBodyWithQueryParams(param.query, param.user,  options).toPromise();
    }

    /**
     * To test \"client\" model
     * To test \"client\" model
     * @param param the request object
     */
    public testClientModelWithHttpInfo(param: FakeApiTestClientModelRequest, options?: ConfigurationOptions): Promise<HttpInfo<Client>> {
        return this.api.testClientModelWithHttpInfo(param.client,  options).toPromise();
    }

    /**
     * To test \"client\" model
     * To test \"client\" model
     * @param param the request object
     */
    public testClientModel(param: FakeApiTestClientModelRequest, options?: ConfigurationOptions): Promise<Client> {
        return this.api.testClientModel(param.client,  options).toPromise();
    }

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * @param param the request object
     */
    public testEndpointParametersWithHttpInfo(param: FakeApiTestEndpointParametersRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEndpointParametersWithHttpInfo(param.number, param._double, param.patternWithoutDelimiter, param._byte, param.integer, param.int32, param.int64, param._float, param.string, param.binary, param.date, param.dateTime, param.password, param.callback,  options).toPromise();
    }

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * @param param the request object
     */
    public testEndpointParameters(param: FakeApiTestEndpointParametersRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEndpointParameters(param.number, param._double, param.patternWithoutDelimiter, param._byte, param.integer, param.int32, param.int64, param._float, param.string, param.binary, param.date, param.dateTime, param.password, param.callback,  options).toPromise();
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * @param param the request object
     */
    public testEnumParametersWithHttpInfo(param: FakeApiTestEnumParametersRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEnumParametersWithHttpInfo(param.enumHeaderStringArray, param.enumHeaderString, param.enumQueryStringArray, param.enumQueryString, param.enumQueryInteger, param.enumQueryDouble, param.enumQueryModelArray, param.enumFormStringArray, param.enumFormString,  options).toPromise();
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * @param param the request object
     */
    public testEnumParameters(param: FakeApiTestEnumParametersRequest = {}, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEnumParameters(param.enumHeaderStringArray, param.enumHeaderString, param.enumQueryStringArray, param.enumQueryString, param.enumQueryInteger, param.enumQueryDouble, param.enumQueryModelArray, param.enumFormStringArray, param.enumFormString,  options).toPromise();
    }

    /**
     * Fake endpoint to test group parameters (optional)
     * Fake endpoint to test group parameters (optional)
     * @param param the request object
     */
    public testGroupParametersWithHttpInfo(param: FakeApiTestGroupParametersRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testGroupParametersWithHttpInfo(param.requiredStringGroup, param.requiredBooleanGroup, param.requiredInt64Group, param.stringGroup, param.booleanGroup, param.int64Group,  options).toPromise();
    }

    /**
     * Fake endpoint to test group parameters (optional)
     * Fake endpoint to test group parameters (optional)
     * @param param the request object
     */
    public testGroupParameters(param: FakeApiTestGroupParametersRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testGroupParameters(param.requiredStringGroup, param.requiredBooleanGroup, param.requiredInt64Group, param.stringGroup, param.booleanGroup, param.int64Group,  options).toPromise();
    }

    /**
     * 
     * test inline additionalProperties
     * @param param the request object
     */
    public testInlineAdditionalPropertiesWithHttpInfo(param: FakeApiTestInlineAdditionalPropertiesRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testInlineAdditionalPropertiesWithHttpInfo(param.requestBody,  options).toPromise();
    }

    /**
     * 
     * test inline additionalProperties
     * @param param the request object
     */
    public testInlineAdditionalProperties(param: FakeApiTestInlineAdditionalPropertiesRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testInlineAdditionalProperties(param.requestBody,  options).toPromise();
    }

    /**
     * 
     * test json serialization of form data
     * @param param the request object
     */
    public testJsonFormDataWithHttpInfo(param: FakeApiTestJsonFormDataRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testJsonFormDataWithHttpInfo(param.param, param.param2,  options).toPromise();
    }

    /**
     * 
     * test json serialization of form data
     * @param param the request object
     */
    public testJsonFormData(param: FakeApiTestJsonFormDataRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testJsonFormData(param.param, param.param2,  options).toPromise();
    }

    /**
     * To test the collection format in query parameters
     * @param param the request object
     */
    public testQueryParameterCollectionFormatWithHttpInfo(param: FakeApiTestQueryParameterCollectionFormatRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testQueryParameterCollectionFormatWithHttpInfo(param.pipe, param.ioutil, param.http, param.url, param.context, param.allowEmpty, param.language,  options).toPromise();
    }

    /**
     * To test the collection format in query parameters
     * @param param the request object
     */
    public testQueryParameterCollectionFormat(param: FakeApiTestQueryParameterCollectionFormatRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testQueryParameterCollectionFormat(param.pipe, param.ioutil, param.http, param.url, param.context, param.allowEmpty, param.language,  options).toPromise();
    }

}

import { ObservableFakeClassnameTags123Api } from "./ObservableAPI";
import { FakeClassnameTags123ApiRequestFactory, FakeClassnameTags123ApiResponseProcessor} from "../apis/FakeClassnameTags123Api";

export interface FakeClassnameTags123ApiTestClassnameRequest {
    /**
     * client model
     * @type Client
     * @memberof FakeClassnameTags123ApitestClassname
     */
    client: Client
}

export class ObjectFakeClassnameTags123Api {
    private api: ObservableFakeClassnameTags123Api

    public constructor(configuration: Configuration, requestFactory?: FakeClassnameTags123ApiRequestFactory, responseProcessor?: FakeClassnameTags123ApiResponseProcessor) {
        this.api = new ObservableFakeClassnameTags123Api(configuration, requestFactory, responseProcessor);
    }

    /**
     * To test class name in snake case
     * To test class name in snake case
     * @param param the request object
     */
    public testClassnameWithHttpInfo(param: FakeClassnameTags123ApiTestClassnameRequest, options?: ConfigurationOptions): Promise<HttpInfo<Client>> {
        return this.api.testClassnameWithHttpInfo(param.client,  options).toPromise();
    }

    /**
     * To test class name in snake case
     * To test class name in snake case
     * @param param the request object
     */
    public testClassname(param: FakeClassnameTags123ApiTestClassnameRequest, options?: ConfigurationOptions): Promise<Client> {
        return this.api.testClassname(param.client,  options).toPromise();
    }

}

import { ObservablePetApi } from "./ObservableAPI";
import { PetApiRequestFactory, PetApiResponseProcessor} from "../apis/PetApi";

export interface PetApiAddPetRequest {
    /**
     * Pet object that needs to be added to the store
     * @type Pet
     * @memberof PetApiaddPet
     */
    pet: Pet
}

export interface PetApiDeletePetRequest {
    /**
     * Pet id to delete
     * Defaults to: undefined
     * @type number
     * @memberof PetApideletePet
     */
    petId: number
    /**
     * 
     * Defaults to: undefined
     * @type string
     * @memberof PetApideletePet
     */
    apiKey?: string
}

export interface PetApiFindPetsByStatusRequest {
    /**
     * Status values that need to be considered for filter
     * Defaults to: undefined
     * @type Array&lt;&#39;available&#39; | &#39;pending&#39; | &#39;sold&#39;&gt;
     * @memberof PetApifindPetsByStatus
     */
    status: Array<'available' | 'pending' | 'sold'>
}

export interface PetApiFindPetsByTagsRequest {
    /**
     * Tags to filter by
     * Defaults to: undefined
     * @type Set&lt;string&gt;
     * @memberof PetApifindPetsByTags
     */
    tags: Set<string>
}

export interface PetApiGetPetByIdRequest {
    /**
     * ID of pet to return
     * Defaults to: undefined
     * @type number
     * @memberof PetApigetPetById
     */
    petId: number
}

export interface PetApiUpdatePetRequest {
    /**
     * Pet object that needs to be added to the store
     * @type Pet
     * @memberof PetApiupdatePet
     */
    pet: Pet
}

export interface PetApiUpdatePetWithFormRequest {
    /**
     * ID of pet that needs to be updated
     * Defaults to: undefined
     * @type number
     * @memberof PetApiupdatePetWithForm
     */
    petId: number
    /**
     * Updated name of the pet
     * Defaults to: undefined
     * @type string
     * @memberof PetApiupdatePetWithForm
     */
    name?: string
    /**
     * Updated status of the pet
     * Defaults to: undefined
     * @type string
     * @memberof PetApiupdatePetWithForm
     */
    status?: string
}

export interface PetApiUploadFileRequest {
    /**
     * ID of pet to update
     * Defaults to: undefined
     * @type number
     * @memberof PetApiuploadFile
     */
    petId: number
    /**
     * Additional data to pass to server
     * Defaults to: undefined
     * @type string
     * @memberof PetApiuploadFile
     */
    additionalMetadata?: string
    /**
     * file to upload
     * Defaults to: undefined
     * @type HttpFile
     * @memberof PetApiuploadFile
     */
    file?: HttpFile
}

export interface PetApiUploadFileWithRequiredFileRequest {
    /**
     * ID of pet to update
     * Defaults to: undefined
     * @type number
     * @memberof PetApiuploadFileWithRequiredFile
     */
    petId: number
    /**
     * file to upload
     * Defaults to: undefined
     * @type HttpFile
     * @memberof PetApiuploadFileWithRequiredFile
     */
    requiredFile: HttpFile
    /**
     * Additional data to pass to server
     * Defaults to: undefined
     * @type string
     * @memberof PetApiuploadFileWithRequiredFile
     */
    additionalMetadata?: string
}

export class ObjectPetApi {
    private api: ObservablePetApi

    public constructor(configuration: Configuration, requestFactory?: PetApiRequestFactory, responseProcessor?: PetApiResponseProcessor) {
        this.api = new ObservablePetApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * 
     * Add a new pet to the store
     * @param param the request object
     */
    public addPetWithHttpInfo(param: PetApiAddPetRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.addPetWithHttpInfo(param.pet,  options).toPromise();
    }

    /**
     * 
     * Add a new pet to the store
     * @param param the request object
     */
    public addPet(param: PetApiAddPetRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.addPet(param.pet,  options).toPromise();
    }

    /**
     * 
     * Deletes a pet
     * @param param the request object
     */
    public deletePetWithHttpInfo(param: PetApiDeletePetRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.deletePetWithHttpInfo(param.petId, param.apiKey,  options).toPromise();
    }

    /**
     * 
     * Deletes a pet
     * @param param the request object
     */
    public deletePet(param: PetApiDeletePetRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.deletePet(param.petId, param.apiKey,  options).toPromise();
    }

    /**
     * Multiple status values can be provided with comma separated strings
     * Finds Pets by status
     * @param param the request object
     */
    public findPetsByStatusWithHttpInfo(param: PetApiFindPetsByStatusRequest, options?: ConfigurationOptions): Promise<HttpInfo<Array<Pet>>> {
        return this.api.findPetsByStatusWithHttpInfo(param.status,  options).toPromise();
    }

    /**
     * Multiple status values can be provided with comma separated strings
     * Finds Pets by status
     * @param param the request object
     */
    public findPetsByStatus(param: PetApiFindPetsByStatusRequest, options?: ConfigurationOptions): Promise<Array<Pet>> {
        return this.api.findPetsByStatus(param.status,  options).toPromise();
    }

    /**
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * Finds Pets by tags
     * @param param the request object
     */
    public findPetsByTagsWithHttpInfo(param: PetApiFindPetsByTagsRequest, options?: ConfigurationOptions): Promise<HttpInfo<Set<Pet>>> {
        return this.api.findPetsByTagsWithHttpInfo(param.tags,  options).toPromise();
    }

    /**
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * Finds Pets by tags
     * @param param the request object
     */
    public findPetsByTags(param: PetApiFindPetsByTagsRequest, options?: ConfigurationOptions): Promise<Set<Pet>> {
        return this.api.findPetsByTags(param.tags,  options).toPromise();
    }

    /**
     * Returns a single pet
     * Find pet by ID
     * @param param the request object
     */
    public getPetByIdWithHttpInfo(param: PetApiGetPetByIdRequest, options?: ConfigurationOptions): Promise<HttpInfo<Pet>> {
        return this.api.getPetByIdWithHttpInfo(param.petId,  options).toPromise();
    }

    /**
     * Returns a single pet
     * Find pet by ID
     * @param param the request object
     */
    public getPetById(param: PetApiGetPetByIdRequest, options?: ConfigurationOptions): Promise<Pet> {
        return this.api.getPetById(param.petId,  options).toPromise();
    }

    /**
     * 
     * Update an existing pet
     * @param param the request object
     */
    public updatePetWithHttpInfo(param: PetApiUpdatePetRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.updatePetWithHttpInfo(param.pet,  options).toPromise();
    }

    /**
     * 
     * Update an existing pet
     * @param param the request object
     */
    public updatePet(param: PetApiUpdatePetRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.updatePet(param.pet,  options).toPromise();
    }

    /**
     * 
     * Updates a pet in the store with form data
     * @param param the request object
     */
    public updatePetWithFormWithHttpInfo(param: PetApiUpdatePetWithFormRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.updatePetWithFormWithHttpInfo(param.petId, param.name, param.status,  options).toPromise();
    }

    /**
     * 
     * Updates a pet in the store with form data
     * @param param the request object
     */
    public updatePetWithForm(param: PetApiUpdatePetWithFormRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.updatePetWithForm(param.petId, param.name, param.status,  options).toPromise();
    }

    /**
     * 
     * uploads an image
     * @param param the request object
     */
    public uploadFileWithHttpInfo(param: PetApiUploadFileRequest, options?: ConfigurationOptions): Promise<HttpInfo<ApiResponse>> {
        return this.api.uploadFileWithHttpInfo(param.petId, param.additionalMetadata, param.file,  options).toPromise();
    }

    /**
     * 
     * uploads an image
     * @param param the request object
     */
    public uploadFile(param: PetApiUploadFileRequest, options?: ConfigurationOptions): Promise<ApiResponse> {
        return this.api.uploadFile(param.petId, param.additionalMetadata, param.file,  options).toPromise();
    }

    /**
     * 
     * uploads an image (required)
     * @param param the request object
     */
    public uploadFileWithRequiredFileWithHttpInfo(param: PetApiUploadFileWithRequiredFileRequest, options?: ConfigurationOptions): Promise<HttpInfo<ApiResponse>> {
        return this.api.uploadFileWithRequiredFileWithHttpInfo(param.petId, param.requiredFile, param.additionalMetadata,  options).toPromise();
    }

    /**
     * 
     * uploads an image (required)
     * @param param the request object
     */
    public uploadFileWithRequiredFile(param: PetApiUploadFileWithRequiredFileRequest, options?: ConfigurationOptions): Promise<ApiResponse> {
        return this.api.uploadFileWithRequiredFile(param.petId, param.requiredFile, param.additionalMetadata,  options).toPromise();
    }

}

import { ObservableStoreApi } from "./ObservableAPI";
import { StoreApiRequestFactory, StoreApiResponseProcessor} from "../apis/StoreApi";

export interface StoreApiDeleteOrderRequest {
    /**
     * ID of the order that needs to be deleted
     * Defaults to: undefined
     * @type string
     * @memberof StoreApideleteOrder
     */
    orderId: string
}

export interface StoreApiGetInventoryRequest {
}

export interface StoreApiGetOrderByIdRequest {
    /**
     * ID of pet that needs to be fetched
     * Minimum: 1
     * Maximum: 5
     * Defaults to: undefined
     * @type number
     * @memberof StoreApigetOrderById
     */
    orderId: number
}

export interface StoreApiPlaceOrderRequest {
    /**
     * order placed for purchasing the pet
     * @type Order
     * @memberof StoreApiplaceOrder
     */
    order: Order
}

export class ObjectStoreApi {
    private api: ObservableStoreApi

    public constructor(configuration: Configuration, requestFactory?: StoreApiRequestFactory, responseProcessor?: StoreApiResponseProcessor) {
        this.api = new ObservableStoreApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
     * Delete purchase order by ID
     * @param param the request object
     */
    public deleteOrderWithHttpInfo(param: StoreApiDeleteOrderRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.deleteOrderWithHttpInfo(param.orderId,  options).toPromise();
    }

    /**
     * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
     * Delete purchase order by ID
     * @param param the request object
     */
    public deleteOrder(param: StoreApiDeleteOrderRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.deleteOrder(param.orderId,  options).toPromise();
    }

    /**
     * Returns a map of status codes to quantities
     * Returns pet inventories by status
     * @param param the request object
     */
    public getInventoryWithHttpInfo(param: StoreApiGetInventoryRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<{ [key: string]: number; }>> {
        return this.api.getInventoryWithHttpInfo( options).toPromise();
    }

    /**
     * Returns a map of status codes to quantities
     * Returns pet inventories by status
     * @param param the request object
     */
    public getInventory(param: StoreApiGetInventoryRequest = {}, options?: ConfigurationOptions): Promise<{ [key: string]: number; }> {
        return this.api.getInventory( options).toPromise();
    }

    /**
     * For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions
     * Find purchase order by ID
     * @param param the request object
     */
    public getOrderByIdWithHttpInfo(param: StoreApiGetOrderByIdRequest, options?: ConfigurationOptions): Promise<HttpInfo<Order>> {
        return this.api.getOrderByIdWithHttpInfo(param.orderId,  options).toPromise();
    }

    /**
     * For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions
     * Find purchase order by ID
     * @param param the request object
     */
    public getOrderById(param: StoreApiGetOrderByIdRequest, options?: ConfigurationOptions): Promise<Order> {
        return this.api.getOrderById(param.orderId,  options).toPromise();
    }

    /**
     * 
     * Place an order for a pet
     * @param param the request object
     */
    public placeOrderWithHttpInfo(param: StoreApiPlaceOrderRequest, options?: ConfigurationOptions): Promise<HttpInfo<Order>> {
        return this.api.placeOrderWithHttpInfo(param.order,  options).toPromise();
    }

    /**
     * 
     * Place an order for a pet
     * @param param the request object
     */
    public placeOrder(param: StoreApiPlaceOrderRequest, options?: ConfigurationOptions): Promise<Order> {
        return this.api.placeOrder(param.order,  options).toPromise();
    }

}

import { ObservableUserApi } from "./ObservableAPI";
import { UserApiRequestFactory, UserApiResponseProcessor} from "../apis/UserApi";

export interface UserApiCreateUserRequest {
    /**
     * Created user object
     * @type User
     * @memberof UserApicreateUser
     */
    user: User
}

export interface UserApiCreateUsersWithArrayInputRequest {
    /**
     * List of user object
     * @type Array&lt;User&gt;
     * @memberof UserApicreateUsersWithArrayInput
     */
    user: Array<User>
}

export interface UserApiCreateUsersWithListInputRequest {
    /**
     * List of user object
     * @type Array&lt;User&gt;
     * @memberof UserApicreateUsersWithListInput
     */
    user: Array<User>
}

export interface UserApiDeleteUserRequest {
    /**
     * The name that needs to be deleted
     * Defaults to: undefined
     * @type string
     * @memberof UserApideleteUser
     */
    username: string
}

export interface UserApiGetUserByNameRequest {
    /**
     * The name that needs to be fetched. Use user1 for testing.
     * Defaults to: undefined
     * @type string
     * @memberof UserApigetUserByName
     */
    username: string
}

export interface UserApiLoginUserRequest {
    /**
     * The user name for login
     * Defaults to: undefined
     * @type string
     * @memberof UserApiloginUser
     */
    username: string
    /**
     * The password for login in clear text
     * Defaults to: undefined
     * @type string
     * @memberof UserApiloginUser
     */
    password: string
}

export interface UserApiLogoutUserRequest {
}

export interface UserApiUpdateUserRequest {
    /**
     * name that need to be deleted
     * Defaults to: undefined
     * @type string
     * @memberof UserApiupdateUser
     */
    username: string
    /**
     * Updated user object
     * @type User
     * @memberof UserApiupdateUser
     */
    user: User
}

export class ObjectUserApi {
    private api: ObservableUserApi

    public constructor(configuration: Configuration, requestFactory?: UserApiRequestFactory, responseProcessor?: UserApiResponseProcessor) {
        this.api = new ObservableUserApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * This can only be done by the logged in user.
     * Create user
     * @param param the request object
     */
    public createUserWithHttpInfo(param: UserApiCreateUserRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.createUserWithHttpInfo(param.user,  options).toPromise();
    }

    /**
     * This can only be done by the logged in user.
     * Create user
     * @param param the request object
     */
    public createUser(param: UserApiCreateUserRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.createUser(param.user,  options).toPromise();
    }

    /**
     * 
     * Creates list of users with given input array
     * @param param the request object
     */
    public createUsersWithArrayInputWithHttpInfo(param: UserApiCreateUsersWithArrayInputRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.createUsersWithArrayInputWithHttpInfo(param.user,  options).toPromise();
    }

    /**
     * 
     * Creates list of users with given input array
     * @param param the request object
     */
    public createUsersWithArrayInput(param: UserApiCreateUsersWithArrayInputRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.createUsersWithArrayInput(param.user,  options).toPromise();
    }

    /**
     * 
     * Creates list of users with given input array
     * @param param the request object
     */
    public createUsersWithListInputWithHttpInfo(param: UserApiCreateUsersWithListInputRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.createUsersWithListInputWithHttpInfo(param.user,  options).toPromise();
    }

    /**
     * 
     * Creates list of users with given input array
     * @param param the request object
     */
    public createUsersWithListInput(param: UserApiCreateUsersWithListInputRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.createUsersWithListInput(param.user,  options).toPromise();
    }

    /**
     * This can only be done by the logged in user.
     * Delete user
     * @param param the request object
     */
    public deleteUserWithHttpInfo(param: UserApiDeleteUserRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.deleteUserWithHttpInfo(param.username,  options).toPromise();
    }

    /**
     * This can only be done by the logged in user.
     * Delete user
     * @param param the request object
     */
    public deleteUser(param: UserApiDeleteUserRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.deleteUser(param.username,  options).toPromise();
    }

    /**
     * 
     * Get user by user name
     * @param param the request object
     */
    public getUserByNameWithHttpInfo(param: UserApiGetUserByNameRequest, options?: ConfigurationOptions): Promise<HttpInfo<User>> {
        return this.api.getUserByNameWithHttpInfo(param.username,  options).toPromise();
    }

    /**
     * 
     * Get user by user name
     * @param param the request object
     */
    public getUserByName(param: UserApiGetUserByNameRequest, options?: ConfigurationOptions): Promise<User> {
        return this.api.getUserByName(param.username,  options).toPromise();
    }

    /**
     * 
     * Logs user into the system
     * @param param the request object
     */
    public loginUserWithHttpInfo(param: UserApiLoginUserRequest, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.loginUserWithHttpInfo(param.username, param.password,  options).toPromise();
    }

    /**
     * 
     * Logs user into the system
     * @param param the request object
     */
    public loginUser(param: UserApiLoginUserRequest, options?: ConfigurationOptions): Promise<string> {
        return this.api.loginUser(param.username, param.password,  options).toPromise();
    }

    /**
     * 
     * Logs out current logged in user session
     * @param param the request object
     */
    public logoutUserWithHttpInfo(param: UserApiLogoutUserRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.logoutUserWithHttpInfo( options).toPromise();
    }

    /**
     * 
     * Logs out current logged in user session
     * @param param the request object
     */
    public logoutUser(param: UserApiLogoutUserRequest = {}, options?: ConfigurationOptions): Promise<void> {
        return this.api.logoutUser( options).toPromise();
    }

    /**
     * This can only be done by the logged in user.
     * Updated user
     * @param param the request object
     */
    public updateUserWithHttpInfo(param: UserApiUpdateUserRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.updateUserWithHttpInfo(param.username, param.user,  options).toPromise();
    }

    /**
     * This can only be done by the logged in user.
     * Updated user
     * @param param the request object
     */
    public updateUser(param: UserApiUpdateUserRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.updateUser(param.username, param.user,  options).toPromise();
    }

}
