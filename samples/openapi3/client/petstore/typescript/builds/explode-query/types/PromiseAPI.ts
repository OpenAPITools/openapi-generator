import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration, PromiseConfigurationOptions, wrapOptions } from '../configuration'
import { PromiseMiddleware, Middleware, PromiseMiddlewareWrapper } from '../middleware';

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
import { ObservableAnotherFakeApi } from './ObservableAPI';

import { AnotherFakeApiRequestFactory, AnotherFakeApiResponseProcessor} from "../apis/AnotherFakeApi";
export class PromiseAnotherFakeApi {
    private api: ObservableAnotherFakeApi

    public constructor(
        configuration: Configuration,
        requestFactory?: AnotherFakeApiRequestFactory,
        responseProcessor?: AnotherFakeApiResponseProcessor
    ) {
        this.api = new ObservableAnotherFakeApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * To test special tags and operation ID starting with number
     * To test special tags
     * @param client client model
     */
    public _123testSpecialTagsWithHttpInfo(client: Client, _options?: PromiseConfigurationOptions): Promise<HttpInfo<Client>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api._123testSpecialTagsWithHttpInfo(client, observableOptions);
        return result.toPromise();
    }

    /**
     * To test special tags and operation ID starting with number
     * To test special tags
     * @param client client model
     */
    public _123testSpecialTags(client: Client, _options?: PromiseConfigurationOptions): Promise<Client> {
        const observableOptions = wrapOptions(_options);
        const result = this.api._123testSpecialTags(client, observableOptions);
        return result.toPromise();
    }


}



import { ObservableDefaultApi } from './ObservableAPI';

import { DefaultApiRequestFactory, DefaultApiResponseProcessor} from "../apis/DefaultApi";
export class PromiseDefaultApi {
    private api: ObservableDefaultApi

    public constructor(
        configuration: Configuration,
        requestFactory?: DefaultApiRequestFactory,
        responseProcessor?: DefaultApiResponseProcessor
    ) {
        this.api = new ObservableDefaultApi(configuration, requestFactory, responseProcessor);
    }

    /**
     */
    public fooGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fooGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public fooGet(_options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fooGet(observableOptions);
        return result.toPromise();
    }


}



import { ObservableFakeApi } from './ObservableAPI';

import { FakeApiRequestFactory, FakeApiResponseProcessor} from "../apis/FakeApi";
export class PromiseFakeApi {
    private api: ObservableFakeApi

    public constructor(
        configuration: Configuration,
        requestFactory?: FakeApiRequestFactory,
        responseProcessor?: FakeApiResponseProcessor
    ) {
        this.api = new ObservableFakeApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
     */
    public fakeBigDecimalMapWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<FakeBigDecimalMap200Response>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fakeBigDecimalMapWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
     */
    public fakeBigDecimalMap(_options?: PromiseConfigurationOptions): Promise<FakeBigDecimalMap200Response> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fakeBigDecimalMap(observableOptions);
        return result.toPromise();
    }

    /**
     * Health check endpoint
     */
    public fakeHealthGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<HealthCheckResult>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fakeHealthGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     * Health check endpoint
     */
    public fakeHealthGet(_options?: PromiseConfigurationOptions): Promise<HealthCheckResult> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fakeHealthGet(observableOptions);
        return result.toPromise();
    }

    /**
     * test http signature authentication
     * @param pet Pet object that needs to be added to the store
     * @param [query1] query parameter
     * @param [header1] header parameter
     */
    public fakeHttpSignatureTestWithHttpInfo(pet: Pet, query1?: string, header1?: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fakeHttpSignatureTestWithHttpInfo(pet, query1, header1, observableOptions);
        return result.toPromise();
    }

    /**
     * test http signature authentication
     * @param pet Pet object that needs to be added to the store
     * @param [query1] query parameter
     * @param [header1] header parameter
     */
    public fakeHttpSignatureTest(pet: Pet, query1?: string, header1?: string, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fakeHttpSignatureTest(pet, query1, header1, observableOptions);
        return result.toPromise();
    }

    /**
     * Test serialization of outer boolean types
     * @param [body] Input boolean as post body
     */
    public fakeOuterBooleanSerializeWithHttpInfo(body?: boolean, _options?: PromiseConfigurationOptions): Promise<HttpInfo<boolean>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fakeOuterBooleanSerializeWithHttpInfo(body, observableOptions);
        return result.toPromise();
    }

    /**
     * Test serialization of outer boolean types
     * @param [body] Input boolean as post body
     */
    public fakeOuterBooleanSerialize(body?: boolean, _options?: PromiseConfigurationOptions): Promise<boolean> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fakeOuterBooleanSerialize(body, observableOptions);
        return result.toPromise();
    }

    /**
     * Test serialization of object with outer number type
     * @param [outerComposite] Input composite as post body
     */
    public fakeOuterCompositeSerializeWithHttpInfo(outerComposite?: OuterComposite, _options?: PromiseConfigurationOptions): Promise<HttpInfo<OuterComposite>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fakeOuterCompositeSerializeWithHttpInfo(outerComposite, observableOptions);
        return result.toPromise();
    }

    /**
     * Test serialization of object with outer number type
     * @param [outerComposite] Input composite as post body
     */
    public fakeOuterCompositeSerialize(outerComposite?: OuterComposite, _options?: PromiseConfigurationOptions): Promise<OuterComposite> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fakeOuterCompositeSerialize(outerComposite, observableOptions);
        return result.toPromise();
    }

    /**
     * Test serialization of outer number types
     * @param [body] Input number as post body
     */
    public fakeOuterNumberSerializeWithHttpInfo(body?: number, _options?: PromiseConfigurationOptions): Promise<HttpInfo<number>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fakeOuterNumberSerializeWithHttpInfo(body, observableOptions);
        return result.toPromise();
    }

    /**
     * Test serialization of outer number types
     * @param [body] Input number as post body
     */
    public fakeOuterNumberSerialize(body?: number, _options?: PromiseConfigurationOptions): Promise<number> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fakeOuterNumberSerialize(body, observableOptions);
        return result.toPromise();
    }

    /**
     * Test serialization of outer string types
     * @param [body] Input string as post body
     */
    public fakeOuterStringSerializeWithHttpInfo(body?: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fakeOuterStringSerializeWithHttpInfo(body, observableOptions);
        return result.toPromise();
    }

    /**
     * Test serialization of outer string types
     * @param [body] Input string as post body
     */
    public fakeOuterStringSerialize(body?: string, _options?: PromiseConfigurationOptions): Promise<string> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fakeOuterStringSerialize(body, observableOptions);
        return result.toPromise();
    }

    /**
     * Test serialization of enum (int) properties with examples
     * @param outerObjectWithEnumProperty Input enum (int) as post body
     */
    public fakePropertyEnumIntegerSerializeWithHttpInfo(outerObjectWithEnumProperty: OuterObjectWithEnumProperty, _options?: PromiseConfigurationOptions): Promise<HttpInfo<OuterObjectWithEnumProperty>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fakePropertyEnumIntegerSerializeWithHttpInfo(outerObjectWithEnumProperty, observableOptions);
        return result.toPromise();
    }

    /**
     * Test serialization of enum (int) properties with examples
     * @param outerObjectWithEnumProperty Input enum (int) as post body
     */
    public fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty: OuterObjectWithEnumProperty, _options?: PromiseConfigurationOptions): Promise<OuterObjectWithEnumProperty> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty, observableOptions);
        return result.toPromise();
    }

    /**
     * For this test, the body has to be a binary file.
     * @param body image to upload
     */
    public testBodyWithBinaryWithHttpInfo(body: HttpFile, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testBodyWithBinaryWithHttpInfo(body, observableOptions);
        return result.toPromise();
    }

    /**
     * For this test, the body has to be a binary file.
     * @param body image to upload
     */
    public testBodyWithBinary(body: HttpFile, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testBodyWithBinary(body, observableOptions);
        return result.toPromise();
    }

    /**
     * For this test, the body for this request must reference a schema named `File`.
     * @param fileSchemaTestClass
     */
    public testBodyWithFileSchemaWithHttpInfo(fileSchemaTestClass: FileSchemaTestClass, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testBodyWithFileSchemaWithHttpInfo(fileSchemaTestClass, observableOptions);
        return result.toPromise();
    }

    /**
     * For this test, the body for this request must reference a schema named `File`.
     * @param fileSchemaTestClass
     */
    public testBodyWithFileSchema(fileSchemaTestClass: FileSchemaTestClass, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testBodyWithFileSchema(fileSchemaTestClass, observableOptions);
        return result.toPromise();
    }

    /**
     * @param query
     * @param user
     */
    public testBodyWithQueryParamsWithHttpInfo(query: string, user: User, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testBodyWithQueryParamsWithHttpInfo(query, user, observableOptions);
        return result.toPromise();
    }

    /**
     * @param query
     * @param user
     */
    public testBodyWithQueryParams(query: string, user: User, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testBodyWithQueryParams(query, user, observableOptions);
        return result.toPromise();
    }

    /**
     * To test \"client\" model
     * To test \"client\" model
     * @param client client model
     */
    public testClientModelWithHttpInfo(client: Client, _options?: PromiseConfigurationOptions): Promise<HttpInfo<Client>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testClientModelWithHttpInfo(client, observableOptions);
        return result.toPromise();
    }

    /**
     * To test \"client\" model
     * To test \"client\" model
     * @param client client model
     */
    public testClientModel(client: Client, _options?: PromiseConfigurationOptions): Promise<Client> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testClientModel(client, observableOptions);
        return result.toPromise();
    }

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * @param number None
     * @param _double None
     * @param patternWithoutDelimiter None
     * @param _byte None
     * @param [integer] None
     * @param [int32] None
     * @param [int64] None
     * @param [_float] None
     * @param [string] None
     * @param [binary] None
     * @param [date] None
     * @param [dateTime] None
     * @param [password] None
     * @param [callback] None
     */
    public testEndpointParametersWithHttpInfo(number: number, _double: number, patternWithoutDelimiter: string, _byte: string, integer?: number, int32?: number, int64?: number, _float?: number, string?: string, binary?: HttpFile, date?: string, dateTime?: Date, password?: string, callback?: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEndpointParametersWithHttpInfo(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, callback, observableOptions);
        return result.toPromise();
    }

    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * @param number None
     * @param _double None
     * @param patternWithoutDelimiter None
     * @param _byte None
     * @param [integer] None
     * @param [int32] None
     * @param [int64] None
     * @param [_float] None
     * @param [string] None
     * @param [binary] None
     * @param [date] None
     * @param [dateTime] None
     * @param [password] None
     * @param [callback] None
     */
    public testEndpointParameters(number: number, _double: number, patternWithoutDelimiter: string, _byte: string, integer?: number, int32?: number, int64?: number, _float?: number, string?: string, binary?: HttpFile, date?: string, dateTime?: Date, password?: string, callback?: string, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, callback, observableOptions);
        return result.toPromise();
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * @param [enumHeaderStringArray] Header parameter enum test (string array)
     * @param [enumHeaderString] Header parameter enum test (string)
     * @param [enumQueryStringArray] Query parameter enum test (string array)
     * @param [enumQueryString] Query parameter enum test (string)
     * @param [enumQueryInteger] Query parameter enum test (double)
     * @param [enumQueryDouble] Query parameter enum test (double)
     * @param [enumQueryModelArray]
     * @param [enumFormStringArray] Form parameter enum test (string array)
     * @param [enumFormString] Form parameter enum test (string)
     */
    public testEnumParametersWithHttpInfo(enumHeaderStringArray?: Array<'>' | '$'>, enumHeaderString?: '_abc' | '-efg' | '(xyz)', enumQueryStringArray?: Array<'>' | '$'>, enumQueryString?: '_abc' | '-efg' | '(xyz)', enumQueryInteger?: 1 | -2, enumQueryDouble?: 1.1 | -1.2, enumQueryModelArray?: Array<EnumClass>, enumFormStringArray?: Array<string>, enumFormString?: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEnumParametersWithHttpInfo(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString, observableOptions);
        return result.toPromise();
    }

    /**
     * To test enum parameters
     * To test enum parameters
     * @param [enumHeaderStringArray] Header parameter enum test (string array)
     * @param [enumHeaderString] Header parameter enum test (string)
     * @param [enumQueryStringArray] Query parameter enum test (string array)
     * @param [enumQueryString] Query parameter enum test (string)
     * @param [enumQueryInteger] Query parameter enum test (double)
     * @param [enumQueryDouble] Query parameter enum test (double)
     * @param [enumQueryModelArray]
     * @param [enumFormStringArray] Form parameter enum test (string array)
     * @param [enumFormString] Form parameter enum test (string)
     */
    public testEnumParameters(enumHeaderStringArray?: Array<'>' | '$'>, enumHeaderString?: '_abc' | '-efg' | '(xyz)', enumQueryStringArray?: Array<'>' | '$'>, enumQueryString?: '_abc' | '-efg' | '(xyz)', enumQueryInteger?: 1 | -2, enumQueryDouble?: 1.1 | -1.2, enumQueryModelArray?: Array<EnumClass>, enumFormStringArray?: Array<string>, enumFormString?: string, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString, observableOptions);
        return result.toPromise();
    }

    /**
     * Fake endpoint to test group parameters (optional)
     * Fake endpoint to test group parameters (optional)
     * @param requiredStringGroup Required String in group parameters
     * @param requiredBooleanGroup Required Boolean in group parameters
     * @param requiredInt64Group Required Integer in group parameters
     * @param [stringGroup] String in group parameters
     * @param [booleanGroup] Boolean in group parameters
     * @param [int64Group] Integer in group parameters
     */
    public testGroupParametersWithHttpInfo(requiredStringGroup: number, requiredBooleanGroup: boolean, requiredInt64Group: number, stringGroup?: number, booleanGroup?: boolean, int64Group?: number, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testGroupParametersWithHttpInfo(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group, observableOptions);
        return result.toPromise();
    }

    /**
     * Fake endpoint to test group parameters (optional)
     * Fake endpoint to test group parameters (optional)
     * @param requiredStringGroup Required String in group parameters
     * @param requiredBooleanGroup Required Boolean in group parameters
     * @param requiredInt64Group Required Integer in group parameters
     * @param [stringGroup] String in group parameters
     * @param [booleanGroup] Boolean in group parameters
     * @param [int64Group] Integer in group parameters
     */
    public testGroupParameters(requiredStringGroup: number, requiredBooleanGroup: boolean, requiredInt64Group: number, stringGroup?: number, booleanGroup?: boolean, int64Group?: number, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * test inline additionalProperties
     * @param requestBody request body
     */
    public testInlineAdditionalPropertiesWithHttpInfo(requestBody: { [key: string]: string; }, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testInlineAdditionalPropertiesWithHttpInfo(requestBody, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * test inline additionalProperties
     * @param requestBody request body
     */
    public testInlineAdditionalProperties(requestBody: { [key: string]: string; }, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testInlineAdditionalProperties(requestBody, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * test json serialization of form data
     * @param param field1
     * @param param2 field2
     */
    public testJsonFormDataWithHttpInfo(param: string, param2: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testJsonFormDataWithHttpInfo(param, param2, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * test json serialization of form data
     * @param param field1
     * @param param2 field2
     */
    public testJsonFormData(param: string, param2: string, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testJsonFormData(param, param2, observableOptions);
        return result.toPromise();
    }

    /**
     * To test the collection format in query parameters
     * @param pipe
     * @param ioutil
     * @param http
     * @param url
     * @param context
     * @param allowEmpty
     * @param [language]
     */
    public testQueryParameterCollectionFormatWithHttpInfo(pipe: Array<string>, ioutil: Array<string>, http: Array<string>, url: Array<string>, context: Array<string>, allowEmpty: string, language?: { [key: string]: string; }, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testQueryParameterCollectionFormatWithHttpInfo(pipe, ioutil, http, url, context, allowEmpty, language, observableOptions);
        return result.toPromise();
    }

    /**
     * To test the collection format in query parameters
     * @param pipe
     * @param ioutil
     * @param http
     * @param url
     * @param context
     * @param allowEmpty
     * @param [language]
     */
    public testQueryParameterCollectionFormat(pipe: Array<string>, ioutil: Array<string>, http: Array<string>, url: Array<string>, context: Array<string>, allowEmpty: string, language?: { [key: string]: string; }, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, language, observableOptions);
        return result.toPromise();
    }


}



import { ObservableFakeClassnameTags123Api } from './ObservableAPI';

import { FakeClassnameTags123ApiRequestFactory, FakeClassnameTags123ApiResponseProcessor} from "../apis/FakeClassnameTags123Api";
export class PromiseFakeClassnameTags123Api {
    private api: ObservableFakeClassnameTags123Api

    public constructor(
        configuration: Configuration,
        requestFactory?: FakeClassnameTags123ApiRequestFactory,
        responseProcessor?: FakeClassnameTags123ApiResponseProcessor
    ) {
        this.api = new ObservableFakeClassnameTags123Api(configuration, requestFactory, responseProcessor);
    }

    /**
     * To test class name in snake case
     * To test class name in snake case
     * @param client client model
     */
    public testClassnameWithHttpInfo(client: Client, _options?: PromiseConfigurationOptions): Promise<HttpInfo<Client>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testClassnameWithHttpInfo(client, observableOptions);
        return result.toPromise();
    }

    /**
     * To test class name in snake case
     * To test class name in snake case
     * @param client client model
     */
    public testClassname(client: Client, _options?: PromiseConfigurationOptions): Promise<Client> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testClassname(client, observableOptions);
        return result.toPromise();
    }


}



import { ObservablePetApi } from './ObservableAPI';

import { PetApiRequestFactory, PetApiResponseProcessor} from "../apis/PetApi";
export class PromisePetApi {
    private api: ObservablePetApi

    public constructor(
        configuration: Configuration,
        requestFactory?: PetApiRequestFactory,
        responseProcessor?: PetApiResponseProcessor
    ) {
        this.api = new ObservablePetApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * 
     * Add a new pet to the store
     * @param pet Pet object that needs to be added to the store
     */
    public addPetWithHttpInfo(pet: Pet, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.addPetWithHttpInfo(pet, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Add a new pet to the store
     * @param pet Pet object that needs to be added to the store
     */
    public addPet(pet: Pet, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.addPet(pet, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Deletes a pet
     * @param petId Pet id to delete
     * @param [apiKey]
     */
    public deletePetWithHttpInfo(petId: number, apiKey?: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.deletePetWithHttpInfo(petId, apiKey, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Deletes a pet
     * @param petId Pet id to delete
     * @param [apiKey]
     */
    public deletePet(petId: number, apiKey?: string, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.deletePet(petId, apiKey, observableOptions);
        return result.toPromise();
    }

    /**
     * Multiple status values can be provided with comma separated strings
     * Finds Pets by status
     * @param status Status values that need to be considered for filter
     */
    public findPetsByStatusWithHttpInfo(status: Array<'available' | 'pending' | 'sold'>, _options?: PromiseConfigurationOptions): Promise<HttpInfo<Array<Pet>>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.findPetsByStatusWithHttpInfo(status, observableOptions);
        return result.toPromise();
    }

    /**
     * Multiple status values can be provided with comma separated strings
     * Finds Pets by status
     * @param status Status values that need to be considered for filter
     */
    public findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, _options?: PromiseConfigurationOptions): Promise<Array<Pet>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.findPetsByStatus(status, observableOptions);
        return result.toPromise();
    }

    /**
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * Finds Pets by tags
     * @param tags Tags to filter by
     */
    public findPetsByTagsWithHttpInfo(tags: Set<string>, _options?: PromiseConfigurationOptions): Promise<HttpInfo<Set<Pet>>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.findPetsByTagsWithHttpInfo(tags, observableOptions);
        return result.toPromise();
    }

    /**
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * Finds Pets by tags
     * @param tags Tags to filter by
     */
    public findPetsByTags(tags: Set<string>, _options?: PromiseConfigurationOptions): Promise<Set<Pet>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.findPetsByTags(tags, observableOptions);
        return result.toPromise();
    }

    /**
     * Returns a single pet
     * Find pet by ID
     * @param petId ID of pet to return
     */
    public getPetByIdWithHttpInfo(petId: number, _options?: PromiseConfigurationOptions): Promise<HttpInfo<Pet>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.getPetByIdWithHttpInfo(petId, observableOptions);
        return result.toPromise();
    }

    /**
     * Returns a single pet
     * Find pet by ID
     * @param petId ID of pet to return
     */
    public getPetById(petId: number, _options?: PromiseConfigurationOptions): Promise<Pet> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.getPetById(petId, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Update an existing pet
     * @param pet Pet object that needs to be added to the store
     */
    public updatePetWithHttpInfo(pet: Pet, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.updatePetWithHttpInfo(pet, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Update an existing pet
     * @param pet Pet object that needs to be added to the store
     */
    public updatePet(pet: Pet, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.updatePet(pet, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Updates a pet in the store with form data
     * @param petId ID of pet that needs to be updated
     * @param [name] Updated name of the pet
     * @param [status] Updated status of the pet
     */
    public updatePetWithFormWithHttpInfo(petId: number, name?: string, status?: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.updatePetWithFormWithHttpInfo(petId, name, status, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Updates a pet in the store with form data
     * @param petId ID of pet that needs to be updated
     * @param [name] Updated name of the pet
     * @param [status] Updated status of the pet
     */
    public updatePetWithForm(petId: number, name?: string, status?: string, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.updatePetWithForm(petId, name, status, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * uploads an image
     * @param petId ID of pet to update
     * @param [additionalMetadata] Additional data to pass to server
     * @param [file] file to upload
     */
    public uploadFileWithHttpInfo(petId: number, additionalMetadata?: string, file?: HttpFile, _options?: PromiseConfigurationOptions): Promise<HttpInfo<ApiResponse>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.uploadFileWithHttpInfo(petId, additionalMetadata, file, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * uploads an image
     * @param petId ID of pet to update
     * @param [additionalMetadata] Additional data to pass to server
     * @param [file] file to upload
     */
    public uploadFile(petId: number, additionalMetadata?: string, file?: HttpFile, _options?: PromiseConfigurationOptions): Promise<ApiResponse> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.uploadFile(petId, additionalMetadata, file, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * uploads an image (required)
     * @param petId ID of pet to update
     * @param requiredFile file to upload
     * @param [additionalMetadata] Additional data to pass to server
     */
    public uploadFileWithRequiredFileWithHttpInfo(petId: number, requiredFile: HttpFile, additionalMetadata?: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<ApiResponse>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.uploadFileWithRequiredFileWithHttpInfo(petId, requiredFile, additionalMetadata, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * uploads an image (required)
     * @param petId ID of pet to update
     * @param requiredFile file to upload
     * @param [additionalMetadata] Additional data to pass to server
     */
    public uploadFileWithRequiredFile(petId: number, requiredFile: HttpFile, additionalMetadata?: string, _options?: PromiseConfigurationOptions): Promise<ApiResponse> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.uploadFileWithRequiredFile(petId, requiredFile, additionalMetadata, observableOptions);
        return result.toPromise();
    }


}



import { ObservableStoreApi } from './ObservableAPI';

import { StoreApiRequestFactory, StoreApiResponseProcessor} from "../apis/StoreApi";
export class PromiseStoreApi {
    private api: ObservableStoreApi

    public constructor(
        configuration: Configuration,
        requestFactory?: StoreApiRequestFactory,
        responseProcessor?: StoreApiResponseProcessor
    ) {
        this.api = new ObservableStoreApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
     * Delete purchase order by ID
     * @param orderId ID of the order that needs to be deleted
     */
    public deleteOrderWithHttpInfo(orderId: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.deleteOrderWithHttpInfo(orderId, observableOptions);
        return result.toPromise();
    }

    /**
     * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
     * Delete purchase order by ID
     * @param orderId ID of the order that needs to be deleted
     */
    public deleteOrder(orderId: string, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.deleteOrder(orderId, observableOptions);
        return result.toPromise();
    }

    /**
     * Returns a map of status codes to quantities
     * Returns pet inventories by status
     */
    public getInventoryWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<{ [key: string]: number; }>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.getInventoryWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     * Returns a map of status codes to quantities
     * Returns pet inventories by status
     */
    public getInventory(_options?: PromiseConfigurationOptions): Promise<{ [key: string]: number; }> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.getInventory(observableOptions);
        return result.toPromise();
    }

    /**
     * For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions
     * Find purchase order by ID
     * @param orderId ID of pet that needs to be fetched
     */
    public getOrderByIdWithHttpInfo(orderId: number, _options?: PromiseConfigurationOptions): Promise<HttpInfo<Order>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.getOrderByIdWithHttpInfo(orderId, observableOptions);
        return result.toPromise();
    }

    /**
     * For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions
     * Find purchase order by ID
     * @param orderId ID of pet that needs to be fetched
     */
    public getOrderById(orderId: number, _options?: PromiseConfigurationOptions): Promise<Order> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.getOrderById(orderId, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Place an order for a pet
     * @param order order placed for purchasing the pet
     */
    public placeOrderWithHttpInfo(order: Order, _options?: PromiseConfigurationOptions): Promise<HttpInfo<Order>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.placeOrderWithHttpInfo(order, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Place an order for a pet
     * @param order order placed for purchasing the pet
     */
    public placeOrder(order: Order, _options?: PromiseConfigurationOptions): Promise<Order> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.placeOrder(order, observableOptions);
        return result.toPromise();
    }


}



import { ObservableUserApi } from './ObservableAPI';

import { UserApiRequestFactory, UserApiResponseProcessor} from "../apis/UserApi";
export class PromiseUserApi {
    private api: ObservableUserApi

    public constructor(
        configuration: Configuration,
        requestFactory?: UserApiRequestFactory,
        responseProcessor?: UserApiResponseProcessor
    ) {
        this.api = new ObservableUserApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * This can only be done by the logged in user.
     * Create user
     * @param user Created user object
     */
    public createUserWithHttpInfo(user: User, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.createUserWithHttpInfo(user, observableOptions);
        return result.toPromise();
    }

    /**
     * This can only be done by the logged in user.
     * Create user
     * @param user Created user object
     */
    public createUser(user: User, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.createUser(user, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Creates list of users with given input array
     * @param user List of user object
     */
    public createUsersWithArrayInputWithHttpInfo(user: Array<User>, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.createUsersWithArrayInputWithHttpInfo(user, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Creates list of users with given input array
     * @param user List of user object
     */
    public createUsersWithArrayInput(user: Array<User>, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.createUsersWithArrayInput(user, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Creates list of users with given input array
     * @param user List of user object
     */
    public createUsersWithListInputWithHttpInfo(user: Array<User>, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.createUsersWithListInputWithHttpInfo(user, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Creates list of users with given input array
     * @param user List of user object
     */
    public createUsersWithListInput(user: Array<User>, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.createUsersWithListInput(user, observableOptions);
        return result.toPromise();
    }

    /**
     * This can only be done by the logged in user.
     * Delete user
     * @param username The name that needs to be deleted
     */
    public deleteUserWithHttpInfo(username: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.deleteUserWithHttpInfo(username, observableOptions);
        return result.toPromise();
    }

    /**
     * This can only be done by the logged in user.
     * Delete user
     * @param username The name that needs to be deleted
     */
    public deleteUser(username: string, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.deleteUser(username, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Get user by user name
     * @param username The name that needs to be fetched. Use user1 for testing.
     */
    public getUserByNameWithHttpInfo(username: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<User>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.getUserByNameWithHttpInfo(username, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Get user by user name
     * @param username The name that needs to be fetched. Use user1 for testing.
     */
    public getUserByName(username: string, _options?: PromiseConfigurationOptions): Promise<User> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.getUserByName(username, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Logs user into the system
     * @param username The user name for login
     * @param password The password for login in clear text
     */
    public loginUserWithHttpInfo(username: string, password: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.loginUserWithHttpInfo(username, password, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Logs user into the system
     * @param username The user name for login
     * @param password The password for login in clear text
     */
    public loginUser(username: string, password: string, _options?: PromiseConfigurationOptions): Promise<string> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.loginUser(username, password, observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Logs out current logged in user session
     */
    public logoutUserWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.logoutUserWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     * 
     * Logs out current logged in user session
     */
    public logoutUser(_options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.logoutUser(observableOptions);
        return result.toPromise();
    }

    /**
     * This can only be done by the logged in user.
     * Updated user
     * @param username name that need to be deleted
     * @param user Updated user object
     */
    public updateUserWithHttpInfo(username: string, user: User, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.updateUserWithHttpInfo(username, user, observableOptions);
        return result.toPromise();
    }

    /**
     * This can only be done by the logged in user.
     * Updated user
     * @param username name that need to be deleted
     * @param user Updated user object
     */
    public updateUser(username: string, user: User, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.updateUser(username, user, observableOptions);
        return result.toPromise();
    }


}



