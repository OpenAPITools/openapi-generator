import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration, ConfigurationOptions, mergeConfiguration } from '../configuration'
import type { Middleware } from '../middleware';
import { Observable, of, from } from '../rxjsStub';
import {mergeMap, map} from  '../rxjsStub';
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

import { AnotherFakeApiRequestFactory, AnotherFakeApiResponseProcessor} from "../apis/AnotherFakeApi";
export class ObservableAnotherFakeApi {
    private requestFactory: AnotherFakeApiRequestFactory;
    private responseProcessor: AnotherFakeApiResponseProcessor;
    private configuration: Configuration;

    public constructor(
        configuration: Configuration,
        requestFactory?: AnotherFakeApiRequestFactory,
        responseProcessor?: AnotherFakeApiResponseProcessor
    ) {
        this.configuration = configuration;
        this.requestFactory = requestFactory || new AnotherFakeApiRequestFactory(configuration);
        this.responseProcessor = responseProcessor || new AnotherFakeApiResponseProcessor();
    }

    /**
     * To test special tags and operation ID starting with number
     * To test special tags
     * @param client client model
     */
    public _123testSpecialTagsWithHttpInfo(client: Client, _options?: ConfigurationOptions): Observable<HttpInfo<Client>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory._123testSpecialTags(client, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor._123testSpecialTagsWithHttpInfo(rsp)));
            }));
    }

    /**
     * To test special tags and operation ID starting with number
     * To test special tags
     * @param client client model
     */
    public _123testSpecialTags(client: Client, _options?: ConfigurationOptions): Observable<Client> {
        return this._123testSpecialTagsWithHttpInfo(client, _options).pipe(map((apiResponse: HttpInfo<Client>) => apiResponse.data));
    }

}

import { DefaultApiRequestFactory, DefaultApiResponseProcessor} from "../apis/DefaultApi";
export class ObservableDefaultApi {
    private requestFactory: DefaultApiRequestFactory;
    private responseProcessor: DefaultApiResponseProcessor;
    private configuration: Configuration;

    public constructor(
        configuration: Configuration,
        requestFactory?: DefaultApiRequestFactory,
        responseProcessor?: DefaultApiResponseProcessor
    ) {
        this.configuration = configuration;
        this.requestFactory = requestFactory || new DefaultApiRequestFactory(configuration);
        this.responseProcessor = responseProcessor || new DefaultApiResponseProcessor();
    }

    /**
     */
    public fooGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.fooGet(_config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.fooGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public fooGet(_options?: ConfigurationOptions): Observable<void> {
        return this.fooGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

}

import { FakeApiRequestFactory, FakeApiResponseProcessor} from "../apis/FakeApi";
export class ObservableFakeApi {
    private requestFactory: FakeApiRequestFactory;
    private responseProcessor: FakeApiResponseProcessor;
    private configuration: Configuration;

    public constructor(
        configuration: Configuration,
        requestFactory?: FakeApiRequestFactory,
        responseProcessor?: FakeApiResponseProcessor
    ) {
        this.configuration = configuration;
        this.requestFactory = requestFactory || new FakeApiRequestFactory(configuration);
        this.responseProcessor = responseProcessor || new FakeApiResponseProcessor();
    }

    /**
     * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
     */
    public fakeBigDecimalMapWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<FakeBigDecimalMap200Response>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.fakeBigDecimalMap(_config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.fakeBigDecimalMapWithHttpInfo(rsp)));
            }));
    }

    /**
     * for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys
     */
    public fakeBigDecimalMap(_options?: ConfigurationOptions): Observable<FakeBigDecimalMap200Response> {
        return this.fakeBigDecimalMapWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<FakeBigDecimalMap200Response>) => apiResponse.data));
    }

    /**
     * Health check endpoint
     */
    public fakeHealthGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<HealthCheckResult>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.fakeHealthGet(_config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.fakeHealthGetWithHttpInfo(rsp)));
            }));
    }

    /**
     * Health check endpoint
     */
    public fakeHealthGet(_options?: ConfigurationOptions): Observable<HealthCheckResult> {
        return this.fakeHealthGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<HealthCheckResult>) => apiResponse.data));
    }

    /**
     * test http signature authentication
     * @param pet Pet object that needs to be added to the store
     * @param [query1] query parameter
     * @param [header1] header parameter
     */
    public fakeHttpSignatureTestWithHttpInfo(pet: Pet, query1?: string, header1?: string, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.fakeHttpSignatureTest(pet, query1, header1, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.fakeHttpSignatureTestWithHttpInfo(rsp)));
            }));
    }

    /**
     * test http signature authentication
     * @param pet Pet object that needs to be added to the store
     * @param [query1] query parameter
     * @param [header1] header parameter
     */
    public fakeHttpSignatureTest(pet: Pet, query1?: string, header1?: string, _options?: ConfigurationOptions): Observable<void> {
        return this.fakeHttpSignatureTestWithHttpInfo(pet, query1, header1, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * Test serialization of outer boolean types
     * @param [body] Input boolean as post body
     */
    public fakeOuterBooleanSerializeWithHttpInfo(body?: boolean, _options?: ConfigurationOptions): Observable<HttpInfo<boolean>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.fakeOuterBooleanSerialize(body, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.fakeOuterBooleanSerializeWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test serialization of outer boolean types
     * @param [body] Input boolean as post body
     */
    public fakeOuterBooleanSerialize(body?: boolean, _options?: ConfigurationOptions): Observable<boolean> {
        return this.fakeOuterBooleanSerializeWithHttpInfo(body, _options).pipe(map((apiResponse: HttpInfo<boolean>) => apiResponse.data));
    }

    /**
     * Test serialization of object with outer number type
     * @param [outerComposite] Input composite as post body
     */
    public fakeOuterCompositeSerializeWithHttpInfo(outerComposite?: OuterComposite, _options?: ConfigurationOptions): Observable<HttpInfo<OuterComposite>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.fakeOuterCompositeSerialize(outerComposite, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.fakeOuterCompositeSerializeWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test serialization of object with outer number type
     * @param [outerComposite] Input composite as post body
     */
    public fakeOuterCompositeSerialize(outerComposite?: OuterComposite, _options?: ConfigurationOptions): Observable<OuterComposite> {
        return this.fakeOuterCompositeSerializeWithHttpInfo(outerComposite, _options).pipe(map((apiResponse: HttpInfo<OuterComposite>) => apiResponse.data));
    }

    /**
     * Test serialization of outer number types
     * @param [body] Input number as post body
     */
    public fakeOuterNumberSerializeWithHttpInfo(body?: number, _options?: ConfigurationOptions): Observable<HttpInfo<number>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.fakeOuterNumberSerialize(body, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.fakeOuterNumberSerializeWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test serialization of outer number types
     * @param [body] Input number as post body
     */
    public fakeOuterNumberSerialize(body?: number, _options?: ConfigurationOptions): Observable<number> {
        return this.fakeOuterNumberSerializeWithHttpInfo(body, _options).pipe(map((apiResponse: HttpInfo<number>) => apiResponse.data));
    }

    /**
     * Test serialization of outer string types
     * @param [body] Input string as post body
     */
    public fakeOuterStringSerializeWithHttpInfo(body?: string, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.fakeOuterStringSerialize(body, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.fakeOuterStringSerializeWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test serialization of outer string types
     * @param [body] Input string as post body
     */
    public fakeOuterStringSerialize(body?: string, _options?: ConfigurationOptions): Observable<string> {
        return this.fakeOuterStringSerializeWithHttpInfo(body, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test serialization of enum (int) properties with examples
     * @param outerObjectWithEnumProperty Input enum (int) as post body
     */
    public fakePropertyEnumIntegerSerializeWithHttpInfo(outerObjectWithEnumProperty: OuterObjectWithEnumProperty, _options?: ConfigurationOptions): Observable<HttpInfo<OuterObjectWithEnumProperty>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.fakePropertyEnumIntegerSerializeWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test serialization of enum (int) properties with examples
     * @param outerObjectWithEnumProperty Input enum (int) as post body
     */
    public fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty: OuterObjectWithEnumProperty, _options?: ConfigurationOptions): Observable<OuterObjectWithEnumProperty> {
        return this.fakePropertyEnumIntegerSerializeWithHttpInfo(outerObjectWithEnumProperty, _options).pipe(map((apiResponse: HttpInfo<OuterObjectWithEnumProperty>) => apiResponse.data));
    }

    /**
     * For this test, the body has to be a binary file.
     * @param body image to upload
     */
    public testBodyWithBinaryWithHttpInfo(body: HttpFile, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testBodyWithBinary(body, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testBodyWithBinaryWithHttpInfo(rsp)));
            }));
    }

    /**
     * For this test, the body has to be a binary file.
     * @param body image to upload
     */
    public testBodyWithBinary(body: HttpFile, _options?: ConfigurationOptions): Observable<void> {
        return this.testBodyWithBinaryWithHttpInfo(body, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * For this test, the body for this request must reference a schema named `File`.
     * @param fileSchemaTestClass
     */
    public testBodyWithFileSchemaWithHttpInfo(fileSchemaTestClass: FileSchemaTestClass, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testBodyWithFileSchema(fileSchemaTestClass, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testBodyWithFileSchemaWithHttpInfo(rsp)));
            }));
    }

    /**
     * For this test, the body for this request must reference a schema named `File`.
     * @param fileSchemaTestClass
     */
    public testBodyWithFileSchema(fileSchemaTestClass: FileSchemaTestClass, _options?: ConfigurationOptions): Observable<void> {
        return this.testBodyWithFileSchemaWithHttpInfo(fileSchemaTestClass, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * @param query
     * @param user
     */
    public testBodyWithQueryParamsWithHttpInfo(query: string, user: User, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testBodyWithQueryParams(query, user, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testBodyWithQueryParamsWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param query
     * @param user
     */
    public testBodyWithQueryParams(query: string, user: User, _options?: ConfigurationOptions): Observable<void> {
        return this.testBodyWithQueryParamsWithHttpInfo(query, user, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * To test \"client\" model
     * To test \"client\" model
     * @param client client model
     */
    public testClientModelWithHttpInfo(client: Client, _options?: ConfigurationOptions): Observable<HttpInfo<Client>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testClientModel(client, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testClientModelWithHttpInfo(rsp)));
            }));
    }

    /**
     * To test \"client\" model
     * To test \"client\" model
     * @param client client model
     */
    public testClientModel(client: Client, _options?: ConfigurationOptions): Observable<Client> {
        return this.testClientModelWithHttpInfo(client, _options).pipe(map((apiResponse: HttpInfo<Client>) => apiResponse.data));
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
    public testEndpointParametersWithHttpInfo(number: number, _double: number, patternWithoutDelimiter: string, _byte: string, integer?: number, int32?: number, int64?: number, _float?: number, string?: string, binary?: HttpFile, date?: string, dateTime?: Date, password?: string, callback?: string, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, callback, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEndpointParametersWithHttpInfo(rsp)));
            }));
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
    public testEndpointParameters(number: number, _double: number, patternWithoutDelimiter: string, _byte: string, integer?: number, int32?: number, int64?: number, _float?: number, string?: string, binary?: HttpFile, date?: string, dateTime?: Date, password?: string, callback?: string, _options?: ConfigurationOptions): Observable<void> {
        return this.testEndpointParametersWithHttpInfo(number, _double, patternWithoutDelimiter, _byte, integer, int32, int64, _float, string, binary, date, dateTime, password, callback, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
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
    public testEnumParametersWithHttpInfo(enumHeaderStringArray?: Array<'>' | '$'>, enumHeaderString?: '_abc' | '-efg' | '(xyz)', enumQueryStringArray?: Array<'>' | '$'>, enumQueryString?: '_abc' | '-efg' | '(xyz)', enumQueryInteger?: 1 | -2, enumQueryDouble?: 1.1 | -1.2, enumQueryModelArray?: Array<EnumClass>, enumFormStringArray?: Array<string>, enumFormString?: string, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEnumParametersWithHttpInfo(rsp)));
            }));
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
    public testEnumParameters(enumHeaderStringArray?: Array<'>' | '$'>, enumHeaderString?: '_abc' | '-efg' | '(xyz)', enumQueryStringArray?: Array<'>' | '$'>, enumQueryString?: '_abc' | '-efg' | '(xyz)', enumQueryInteger?: 1 | -2, enumQueryDouble?: 1.1 | -1.2, enumQueryModelArray?: Array<EnumClass>, enumFormStringArray?: Array<string>, enumFormString?: string, _options?: ConfigurationOptions): Observable<void> {
        return this.testEnumParametersWithHttpInfo(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
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
    public testGroupParametersWithHttpInfo(requiredStringGroup: number, requiredBooleanGroup: boolean, requiredInt64Group: number, stringGroup?: number, booleanGroup?: boolean, int64Group?: number, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testGroupParametersWithHttpInfo(rsp)));
            }));
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
    public testGroupParameters(requiredStringGroup: number, requiredBooleanGroup: boolean, requiredInt64Group: number, stringGroup?: number, booleanGroup?: boolean, int64Group?: number, _options?: ConfigurationOptions): Observable<void> {
        return this.testGroupParametersWithHttpInfo(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * 
     * test inline additionalProperties
     * @param requestBody request body
     */
    public testInlineAdditionalPropertiesWithHttpInfo(requestBody: { [key: string]: string; }, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testInlineAdditionalProperties(requestBody, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testInlineAdditionalPropertiesWithHttpInfo(rsp)));
            }));
    }

    /**
     * 
     * test inline additionalProperties
     * @param requestBody request body
     */
    public testInlineAdditionalProperties(requestBody: { [key: string]: string; }, _options?: ConfigurationOptions): Observable<void> {
        return this.testInlineAdditionalPropertiesWithHttpInfo(requestBody, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * 
     * test json serialization of form data
     * @param param field1
     * @param param2 field2
     */
    public testJsonFormDataWithHttpInfo(param: string, param2: string, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testJsonFormData(param, param2, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testJsonFormDataWithHttpInfo(rsp)));
            }));
    }

    /**
     * 
     * test json serialization of form data
     * @param param field1
     * @param param2 field2
     */
    public testJsonFormData(param: string, param2: string, _options?: ConfigurationOptions): Observable<void> {
        return this.testJsonFormDataWithHttpInfo(param, param2, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
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
    public testQueryParameterCollectionFormatWithHttpInfo(pipe: Array<string>, ioutil: Array<string>, http: Array<string>, url: Array<string>, context: Array<string>, allowEmpty: string, language?: { [key: string]: string; }, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, language, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testQueryParameterCollectionFormatWithHttpInfo(rsp)));
            }));
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
    public testQueryParameterCollectionFormat(pipe: Array<string>, ioutil: Array<string>, http: Array<string>, url: Array<string>, context: Array<string>, allowEmpty: string, language?: { [key: string]: string; }, _options?: ConfigurationOptions): Observable<void> {
        return this.testQueryParameterCollectionFormatWithHttpInfo(pipe, ioutil, http, url, context, allowEmpty, language, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

}

import { FakeClassnameTags123ApiRequestFactory, FakeClassnameTags123ApiResponseProcessor} from "../apis/FakeClassnameTags123Api";
export class ObservableFakeClassnameTags123Api {
    private requestFactory: FakeClassnameTags123ApiRequestFactory;
    private responseProcessor: FakeClassnameTags123ApiResponseProcessor;
    private configuration: Configuration;

    public constructor(
        configuration: Configuration,
        requestFactory?: FakeClassnameTags123ApiRequestFactory,
        responseProcessor?: FakeClassnameTags123ApiResponseProcessor
    ) {
        this.configuration = configuration;
        this.requestFactory = requestFactory || new FakeClassnameTags123ApiRequestFactory(configuration);
        this.responseProcessor = responseProcessor || new FakeClassnameTags123ApiResponseProcessor();
    }

    /**
     * To test class name in snake case
     * To test class name in snake case
     * @param client client model
     */
    public testClassnameWithHttpInfo(client: Client, _options?: ConfigurationOptions): Observable<HttpInfo<Client>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testClassname(client, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testClassnameWithHttpInfo(rsp)));
            }));
    }

    /**
     * To test class name in snake case
     * To test class name in snake case
     * @param client client model
     */
    public testClassname(client: Client, _options?: ConfigurationOptions): Observable<Client> {
        return this.testClassnameWithHttpInfo(client, _options).pipe(map((apiResponse: HttpInfo<Client>) => apiResponse.data));
    }

}

import { PetApiRequestFactory, PetApiResponseProcessor} from "../apis/PetApi";
export class ObservablePetApi {
    private requestFactory: PetApiRequestFactory;
    private responseProcessor: PetApiResponseProcessor;
    private configuration: Configuration;

    public constructor(
        configuration: Configuration,
        requestFactory?: PetApiRequestFactory,
        responseProcessor?: PetApiResponseProcessor
    ) {
        this.configuration = configuration;
        this.requestFactory = requestFactory || new PetApiRequestFactory(configuration);
        this.responseProcessor = responseProcessor || new PetApiResponseProcessor();
    }

    /**
     * 
     * Add a new pet to the store
     * @param pet Pet object that needs to be added to the store
     */
    public addPetWithHttpInfo(pet: Pet, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.addPet(pet, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.addPetWithHttpInfo(rsp)));
            }));
    }

    /**
     * 
     * Add a new pet to the store
     * @param pet Pet object that needs to be added to the store
     */
    public addPet(pet: Pet, _options?: ConfigurationOptions): Observable<void> {
        return this.addPetWithHttpInfo(pet, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * 
     * Deletes a pet
     * @param petId Pet id to delete
     * @param [apiKey]
     */
    public deletePetWithHttpInfo(petId: number, apiKey?: string, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.deletePet(petId, apiKey, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.deletePetWithHttpInfo(rsp)));
            }));
    }

    /**
     * 
     * Deletes a pet
     * @param petId Pet id to delete
     * @param [apiKey]
     */
    public deletePet(petId: number, apiKey?: string, _options?: ConfigurationOptions): Observable<void> {
        return this.deletePetWithHttpInfo(petId, apiKey, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * Multiple status values can be provided with comma separated strings
     * Finds Pets by status
     * @param status Status values that need to be considered for filter
     */
    public findPetsByStatusWithHttpInfo(status: Array<'available' | 'pending' | 'sold'>, _options?: ConfigurationOptions): Observable<HttpInfo<Array<Pet>>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.findPetsByStatus(status, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.findPetsByStatusWithHttpInfo(rsp)));
            }));
    }

    /**
     * Multiple status values can be provided with comma separated strings
     * Finds Pets by status
     * @param status Status values that need to be considered for filter
     */
    public findPetsByStatus(status: Array<'available' | 'pending' | 'sold'>, _options?: ConfigurationOptions): Observable<Array<Pet>> {
        return this.findPetsByStatusWithHttpInfo(status, _options).pipe(map((apiResponse: HttpInfo<Array<Pet>>) => apiResponse.data));
    }

    /**
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * Finds Pets by tags
     * @param tags Tags to filter by
     */
    public findPetsByTagsWithHttpInfo(tags: Set<string>, _options?: ConfigurationOptions): Observable<HttpInfo<Set<Pet>>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.findPetsByTags(tags, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.findPetsByTagsWithHttpInfo(rsp)));
            }));
    }

    /**
     * Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
     * Finds Pets by tags
     * @param tags Tags to filter by
     */
    public findPetsByTags(tags: Set<string>, _options?: ConfigurationOptions): Observable<Set<Pet>> {
        return this.findPetsByTagsWithHttpInfo(tags, _options).pipe(map((apiResponse: HttpInfo<Set<Pet>>) => apiResponse.data));
    }

    /**
     * Returns a single pet
     * Find pet by ID
     * @param petId ID of pet to return
     */
    public getPetByIdWithHttpInfo(petId: number, _options?: ConfigurationOptions): Observable<HttpInfo<Pet>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.getPetById(petId, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.getPetByIdWithHttpInfo(rsp)));
            }));
    }

    /**
     * Returns a single pet
     * Find pet by ID
     * @param petId ID of pet to return
     */
    public getPetById(petId: number, _options?: ConfigurationOptions): Observable<Pet> {
        return this.getPetByIdWithHttpInfo(petId, _options).pipe(map((apiResponse: HttpInfo<Pet>) => apiResponse.data));
    }

    /**
     * 
     * Update an existing pet
     * @param pet Pet object that needs to be added to the store
     */
    public updatePetWithHttpInfo(pet: Pet, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.updatePet(pet, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.updatePetWithHttpInfo(rsp)));
            }));
    }

    /**
     * 
     * Update an existing pet
     * @param pet Pet object that needs to be added to the store
     */
    public updatePet(pet: Pet, _options?: ConfigurationOptions): Observable<void> {
        return this.updatePetWithHttpInfo(pet, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * 
     * Updates a pet in the store with form data
     * @param petId ID of pet that needs to be updated
     * @param [name] Updated name of the pet
     * @param [status] Updated status of the pet
     */
    public updatePetWithFormWithHttpInfo(petId: number, name?: string, status?: string, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.updatePetWithForm(petId, name, status, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.updatePetWithFormWithHttpInfo(rsp)));
            }));
    }

    /**
     * 
     * Updates a pet in the store with form data
     * @param petId ID of pet that needs to be updated
     * @param [name] Updated name of the pet
     * @param [status] Updated status of the pet
     */
    public updatePetWithForm(petId: number, name?: string, status?: string, _options?: ConfigurationOptions): Observable<void> {
        return this.updatePetWithFormWithHttpInfo(petId, name, status, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * 
     * uploads an image
     * @param petId ID of pet to update
     * @param [additionalMetadata] Additional data to pass to server
     * @param [file] file to upload
     */
    public uploadFileWithHttpInfo(petId: number, additionalMetadata?: string, file?: HttpFile, _options?: ConfigurationOptions): Observable<HttpInfo<ApiResponse>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.uploadFile(petId, additionalMetadata, file, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.uploadFileWithHttpInfo(rsp)));
            }));
    }

    /**
     * 
     * uploads an image
     * @param petId ID of pet to update
     * @param [additionalMetadata] Additional data to pass to server
     * @param [file] file to upload
     */
    public uploadFile(petId: number, additionalMetadata?: string, file?: HttpFile, _options?: ConfigurationOptions): Observable<ApiResponse> {
        return this.uploadFileWithHttpInfo(petId, additionalMetadata, file, _options).pipe(map((apiResponse: HttpInfo<ApiResponse>) => apiResponse.data));
    }

    /**
     * 
     * uploads an image (required)
     * @param petId ID of pet to update
     * @param requiredFile file to upload
     * @param [additionalMetadata] Additional data to pass to server
     */
    public uploadFileWithRequiredFileWithHttpInfo(petId: number, requiredFile: HttpFile, additionalMetadata?: string, _options?: ConfigurationOptions): Observable<HttpInfo<ApiResponse>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.uploadFileWithRequiredFile(petId, requiredFile, additionalMetadata, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.uploadFileWithRequiredFileWithHttpInfo(rsp)));
            }));
    }

    /**
     * 
     * uploads an image (required)
     * @param petId ID of pet to update
     * @param requiredFile file to upload
     * @param [additionalMetadata] Additional data to pass to server
     */
    public uploadFileWithRequiredFile(petId: number, requiredFile: HttpFile, additionalMetadata?: string, _options?: ConfigurationOptions): Observable<ApiResponse> {
        return this.uploadFileWithRequiredFileWithHttpInfo(petId, requiredFile, additionalMetadata, _options).pipe(map((apiResponse: HttpInfo<ApiResponse>) => apiResponse.data));
    }

}

import { StoreApiRequestFactory, StoreApiResponseProcessor} from "../apis/StoreApi";
export class ObservableStoreApi {
    private requestFactory: StoreApiRequestFactory;
    private responseProcessor: StoreApiResponseProcessor;
    private configuration: Configuration;

    public constructor(
        configuration: Configuration,
        requestFactory?: StoreApiRequestFactory,
        responseProcessor?: StoreApiResponseProcessor
    ) {
        this.configuration = configuration;
        this.requestFactory = requestFactory || new StoreApiRequestFactory(configuration);
        this.responseProcessor = responseProcessor || new StoreApiResponseProcessor();
    }

    /**
     * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
     * Delete purchase order by ID
     * @param orderId ID of the order that needs to be deleted
     */
    public deleteOrderWithHttpInfo(orderId: string, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.deleteOrder(orderId, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.deleteOrderWithHttpInfo(rsp)));
            }));
    }

    /**
     * For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
     * Delete purchase order by ID
     * @param orderId ID of the order that needs to be deleted
     */
    public deleteOrder(orderId: string, _options?: ConfigurationOptions): Observable<void> {
        return this.deleteOrderWithHttpInfo(orderId, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * Returns a map of status codes to quantities
     * Returns pet inventories by status
     */
    public getInventoryWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<{ [key: string]: number; }>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.getInventory(_config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.getInventoryWithHttpInfo(rsp)));
            }));
    }

    /**
     * Returns a map of status codes to quantities
     * Returns pet inventories by status
     */
    public getInventory(_options?: ConfigurationOptions): Observable<{ [key: string]: number; }> {
        return this.getInventoryWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<{ [key: string]: number; }>) => apiResponse.data));
    }

    /**
     * For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions
     * Find purchase order by ID
     * @param orderId ID of pet that needs to be fetched
     */
    public getOrderByIdWithHttpInfo(orderId: number, _options?: ConfigurationOptions): Observable<HttpInfo<Order>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.getOrderById(orderId, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.getOrderByIdWithHttpInfo(rsp)));
            }));
    }

    /**
     * For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions
     * Find purchase order by ID
     * @param orderId ID of pet that needs to be fetched
     */
    public getOrderById(orderId: number, _options?: ConfigurationOptions): Observable<Order> {
        return this.getOrderByIdWithHttpInfo(orderId, _options).pipe(map((apiResponse: HttpInfo<Order>) => apiResponse.data));
    }

    /**
     * 
     * Place an order for a pet
     * @param order order placed for purchasing the pet
     */
    public placeOrderWithHttpInfo(order: Order, _options?: ConfigurationOptions): Observable<HttpInfo<Order>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.placeOrder(order, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.placeOrderWithHttpInfo(rsp)));
            }));
    }

    /**
     * 
     * Place an order for a pet
     * @param order order placed for purchasing the pet
     */
    public placeOrder(order: Order, _options?: ConfigurationOptions): Observable<Order> {
        return this.placeOrderWithHttpInfo(order, _options).pipe(map((apiResponse: HttpInfo<Order>) => apiResponse.data));
    }

}

import { UserApiRequestFactory, UserApiResponseProcessor} from "../apis/UserApi";
export class ObservableUserApi {
    private requestFactory: UserApiRequestFactory;
    private responseProcessor: UserApiResponseProcessor;
    private configuration: Configuration;

    public constructor(
        configuration: Configuration,
        requestFactory?: UserApiRequestFactory,
        responseProcessor?: UserApiResponseProcessor
    ) {
        this.configuration = configuration;
        this.requestFactory = requestFactory || new UserApiRequestFactory(configuration);
        this.responseProcessor = responseProcessor || new UserApiResponseProcessor();
    }

    /**
     * This can only be done by the logged in user.
     * Create user
     * @param user Created user object
     */
    public createUserWithHttpInfo(user: User, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.createUser(user, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.createUserWithHttpInfo(rsp)));
            }));
    }

    /**
     * This can only be done by the logged in user.
     * Create user
     * @param user Created user object
     */
    public createUser(user: User, _options?: ConfigurationOptions): Observable<void> {
        return this.createUserWithHttpInfo(user, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * 
     * Creates list of users with given input array
     * @param user List of user object
     */
    public createUsersWithArrayInputWithHttpInfo(user: Array<User>, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.createUsersWithArrayInput(user, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.createUsersWithArrayInputWithHttpInfo(rsp)));
            }));
    }

    /**
     * 
     * Creates list of users with given input array
     * @param user List of user object
     */
    public createUsersWithArrayInput(user: Array<User>, _options?: ConfigurationOptions): Observable<void> {
        return this.createUsersWithArrayInputWithHttpInfo(user, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * 
     * Creates list of users with given input array
     * @param user List of user object
     */
    public createUsersWithListInputWithHttpInfo(user: Array<User>, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.createUsersWithListInput(user, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.createUsersWithListInputWithHttpInfo(rsp)));
            }));
    }

    /**
     * 
     * Creates list of users with given input array
     * @param user List of user object
     */
    public createUsersWithListInput(user: Array<User>, _options?: ConfigurationOptions): Observable<void> {
        return this.createUsersWithListInputWithHttpInfo(user, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * This can only be done by the logged in user.
     * Delete user
     * @param username The name that needs to be deleted
     */
    public deleteUserWithHttpInfo(username: string, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.deleteUser(username, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.deleteUserWithHttpInfo(rsp)));
            }));
    }

    /**
     * This can only be done by the logged in user.
     * Delete user
     * @param username The name that needs to be deleted
     */
    public deleteUser(username: string, _options?: ConfigurationOptions): Observable<void> {
        return this.deleteUserWithHttpInfo(username, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * 
     * Get user by user name
     * @param username The name that needs to be fetched. Use user1 for testing.
     */
    public getUserByNameWithHttpInfo(username: string, _options?: ConfigurationOptions): Observable<HttpInfo<User>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.getUserByName(username, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.getUserByNameWithHttpInfo(rsp)));
            }));
    }

    /**
     * 
     * Get user by user name
     * @param username The name that needs to be fetched. Use user1 for testing.
     */
    public getUserByName(username: string, _options?: ConfigurationOptions): Observable<User> {
        return this.getUserByNameWithHttpInfo(username, _options).pipe(map((apiResponse: HttpInfo<User>) => apiResponse.data));
    }

    /**
     * 
     * Logs user into the system
     * @param username The user name for login
     * @param password The password for login in clear text
     */
    public loginUserWithHttpInfo(username: string, password: string, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.loginUser(username, password, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.loginUserWithHttpInfo(rsp)));
            }));
    }

    /**
     * 
     * Logs user into the system
     * @param username The user name for login
     * @param password The password for login in clear text
     */
    public loginUser(username: string, password: string, _options?: ConfigurationOptions): Observable<string> {
        return this.loginUserWithHttpInfo(username, password, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * 
     * Logs out current logged in user session
     */
    public logoutUserWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.logoutUser(_config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.logoutUserWithHttpInfo(rsp)));
            }));
    }

    /**
     * 
     * Logs out current logged in user session
     */
    public logoutUser(_options?: ConfigurationOptions): Observable<void> {
        return this.logoutUserWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * This can only be done by the logged in user.
     * Updated user
     * @param username name that need to be deleted
     * @param user Updated user object
     */
    public updateUserWithHttpInfo(username: string, user: User, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.updateUser(username, user, _config);
        // build promise chain
        let middlewarePreObservable = from<RequestContext>(requestContextPromise);
        for (const middleware of _config.middleware) {
            middlewarePreObservable = middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => middleware.pre(ctx)));
        }

        return middlewarePreObservable.pipe(mergeMap((ctx: RequestContext) => _config.httpApi.send(ctx))).
            pipe(mergeMap((response: ResponseContext) => {
                let middlewarePostObservable = of(response);
                for (const middleware of _config.middleware.reverse()) {
                    middlewarePostObservable = middlewarePostObservable.pipe(mergeMap((rsp: ResponseContext) => middleware.post(rsp)));
                }
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.updateUserWithHttpInfo(rsp)));
            }));
    }

    /**
     * This can only be done by the logged in user.
     * Updated user
     * @param username name that need to be deleted
     * @param user Updated user object
     */
    public updateUser(username: string, user: User, _options?: ConfigurationOptions): Observable<void> {
        return this.updateUserWithHttpInfo(username, user, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

}
