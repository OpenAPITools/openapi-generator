import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration, ConfigurationOptions, mergeConfiguration } from '../configuration'
import type { Middleware } from '../middleware';
import { Observable, of, from } from '../rxjsStub';
import {mergeMap, map} from  '../rxjsStub';
import { ComplexObject } from '../models/ComplexObject';
import { CompositeObject } from '../models/CompositeObject';

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
    public testDecodeArrayOfArraysGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<Array<Array<string>>>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testDecodeArrayOfArraysGet(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testDecodeArrayOfArraysGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public testDecodeArrayOfArraysGet(_options?: ConfigurationOptions): Observable<Array<Array<string>>> {
        return this.testDecodeArrayOfArraysGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<Array<Array<string>>>) => apiResponse.data));
    }

    /**
     */
    public testDecodeArrayOfGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<Array<string>>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testDecodeArrayOfGet(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testDecodeArrayOfGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public testDecodeArrayOfGet(_options?: ConfigurationOptions): Observable<Array<string>> {
        return this.testDecodeArrayOfGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<Array<string>>) => apiResponse.data));
    }

    /**
     */
    public testDecodeArrayOfMapsOfObjectsGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<Array<{ [key: string]: ComplexObject; }>>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testDecodeArrayOfMapsOfObjectsGet(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testDecodeArrayOfMapsOfObjectsGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public testDecodeArrayOfMapsOfObjectsGet(_options?: ConfigurationOptions): Observable<Array<{ [key: string]: ComplexObject; }>> {
        return this.testDecodeArrayOfMapsOfObjectsGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<Array<{ [key: string]: ComplexObject; }>>) => apiResponse.data));
    }

    /**
     */
    public testDecodeArrayOfNullableGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<Array<string | null>>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testDecodeArrayOfNullableGet(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testDecodeArrayOfNullableGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public testDecodeArrayOfNullableGet(_options?: ConfigurationOptions): Observable<Array<string | null>> {
        return this.testDecodeArrayOfNullableGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<Array<string | null>>) => apiResponse.data));
    }

    /**
     */
    public testDecodeArrayOfNullableObjectsGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<Array<ComplexObject>>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testDecodeArrayOfNullableObjectsGet(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testDecodeArrayOfNullableObjectsGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public testDecodeArrayOfNullableObjectsGet(_options?: ConfigurationOptions): Observable<Array<ComplexObject>> {
        return this.testDecodeArrayOfNullableObjectsGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<Array<ComplexObject>>) => apiResponse.data));
    }

    /**
     */
    public testDecodeCompositeObjectsGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<CompositeObject>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testDecodeCompositeObjectsGet(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testDecodeCompositeObjectsGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public testDecodeCompositeObjectsGet(_options?: ConfigurationOptions): Observable<CompositeObject> {
        return this.testDecodeCompositeObjectsGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<CompositeObject>) => apiResponse.data));
    }

    /**
     */
    public testDecodeMapOfMapsOfObjectsGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<{ [key: string]: { [key: string]: ComplexObject; }; }>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testDecodeMapOfMapsOfObjectsGet(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testDecodeMapOfMapsOfObjectsGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public testDecodeMapOfMapsOfObjectsGet(_options?: ConfigurationOptions): Observable<{ [key: string]: { [key: string]: ComplexObject; }; }> {
        return this.testDecodeMapOfMapsOfObjectsGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<{ [key: string]: { [key: string]: ComplexObject; }; }>) => apiResponse.data));
    }

    /**
     */
    public testDecodeMapOfObjectsGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<{ [key: string]: ComplexObject | null; }>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testDecodeMapOfObjectsGet(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testDecodeMapOfObjectsGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public testDecodeMapOfObjectsGet(_options?: ConfigurationOptions): Observable<{ [key: string]: ComplexObject | null; }> {
        return this.testDecodeMapOfObjectsGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<{ [key: string]: ComplexObject | null; }>) => apiResponse.data));
    }

    /**
     */
    public testDecodeMapOfPrimitiveGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<{ [key: string]: string; }>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testDecodeMapOfPrimitiveGet(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testDecodeMapOfPrimitiveGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public testDecodeMapOfPrimitiveGet(_options?: ConfigurationOptions): Observable<{ [key: string]: string; }> {
        return this.testDecodeMapOfPrimitiveGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<{ [key: string]: string; }>) => apiResponse.data));
    }

    /**
     */
    public testDecodeNullableArrayGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<Array<string>>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testDecodeNullableArrayGet(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testDecodeNullableArrayGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public testDecodeNullableArrayGet(_options?: ConfigurationOptions): Observable<Array<string>> {
        return this.testDecodeNullableArrayGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<Array<string>>) => apiResponse.data));
    }

    /**
     */
    public testDecodeNullableGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testDecodeNullableGet(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testDecodeNullableGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public testDecodeNullableGet(_options?: ConfigurationOptions): Observable<string> {
        return this.testDecodeNullableGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     */
    public testDecodeObjectGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<ComplexObject>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testDecodeObjectGet(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testDecodeObjectGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public testDecodeObjectGet(_options?: ConfigurationOptions): Observable<ComplexObject> {
        return this.testDecodeObjectGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<ComplexObject>) => apiResponse.data));
    }

    /**
     */
    public testDecodePrimitiveBooleanGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<boolean>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testDecodePrimitiveBooleanGet(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testDecodePrimitiveBooleanGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public testDecodePrimitiveBooleanGet(_options?: ConfigurationOptions): Observable<boolean> {
        return this.testDecodePrimitiveBooleanGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<boolean>) => apiResponse.data));
    }

    /**
     */
    public testDecodePrimitiveIntegerGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<number>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testDecodePrimitiveIntegerGet(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testDecodePrimitiveIntegerGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public testDecodePrimitiveIntegerGet(_options?: ConfigurationOptions): Observable<number> {
        return this.testDecodePrimitiveIntegerGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<number>) => apiResponse.data));
    }

    /**
     */
    public testDecodePrimitiveNumberGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<number>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testDecodePrimitiveNumberGet(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testDecodePrimitiveNumberGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public testDecodePrimitiveNumberGet(_options?: ConfigurationOptions): Observable<number> {
        return this.testDecodePrimitiveNumberGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<number>) => apiResponse.data));
    }

    /**
     */
    public testDecodePrimitiveStringGetWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testDecodePrimitiveStringGet(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testDecodePrimitiveStringGetWithHttpInfo(rsp)));
            }));
    }

    /**
     */
    public testDecodePrimitiveStringGet(_options?: ConfigurationOptions): Observable<string> {
        return this.testDecodePrimitiveStringGetWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfArraysPostWithHttpInfo(requestBody: Array<Array<string>>, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEncodeArrayOfArraysPost(requestBody, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEncodeArrayOfArraysPostWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfArraysPost(requestBody: Array<Array<string>>, _options?: ConfigurationOptions): Observable<void> {
        return this.testEncodeArrayOfArraysPostWithHttpInfo(requestBody, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * @param complexObject
     */
    public testEncodeArrayOfMapsOfObjectsPostWithHttpInfo(complexObject: Array<{ [key: string]: ComplexObject; }>, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEncodeArrayOfMapsOfObjectsPost(complexObject, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEncodeArrayOfMapsOfObjectsPostWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param complexObject
     */
    public testEncodeArrayOfMapsOfObjectsPost(complexObject: Array<{ [key: string]: ComplexObject; }>, _options?: ConfigurationOptions): Observable<void> {
        return this.testEncodeArrayOfMapsOfObjectsPostWithHttpInfo(complexObject, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * @param complexObject
     */
    public testEncodeArrayOfNullableObjectsPostWithHttpInfo(complexObject: Array<ComplexObject>, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEncodeArrayOfNullableObjectsPost(complexObject, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEncodeArrayOfNullableObjectsPostWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param complexObject
     */
    public testEncodeArrayOfNullableObjectsPost(complexObject: Array<ComplexObject>, _options?: ConfigurationOptions): Observable<void> {
        return this.testEncodeArrayOfNullableObjectsPostWithHttpInfo(complexObject, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfNullablePostWithHttpInfo(requestBody: Array<string | null>, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEncodeArrayOfNullablePost(requestBody, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEncodeArrayOfNullablePostWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfNullablePost(requestBody: Array<string | null>, _options?: ConfigurationOptions): Observable<void> {
        return this.testEncodeArrayOfNullablePostWithHttpInfo(requestBody, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfPostWithHttpInfo(requestBody: Array<string>, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEncodeArrayOfPost(requestBody, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEncodeArrayOfPostWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfPost(requestBody: Array<string>, _options?: ConfigurationOptions): Observable<void> {
        return this.testEncodeArrayOfPostWithHttpInfo(requestBody, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * @param compositeObject
     */
    public testEncodeCompositeObjectsPostWithHttpInfo(compositeObject: CompositeObject, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEncodeCompositeObjectsPost(compositeObject, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEncodeCompositeObjectsPostWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param compositeObject
     */
    public testEncodeCompositeObjectsPost(compositeObject: CompositeObject, _options?: ConfigurationOptions): Observable<void> {
        return this.testEncodeCompositeObjectsPostWithHttpInfo(compositeObject, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfMapsOfObjectsPostWithHttpInfo(requestBody: { [key: string]: { [key: string]: ComplexObject; }; }, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEncodeMapOfMapsOfObjectsPost(requestBody, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEncodeMapOfMapsOfObjectsPostWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfMapsOfObjectsPost(requestBody: { [key: string]: { [key: string]: ComplexObject; }; }, _options?: ConfigurationOptions): Observable<void> {
        return this.testEncodeMapOfMapsOfObjectsPostWithHttpInfo(requestBody, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfObjectsPostWithHttpInfo(requestBody: { [key: string]: ComplexObject | null; }, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEncodeMapOfObjectsPost(requestBody, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEncodeMapOfObjectsPostWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfObjectsPost(requestBody: { [key: string]: ComplexObject | null; }, _options?: ConfigurationOptions): Observable<void> {
        return this.testEncodeMapOfObjectsPostWithHttpInfo(requestBody, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfPrimitivePostWithHttpInfo(requestBody: { [key: string]: string; }, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEncodeMapOfPrimitivePost(requestBody, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEncodeMapOfPrimitivePostWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfPrimitivePost(requestBody: { [key: string]: string; }, _options?: ConfigurationOptions): Observable<void> {
        return this.testEncodeMapOfPrimitivePostWithHttpInfo(requestBody, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * @param [requestBody]
     */
    public testEncodeNullableArrayPostWithHttpInfo(requestBody?: Array<string>, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEncodeNullableArrayPost(requestBody, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEncodeNullableArrayPostWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param [requestBody]
     */
    public testEncodeNullableArrayPost(requestBody?: Array<string>, _options?: ConfigurationOptions): Observable<void> {
        return this.testEncodeNullableArrayPostWithHttpInfo(requestBody, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * @param [body]
     */
    public testEncodeNullablePostWithHttpInfo(body?: string, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEncodeNullablePost(body, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEncodeNullablePostWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param [body]
     */
    public testEncodeNullablePost(body?: string, _options?: ConfigurationOptions): Observable<void> {
        return this.testEncodeNullablePostWithHttpInfo(body, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * @param complexObject
     */
    public testEncodeObjectPostWithHttpInfo(complexObject: ComplexObject, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEncodeObjectPost(complexObject, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEncodeObjectPostWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param complexObject
     */
    public testEncodeObjectPost(complexObject: ComplexObject, _options?: ConfigurationOptions): Observable<void> {
        return this.testEncodeObjectPostWithHttpInfo(complexObject, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * @param body
     */
    public testEncodePrimitiveBooleanPostWithHttpInfo(body: boolean, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEncodePrimitiveBooleanPost(body, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEncodePrimitiveBooleanPostWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param body
     */
    public testEncodePrimitiveBooleanPost(body: boolean, _options?: ConfigurationOptions): Observable<void> {
        return this.testEncodePrimitiveBooleanPostWithHttpInfo(body, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * @param body
     */
    public testEncodePrimitiveIntegerPostWithHttpInfo(body: number, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEncodePrimitiveIntegerPost(body, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEncodePrimitiveIntegerPostWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param body
     */
    public testEncodePrimitiveIntegerPost(body: number, _options?: ConfigurationOptions): Observable<void> {
        return this.testEncodePrimitiveIntegerPostWithHttpInfo(body, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * @param body
     */
    public testEncodePrimitiveNumberPostWithHttpInfo(body: number, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEncodePrimitiveNumberPost(body, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEncodePrimitiveNumberPostWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param body
     */
    public testEncodePrimitiveNumberPost(body: number, _options?: ConfigurationOptions): Observable<void> {
        return this.testEncodePrimitiveNumberPostWithHttpInfo(body, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

    /**
     * @param body
     */
    public testEncodePrimitiveStringPostWithHttpInfo(body: string, _options?: ConfigurationOptions): Observable<HttpInfo<void>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEncodePrimitiveStringPost(body, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEncodePrimitiveStringPostWithHttpInfo(rsp)));
            }));
    }

    /**
     * @param body
     */
    public testEncodePrimitiveStringPost(body: string, _options?: ConfigurationOptions): Observable<void> {
        return this.testEncodePrimitiveStringPostWithHttpInfo(body, _options).pipe(map((apiResponse: HttpInfo<void>) => apiResponse.data));
    }

}
