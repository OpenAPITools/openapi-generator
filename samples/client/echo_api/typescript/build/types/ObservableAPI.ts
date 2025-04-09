import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration, ConfigurationOptions, mergeConfiguration } from '../configuration'
import type { Middleware } from '../middleware';
import { Observable, of, from } from '../rxjsStub';
import {mergeMap, map} from  '../rxjsStub';
import { Bird } from '../models/Bird';
import { Category } from '../models/Category';
import { DataQuery } from '../models/DataQuery';
import { DefaultValue } from '../models/DefaultValue';
import { NumberPropertiesOnly } from '../models/NumberPropertiesOnly';
import { Pet } from '../models/Pet';
import { Query } from '../models/Query';
import { StringEnumRef } from '../models/StringEnumRef';
import { Tag } from '../models/Tag';
import { TestFormObjectMultipartRequestMarker } from '../models/TestFormObjectMultipartRequestMarker';
import { TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter } from '../models/TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter';
import { TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter } from '../models/TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter';

import { AuthApiRequestFactory, AuthApiResponseProcessor} from "../apis/AuthApi";
export class ObservableAuthApi {
    private requestFactory: AuthApiRequestFactory;
    private responseProcessor: AuthApiResponseProcessor;
    private configuration: Configuration;

    public constructor(
        configuration: Configuration,
        requestFactory?: AuthApiRequestFactory,
        responseProcessor?: AuthApiResponseProcessor
    ) {
        this.configuration = configuration;
        this.requestFactory = requestFactory || new AuthApiRequestFactory(configuration);
        this.responseProcessor = responseProcessor || new AuthApiResponseProcessor();
    }

    /**
     * To test HTTP basic authentication
     * To test HTTP basic authentication
     */
    public testAuthHttpBasicWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testAuthHttpBasic(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testAuthHttpBasicWithHttpInfo(rsp)));
            }));
    }

    /**
     * To test HTTP basic authentication
     * To test HTTP basic authentication
     */
    public testAuthHttpBasic(_options?: ConfigurationOptions): Observable<string> {
        return this.testAuthHttpBasicWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * To test HTTP bearer authentication
     * To test HTTP bearer authentication
     */
    public testAuthHttpBearerWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testAuthHttpBearer(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testAuthHttpBearerWithHttpInfo(rsp)));
            }));
    }

    /**
     * To test HTTP bearer authentication
     * To test HTTP bearer authentication
     */
    public testAuthHttpBearer(_options?: ConfigurationOptions): Observable<string> {
        return this.testAuthHttpBearerWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

}

import { BodyApiRequestFactory, BodyApiResponseProcessor} from "../apis/BodyApi";
export class ObservableBodyApi {
    private requestFactory: BodyApiRequestFactory;
    private responseProcessor: BodyApiResponseProcessor;
    private configuration: Configuration;

    public constructor(
        configuration: Configuration,
        requestFactory?: BodyApiRequestFactory,
        responseProcessor?: BodyApiResponseProcessor
    ) {
        this.configuration = configuration;
        this.requestFactory = requestFactory || new BodyApiRequestFactory(configuration);
        this.responseProcessor = responseProcessor || new BodyApiResponseProcessor();
    }

    /**
     * Test binary (gif) response body
     * Test binary (gif) response body
     */
    public testBinaryGifWithHttpInfo(_options?: ConfigurationOptions): Observable<HttpInfo<HttpFile>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testBinaryGif(_config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testBinaryGifWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test binary (gif) response body
     * Test binary (gif) response body
     */
    public testBinaryGif(_options?: ConfigurationOptions): Observable<HttpFile> {
        return this.testBinaryGifWithHttpInfo(_options).pipe(map((apiResponse: HttpInfo<HttpFile>) => apiResponse.data));
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param [body]
     */
    public testBodyApplicationOctetstreamBinaryWithHttpInfo(body?: HttpFile, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testBodyApplicationOctetstreamBinary(body, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testBodyApplicationOctetstreamBinaryWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param [body]
     */
    public testBodyApplicationOctetstreamBinary(body?: HttpFile, _options?: ConfigurationOptions): Observable<string> {
        return this.testBodyApplicationOctetstreamBinaryWithHttpInfo(body, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test array of binary in multipart mime
     * Test array of binary in multipart mime
     * @param files
     */
    public testBodyMultipartFormdataArrayOfBinaryWithHttpInfo(files: Array<HttpFile>, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testBodyMultipartFormdataArrayOfBinary(files, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testBodyMultipartFormdataArrayOfBinaryWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test array of binary in multipart mime
     * Test array of binary in multipart mime
     * @param files
     */
    public testBodyMultipartFormdataArrayOfBinary(files: Array<HttpFile>, _options?: ConfigurationOptions): Observable<string> {
        return this.testBodyMultipartFormdataArrayOfBinaryWithHttpInfo(files, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test single binary in multipart mime
     * Test single binary in multipart mime
     * @param [myFile]
     */
    public testBodyMultipartFormdataSingleBinaryWithHttpInfo(myFile?: HttpFile, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testBodyMultipartFormdataSingleBinary(myFile, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testBodyMultipartFormdataSingleBinaryWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test single binary in multipart mime
     * Test single binary in multipart mime
     * @param [myFile]
     */
    public testBodyMultipartFormdataSingleBinary(myFile?: HttpFile, _options?: ConfigurationOptions): Observable<string> {
        return this.testBodyMultipartFormdataSingleBinaryWithHttpInfo(myFile, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param [pet] Pet object that needs to be added to the store
     */
    public testEchoBodyAllOfPetWithHttpInfo(pet?: Pet, _options?: ConfigurationOptions): Observable<HttpInfo<Pet>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEchoBodyAllOfPet(pet, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEchoBodyAllOfPetWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param [pet] Pet object that needs to be added to the store
     */
    public testEchoBodyAllOfPet(pet?: Pet, _options?: ConfigurationOptions): Observable<Pet> {
        return this.testEchoBodyAllOfPetWithHttpInfo(pet, _options).pipe(map((apiResponse: HttpInfo<Pet>) => apiResponse.data));
    }

    /**
     * Test free form object
     * Test free form object
     * @param [body] Free form object
     */
    public testEchoBodyFreeFormObjectResponseStringWithHttpInfo(body?: any, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEchoBodyFreeFormObjectResponseString(body, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEchoBodyFreeFormObjectResponseStringWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test free form object
     * Test free form object
     * @param [body] Free form object
     */
    public testEchoBodyFreeFormObjectResponseString(body?: any, _options?: ConfigurationOptions): Observable<string> {
        return this.testEchoBodyFreeFormObjectResponseStringWithHttpInfo(body, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param [pet] Pet object that needs to be added to the store
     */
    public testEchoBodyPetWithHttpInfo(pet?: Pet, _options?: ConfigurationOptions): Observable<HttpInfo<Pet>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEchoBodyPet(pet, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEchoBodyPetWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param [pet] Pet object that needs to be added to the store
     */
    public testEchoBodyPet(pet?: Pet, _options?: ConfigurationOptions): Observable<Pet> {
        return this.testEchoBodyPetWithHttpInfo(pet, _options).pipe(map((apiResponse: HttpInfo<Pet>) => apiResponse.data));
    }

    /**
     * Test empty response body
     * Test empty response body
     * @param [pet] Pet object that needs to be added to the store
     */
    public testEchoBodyPetResponseStringWithHttpInfo(pet?: Pet, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEchoBodyPetResponseString(pet, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEchoBodyPetResponseStringWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test empty response body
     * Test empty response body
     * @param [pet] Pet object that needs to be added to the store
     */
    public testEchoBodyPetResponseString(pet?: Pet, _options?: ConfigurationOptions): Observable<string> {
        return this.testEchoBodyPetResponseStringWithHttpInfo(pet, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test string enum response body
     * Test string enum response body
     * @param [body] String enum
     */
    public testEchoBodyStringEnumWithHttpInfo(body?: string, _options?: ConfigurationOptions): Observable<HttpInfo<StringEnumRef>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEchoBodyStringEnum(body, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEchoBodyStringEnumWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test string enum response body
     * Test string enum response body
     * @param [body] String enum
     */
    public testEchoBodyStringEnum(body?: string, _options?: ConfigurationOptions): Observable<StringEnumRef> {
        return this.testEchoBodyStringEnumWithHttpInfo(body, _options).pipe(map((apiResponse: HttpInfo<StringEnumRef>) => apiResponse.data));
    }

    /**
     * Test empty json (request body)
     * Test empty json (request body)
     * @param [tag] Tag object
     */
    public testEchoBodyTagResponseStringWithHttpInfo(tag?: Tag, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEchoBodyTagResponseString(tag, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEchoBodyTagResponseStringWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test empty json (request body)
     * Test empty json (request body)
     * @param [tag] Tag object
     */
    public testEchoBodyTagResponseString(tag?: Tag, _options?: ConfigurationOptions): Observable<string> {
        return this.testEchoBodyTagResponseStringWithHttpInfo(tag, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

}

import { FormApiRequestFactory, FormApiResponseProcessor} from "../apis/FormApi";
export class ObservableFormApi {
    private requestFactory: FormApiRequestFactory;
    private responseProcessor: FormApiResponseProcessor;
    private configuration: Configuration;

    public constructor(
        configuration: Configuration,
        requestFactory?: FormApiRequestFactory,
        responseProcessor?: FormApiResponseProcessor
    ) {
        this.configuration = configuration;
        this.requestFactory = requestFactory || new FormApiRequestFactory(configuration);
        this.responseProcessor = responseProcessor || new FormApiResponseProcessor();
    }

    /**
     * Test form parameter(s)
     * Test form parameter(s)
     * @param [integerForm]
     * @param [booleanForm]
     * @param [stringForm]
     */
    public testFormIntegerBooleanStringWithHttpInfo(integerForm?: number, booleanForm?: boolean, stringForm?: string, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testFormIntegerBooleanString(integerForm, booleanForm, stringForm, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testFormIntegerBooleanStringWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test form parameter(s)
     * Test form parameter(s)
     * @param [integerForm]
     * @param [booleanForm]
     * @param [stringForm]
     */
    public testFormIntegerBooleanString(integerForm?: number, booleanForm?: boolean, stringForm?: string, _options?: ConfigurationOptions): Observable<string> {
        return this.testFormIntegerBooleanStringWithHttpInfo(integerForm, booleanForm, stringForm, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test form parameter(s) for multipart schema
     * Test form parameter(s) for multipart schema
     * @param marker
     */
    public testFormObjectMultipartWithHttpInfo(marker: TestFormObjectMultipartRequestMarker, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testFormObjectMultipart(marker, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testFormObjectMultipartWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test form parameter(s) for multipart schema
     * Test form parameter(s) for multipart schema
     * @param marker
     */
    public testFormObjectMultipart(marker: TestFormObjectMultipartRequestMarker, _options?: ConfigurationOptions): Observable<string> {
        return this.testFormObjectMultipartWithHttpInfo(marker, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test form parameter(s) for oneOf schema
     * Test form parameter(s) for oneOf schema
     * @param [form1]
     * @param [form2]
     * @param [form3]
     * @param [form4]
     * @param [id]
     * @param [name]
     */
    public testFormOneofWithHttpInfo(form1?: string, form2?: number, form3?: string, form4?: boolean, id?: number, name?: string, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testFormOneof(form1, form2, form3, form4, id, name, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testFormOneofWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test form parameter(s) for oneOf schema
     * Test form parameter(s) for oneOf schema
     * @param [form1]
     * @param [form2]
     * @param [form3]
     * @param [form4]
     * @param [id]
     * @param [name]
     */
    public testFormOneof(form1?: string, form2?: number, form3?: string, form4?: boolean, id?: number, name?: string, _options?: ConfigurationOptions): Observable<string> {
        return this.testFormOneofWithHttpInfo(form1, form2, form3, form4, id, name, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

}

import { HeaderApiRequestFactory, HeaderApiResponseProcessor} from "../apis/HeaderApi";
export class ObservableHeaderApi {
    private requestFactory: HeaderApiRequestFactory;
    private responseProcessor: HeaderApiResponseProcessor;
    private configuration: Configuration;

    public constructor(
        configuration: Configuration,
        requestFactory?: HeaderApiRequestFactory,
        responseProcessor?: HeaderApiResponseProcessor
    ) {
        this.configuration = configuration;
        this.requestFactory = requestFactory || new HeaderApiRequestFactory(configuration);
        this.responseProcessor = responseProcessor || new HeaderApiResponseProcessor();
    }

    /**
     * Test header parameter(s)
     * Test header parameter(s)
     * @param [integerHeader]
     * @param [booleanHeader]
     * @param [stringHeader]
     * @param [enumNonrefStringHeader]
     * @param [enumRefStringHeader]
     */
    public testHeaderIntegerBooleanStringEnumsWithHttpInfo(integerHeader?: number, booleanHeader?: boolean, stringHeader?: string, enumNonrefStringHeader?: 'success' | 'failure' | 'unclassified', enumRefStringHeader?: StringEnumRef, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testHeaderIntegerBooleanStringEnums(integerHeader, booleanHeader, stringHeader, enumNonrefStringHeader, enumRefStringHeader, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testHeaderIntegerBooleanStringEnumsWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test header parameter(s)
     * Test header parameter(s)
     * @param [integerHeader]
     * @param [booleanHeader]
     * @param [stringHeader]
     * @param [enumNonrefStringHeader]
     * @param [enumRefStringHeader]
     */
    public testHeaderIntegerBooleanStringEnums(integerHeader?: number, booleanHeader?: boolean, stringHeader?: string, enumNonrefStringHeader?: 'success' | 'failure' | 'unclassified', enumRefStringHeader?: StringEnumRef, _options?: ConfigurationOptions): Observable<string> {
        return this.testHeaderIntegerBooleanStringEnumsWithHttpInfo(integerHeader, booleanHeader, stringHeader, enumNonrefStringHeader, enumRefStringHeader, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

}

import { PathApiRequestFactory, PathApiResponseProcessor} from "../apis/PathApi";
export class ObservablePathApi {
    private requestFactory: PathApiRequestFactory;
    private responseProcessor: PathApiResponseProcessor;
    private configuration: Configuration;

    public constructor(
        configuration: Configuration,
        requestFactory?: PathApiRequestFactory,
        responseProcessor?: PathApiResponseProcessor
    ) {
        this.configuration = configuration;
        this.requestFactory = requestFactory || new PathApiRequestFactory(configuration);
        this.responseProcessor = responseProcessor || new PathApiResponseProcessor();
    }

    /**
     * Test path parameter(s)
     * Test path parameter(s)
     * @param pathString
     * @param pathInteger
     * @param enumNonrefStringPath
     * @param enumRefStringPath
     */
    public testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathWithHttpInfo(pathString: string, pathInteger: number, enumNonrefStringPath: 'success' | 'failure' | 'unclassified', enumRefStringPath: StringEnumRef, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test path parameter(s)
     * Test path parameter(s)
     * @param pathString
     * @param pathInteger
     * @param enumNonrefStringPath
     * @param enumRefStringPath
     */
    public testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(pathString: string, pathInteger: number, enumNonrefStringPath: 'success' | 'failure' | 'unclassified', enumRefStringPath: StringEnumRef, _options?: ConfigurationOptions): Observable<string> {
        return this.testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathWithHttpInfo(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

}

import { QueryApiRequestFactory, QueryApiResponseProcessor} from "../apis/QueryApi";
export class ObservableQueryApi {
    private requestFactory: QueryApiRequestFactory;
    private responseProcessor: QueryApiResponseProcessor;
    private configuration: Configuration;

    public constructor(
        configuration: Configuration,
        requestFactory?: QueryApiRequestFactory,
        responseProcessor?: QueryApiResponseProcessor
    ) {
        this.configuration = configuration;
        this.requestFactory = requestFactory || new QueryApiRequestFactory(configuration);
        this.responseProcessor = responseProcessor || new QueryApiResponseProcessor();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [enumNonrefStringQuery]
     * @param [enumRefStringQuery]
     */
    public testEnumRefStringWithHttpInfo(enumNonrefStringQuery?: 'success' | 'failure' | 'unclassified', enumRefStringQuery?: StringEnumRef, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testEnumRefString(enumNonrefStringQuery, enumRefStringQuery, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testEnumRefStringWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [enumNonrefStringQuery]
     * @param [enumRefStringQuery]
     */
    public testEnumRefString(enumNonrefStringQuery?: 'success' | 'failure' | 'unclassified', enumRefStringQuery?: StringEnumRef, _options?: ConfigurationOptions): Observable<string> {
        return this.testEnumRefStringWithHttpInfo(enumNonrefStringQuery, enumRefStringQuery, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [datetimeQuery]
     * @param [dateQuery]
     * @param [stringQuery]
     */
    public testQueryDatetimeDateStringWithHttpInfo(datetimeQuery?: Date, dateQuery?: string, stringQuery?: string, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testQueryDatetimeDateString(datetimeQuery, dateQuery, stringQuery, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testQueryDatetimeDateStringWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [datetimeQuery]
     * @param [dateQuery]
     * @param [stringQuery]
     */
    public testQueryDatetimeDateString(datetimeQuery?: Date, dateQuery?: string, stringQuery?: string, _options?: ConfigurationOptions): Observable<string> {
        return this.testQueryDatetimeDateStringWithHttpInfo(datetimeQuery, dateQuery, stringQuery, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [integerQuery]
     * @param [booleanQuery]
     * @param [stringQuery]
     */
    public testQueryIntegerBooleanStringWithHttpInfo(integerQuery?: number, booleanQuery?: boolean, stringQuery?: string, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testQueryIntegerBooleanString(integerQuery, booleanQuery, stringQuery, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testQueryIntegerBooleanStringWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [integerQuery]
     * @param [booleanQuery]
     * @param [stringQuery]
     */
    public testQueryIntegerBooleanString(integerQuery?: number, booleanQuery?: boolean, stringQuery?: string, _options?: ConfigurationOptions): Observable<string> {
        return this.testQueryIntegerBooleanStringWithHttpInfo(integerQuery, booleanQuery, stringQuery, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo(queryObject?: Pet, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testQueryStyleDeepObjectExplodeTrueObject(queryObject, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleDeepObjectExplodeTrueObject(queryObject?: Pet, _options?: ConfigurationOptions): Observable<string> {
        return this.testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo(queryObject, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo(queryObject?: TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testQueryStyleDeepObjectExplodeTrueObjectAllOf(queryObject, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleDeepObjectExplodeTrueObjectAllOf(queryObject?: TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter, _options?: ConfigurationOptions): Observable<string> {
        return this.testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo(queryObject, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeFalseArrayIntegerWithHttpInfo(queryObject?: Array<number>, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testQueryStyleFormExplodeFalseArrayInteger(queryObject, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testQueryStyleFormExplodeFalseArrayIntegerWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeFalseArrayInteger(queryObject?: Array<number>, _options?: ConfigurationOptions): Observable<string> {
        return this.testQueryStyleFormExplodeFalseArrayIntegerWithHttpInfo(queryObject, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeFalseArrayStringWithHttpInfo(queryObject?: Array<string>, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testQueryStyleFormExplodeFalseArrayString(queryObject, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testQueryStyleFormExplodeFalseArrayStringWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeFalseArrayString(queryObject?: Array<string>, _options?: ConfigurationOptions): Observable<string> {
        return this.testQueryStyleFormExplodeFalseArrayStringWithHttpInfo(queryObject, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(queryObject?: TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testQueryStyleFormExplodeTrueArrayString(queryObject, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeTrueArrayString(queryObject?: TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter, _options?: ConfigurationOptions): Observable<string> {
        return this.testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(queryObject, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeTrueObjectWithHttpInfo(queryObject?: Pet, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testQueryStyleFormExplodeTrueObject(queryObject, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testQueryStyleFormExplodeTrueObjectWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeTrueObject(queryObject?: Pet, _options?: ConfigurationOptions): Observable<string> {
        return this.testQueryStyleFormExplodeTrueObjectWithHttpInfo(queryObject, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo(queryObject?: DataQuery, _options?: ConfigurationOptions): Observable<HttpInfo<string>> {
        const _config = mergeConfiguration(this.configuration, _options);

        const requestContextPromise = this.requestFactory.testQueryStyleFormExplodeTrueObjectAllOf(queryObject, _config);
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
                return middlewarePostObservable.pipe(map((rsp: ResponseContext) => this.responseProcessor.testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo(rsp)));
            }));
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeTrueObjectAllOf(queryObject?: DataQuery, _options?: ConfigurationOptions): Observable<string> {
        return this.testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo(queryObject, _options).pipe(map((apiResponse: HttpInfo<string>) => apiResponse.data));
    }

}
