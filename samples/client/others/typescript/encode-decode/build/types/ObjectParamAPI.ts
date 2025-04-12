import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration, ConfigurationOptions } from '../configuration'
import type { Middleware } from '../middleware';

import { ComplexObject } from '../models/ComplexObject';
import { CompositeObject } from '../models/CompositeObject';

import { ObservableDefaultApi } from "./ObservableAPI";
import { DefaultApiRequestFactory, DefaultApiResponseProcessor} from "../apis/DefaultApi";

export interface DefaultApiTestDecodeArrayOfArraysGetRequest {
}

export interface DefaultApiTestDecodeArrayOfGetRequest {
}

export interface DefaultApiTestDecodeArrayOfMapsOfObjectsGetRequest {
}

export interface DefaultApiTestDecodeArrayOfNullableGetRequest {
}

export interface DefaultApiTestDecodeArrayOfNullableObjectsGetRequest {
}

export interface DefaultApiTestDecodeCompositeObjectsGetRequest {
}

export interface DefaultApiTestDecodeMapOfMapsOfObjectsGetRequest {
}

export interface DefaultApiTestDecodeMapOfObjectsGetRequest {
}

export interface DefaultApiTestDecodeMapOfPrimitiveGetRequest {
}

export interface DefaultApiTestDecodeNullableArrayGetRequest {
}

export interface DefaultApiTestDecodeNullableGetRequest {
}

export interface DefaultApiTestDecodeObjectGetRequest {
}

export interface DefaultApiTestDecodePrimitiveBooleanGetRequest {
}

export interface DefaultApiTestDecodePrimitiveIntegerGetRequest {
}

export interface DefaultApiTestDecodePrimitiveNumberGetRequest {
}

export interface DefaultApiTestDecodePrimitiveStringGetRequest {
}

export interface DefaultApiTestEncodeArrayOfArraysPostRequest {
    /**
     * 
     * @type Array&lt;Array&lt;string&gt;&gt;
     * @memberof DefaultApitestEncodeArrayOfArraysPost
     */
    requestBody: Array<Array<string>>
}

export interface DefaultApiTestEncodeArrayOfMapsOfObjectsPostRequest {
    /**
     * 
     * @type Array&lt;{ [key: string]: ComplexObject; }&gt;
     * @memberof DefaultApitestEncodeArrayOfMapsOfObjectsPost
     */
    complexObject: Array<{ [key: string]: ComplexObject; }>
}

export interface DefaultApiTestEncodeArrayOfNullableObjectsPostRequest {
    /**
     * 
     * @type Array&lt;ComplexObject&gt;
     * @memberof DefaultApitestEncodeArrayOfNullableObjectsPost
     */
    complexObject: Array<ComplexObject>
}

export interface DefaultApiTestEncodeArrayOfNullablePostRequest {
    /**
     * 
     * @type Array&lt;string | null&gt;
     * @memberof DefaultApitestEncodeArrayOfNullablePost
     */
    requestBody: Array<string | null>
}

export interface DefaultApiTestEncodeArrayOfPostRequest {
    /**
     * 
     * @type Array&lt;string&gt;
     * @memberof DefaultApitestEncodeArrayOfPost
     */
    requestBody: Array<string>
}

export interface DefaultApiTestEncodeCompositeObjectsPostRequest {
    /**
     * 
     * @type CompositeObject
     * @memberof DefaultApitestEncodeCompositeObjectsPost
     */
    compositeObject: CompositeObject
}

export interface DefaultApiTestEncodeMapOfMapsOfObjectsPostRequest {
    /**
     * 
     * @type { [key: string]: { [key: string]: ComplexObject; }; }
     * @memberof DefaultApitestEncodeMapOfMapsOfObjectsPost
     */
    requestBody: { [key: string]: { [key: string]: ComplexObject; }; }
}

export interface DefaultApiTestEncodeMapOfObjectsPostRequest {
    /**
     * 
     * @type { [key: string]: ComplexObject | null; }
     * @memberof DefaultApitestEncodeMapOfObjectsPost
     */
    requestBody: { [key: string]: ComplexObject | null; }
}

export interface DefaultApiTestEncodeMapOfPrimitivePostRequest {
    /**
     * 
     * @type { [key: string]: string; }
     * @memberof DefaultApitestEncodeMapOfPrimitivePost
     */
    requestBody: { [key: string]: string; }
}

export interface DefaultApiTestEncodeNullableArrayPostRequest {
    /**
     * 
     * @type Array&lt;string&gt;
     * @memberof DefaultApitestEncodeNullableArrayPost
     */
    requestBody?: Array<string>
}

export interface DefaultApiTestEncodeNullablePostRequest {
    /**
     * 
     * @type string
     * @memberof DefaultApitestEncodeNullablePost
     */
    body?: string
}

export interface DefaultApiTestEncodeObjectPostRequest {
    /**
     * 
     * @type ComplexObject
     * @memberof DefaultApitestEncodeObjectPost
     */
    complexObject: ComplexObject
}

export interface DefaultApiTestEncodePrimitiveBooleanPostRequest {
    /**
     * 
     * @type boolean
     * @memberof DefaultApitestEncodePrimitiveBooleanPost
     */
    body: boolean
}

export interface DefaultApiTestEncodePrimitiveIntegerPostRequest {
    /**
     * 
     * @type number
     * @memberof DefaultApitestEncodePrimitiveIntegerPost
     */
    body: number
}

export interface DefaultApiTestEncodePrimitiveNumberPostRequest {
    /**
     * 
     * @type number
     * @memberof DefaultApitestEncodePrimitiveNumberPost
     */
    body: number
}

export interface DefaultApiTestEncodePrimitiveStringPostRequest {
    /**
     * 
     * @type string
     * @memberof DefaultApitestEncodePrimitiveStringPost
     */
    body: string
}

export class ObjectDefaultApi {
    private api: ObservableDefaultApi

    public constructor(configuration: Configuration, requestFactory?: DefaultApiRequestFactory, responseProcessor?: DefaultApiResponseProcessor) {
        this.api = new ObservableDefaultApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * @param param the request object
     */
    public testDecodeArrayOfArraysGetWithHttpInfo(param: DefaultApiTestDecodeArrayOfArraysGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<Array<Array<string>>>> {
        return this.api.testDecodeArrayOfArraysGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeArrayOfArraysGet(param: DefaultApiTestDecodeArrayOfArraysGetRequest = {}, options?: ConfigurationOptions): Promise<Array<Array<string>>> {
        return this.api.testDecodeArrayOfArraysGet( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeArrayOfGetWithHttpInfo(param: DefaultApiTestDecodeArrayOfGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<Array<string>>> {
        return this.api.testDecodeArrayOfGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeArrayOfGet(param: DefaultApiTestDecodeArrayOfGetRequest = {}, options?: ConfigurationOptions): Promise<Array<string>> {
        return this.api.testDecodeArrayOfGet( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeArrayOfMapsOfObjectsGetWithHttpInfo(param: DefaultApiTestDecodeArrayOfMapsOfObjectsGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<Array<{ [key: string]: ComplexObject; }>>> {
        return this.api.testDecodeArrayOfMapsOfObjectsGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeArrayOfMapsOfObjectsGet(param: DefaultApiTestDecodeArrayOfMapsOfObjectsGetRequest = {}, options?: ConfigurationOptions): Promise<Array<{ [key: string]: ComplexObject; }>> {
        return this.api.testDecodeArrayOfMapsOfObjectsGet( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeArrayOfNullableGetWithHttpInfo(param: DefaultApiTestDecodeArrayOfNullableGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<Array<string | null>>> {
        return this.api.testDecodeArrayOfNullableGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeArrayOfNullableGet(param: DefaultApiTestDecodeArrayOfNullableGetRequest = {}, options?: ConfigurationOptions): Promise<Array<string | null>> {
        return this.api.testDecodeArrayOfNullableGet( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeArrayOfNullableObjectsGetWithHttpInfo(param: DefaultApiTestDecodeArrayOfNullableObjectsGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<Array<ComplexObject>>> {
        return this.api.testDecodeArrayOfNullableObjectsGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeArrayOfNullableObjectsGet(param: DefaultApiTestDecodeArrayOfNullableObjectsGetRequest = {}, options?: ConfigurationOptions): Promise<Array<ComplexObject>> {
        return this.api.testDecodeArrayOfNullableObjectsGet( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeCompositeObjectsGetWithHttpInfo(param: DefaultApiTestDecodeCompositeObjectsGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<CompositeObject>> {
        return this.api.testDecodeCompositeObjectsGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeCompositeObjectsGet(param: DefaultApiTestDecodeCompositeObjectsGetRequest = {}, options?: ConfigurationOptions): Promise<CompositeObject> {
        return this.api.testDecodeCompositeObjectsGet( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeMapOfMapsOfObjectsGetWithHttpInfo(param: DefaultApiTestDecodeMapOfMapsOfObjectsGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<{ [key: string]: { [key: string]: ComplexObject; }; }>> {
        return this.api.testDecodeMapOfMapsOfObjectsGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeMapOfMapsOfObjectsGet(param: DefaultApiTestDecodeMapOfMapsOfObjectsGetRequest = {}, options?: ConfigurationOptions): Promise<{ [key: string]: { [key: string]: ComplexObject; }; }> {
        return this.api.testDecodeMapOfMapsOfObjectsGet( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeMapOfObjectsGetWithHttpInfo(param: DefaultApiTestDecodeMapOfObjectsGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<{ [key: string]: ComplexObject | null; }>> {
        return this.api.testDecodeMapOfObjectsGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeMapOfObjectsGet(param: DefaultApiTestDecodeMapOfObjectsGetRequest = {}, options?: ConfigurationOptions): Promise<{ [key: string]: ComplexObject | null; }> {
        return this.api.testDecodeMapOfObjectsGet( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeMapOfPrimitiveGetWithHttpInfo(param: DefaultApiTestDecodeMapOfPrimitiveGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<{ [key: string]: string; }>> {
        return this.api.testDecodeMapOfPrimitiveGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeMapOfPrimitiveGet(param: DefaultApiTestDecodeMapOfPrimitiveGetRequest = {}, options?: ConfigurationOptions): Promise<{ [key: string]: string; }> {
        return this.api.testDecodeMapOfPrimitiveGet( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeNullableArrayGetWithHttpInfo(param: DefaultApiTestDecodeNullableArrayGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<Array<string>>> {
        return this.api.testDecodeNullableArrayGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeNullableArrayGet(param: DefaultApiTestDecodeNullableArrayGetRequest = {}, options?: ConfigurationOptions): Promise<Array<string>> {
        return this.api.testDecodeNullableArrayGet( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeNullableGetWithHttpInfo(param: DefaultApiTestDecodeNullableGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testDecodeNullableGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeNullableGet(param: DefaultApiTestDecodeNullableGetRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testDecodeNullableGet( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeObjectGetWithHttpInfo(param: DefaultApiTestDecodeObjectGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<ComplexObject>> {
        return this.api.testDecodeObjectGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodeObjectGet(param: DefaultApiTestDecodeObjectGetRequest = {}, options?: ConfigurationOptions): Promise<ComplexObject> {
        return this.api.testDecodeObjectGet( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodePrimitiveBooleanGetWithHttpInfo(param: DefaultApiTestDecodePrimitiveBooleanGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<boolean>> {
        return this.api.testDecodePrimitiveBooleanGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodePrimitiveBooleanGet(param: DefaultApiTestDecodePrimitiveBooleanGetRequest = {}, options?: ConfigurationOptions): Promise<boolean> {
        return this.api.testDecodePrimitiveBooleanGet( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodePrimitiveIntegerGetWithHttpInfo(param: DefaultApiTestDecodePrimitiveIntegerGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<number>> {
        return this.api.testDecodePrimitiveIntegerGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodePrimitiveIntegerGet(param: DefaultApiTestDecodePrimitiveIntegerGetRequest = {}, options?: ConfigurationOptions): Promise<number> {
        return this.api.testDecodePrimitiveIntegerGet( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodePrimitiveNumberGetWithHttpInfo(param: DefaultApiTestDecodePrimitiveNumberGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<number>> {
        return this.api.testDecodePrimitiveNumberGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodePrimitiveNumberGet(param: DefaultApiTestDecodePrimitiveNumberGetRequest = {}, options?: ConfigurationOptions): Promise<number> {
        return this.api.testDecodePrimitiveNumberGet( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodePrimitiveStringGetWithHttpInfo(param: DefaultApiTestDecodePrimitiveStringGetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testDecodePrimitiveStringGetWithHttpInfo( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testDecodePrimitiveStringGet(param: DefaultApiTestDecodePrimitiveStringGetRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testDecodePrimitiveStringGet( options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeArrayOfArraysPostWithHttpInfo(param: DefaultApiTestEncodeArrayOfArraysPostRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEncodeArrayOfArraysPostWithHttpInfo(param.requestBody,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeArrayOfArraysPost(param: DefaultApiTestEncodeArrayOfArraysPostRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEncodeArrayOfArraysPost(param.requestBody,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeArrayOfMapsOfObjectsPostWithHttpInfo(param: DefaultApiTestEncodeArrayOfMapsOfObjectsPostRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEncodeArrayOfMapsOfObjectsPostWithHttpInfo(param.complexObject,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeArrayOfMapsOfObjectsPost(param: DefaultApiTestEncodeArrayOfMapsOfObjectsPostRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEncodeArrayOfMapsOfObjectsPost(param.complexObject,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeArrayOfNullableObjectsPostWithHttpInfo(param: DefaultApiTestEncodeArrayOfNullableObjectsPostRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEncodeArrayOfNullableObjectsPostWithHttpInfo(param.complexObject,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeArrayOfNullableObjectsPost(param: DefaultApiTestEncodeArrayOfNullableObjectsPostRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEncodeArrayOfNullableObjectsPost(param.complexObject,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeArrayOfNullablePostWithHttpInfo(param: DefaultApiTestEncodeArrayOfNullablePostRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEncodeArrayOfNullablePostWithHttpInfo(param.requestBody,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeArrayOfNullablePost(param: DefaultApiTestEncodeArrayOfNullablePostRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEncodeArrayOfNullablePost(param.requestBody,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeArrayOfPostWithHttpInfo(param: DefaultApiTestEncodeArrayOfPostRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEncodeArrayOfPostWithHttpInfo(param.requestBody,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeArrayOfPost(param: DefaultApiTestEncodeArrayOfPostRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEncodeArrayOfPost(param.requestBody,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeCompositeObjectsPostWithHttpInfo(param: DefaultApiTestEncodeCompositeObjectsPostRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEncodeCompositeObjectsPostWithHttpInfo(param.compositeObject,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeCompositeObjectsPost(param: DefaultApiTestEncodeCompositeObjectsPostRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEncodeCompositeObjectsPost(param.compositeObject,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeMapOfMapsOfObjectsPostWithHttpInfo(param: DefaultApiTestEncodeMapOfMapsOfObjectsPostRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEncodeMapOfMapsOfObjectsPostWithHttpInfo(param.requestBody,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeMapOfMapsOfObjectsPost(param: DefaultApiTestEncodeMapOfMapsOfObjectsPostRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEncodeMapOfMapsOfObjectsPost(param.requestBody,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeMapOfObjectsPostWithHttpInfo(param: DefaultApiTestEncodeMapOfObjectsPostRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEncodeMapOfObjectsPostWithHttpInfo(param.requestBody,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeMapOfObjectsPost(param: DefaultApiTestEncodeMapOfObjectsPostRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEncodeMapOfObjectsPost(param.requestBody,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeMapOfPrimitivePostWithHttpInfo(param: DefaultApiTestEncodeMapOfPrimitivePostRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEncodeMapOfPrimitivePostWithHttpInfo(param.requestBody,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeMapOfPrimitivePost(param: DefaultApiTestEncodeMapOfPrimitivePostRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEncodeMapOfPrimitivePost(param.requestBody,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeNullableArrayPostWithHttpInfo(param: DefaultApiTestEncodeNullableArrayPostRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEncodeNullableArrayPostWithHttpInfo(param.requestBody,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeNullableArrayPost(param: DefaultApiTestEncodeNullableArrayPostRequest = {}, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEncodeNullableArrayPost(param.requestBody,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeNullablePostWithHttpInfo(param: DefaultApiTestEncodeNullablePostRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEncodeNullablePostWithHttpInfo(param.body,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeNullablePost(param: DefaultApiTestEncodeNullablePostRequest = {}, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEncodeNullablePost(param.body,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeObjectPostWithHttpInfo(param: DefaultApiTestEncodeObjectPostRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEncodeObjectPostWithHttpInfo(param.complexObject,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodeObjectPost(param: DefaultApiTestEncodeObjectPostRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEncodeObjectPost(param.complexObject,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodePrimitiveBooleanPostWithHttpInfo(param: DefaultApiTestEncodePrimitiveBooleanPostRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEncodePrimitiveBooleanPostWithHttpInfo(param.body,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodePrimitiveBooleanPost(param: DefaultApiTestEncodePrimitiveBooleanPostRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEncodePrimitiveBooleanPost(param.body,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodePrimitiveIntegerPostWithHttpInfo(param: DefaultApiTestEncodePrimitiveIntegerPostRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEncodePrimitiveIntegerPostWithHttpInfo(param.body,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodePrimitiveIntegerPost(param: DefaultApiTestEncodePrimitiveIntegerPostRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEncodePrimitiveIntegerPost(param.body,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodePrimitiveNumberPostWithHttpInfo(param: DefaultApiTestEncodePrimitiveNumberPostRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEncodePrimitiveNumberPostWithHttpInfo(param.body,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodePrimitiveNumberPost(param: DefaultApiTestEncodePrimitiveNumberPostRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEncodePrimitiveNumberPost(param.body,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodePrimitiveStringPostWithHttpInfo(param: DefaultApiTestEncodePrimitiveStringPostRequest, options?: ConfigurationOptions): Promise<HttpInfo<void>> {
        return this.api.testEncodePrimitiveStringPostWithHttpInfo(param.body,  options).toPromise();
    }

    /**
     * @param param the request object
     */
    public testEncodePrimitiveStringPost(param: DefaultApiTestEncodePrimitiveStringPostRequest, options?: ConfigurationOptions): Promise<void> {
        return this.api.testEncodePrimitiveStringPost(param.body,  options).toPromise();
    }

}
