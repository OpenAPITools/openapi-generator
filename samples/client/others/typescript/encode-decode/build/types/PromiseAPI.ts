import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration, PromiseConfigurationOptions, wrapOptions } from '../configuration'
import { PromiseMiddleware, Middleware, PromiseMiddlewareWrapper } from '../middleware';

import { ComplexObject } from '../models/ComplexObject';
import { CompositeObject } from '../models/CompositeObject';
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
    public testDecodeArrayOfArraysGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<Array<Array<string>>>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeArrayOfArraysGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfArraysGet(_options?: PromiseConfigurationOptions): Promise<Array<Array<string>>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeArrayOfArraysGet(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<Array<string>>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeArrayOfGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfGet(_options?: PromiseConfigurationOptions): Promise<Array<string>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeArrayOfGet(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfMapsOfObjectsGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<Array<{ [key: string]: ComplexObject; }>>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeArrayOfMapsOfObjectsGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfMapsOfObjectsGet(_options?: PromiseConfigurationOptions): Promise<Array<{ [key: string]: ComplexObject; }>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeArrayOfMapsOfObjectsGet(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfNullableGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<Array<string | null>>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeArrayOfNullableGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfNullableGet(_options?: PromiseConfigurationOptions): Promise<Array<string | null>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeArrayOfNullableGet(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfNullableObjectsGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<Array<ComplexObject>>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeArrayOfNullableObjectsGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfNullableObjectsGet(_options?: PromiseConfigurationOptions): Promise<Array<ComplexObject>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeArrayOfNullableObjectsGet(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeCompositeObjectsGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<CompositeObject>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeCompositeObjectsGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeCompositeObjectsGet(_options?: PromiseConfigurationOptions): Promise<CompositeObject> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeCompositeObjectsGet(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeMapOfMapsOfObjectsGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<{ [key: string]: { [key: string]: ComplexObject; }; }>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeMapOfMapsOfObjectsGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeMapOfMapsOfObjectsGet(_options?: PromiseConfigurationOptions): Promise<{ [key: string]: { [key: string]: ComplexObject; }; }> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeMapOfMapsOfObjectsGet(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeMapOfObjectsGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<{ [key: string]: ComplexObject | null; }>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeMapOfObjectsGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeMapOfObjectsGet(_options?: PromiseConfigurationOptions): Promise<{ [key: string]: ComplexObject | null; }> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeMapOfObjectsGet(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeMapOfPrimitiveGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<{ [key: string]: string; }>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeMapOfPrimitiveGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeMapOfPrimitiveGet(_options?: PromiseConfigurationOptions): Promise<{ [key: string]: string; }> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeMapOfPrimitiveGet(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeNullableArrayGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<Array<string>>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeNullableArrayGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeNullableArrayGet(_options?: PromiseConfigurationOptions): Promise<Array<string>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeNullableArrayGet(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeNullableGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeNullableGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeNullableGet(_options?: PromiseConfigurationOptions): Promise<string> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeNullableGet(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeObjectGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<ComplexObject>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeObjectGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodeObjectGet(_options?: PromiseConfigurationOptions): Promise<ComplexObject> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodeObjectGet(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodePrimitiveBooleanGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<boolean>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodePrimitiveBooleanGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodePrimitiveBooleanGet(_options?: PromiseConfigurationOptions): Promise<boolean> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodePrimitiveBooleanGet(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodePrimitiveIntegerGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<number>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodePrimitiveIntegerGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodePrimitiveIntegerGet(_options?: PromiseConfigurationOptions): Promise<number> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodePrimitiveIntegerGet(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodePrimitiveNumberGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<number>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodePrimitiveNumberGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodePrimitiveNumberGet(_options?: PromiseConfigurationOptions): Promise<number> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodePrimitiveNumberGet(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodePrimitiveStringGetWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodePrimitiveStringGetWithHttpInfo(observableOptions);
        return result.toPromise();
    }

    /**
     */
    public testDecodePrimitiveStringGet(_options?: PromiseConfigurationOptions): Promise<string> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testDecodePrimitiveStringGet(observableOptions);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfArraysPostWithHttpInfo(requestBody: Array<Array<string>>, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeArrayOfArraysPostWithHttpInfo(requestBody, observableOptions);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfArraysPost(requestBody: Array<Array<string>>, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeArrayOfArraysPost(requestBody, observableOptions);
        return result.toPromise();
    }

    /**
     * @param complexObject
     */
    public testEncodeArrayOfMapsOfObjectsPostWithHttpInfo(complexObject: Array<{ [key: string]: ComplexObject; }>, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeArrayOfMapsOfObjectsPostWithHttpInfo(complexObject, observableOptions);
        return result.toPromise();
    }

    /**
     * @param complexObject
     */
    public testEncodeArrayOfMapsOfObjectsPost(complexObject: Array<{ [key: string]: ComplexObject; }>, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeArrayOfMapsOfObjectsPost(complexObject, observableOptions);
        return result.toPromise();
    }

    /**
     * @param complexObject
     */
    public testEncodeArrayOfNullableObjectsPostWithHttpInfo(complexObject: Array<ComplexObject>, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeArrayOfNullableObjectsPostWithHttpInfo(complexObject, observableOptions);
        return result.toPromise();
    }

    /**
     * @param complexObject
     */
    public testEncodeArrayOfNullableObjectsPost(complexObject: Array<ComplexObject>, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeArrayOfNullableObjectsPost(complexObject, observableOptions);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfNullablePostWithHttpInfo(requestBody: Array<string | null>, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeArrayOfNullablePostWithHttpInfo(requestBody, observableOptions);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfNullablePost(requestBody: Array<string | null>, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeArrayOfNullablePost(requestBody, observableOptions);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfPostWithHttpInfo(requestBody: Array<string>, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeArrayOfPostWithHttpInfo(requestBody, observableOptions);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfPost(requestBody: Array<string>, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeArrayOfPost(requestBody, observableOptions);
        return result.toPromise();
    }

    /**
     * @param compositeObject
     */
    public testEncodeCompositeObjectsPostWithHttpInfo(compositeObject: CompositeObject, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeCompositeObjectsPostWithHttpInfo(compositeObject, observableOptions);
        return result.toPromise();
    }

    /**
     * @param compositeObject
     */
    public testEncodeCompositeObjectsPost(compositeObject: CompositeObject, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeCompositeObjectsPost(compositeObject, observableOptions);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfMapsOfObjectsPostWithHttpInfo(requestBody: { [key: string]: { [key: string]: ComplexObject; }; }, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeMapOfMapsOfObjectsPostWithHttpInfo(requestBody, observableOptions);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfMapsOfObjectsPost(requestBody: { [key: string]: { [key: string]: ComplexObject; }; }, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeMapOfMapsOfObjectsPost(requestBody, observableOptions);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfObjectsPostWithHttpInfo(requestBody: { [key: string]: ComplexObject | null; }, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeMapOfObjectsPostWithHttpInfo(requestBody, observableOptions);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfObjectsPost(requestBody: { [key: string]: ComplexObject | null; }, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeMapOfObjectsPost(requestBody, observableOptions);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfPrimitivePostWithHttpInfo(requestBody: { [key: string]: string; }, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeMapOfPrimitivePostWithHttpInfo(requestBody, observableOptions);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfPrimitivePost(requestBody: { [key: string]: string; }, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeMapOfPrimitivePost(requestBody, observableOptions);
        return result.toPromise();
    }

    /**
     * @param [requestBody]
     */
    public testEncodeNullableArrayPostWithHttpInfo(requestBody?: Array<string>, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeNullableArrayPostWithHttpInfo(requestBody, observableOptions);
        return result.toPromise();
    }

    /**
     * @param [requestBody]
     */
    public testEncodeNullableArrayPost(requestBody?: Array<string>, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeNullableArrayPost(requestBody, observableOptions);
        return result.toPromise();
    }

    /**
     * @param [body]
     */
    public testEncodeNullablePostWithHttpInfo(body?: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeNullablePostWithHttpInfo(body, observableOptions);
        return result.toPromise();
    }

    /**
     * @param [body]
     */
    public testEncodeNullablePost(body?: string, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeNullablePost(body, observableOptions);
        return result.toPromise();
    }

    /**
     * @param complexObject
     */
    public testEncodeObjectPostWithHttpInfo(complexObject: ComplexObject, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeObjectPostWithHttpInfo(complexObject, observableOptions);
        return result.toPromise();
    }

    /**
     * @param complexObject
     */
    public testEncodeObjectPost(complexObject: ComplexObject, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodeObjectPost(complexObject, observableOptions);
        return result.toPromise();
    }

    /**
     * @param body
     */
    public testEncodePrimitiveBooleanPostWithHttpInfo(body: boolean, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodePrimitiveBooleanPostWithHttpInfo(body, observableOptions);
        return result.toPromise();
    }

    /**
     * @param body
     */
    public testEncodePrimitiveBooleanPost(body: boolean, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodePrimitiveBooleanPost(body, observableOptions);
        return result.toPromise();
    }

    /**
     * @param body
     */
    public testEncodePrimitiveIntegerPostWithHttpInfo(body: number, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodePrimitiveIntegerPostWithHttpInfo(body, observableOptions);
        return result.toPromise();
    }

    /**
     * @param body
     */
    public testEncodePrimitiveIntegerPost(body: number, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodePrimitiveIntegerPost(body, observableOptions);
        return result.toPromise();
    }

    /**
     * @param body
     */
    public testEncodePrimitiveNumberPostWithHttpInfo(body: number, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodePrimitiveNumberPostWithHttpInfo(body, observableOptions);
        return result.toPromise();
    }

    /**
     * @param body
     */
    public testEncodePrimitiveNumberPost(body: number, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodePrimitiveNumberPost(body, observableOptions);
        return result.toPromise();
    }

    /**
     * @param body
     */
    public testEncodePrimitiveStringPostWithHttpInfo(body: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<void>> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodePrimitiveStringPostWithHttpInfo(body, observableOptions);
        return result.toPromise();
    }

    /**
     * @param body
     */
    public testEncodePrimitiveStringPost(body: string, _options?: PromiseConfigurationOptions): Promise<void> {
        const observableOptions = wrapOptions(_options);
        const result = this.api.testEncodePrimitiveStringPost(body, observableOptions);
        return result.toPromise();
    }


}



