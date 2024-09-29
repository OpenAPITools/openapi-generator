import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration} from '../configuration'

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
    public testDecodeArrayOfArraysGetWithHttpInfo(_options?: Configuration): Promise<HttpInfo<Array<Array<string>>>> {
        const result = this.api.testDecodeArrayOfArraysGetWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfArraysGet(_options?: Configuration): Promise<Array<Array<string>>> {
        const result = this.api.testDecodeArrayOfArraysGet(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfGetWithHttpInfo(_options?: Configuration): Promise<HttpInfo<Array<string>>> {
        const result = this.api.testDecodeArrayOfGetWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfGet(_options?: Configuration): Promise<Array<string>> {
        const result = this.api.testDecodeArrayOfGet(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfMapsOfObjectsGetWithHttpInfo(_options?: Configuration): Promise<HttpInfo<Array<{ [key: string]: ComplexObject; }>>> {
        const result = this.api.testDecodeArrayOfMapsOfObjectsGetWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfMapsOfObjectsGet(_options?: Configuration): Promise<Array<{ [key: string]: ComplexObject; }>> {
        const result = this.api.testDecodeArrayOfMapsOfObjectsGet(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfNullableGetWithHttpInfo(_options?: Configuration): Promise<HttpInfo<Array<string | null>>> {
        const result = this.api.testDecodeArrayOfNullableGetWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfNullableGet(_options?: Configuration): Promise<Array<string | null>> {
        const result = this.api.testDecodeArrayOfNullableGet(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfNullableObjectsGetWithHttpInfo(_options?: Configuration): Promise<HttpInfo<Array<ComplexObject>>> {
        const result = this.api.testDecodeArrayOfNullableObjectsGetWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeArrayOfNullableObjectsGet(_options?: Configuration): Promise<Array<ComplexObject>> {
        const result = this.api.testDecodeArrayOfNullableObjectsGet(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeCompositeObjectsGetWithHttpInfo(_options?: Configuration): Promise<HttpInfo<CompositeObject>> {
        const result = this.api.testDecodeCompositeObjectsGetWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeCompositeObjectsGet(_options?: Configuration): Promise<CompositeObject> {
        const result = this.api.testDecodeCompositeObjectsGet(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeMapOfMapsOfObjectsGetWithHttpInfo(_options?: Configuration): Promise<HttpInfo<{ [key: string]: { [key: string]: ComplexObject; }; }>> {
        const result = this.api.testDecodeMapOfMapsOfObjectsGetWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeMapOfMapsOfObjectsGet(_options?: Configuration): Promise<{ [key: string]: { [key: string]: ComplexObject; }; }> {
        const result = this.api.testDecodeMapOfMapsOfObjectsGet(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeMapOfObjectsGetWithHttpInfo(_options?: Configuration): Promise<HttpInfo<{ [key: string]: ComplexObject | null; }>> {
        const result = this.api.testDecodeMapOfObjectsGetWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeMapOfObjectsGet(_options?: Configuration): Promise<{ [key: string]: ComplexObject | null; }> {
        const result = this.api.testDecodeMapOfObjectsGet(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeMapOfPrimitiveGetWithHttpInfo(_options?: Configuration): Promise<HttpInfo<{ [key: string]: string; }>> {
        const result = this.api.testDecodeMapOfPrimitiveGetWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeMapOfPrimitiveGet(_options?: Configuration): Promise<{ [key: string]: string; }> {
        const result = this.api.testDecodeMapOfPrimitiveGet(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeNullableArrayGetWithHttpInfo(_options?: Configuration): Promise<HttpInfo<Array<string>>> {
        const result = this.api.testDecodeNullableArrayGetWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeNullableArrayGet(_options?: Configuration): Promise<Array<string>> {
        const result = this.api.testDecodeNullableArrayGet(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeNullableGetWithHttpInfo(_options?: Configuration): Promise<HttpInfo<string>> {
        const result = this.api.testDecodeNullableGetWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeNullableGet(_options?: Configuration): Promise<string> {
        const result = this.api.testDecodeNullableGet(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeObjectGetWithHttpInfo(_options?: Configuration): Promise<HttpInfo<ComplexObject>> {
        const result = this.api.testDecodeObjectGetWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodeObjectGet(_options?: Configuration): Promise<ComplexObject> {
        const result = this.api.testDecodeObjectGet(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodePrimitiveBooleanGetWithHttpInfo(_options?: Configuration): Promise<HttpInfo<boolean>> {
        const result = this.api.testDecodePrimitiveBooleanGetWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodePrimitiveBooleanGet(_options?: Configuration): Promise<boolean> {
        const result = this.api.testDecodePrimitiveBooleanGet(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodePrimitiveIntegerGetWithHttpInfo(_options?: Configuration): Promise<HttpInfo<number>> {
        const result = this.api.testDecodePrimitiveIntegerGetWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodePrimitiveIntegerGet(_options?: Configuration): Promise<number> {
        const result = this.api.testDecodePrimitiveIntegerGet(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodePrimitiveNumberGetWithHttpInfo(_options?: Configuration): Promise<HttpInfo<number>> {
        const result = this.api.testDecodePrimitiveNumberGetWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodePrimitiveNumberGet(_options?: Configuration): Promise<number> {
        const result = this.api.testDecodePrimitiveNumberGet(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodePrimitiveStringGetWithHttpInfo(_options?: Configuration): Promise<HttpInfo<string>> {
        const result = this.api.testDecodePrimitiveStringGetWithHttpInfo(_options);
        return result.toPromise();
    }

    /**
     */
    public testDecodePrimitiveStringGet(_options?: Configuration): Promise<string> {
        const result = this.api.testDecodePrimitiveStringGet(_options);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfArraysPostWithHttpInfo(requestBody: Array<Array<string>>, _options?: Configuration): Promise<HttpInfo<void>> {
        const result = this.api.testEncodeArrayOfArraysPostWithHttpInfo(requestBody, _options);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfArraysPost(requestBody: Array<Array<string>>, _options?: Configuration): Promise<void> {
        const result = this.api.testEncodeArrayOfArraysPost(requestBody, _options);
        return result.toPromise();
    }

    /**
     * @param complexObject
     */
    public testEncodeArrayOfMapsOfObjectsPostWithHttpInfo(complexObject: Array<{ [key: string]: ComplexObject; }>, _options?: Configuration): Promise<HttpInfo<void>> {
        const result = this.api.testEncodeArrayOfMapsOfObjectsPostWithHttpInfo(complexObject, _options);
        return result.toPromise();
    }

    /**
     * @param complexObject
     */
    public testEncodeArrayOfMapsOfObjectsPost(complexObject: Array<{ [key: string]: ComplexObject; }>, _options?: Configuration): Promise<void> {
        const result = this.api.testEncodeArrayOfMapsOfObjectsPost(complexObject, _options);
        return result.toPromise();
    }

    /**
     * @param complexObject
     */
    public testEncodeArrayOfNullableObjectsPostWithHttpInfo(complexObject: Array<ComplexObject>, _options?: Configuration): Promise<HttpInfo<void>> {
        const result = this.api.testEncodeArrayOfNullableObjectsPostWithHttpInfo(complexObject, _options);
        return result.toPromise();
    }

    /**
     * @param complexObject
     */
    public testEncodeArrayOfNullableObjectsPost(complexObject: Array<ComplexObject>, _options?: Configuration): Promise<void> {
        const result = this.api.testEncodeArrayOfNullableObjectsPost(complexObject, _options);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfNullablePostWithHttpInfo(requestBody: Array<string | null>, _options?: Configuration): Promise<HttpInfo<void>> {
        const result = this.api.testEncodeArrayOfNullablePostWithHttpInfo(requestBody, _options);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfNullablePost(requestBody: Array<string | null>, _options?: Configuration): Promise<void> {
        const result = this.api.testEncodeArrayOfNullablePost(requestBody, _options);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfPostWithHttpInfo(requestBody: Array<string>, _options?: Configuration): Promise<HttpInfo<void>> {
        const result = this.api.testEncodeArrayOfPostWithHttpInfo(requestBody, _options);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeArrayOfPost(requestBody: Array<string>, _options?: Configuration): Promise<void> {
        const result = this.api.testEncodeArrayOfPost(requestBody, _options);
        return result.toPromise();
    }

    /**
     * @param compositeObject
     */
    public testEncodeCompositeObjectsPostWithHttpInfo(compositeObject: CompositeObject, _options?: Configuration): Promise<HttpInfo<void>> {
        const result = this.api.testEncodeCompositeObjectsPostWithHttpInfo(compositeObject, _options);
        return result.toPromise();
    }

    /**
     * @param compositeObject
     */
    public testEncodeCompositeObjectsPost(compositeObject: CompositeObject, _options?: Configuration): Promise<void> {
        const result = this.api.testEncodeCompositeObjectsPost(compositeObject, _options);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfMapsOfObjectsPostWithHttpInfo(requestBody: { [key: string]: { [key: string]: ComplexObject; }; }, _options?: Configuration): Promise<HttpInfo<void>> {
        const result = this.api.testEncodeMapOfMapsOfObjectsPostWithHttpInfo(requestBody, _options);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfMapsOfObjectsPost(requestBody: { [key: string]: { [key: string]: ComplexObject; }; }, _options?: Configuration): Promise<void> {
        const result = this.api.testEncodeMapOfMapsOfObjectsPost(requestBody, _options);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfObjectsPostWithHttpInfo(requestBody: { [key: string]: ComplexObject | null; }, _options?: Configuration): Promise<HttpInfo<void>> {
        const result = this.api.testEncodeMapOfObjectsPostWithHttpInfo(requestBody, _options);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfObjectsPost(requestBody: { [key: string]: ComplexObject | null; }, _options?: Configuration): Promise<void> {
        const result = this.api.testEncodeMapOfObjectsPost(requestBody, _options);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfPrimitivePostWithHttpInfo(requestBody: { [key: string]: string; }, _options?: Configuration): Promise<HttpInfo<void>> {
        const result = this.api.testEncodeMapOfPrimitivePostWithHttpInfo(requestBody, _options);
        return result.toPromise();
    }

    /**
     * @param requestBody
     */
    public testEncodeMapOfPrimitivePost(requestBody: { [key: string]: string; }, _options?: Configuration): Promise<void> {
        const result = this.api.testEncodeMapOfPrimitivePost(requestBody, _options);
        return result.toPromise();
    }

    /**
     * @param [requestBody]
     */
    public testEncodeNullableArrayPostWithHttpInfo(requestBody?: Array<string>, _options?: Configuration): Promise<HttpInfo<void>> {
        const result = this.api.testEncodeNullableArrayPostWithHttpInfo(requestBody, _options);
        return result.toPromise();
    }

    /**
     * @param [requestBody]
     */
    public testEncodeNullableArrayPost(requestBody?: Array<string>, _options?: Configuration): Promise<void> {
        const result = this.api.testEncodeNullableArrayPost(requestBody, _options);
        return result.toPromise();
    }

    /**
     * @param [body]
     */
    public testEncodeNullablePostWithHttpInfo(body?: string, _options?: Configuration): Promise<HttpInfo<void>> {
        const result = this.api.testEncodeNullablePostWithHttpInfo(body, _options);
        return result.toPromise();
    }

    /**
     * @param [body]
     */
    public testEncodeNullablePost(body?: string, _options?: Configuration): Promise<void> {
        const result = this.api.testEncodeNullablePost(body, _options);
        return result.toPromise();
    }

    /**
     * @param complexObject
     */
    public testEncodeObjectPostWithHttpInfo(complexObject: ComplexObject, _options?: Configuration): Promise<HttpInfo<void>> {
        const result = this.api.testEncodeObjectPostWithHttpInfo(complexObject, _options);
        return result.toPromise();
    }

    /**
     * @param complexObject
     */
    public testEncodeObjectPost(complexObject: ComplexObject, _options?: Configuration): Promise<void> {
        const result = this.api.testEncodeObjectPost(complexObject, _options);
        return result.toPromise();
    }

    /**
     * @param body
     */
    public testEncodePrimitiveBooleanPostWithHttpInfo(body: boolean, _options?: Configuration): Promise<HttpInfo<void>> {
        const result = this.api.testEncodePrimitiveBooleanPostWithHttpInfo(body, _options);
        return result.toPromise();
    }

    /**
     * @param body
     */
    public testEncodePrimitiveBooleanPost(body: boolean, _options?: Configuration): Promise<void> {
        const result = this.api.testEncodePrimitiveBooleanPost(body, _options);
        return result.toPromise();
    }

    /**
     * @param body
     */
    public testEncodePrimitiveIntegerPostWithHttpInfo(body: number, _options?: Configuration): Promise<HttpInfo<void>> {
        const result = this.api.testEncodePrimitiveIntegerPostWithHttpInfo(body, _options);
        return result.toPromise();
    }

    /**
     * @param body
     */
    public testEncodePrimitiveIntegerPost(body: number, _options?: Configuration): Promise<void> {
        const result = this.api.testEncodePrimitiveIntegerPost(body, _options);
        return result.toPromise();
    }

    /**
     * @param body
     */
    public testEncodePrimitiveNumberPostWithHttpInfo(body: number, _options?: Configuration): Promise<HttpInfo<void>> {
        const result = this.api.testEncodePrimitiveNumberPostWithHttpInfo(body, _options);
        return result.toPromise();
    }

    /**
     * @param body
     */
    public testEncodePrimitiveNumberPost(body: number, _options?: Configuration): Promise<void> {
        const result = this.api.testEncodePrimitiveNumberPost(body, _options);
        return result.toPromise();
    }

    /**
     * @param body
     */
    public testEncodePrimitiveStringPostWithHttpInfo(body: string, _options?: Configuration): Promise<HttpInfo<void>> {
        const result = this.api.testEncodePrimitiveStringPostWithHttpInfo(body, _options);
        return result.toPromise();
    }

    /**
     * @param body
     */
    public testEncodePrimitiveStringPost(body: string, _options?: Configuration): Promise<void> {
        const result = this.api.testEncodePrimitiveStringPost(body, _options);
        return result.toPromise();
    }


}



