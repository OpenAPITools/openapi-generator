import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration, PromiseConfigurationOptions, wrapOptions } from '../configuration'
import { PromiseMiddleware, Middleware, PromiseMiddlewareWrapper } from '../middleware';

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
import { ObservableAuthApi } from './ObservableAPI';

import { AuthApiRequestFactory, AuthApiResponseProcessor} from "../apis/AuthApi";
export class PromiseAuthApi {
    private api: ObservableAuthApi

    public constructor(
        configuration: Configuration,
        requestFactory?: AuthApiRequestFactory,
        responseProcessor?: AuthApiResponseProcessor
    ) {
        this.api = new ObservableAuthApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * To test HTTP basic authentication
     * To test HTTP basic authentication
     */
    public testAuthHttpBasicWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testAuthHttpBasicWithHttpInfo(wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * To test HTTP basic authentication
     * To test HTTP basic authentication
     */
    public testAuthHttpBasic(_options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testAuthHttpBasic(wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * To test HTTP bearer authentication
     * To test HTTP bearer authentication
     */
    public testAuthHttpBearerWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testAuthHttpBearerWithHttpInfo(wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * To test HTTP bearer authentication
     * To test HTTP bearer authentication
     */
    public testAuthHttpBearer(_options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testAuthHttpBearer(wrapOptions(_options));
        return result.toPromise();
    }


}



import { ObservableBodyApi } from './ObservableAPI';

import { BodyApiRequestFactory, BodyApiResponseProcessor} from "../apis/BodyApi";
export class PromiseBodyApi {
    private api: ObservableBodyApi

    public constructor(
        configuration: Configuration,
        requestFactory?: BodyApiRequestFactory,
        responseProcessor?: BodyApiResponseProcessor
    ) {
        this.api = new ObservableBodyApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * Test binary (gif) response body
     * Test binary (gif) response body
     */
    public testBinaryGifWithHttpInfo(_options?: PromiseConfigurationOptions): Promise<HttpInfo<HttpFile>> {
        const result = this.api.testBinaryGifWithHttpInfo(wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test binary (gif) response body
     * Test binary (gif) response body
     */
    public testBinaryGif(_options?: PromiseConfigurationOptions): Promise<HttpFile> {
        const result = this.api.testBinaryGif(wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param [body]
     */
    public testBodyApplicationOctetstreamBinaryWithHttpInfo(body?: HttpFile, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testBodyApplicationOctetstreamBinaryWithHttpInfo(body, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param [body]
     */
    public testBodyApplicationOctetstreamBinary(body?: HttpFile, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testBodyApplicationOctetstreamBinary(body, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test array of binary in multipart mime
     * Test array of binary in multipart mime
     * @param files
     */
    public testBodyMultipartFormdataArrayOfBinaryWithHttpInfo(files: Array<HttpFile>, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testBodyMultipartFormdataArrayOfBinaryWithHttpInfo(files, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test array of binary in multipart mime
     * Test array of binary in multipart mime
     * @param files
     */
    public testBodyMultipartFormdataArrayOfBinary(files: Array<HttpFile>, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testBodyMultipartFormdataArrayOfBinary(files, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test single binary in multipart mime
     * Test single binary in multipart mime
     * @param [myFile]
     */
    public testBodyMultipartFormdataSingleBinaryWithHttpInfo(myFile?: HttpFile, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testBodyMultipartFormdataSingleBinaryWithHttpInfo(myFile, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test single binary in multipart mime
     * Test single binary in multipart mime
     * @param [myFile]
     */
    public testBodyMultipartFormdataSingleBinary(myFile?: HttpFile, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testBodyMultipartFormdataSingleBinary(myFile, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param [pet] Pet object that needs to be added to the store
     */
    public testEchoBodyAllOfPetWithHttpInfo(pet?: Pet, _options?: PromiseConfigurationOptions): Promise<HttpInfo<Pet>> {
        const result = this.api.testEchoBodyAllOfPetWithHttpInfo(pet, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param [pet] Pet object that needs to be added to the store
     */
    public testEchoBodyAllOfPet(pet?: Pet, _options?: PromiseConfigurationOptions): Promise<Pet> {
        const result = this.api.testEchoBodyAllOfPet(pet, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test free form object
     * Test free form object
     * @param [body] Free form object
     */
    public testEchoBodyFreeFormObjectResponseStringWithHttpInfo(body?: any, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testEchoBodyFreeFormObjectResponseStringWithHttpInfo(body, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test free form object
     * Test free form object
     * @param [body] Free form object
     */
    public testEchoBodyFreeFormObjectResponseString(body?: any, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testEchoBodyFreeFormObjectResponseString(body, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param [pet] Pet object that needs to be added to the store
     */
    public testEchoBodyPetWithHttpInfo(pet?: Pet, _options?: PromiseConfigurationOptions): Promise<HttpInfo<Pet>> {
        const result = this.api.testEchoBodyPetWithHttpInfo(pet, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param [pet] Pet object that needs to be added to the store
     */
    public testEchoBodyPet(pet?: Pet, _options?: PromiseConfigurationOptions): Promise<Pet> {
        const result = this.api.testEchoBodyPet(pet, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test empty response body
     * Test empty response body
     * @param [pet] Pet object that needs to be added to the store
     */
    public testEchoBodyPetResponseStringWithHttpInfo(pet?: Pet, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testEchoBodyPetResponseStringWithHttpInfo(pet, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test empty response body
     * Test empty response body
     * @param [pet] Pet object that needs to be added to the store
     */
    public testEchoBodyPetResponseString(pet?: Pet, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testEchoBodyPetResponseString(pet, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test string enum response body
     * Test string enum response body
     * @param [body] String enum
     */
    public testEchoBodyStringEnumWithHttpInfo(body?: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<StringEnumRef>> {
        const result = this.api.testEchoBodyStringEnumWithHttpInfo(body, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test string enum response body
     * Test string enum response body
     * @param [body] String enum
     */
    public testEchoBodyStringEnum(body?: string, _options?: PromiseConfigurationOptions): Promise<StringEnumRef> {
        const result = this.api.testEchoBodyStringEnum(body, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test empty json (request body)
     * Test empty json (request body)
     * @param [tag] Tag object
     */
    public testEchoBodyTagResponseStringWithHttpInfo(tag?: Tag, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testEchoBodyTagResponseStringWithHttpInfo(tag, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test empty json (request body)
     * Test empty json (request body)
     * @param [tag] Tag object
     */
    public testEchoBodyTagResponseString(tag?: Tag, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testEchoBodyTagResponseString(tag, wrapOptions(_options));
        return result.toPromise();
    }


}



import { ObservableFormApi } from './ObservableAPI';

import { FormApiRequestFactory, FormApiResponseProcessor} from "../apis/FormApi";
export class PromiseFormApi {
    private api: ObservableFormApi

    public constructor(
        configuration: Configuration,
        requestFactory?: FormApiRequestFactory,
        responseProcessor?: FormApiResponseProcessor
    ) {
        this.api = new ObservableFormApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * Test form parameter(s)
     * Test form parameter(s)
     * @param [integerForm]
     * @param [booleanForm]
     * @param [stringForm]
     */
    public testFormIntegerBooleanStringWithHttpInfo(integerForm?: number, booleanForm?: boolean, stringForm?: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testFormIntegerBooleanStringWithHttpInfo(integerForm, booleanForm, stringForm, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test form parameter(s)
     * Test form parameter(s)
     * @param [integerForm]
     * @param [booleanForm]
     * @param [stringForm]
     */
    public testFormIntegerBooleanString(integerForm?: number, booleanForm?: boolean, stringForm?: string, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testFormIntegerBooleanString(integerForm, booleanForm, stringForm, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test form parameter(s) for multipart schema
     * Test form parameter(s) for multipart schema
     * @param marker
     */
    public testFormObjectMultipartWithHttpInfo(marker: TestFormObjectMultipartRequestMarker, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testFormObjectMultipartWithHttpInfo(marker, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test form parameter(s) for multipart schema
     * Test form parameter(s) for multipart schema
     * @param marker
     */
    public testFormObjectMultipart(marker: TestFormObjectMultipartRequestMarker, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testFormObjectMultipart(marker, wrapOptions(_options));
        return result.toPromise();
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
    public testFormOneofWithHttpInfo(form1?: string, form2?: number, form3?: string, form4?: boolean, id?: number, name?: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testFormOneofWithHttpInfo(form1, form2, form3, form4, id, name, wrapOptions(_options));
        return result.toPromise();
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
    public testFormOneof(form1?: string, form2?: number, form3?: string, form4?: boolean, id?: number, name?: string, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testFormOneof(form1, form2, form3, form4, id, name, wrapOptions(_options));
        return result.toPromise();
    }


}



import { ObservableHeaderApi } from './ObservableAPI';

import { HeaderApiRequestFactory, HeaderApiResponseProcessor} from "../apis/HeaderApi";
export class PromiseHeaderApi {
    private api: ObservableHeaderApi

    public constructor(
        configuration: Configuration,
        requestFactory?: HeaderApiRequestFactory,
        responseProcessor?: HeaderApiResponseProcessor
    ) {
        this.api = new ObservableHeaderApi(configuration, requestFactory, responseProcessor);
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
    public testHeaderIntegerBooleanStringEnumsWithHttpInfo(integerHeader?: number, booleanHeader?: boolean, stringHeader?: string, enumNonrefStringHeader?: 'success' | 'failure' | 'unclassified', enumRefStringHeader?: StringEnumRef, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testHeaderIntegerBooleanStringEnumsWithHttpInfo(integerHeader, booleanHeader, stringHeader, enumNonrefStringHeader, enumRefStringHeader, wrapOptions(_options));
        return result.toPromise();
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
    public testHeaderIntegerBooleanStringEnums(integerHeader?: number, booleanHeader?: boolean, stringHeader?: string, enumNonrefStringHeader?: 'success' | 'failure' | 'unclassified', enumRefStringHeader?: StringEnumRef, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testHeaderIntegerBooleanStringEnums(integerHeader, booleanHeader, stringHeader, enumNonrefStringHeader, enumRefStringHeader, wrapOptions(_options));
        return result.toPromise();
    }


}



import { ObservablePathApi } from './ObservableAPI';

import { PathApiRequestFactory, PathApiResponseProcessor} from "../apis/PathApi";
export class PromisePathApi {
    private api: ObservablePathApi

    public constructor(
        configuration: Configuration,
        requestFactory?: PathApiRequestFactory,
        responseProcessor?: PathApiResponseProcessor
    ) {
        this.api = new ObservablePathApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * Test path parameter(s)
     * Test path parameter(s)
     * @param pathString
     * @param pathInteger
     * @param enumNonrefStringPath
     * @param enumRefStringPath
     */
    public testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathWithHttpInfo(pathString: string, pathInteger: number, enumNonrefStringPath: 'success' | 'failure' | 'unclassified', enumRefStringPath: StringEnumRef, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathWithHttpInfo(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test path parameter(s)
     * Test path parameter(s)
     * @param pathString
     * @param pathInteger
     * @param enumNonrefStringPath
     * @param enumRefStringPath
     */
    public testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(pathString: string, pathInteger: number, enumNonrefStringPath: 'success' | 'failure' | 'unclassified', enumRefStringPath: StringEnumRef, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath, wrapOptions(_options));
        return result.toPromise();
    }


}



import { ObservableQueryApi } from './ObservableAPI';

import { QueryApiRequestFactory, QueryApiResponseProcessor} from "../apis/QueryApi";
export class PromiseQueryApi {
    private api: ObservableQueryApi

    public constructor(
        configuration: Configuration,
        requestFactory?: QueryApiRequestFactory,
        responseProcessor?: QueryApiResponseProcessor
    ) {
        this.api = new ObservableQueryApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [enumNonrefStringQuery]
     * @param [enumRefStringQuery]
     */
    public testEnumRefStringWithHttpInfo(enumNonrefStringQuery?: 'success' | 'failure' | 'unclassified', enumRefStringQuery?: StringEnumRef, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testEnumRefStringWithHttpInfo(enumNonrefStringQuery, enumRefStringQuery, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [enumNonrefStringQuery]
     * @param [enumRefStringQuery]
     */
    public testEnumRefString(enumNonrefStringQuery?: 'success' | 'failure' | 'unclassified', enumRefStringQuery?: StringEnumRef, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testEnumRefString(enumNonrefStringQuery, enumRefStringQuery, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [datetimeQuery]
     * @param [dateQuery]
     * @param [stringQuery]
     */
    public testQueryDatetimeDateStringWithHttpInfo(datetimeQuery?: Date, dateQuery?: string, stringQuery?: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testQueryDatetimeDateStringWithHttpInfo(datetimeQuery, dateQuery, stringQuery, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [datetimeQuery]
     * @param [dateQuery]
     * @param [stringQuery]
     */
    public testQueryDatetimeDateString(datetimeQuery?: Date, dateQuery?: string, stringQuery?: string, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testQueryDatetimeDateString(datetimeQuery, dateQuery, stringQuery, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [integerQuery]
     * @param [booleanQuery]
     * @param [stringQuery]
     */
    public testQueryIntegerBooleanStringWithHttpInfo(integerQuery?: number, booleanQuery?: boolean, stringQuery?: string, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testQueryIntegerBooleanStringWithHttpInfo(integerQuery, booleanQuery, stringQuery, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [integerQuery]
     * @param [booleanQuery]
     * @param [stringQuery]
     */
    public testQueryIntegerBooleanString(integerQuery?: number, booleanQuery?: boolean, stringQuery?: string, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testQueryIntegerBooleanString(integerQuery, booleanQuery, stringQuery, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo(queryObject?: Pet, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo(queryObject, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleDeepObjectExplodeTrueObject(queryObject?: Pet, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testQueryStyleDeepObjectExplodeTrueObject(queryObject, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo(queryObject?: TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo(queryObject, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleDeepObjectExplodeTrueObjectAllOf(queryObject?: TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testQueryStyleDeepObjectExplodeTrueObjectAllOf(queryObject, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeFalseArrayIntegerWithHttpInfo(queryObject?: Array<number>, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testQueryStyleFormExplodeFalseArrayIntegerWithHttpInfo(queryObject, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeFalseArrayInteger(queryObject?: Array<number>, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testQueryStyleFormExplodeFalseArrayInteger(queryObject, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeFalseArrayStringWithHttpInfo(queryObject?: Array<string>, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testQueryStyleFormExplodeFalseArrayStringWithHttpInfo(queryObject, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeFalseArrayString(queryObject?: Array<string>, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testQueryStyleFormExplodeFalseArrayString(queryObject, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(queryObject?: TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(queryObject, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeTrueArrayString(queryObject?: TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testQueryStyleFormExplodeTrueArrayString(queryObject, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeTrueObjectWithHttpInfo(queryObject?: Pet, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testQueryStyleFormExplodeTrueObjectWithHttpInfo(queryObject, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeTrueObject(queryObject?: Pet, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testQueryStyleFormExplodeTrueObject(queryObject, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo(queryObject?: DataQuery, _options?: PromiseConfigurationOptions): Promise<HttpInfo<string>> {
        const result = this.api.testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo(queryObject, wrapOptions(_options));
        return result.toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param [queryObject]
     */
    public testQueryStyleFormExplodeTrueObjectAllOf(queryObject?: DataQuery, _options?: PromiseConfigurationOptions): Promise<string> {
        const result = this.api.testQueryStyleFormExplodeTrueObjectAllOf(queryObject, wrapOptions(_options));
        return result.toPromise();
    }


}



