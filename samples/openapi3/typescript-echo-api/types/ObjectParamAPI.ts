import { ResponseContext, RequestContext, HttpFile, HttpInfo } from '../http/http';
import { Configuration, ConfigurationOptions } from '../configuration'
import type { Middleware } from '../middleware';

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

import { ObservableAuthApi } from "./ObservableAPI";
import { AuthApiRequestFactory, AuthApiResponseProcessor} from "../apis/AuthApi";

export interface AuthApiTestAuthHttpBasicRequest {
}

export interface AuthApiTestAuthHttpBearerRequest {
}

export class ObjectAuthApi {
    private api: ObservableAuthApi

    public constructor(configuration: Configuration, requestFactory?: AuthApiRequestFactory, responseProcessor?: AuthApiResponseProcessor) {
        this.api = new ObservableAuthApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * To test HTTP basic authentication
     * To test HTTP basic authentication
     * @param param the request object
     */
    public testAuthHttpBasicWithHttpInfo(param: AuthApiTestAuthHttpBasicRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testAuthHttpBasicWithHttpInfo( options).toPromise();
    }

    /**
     * To test HTTP basic authentication
     * To test HTTP basic authentication
     * @param param the request object
     */
    public testAuthHttpBasic(param: AuthApiTestAuthHttpBasicRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testAuthHttpBasic( options).toPromise();
    }

    /**
     * To test HTTP bearer authentication
     * To test HTTP bearer authentication
     * @param param the request object
     */
    public testAuthHttpBearerWithHttpInfo(param: AuthApiTestAuthHttpBearerRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testAuthHttpBearerWithHttpInfo( options).toPromise();
    }

    /**
     * To test HTTP bearer authentication
     * To test HTTP bearer authentication
     * @param param the request object
     */
    public testAuthHttpBearer(param: AuthApiTestAuthHttpBearerRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testAuthHttpBearer( options).toPromise();
    }

}

import { ObservableBodyApi } from "./ObservableAPI";
import { BodyApiRequestFactory, BodyApiResponseProcessor} from "../apis/BodyApi";

export interface BodyApiTestBinaryGifRequest {
}

export interface BodyApiTestBodyApplicationOctetstreamBinaryRequest {
    /**
     * 
     * @type HttpFile
     * @memberof BodyApitestBodyApplicationOctetstreamBinary
     */
    body?: HttpFile
}

export interface BodyApiTestBodyMultipartFormdataArrayOfBinaryRequest {
    /**
     * 
     * Defaults to: undefined
     * @type Array&lt;HttpFile&gt;
     * @memberof BodyApitestBodyMultipartFormdataArrayOfBinary
     */
    files: Array<HttpFile>
}

export interface BodyApiTestBodyMultipartFormdataSingleBinaryRequest {
    /**
     * 
     * Defaults to: undefined
     * @type HttpFile
     * @memberof BodyApitestBodyMultipartFormdataSingleBinary
     */
    myFile?: HttpFile
}

export interface BodyApiTestEchoBodyAllOfPetRequest {
    /**
     * Pet object that needs to be added to the store
     * @type Pet
     * @memberof BodyApitestEchoBodyAllOfPet
     */
    pet?: Pet
}

export interface BodyApiTestEchoBodyFreeFormObjectResponseStringRequest {
    /**
     * Free form object
     * @type any
     * @memberof BodyApitestEchoBodyFreeFormObjectResponseString
     */
    body?: any
}

export interface BodyApiTestEchoBodyPetRequest {
    /**
     * Pet object that needs to be added to the store
     * @type Pet
     * @memberof BodyApitestEchoBodyPet
     */
    pet?: Pet
}

export interface BodyApiTestEchoBodyPetResponseStringRequest {
    /**
     * Pet object that needs to be added to the store
     * @type Pet
     * @memberof BodyApitestEchoBodyPetResponseString
     */
    pet?: Pet
}

export interface BodyApiTestEchoBodyStringEnumRequest {
    /**
     * String enum
     * @type string
     * @memberof BodyApitestEchoBodyStringEnum
     */
    body?: string
}

export interface BodyApiTestEchoBodyTagResponseStringRequest {
    /**
     * Tag object
     * @type Tag
     * @memberof BodyApitestEchoBodyTagResponseString
     */
    tag?: Tag
}

export class ObjectBodyApi {
    private api: ObservableBodyApi

    public constructor(configuration: Configuration, requestFactory?: BodyApiRequestFactory, responseProcessor?: BodyApiResponseProcessor) {
        this.api = new ObservableBodyApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * Test binary (gif) response body
     * Test binary (gif) response body
     * @param param the request object
     */
    public testBinaryGifWithHttpInfo(param: BodyApiTestBinaryGifRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<HttpFile>> {
        return this.api.testBinaryGifWithHttpInfo( options).toPromise();
    }

    /**
     * Test binary (gif) response body
     * Test binary (gif) response body
     * @param param the request object
     */
    public testBinaryGif(param: BodyApiTestBinaryGifRequest = {}, options?: ConfigurationOptions): Promise<HttpFile> {
        return this.api.testBinaryGif( options).toPromise();
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param param the request object
     */
    public testBodyApplicationOctetstreamBinaryWithHttpInfo(param: BodyApiTestBodyApplicationOctetstreamBinaryRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testBodyApplicationOctetstreamBinaryWithHttpInfo(param.body,  options).toPromise();
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param param the request object
     */
    public testBodyApplicationOctetstreamBinary(param: BodyApiTestBodyApplicationOctetstreamBinaryRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testBodyApplicationOctetstreamBinary(param.body,  options).toPromise();
    }

    /**
     * Test array of binary in multipart mime
     * Test array of binary in multipart mime
     * @param param the request object
     */
    public testBodyMultipartFormdataArrayOfBinaryWithHttpInfo(param: BodyApiTestBodyMultipartFormdataArrayOfBinaryRequest, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testBodyMultipartFormdataArrayOfBinaryWithHttpInfo(param.files,  options).toPromise();
    }

    /**
     * Test array of binary in multipart mime
     * Test array of binary in multipart mime
     * @param param the request object
     */
    public testBodyMultipartFormdataArrayOfBinary(param: BodyApiTestBodyMultipartFormdataArrayOfBinaryRequest, options?: ConfigurationOptions): Promise<string> {
        return this.api.testBodyMultipartFormdataArrayOfBinary(param.files,  options).toPromise();
    }

    /**
     * Test single binary in multipart mime
     * Test single binary in multipart mime
     * @param param the request object
     */
    public testBodyMultipartFormdataSingleBinaryWithHttpInfo(param: BodyApiTestBodyMultipartFormdataSingleBinaryRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testBodyMultipartFormdataSingleBinaryWithHttpInfo(param.myFile,  options).toPromise();
    }

    /**
     * Test single binary in multipart mime
     * Test single binary in multipart mime
     * @param param the request object
     */
    public testBodyMultipartFormdataSingleBinary(param: BodyApiTestBodyMultipartFormdataSingleBinaryRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testBodyMultipartFormdataSingleBinary(param.myFile,  options).toPromise();
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param param the request object
     */
    public testEchoBodyAllOfPetWithHttpInfo(param: BodyApiTestEchoBodyAllOfPetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<Pet>> {
        return this.api.testEchoBodyAllOfPetWithHttpInfo(param.pet,  options).toPromise();
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param param the request object
     */
    public testEchoBodyAllOfPet(param: BodyApiTestEchoBodyAllOfPetRequest = {}, options?: ConfigurationOptions): Promise<Pet> {
        return this.api.testEchoBodyAllOfPet(param.pet,  options).toPromise();
    }

    /**
     * Test free form object
     * Test free form object
     * @param param the request object
     */
    public testEchoBodyFreeFormObjectResponseStringWithHttpInfo(param: BodyApiTestEchoBodyFreeFormObjectResponseStringRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testEchoBodyFreeFormObjectResponseStringWithHttpInfo(param.body,  options).toPromise();
    }

    /**
     * Test free form object
     * Test free form object
     * @param param the request object
     */
    public testEchoBodyFreeFormObjectResponseString(param: BodyApiTestEchoBodyFreeFormObjectResponseStringRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testEchoBodyFreeFormObjectResponseString(param.body,  options).toPromise();
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param param the request object
     */
    public testEchoBodyPetWithHttpInfo(param: BodyApiTestEchoBodyPetRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<Pet>> {
        return this.api.testEchoBodyPetWithHttpInfo(param.pet,  options).toPromise();
    }

    /**
     * Test body parameter(s)
     * Test body parameter(s)
     * @param param the request object
     */
    public testEchoBodyPet(param: BodyApiTestEchoBodyPetRequest = {}, options?: ConfigurationOptions): Promise<Pet> {
        return this.api.testEchoBodyPet(param.pet,  options).toPromise();
    }

    /**
     * Test empty response body
     * Test empty response body
     * @param param the request object
     */
    public testEchoBodyPetResponseStringWithHttpInfo(param: BodyApiTestEchoBodyPetResponseStringRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testEchoBodyPetResponseStringWithHttpInfo(param.pet,  options).toPromise();
    }

    /**
     * Test empty response body
     * Test empty response body
     * @param param the request object
     */
    public testEchoBodyPetResponseString(param: BodyApiTestEchoBodyPetResponseStringRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testEchoBodyPetResponseString(param.pet,  options).toPromise();
    }

    /**
     * Test string enum response body
     * Test string enum response body
     * @param param the request object
     */
    public testEchoBodyStringEnumWithHttpInfo(param: BodyApiTestEchoBodyStringEnumRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<StringEnumRef>> {
        return this.api.testEchoBodyStringEnumWithHttpInfo(param.body,  options).toPromise();
    }

    /**
     * Test string enum response body
     * Test string enum response body
     * @param param the request object
     */
    public testEchoBodyStringEnum(param: BodyApiTestEchoBodyStringEnumRequest = {}, options?: ConfigurationOptions): Promise<StringEnumRef> {
        return this.api.testEchoBodyStringEnum(param.body,  options).toPromise();
    }

    /**
     * Test empty json (request body)
     * Test empty json (request body)
     * @param param the request object
     */
    public testEchoBodyTagResponseStringWithHttpInfo(param: BodyApiTestEchoBodyTagResponseStringRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testEchoBodyTagResponseStringWithHttpInfo(param.tag,  options).toPromise();
    }

    /**
     * Test empty json (request body)
     * Test empty json (request body)
     * @param param the request object
     */
    public testEchoBodyTagResponseString(param: BodyApiTestEchoBodyTagResponseStringRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testEchoBodyTagResponseString(param.tag,  options).toPromise();
    }

}

import { ObservableFormApi } from "./ObservableAPI";
import { FormApiRequestFactory, FormApiResponseProcessor} from "../apis/FormApi";

export interface FormApiTestFormIntegerBooleanStringRequest {
    /**
     * 
     * Defaults to: undefined
     * @type number
     * @memberof FormApitestFormIntegerBooleanString
     */
    integerForm?: number
    /**
     * 
     * Defaults to: undefined
     * @type boolean
     * @memberof FormApitestFormIntegerBooleanString
     */
    booleanForm?: boolean
    /**
     * 
     * Defaults to: undefined
     * @type string
     * @memberof FormApitestFormIntegerBooleanString
     */
    stringForm?: string
}

export interface FormApiTestFormObjectMultipartRequest {
    /**
     * 
     * Defaults to: undefined
     * @type TestFormObjectMultipartRequestMarker
     * @memberof FormApitestFormObjectMultipart
     */
    marker: TestFormObjectMultipartRequestMarker
}

export interface FormApiTestFormOneofRequest {
    /**
     * 
     * Defaults to: undefined
     * @type string
     * @memberof FormApitestFormOneof
     */
    form1?: string
    /**
     * 
     * Defaults to: undefined
     * @type number
     * @memberof FormApitestFormOneof
     */
    form2?: number
    /**
     * 
     * Defaults to: undefined
     * @type string
     * @memberof FormApitestFormOneof
     */
    form3?: string
    /**
     * 
     * Defaults to: undefined
     * @type boolean
     * @memberof FormApitestFormOneof
     */
    form4?: boolean
    /**
     * 
     * Defaults to: undefined
     * @type number
     * @memberof FormApitestFormOneof
     */
    id?: number
    /**
     * 
     * Defaults to: undefined
     * @type string
     * @memberof FormApitestFormOneof
     */
    name?: string
}

export class ObjectFormApi {
    private api: ObservableFormApi

    public constructor(configuration: Configuration, requestFactory?: FormApiRequestFactory, responseProcessor?: FormApiResponseProcessor) {
        this.api = new ObservableFormApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * Test form parameter(s)
     * Test form parameter(s)
     * @param param the request object
     */
    public testFormIntegerBooleanStringWithHttpInfo(param: FormApiTestFormIntegerBooleanStringRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testFormIntegerBooleanStringWithHttpInfo(param.integerForm, param.booleanForm, param.stringForm,  options).toPromise();
    }

    /**
     * Test form parameter(s)
     * Test form parameter(s)
     * @param param the request object
     */
    public testFormIntegerBooleanString(param: FormApiTestFormIntegerBooleanStringRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testFormIntegerBooleanString(param.integerForm, param.booleanForm, param.stringForm,  options).toPromise();
    }

    /**
     * Test form parameter(s) for multipart schema
     * Test form parameter(s) for multipart schema
     * @param param the request object
     */
    public testFormObjectMultipartWithHttpInfo(param: FormApiTestFormObjectMultipartRequest, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testFormObjectMultipartWithHttpInfo(param.marker,  options).toPromise();
    }

    /**
     * Test form parameter(s) for multipart schema
     * Test form parameter(s) for multipart schema
     * @param param the request object
     */
    public testFormObjectMultipart(param: FormApiTestFormObjectMultipartRequest, options?: ConfigurationOptions): Promise<string> {
        return this.api.testFormObjectMultipart(param.marker,  options).toPromise();
    }

    /**
     * Test form parameter(s) for oneOf schema
     * Test form parameter(s) for oneOf schema
     * @param param the request object
     */
    public testFormOneofWithHttpInfo(param: FormApiTestFormOneofRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testFormOneofWithHttpInfo(param.form1, param.form2, param.form3, param.form4, param.id, param.name,  options).toPromise();
    }

    /**
     * Test form parameter(s) for oneOf schema
     * Test form parameter(s) for oneOf schema
     * @param param the request object
     */
    public testFormOneof(param: FormApiTestFormOneofRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testFormOneof(param.form1, param.form2, param.form3, param.form4, param.id, param.name,  options).toPromise();
    }

}

import { ObservableHeaderApi } from "./ObservableAPI";
import { HeaderApiRequestFactory, HeaderApiResponseProcessor} from "../apis/HeaderApi";

export interface HeaderApiTestHeaderIntegerBooleanStringEnumsRequest {
    /**
     * 
     * Defaults to: undefined
     * @type number
     * @memberof HeaderApitestHeaderIntegerBooleanStringEnums
     */
    integerHeader?: number
    /**
     * 
     * Defaults to: undefined
     * @type boolean
     * @memberof HeaderApitestHeaderIntegerBooleanStringEnums
     */
    booleanHeader?: boolean
    /**
     * 
     * Defaults to: undefined
     * @type string
     * @memberof HeaderApitestHeaderIntegerBooleanStringEnums
     */
    stringHeader?: string
    /**
     * 
     * Defaults to: undefined
     * @type &#39;success&#39; | &#39;failure&#39; | &#39;unclassified&#39;
     * @memberof HeaderApitestHeaderIntegerBooleanStringEnums
     */
    enumNonrefStringHeader?: 'success' | 'failure' | 'unclassified'
    /**
     * 
     * Defaults to: undefined
     * @type StringEnumRef
     * @memberof HeaderApitestHeaderIntegerBooleanStringEnums
     */
    enumRefStringHeader?: StringEnumRef
}

export class ObjectHeaderApi {
    private api: ObservableHeaderApi

    public constructor(configuration: Configuration, requestFactory?: HeaderApiRequestFactory, responseProcessor?: HeaderApiResponseProcessor) {
        this.api = new ObservableHeaderApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * Test header parameter(s)
     * Test header parameter(s)
     * @param param the request object
     */
    public testHeaderIntegerBooleanStringEnumsWithHttpInfo(param: HeaderApiTestHeaderIntegerBooleanStringEnumsRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testHeaderIntegerBooleanStringEnumsWithHttpInfo(param.integerHeader, param.booleanHeader, param.stringHeader, param.enumNonrefStringHeader, param.enumRefStringHeader,  options).toPromise();
    }

    /**
     * Test header parameter(s)
     * Test header parameter(s)
     * @param param the request object
     */
    public testHeaderIntegerBooleanStringEnums(param: HeaderApiTestHeaderIntegerBooleanStringEnumsRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testHeaderIntegerBooleanStringEnums(param.integerHeader, param.booleanHeader, param.stringHeader, param.enumNonrefStringHeader, param.enumRefStringHeader,  options).toPromise();
    }

}

import { ObservablePathApi } from "./ObservableAPI";
import { PathApiRequestFactory, PathApiResponseProcessor} from "../apis/PathApi";

export interface PathApiTestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathRequest {
    /**
     * 
     * Defaults to: undefined
     * @type string
     * @memberof PathApitestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath
     */
    pathString: string
    /**
     * 
     * Defaults to: undefined
     * @type number
     * @memberof PathApitestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath
     */
    pathInteger: number
    /**
     * 
     * Defaults to: undefined
     * @type &#39;success&#39; | &#39;failure&#39; | &#39;unclassified&#39;
     * @memberof PathApitestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath
     */
    enumNonrefStringPath: 'success' | 'failure' | 'unclassified'
    /**
     * 
     * Defaults to: undefined
     * @type StringEnumRef
     * @memberof PathApitestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath
     */
    enumRefStringPath: StringEnumRef
}

export class ObjectPathApi {
    private api: ObservablePathApi

    public constructor(configuration: Configuration, requestFactory?: PathApiRequestFactory, responseProcessor?: PathApiResponseProcessor) {
        this.api = new ObservablePathApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * Test path parameter(s)
     * Test path parameter(s)
     * @param param the request object
     */
    public testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathWithHttpInfo(param: PathApiTestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathRequest, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathWithHttpInfo(param.pathString, param.pathInteger, param.enumNonrefStringPath, param.enumRefStringPath,  options).toPromise();
    }

    /**
     * Test path parameter(s)
     * Test path parameter(s)
     * @param param the request object
     */
    public testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(param: PathApiTestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPathRequest, options?: ConfigurationOptions): Promise<string> {
        return this.api.testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(param.pathString, param.pathInteger, param.enumNonrefStringPath, param.enumRefStringPath,  options).toPromise();
    }

}

import { ObservableQueryApi } from "./ObservableAPI";
import { QueryApiRequestFactory, QueryApiResponseProcessor} from "../apis/QueryApi";

export interface QueryApiDeprecatedTestRequest {
    /**
     * name of pet
     * Defaults to: undefined
     * @type string
     * @memberof QueryApideprecatedTest
     */
    name?: string
}

export interface QueryApiTestEnumRefStringRequest {
    /**
     * 
     * Defaults to: undefined
     * @type &#39;success&#39; | &#39;failure&#39; | &#39;unclassified&#39;
     * @memberof QueryApitestEnumRefString
     */
    enumNonrefStringQuery?: 'success' | 'failure' | 'unclassified'
    /**
     * 
     * Defaults to: undefined
     * @type StringEnumRef
     * @memberof QueryApitestEnumRefString
     */
    enumRefStringQuery?: StringEnumRef
}

export interface QueryApiTestQueryDatetimeDateStringRequest {
    /**
     * 
     * Defaults to: undefined
     * @type Date
     * @memberof QueryApitestQueryDatetimeDateString
     */
    datetimeQuery?: Date
    /**
     * 
     * Defaults to: undefined
     * @type string
     * @memberof QueryApitestQueryDatetimeDateString
     */
    dateQuery?: string
    /**
     * 
     * Defaults to: undefined
     * @type string
     * @memberof QueryApitestQueryDatetimeDateString
     */
    stringQuery?: string
}

export interface QueryApiTestQueryIntegerBooleanStringRequest {
    /**
     * 
     * Defaults to: undefined
     * @type number
     * @memberof QueryApitestQueryIntegerBooleanString
     */
    integerQuery?: number
    /**
     * 
     * Defaults to: undefined
     * @type boolean
     * @memberof QueryApitestQueryIntegerBooleanString
     */
    booleanQuery?: boolean
    /**
     * 
     * Defaults to: undefined
     * @type string
     * @memberof QueryApitestQueryIntegerBooleanString
     */
    stringQuery?: string
}

export interface QueryApiTestQueryStyleDeepObjectExplodeTrueObjectRequest {
    /**
     * 
     * Defaults to: undefined
     * @type Pet
     * @memberof QueryApitestQueryStyleDeepObjectExplodeTrueObject
     */
    queryObject?: Pet
}

export interface QueryApiTestQueryStyleDeepObjectExplodeTrueObjectAllOfRequest {
    /**
     * 
     * Defaults to: undefined
     * @type TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
     * @memberof QueryApitestQueryStyleDeepObjectExplodeTrueObjectAllOf
     */
    queryObject?: TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter
}

export interface QueryApiTestQueryStyleFormExplodeFalseArrayIntegerRequest {
    /**
     * 
     * Defaults to: undefined
     * @type Array&lt;number&gt;
     * @memberof QueryApitestQueryStyleFormExplodeFalseArrayInteger
     */
    queryObject?: Array<number>
}

export interface QueryApiTestQueryStyleFormExplodeFalseArrayStringRequest {
    /**
     * 
     * Defaults to: undefined
     * @type Array&lt;string&gt;
     * @memberof QueryApitestQueryStyleFormExplodeFalseArrayString
     */
    queryObject?: Array<string>
}

export interface QueryApiTestQueryStyleFormExplodeTrueArrayStringRequest {
    /**
     * 
     * Defaults to: undefined
     * @type TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
     * @memberof QueryApitestQueryStyleFormExplodeTrueArrayString
     */
    queryObject?: TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
}

export interface QueryApiTestQueryStyleFormExplodeTrueObjectRequest {
    /**
     * 
     * Defaults to: undefined
     * @type Pet
     * @memberof QueryApitestQueryStyleFormExplodeTrueObject
     */
    queryObject?: Pet
}

export interface QueryApiTestQueryStyleFormExplodeTrueObjectAllOfRequest {
    /**
     * 
     * Defaults to: undefined
     * @type DataQuery
     * @memberof QueryApitestQueryStyleFormExplodeTrueObjectAllOf
     */
    queryObject?: DataQuery
}

export class ObjectQueryApi {
    private api: ObservableQueryApi

    public constructor(configuration: Configuration, requestFactory?: QueryApiRequestFactory, responseProcessor?: QueryApiResponseProcessor) {
        this.api = new ObservableQueryApi(configuration, requestFactory, responseProcessor);
    }

    /**
     * Test deprecation
     * @param param the request object
     */
    public deprecatedTestWithHttpInfo(param: QueryApiDeprecatedTestRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.deprecatedTestWithHttpInfo(param.name,  options).toPromise();
    }

    /**
     * Test deprecation
     * @param param the request object
     */
    public deprecatedTest(param: QueryApiDeprecatedTestRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.deprecatedTest(param.name,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testEnumRefStringWithHttpInfo(param: QueryApiTestEnumRefStringRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testEnumRefStringWithHttpInfo(param.enumNonrefStringQuery, param.enumRefStringQuery,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testEnumRefString(param: QueryApiTestEnumRefStringRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testEnumRefString(param.enumNonrefStringQuery, param.enumRefStringQuery,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryDatetimeDateStringWithHttpInfo(param: QueryApiTestQueryDatetimeDateStringRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testQueryDatetimeDateStringWithHttpInfo(param.datetimeQuery, param.dateQuery, param.stringQuery,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryDatetimeDateString(param: QueryApiTestQueryDatetimeDateStringRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testQueryDatetimeDateString(param.datetimeQuery, param.dateQuery, param.stringQuery,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryIntegerBooleanStringWithHttpInfo(param: QueryApiTestQueryIntegerBooleanStringRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testQueryIntegerBooleanStringWithHttpInfo(param.integerQuery, param.booleanQuery, param.stringQuery,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryIntegerBooleanString(param: QueryApiTestQueryIntegerBooleanStringRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testQueryIntegerBooleanString(param.integerQuery, param.booleanQuery, param.stringQuery,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo(param: QueryApiTestQueryStyleDeepObjectExplodeTrueObjectRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testQueryStyleDeepObjectExplodeTrueObjectWithHttpInfo(param.queryObject,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryStyleDeepObjectExplodeTrueObject(param: QueryApiTestQueryStyleDeepObjectExplodeTrueObjectRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testQueryStyleDeepObjectExplodeTrueObject(param.queryObject,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo(param: QueryApiTestQueryStyleDeepObjectExplodeTrueObjectAllOfRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testQueryStyleDeepObjectExplodeTrueObjectAllOfWithHttpInfo(param.queryObject,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryStyleDeepObjectExplodeTrueObjectAllOf(param: QueryApiTestQueryStyleDeepObjectExplodeTrueObjectAllOfRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testQueryStyleDeepObjectExplodeTrueObjectAllOf(param.queryObject,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryStyleFormExplodeFalseArrayIntegerWithHttpInfo(param: QueryApiTestQueryStyleFormExplodeFalseArrayIntegerRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testQueryStyleFormExplodeFalseArrayIntegerWithHttpInfo(param.queryObject,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryStyleFormExplodeFalseArrayInteger(param: QueryApiTestQueryStyleFormExplodeFalseArrayIntegerRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testQueryStyleFormExplodeFalseArrayInteger(param.queryObject,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryStyleFormExplodeFalseArrayStringWithHttpInfo(param: QueryApiTestQueryStyleFormExplodeFalseArrayStringRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testQueryStyleFormExplodeFalseArrayStringWithHttpInfo(param.queryObject,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryStyleFormExplodeFalseArrayString(param: QueryApiTestQueryStyleFormExplodeFalseArrayStringRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testQueryStyleFormExplodeFalseArrayString(param.queryObject,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(param: QueryApiTestQueryStyleFormExplodeTrueArrayStringRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testQueryStyleFormExplodeTrueArrayStringWithHttpInfo(param.queryObject,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryStyleFormExplodeTrueArrayString(param: QueryApiTestQueryStyleFormExplodeTrueArrayStringRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testQueryStyleFormExplodeTrueArrayString(param.queryObject,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryStyleFormExplodeTrueObjectWithHttpInfo(param: QueryApiTestQueryStyleFormExplodeTrueObjectRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testQueryStyleFormExplodeTrueObjectWithHttpInfo(param.queryObject,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryStyleFormExplodeTrueObject(param: QueryApiTestQueryStyleFormExplodeTrueObjectRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testQueryStyleFormExplodeTrueObject(param.queryObject,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo(param: QueryApiTestQueryStyleFormExplodeTrueObjectAllOfRequest = {}, options?: ConfigurationOptions): Promise<HttpInfo<string>> {
        return this.api.testQueryStyleFormExplodeTrueObjectAllOfWithHttpInfo(param.queryObject,  options).toPromise();
    }

    /**
     * Test query parameter(s)
     * Test query parameter(s)
     * @param param the request object
     */
    public testQueryStyleFormExplodeTrueObjectAllOf(param: QueryApiTestQueryStyleFormExplodeTrueObjectAllOfRequest = {}, options?: ConfigurationOptions): Promise<string> {
        return this.api.testQueryStyleFormExplodeTrueObjectAllOf(param.queryObject,  options).toPromise();
    }

}
