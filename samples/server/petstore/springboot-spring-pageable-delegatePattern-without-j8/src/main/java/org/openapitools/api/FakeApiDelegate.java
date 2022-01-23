package org.openapitools.api;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import org.springframework.format.annotation.DateTimeFormat;
import org.openapitools.model.FileSchemaTestClass;
import org.threeten.bp.LocalDate;
import java.util.Map;
import org.openapitools.model.ModelApiResponse;
import org.threeten.bp.OffsetDateTime;
import org.openapitools.model.OuterComposite;
import org.springframework.core.io.Resource;
import org.openapitools.model.User;
import org.openapitools.model.XmlItem;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;
import javax.annotation.Generated;

/**
 * A delegate to be called by the {@link FakeApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */
@Generated(value = "org.openapitools.codegen.languages.SpringCodegen")
public interface FakeApiDelegate {

    /**
     * POST /fake/create_xml_item : creates an XmlItem
     * this route creates an XmlItem
     *
     * @param xmlItem XmlItem Body (required)
     * @return successful operation (status code 200)
     * @see FakeApi#createXmlItem
     */
    ResponseEntity<Void> createXmlItem(XmlItem xmlItem);

    /**
     * POST /fake/outer/boolean
     * Test serialization of outer boolean types
     *
     * @param body Input boolean as post body (optional)
     * @return Output boolean (status code 200)
     * @see FakeApi#fakeOuterBooleanSerialize
     */
    ResponseEntity<Boolean> fakeOuterBooleanSerialize(Boolean body);

    /**
     * POST /fake/outer/composite
     * Test serialization of object with outer number type
     *
     * @param body Input composite as post body (optional)
     * @return Output composite (status code 200)
     * @see FakeApi#fakeOuterCompositeSerialize
     */
    ResponseEntity<OuterComposite> fakeOuterCompositeSerialize(OuterComposite body);

    /**
     * POST /fake/outer/number
     * Test serialization of outer number types
     *
     * @param body Input number as post body (optional)
     * @return Output number (status code 200)
     * @see FakeApi#fakeOuterNumberSerialize
     */
    ResponseEntity<BigDecimal> fakeOuterNumberSerialize(BigDecimal body);

    /**
     * POST /fake/outer/string
     * Test serialization of outer string types
     *
     * @param body Input string as post body (optional)
     * @return Output string (status code 200)
     * @see FakeApi#fakeOuterStringSerialize
     */
    ResponseEntity<String> fakeOuterStringSerialize(String body);

    /**
     * PUT /fake/body-with-file-schema
     * For this test, the body for this request much reference a schema named &#x60;File&#x60;.
     *
     * @param body  (required)
     * @return Success (status code 200)
     * @see FakeApi#testBodyWithFileSchema
     */
    ResponseEntity<Void> testBodyWithFileSchema(FileSchemaTestClass body);

    /**
     * PUT /fake/body-with-query-params
     *
     * @param query  (required)
     * @param body  (required)
     * @return Success (status code 200)
     * @see FakeApi#testBodyWithQueryParams
     */
    ResponseEntity<Void> testBodyWithQueryParams( query,
        User body);

    /**
     * PATCH /fake : To test \&quot;client\&quot; model
     * To test \&quot;client\&quot; model
     *
     * @param body client model (required)
     * @return successful operation (status code 200)
     * @see FakeApi#testClientModel
     */
    ResponseEntity<Client> testClientModel(Client body);

    /**
     * POST /fake : Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     *
     * @param number None (required)
     * @param _double None (required)
     * @param patternWithoutDelimiter None (required)
     * @param _byte None (required)
     * @param integer None (optional)
     * @param int32 None (optional)
     * @param int64 None (optional)
     * @param _float None (optional)
     * @param string None (optional)
     * @param binary None (optional)
     * @param date None (optional)
     * @param dateTime None (optional)
     * @param password None (optional)
     * @param paramCallback None (optional)
     * @return Invalid username supplied (status code 400)
     *         or User not found (status code 404)
     * @see FakeApi#testEndpointParameters
     */
    ResponseEntity<Void> testEndpointParameters( number,
         _double,
         patternWithoutDelimiter,
         _byte,
         integer,
         int32,
         int64,
         _float,
         string,
        MultipartFile binary,
         date,
         dateTime,
         password,
         paramCallback);

    /**
     * GET /fake : To test enum parameters
     * To test enum parameters
     *
     * @param enumHeaderStringArray Header parameter enum test (string array) (optional)
     * @param enumHeaderString Header parameter enum test (string) (optional, default to -efg)
     * @param enumQueryStringArray Query parameter enum test (string array) (optional)
     * @param enumQueryString Query parameter enum test (string) (optional, default to -efg)
     * @param enumQueryInteger Query parameter enum test (double) (optional)
     * @param enumQueryDouble Query parameter enum test (double) (optional)
     * @param enumFormStringArray Form parameter enum test (string array) (optional, default to $)
     * @param enumFormString Form parameter enum test (string) (optional, default to -efg)
     * @return Invalid request (status code 400)
     *         or Not found (status code 404)
     * @see FakeApi#testEnumParameters
     */
    ResponseEntity<Void> testEnumParameters(List<EnumHeaderStringArrayEnum> enumHeaderStringArray,
        EnumHeaderStringEnum enumHeaderString,
        List<EnumQueryStringArrayEnum> enumQueryStringArray,
        EnumQueryStringEnum enumQueryString,
        EnumQueryIntegerEnum enumQueryInteger,
        EnumQueryDoubleEnum enumQueryDouble,
        List<EnumFormStringArrayEnum> enumFormStringArray,
        EnumFormStringEnum enumFormString);

    /**
     * DELETE /fake : Fake endpoint to test group parameters (optional)
     * Fake endpoint to test group parameters (optional)
     *
     * @param requiredStringGroup Required String in group parameters (required)
     * @param requiredBooleanGroup Required Boolean in group parameters (required)
     * @param requiredInt64Group Required Integer in group parameters (required)
     * @param stringGroup String in group parameters (optional)
     * @param booleanGroup Boolean in group parameters (optional)
     * @param int64Group Integer in group parameters (optional)
     * @return Someting wrong (status code 400)
     * @see FakeApi#testGroupParameters
     */
    ResponseEntity<Void> testGroupParameters( requiredStringGroup,
         requiredBooleanGroup,
         requiredInt64Group,
         stringGroup,
         booleanGroup,
         int64Group);

    /**
     * POST /fake/inline-additionalProperties : test inline additionalProperties
     *
     * @param param request body (required)
     * @return successful operation (status code 200)
     * @see FakeApi#testInlineAdditionalProperties
     */
    ResponseEntity<Void> testInlineAdditionalProperties(Map<String, String> param);

    /**
     * GET /fake/jsonFormData : test json serialization of form data
     *
     * @param param field1 (required)
     * @param param2 field2 (required)
     * @return successful operation (status code 200)
     * @see FakeApi#testJsonFormData
     */
    ResponseEntity<Void> testJsonFormData( param,
         param2);

    /**
     * PUT /fake/test-query-parameters
     * To test the collection format in query parameters
     *
     * @param pipe  (required)
     * @param ioutil  (required)
     * @param http  (required)
     * @param url  (required)
     * @param context  (required)
     * @return Success (status code 200)
     * @see FakeApi#testQueryParameterCollectionFormat
     */
    ResponseEntity<Void> testQueryParameterCollectionFormat( pipe,
         ioutil,
         http,
         url,
         context);

    /**
     * POST /fake/{petId}/uploadImageWithRequiredFile : uploads an image (required)
     *
     * @param petId ID of pet to update (required)
     * @param requiredFile file to upload (required)
     * @param additionalMetadata Additional data to pass to server (optional)
     * @return successful operation (status code 200)
     * @see FakeApi#uploadFileWithRequiredFile
     */
    ResponseEntity<ModelApiResponse> uploadFileWithRequiredFile( petId,
        MultipartFile requiredFile,
         additionalMetadata);

}
