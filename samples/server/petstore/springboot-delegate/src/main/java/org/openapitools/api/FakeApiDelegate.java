package org.openapitools.api;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import org.openapitools.model.FileSchemaTestClass;
import org.threeten.bp.LocalDate;
import java.util.Map;
import org.openapitools.model.ModelApiResponse;
import org.threeten.bp.OffsetDateTime;
import org.openapitools.model.OuterComposite;
import org.springframework.core.io.Resource;
import org.openapitools.model.User;
import org.openapitools.model.XmlItem;
import io.swagger.annotations.*;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * A delegate to be called by the {@link FakeApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */

public interface FakeApiDelegate {

    /**
     * @see FakeApi#createXmlItem
     */
    ResponseEntity<Void> createXmlItem(XmlItem xmlItem);

    /**
     * @see FakeApi#fakeOuterBooleanSerialize
     */
    ResponseEntity<Boolean> fakeOuterBooleanSerialize(Boolean body);

    /**
     * @see FakeApi#fakeOuterCompositeSerialize
     */
    ResponseEntity<OuterComposite> fakeOuterCompositeSerialize(OuterComposite body);

    /**
     * @see FakeApi#fakeOuterNumberSerialize
     */
    ResponseEntity<BigDecimal> fakeOuterNumberSerialize(BigDecimal body);

    /**
     * @see FakeApi#fakeOuterStringSerialize
     */
    ResponseEntity<String> fakeOuterStringSerialize(String body);

    /**
     * @see FakeApi#testBodyWithFileSchema
     */
    ResponseEntity<Void> testBodyWithFileSchema(FileSchemaTestClass body);

    /**
     * @see FakeApi#testBodyWithQueryParams
     */
    ResponseEntity<Void> testBodyWithQueryParams(String query,
        User body);

    /**
     * @see FakeApi#testClientModel
     */
    ResponseEntity<Client> testClientModel(Client body);

    /**
     * @see FakeApi#testEndpointParameters
     */
    ResponseEntity<Void> testEndpointParameters(BigDecimal number,
        Double _double,
        String patternWithoutDelimiter,
        byte[] _byte,
        Integer integer,
        Integer int32,
        Long int64,
        Float _float,
        String string,
        MultipartFile binary,
        LocalDate date,
        OffsetDateTime dateTime,
        String password,
        String paramCallback);

    /**
     * @see FakeApi#testEnumParameters
     */
    ResponseEntity<Void> testEnumParameters(List<String> enumHeaderStringArray,
        String enumHeaderString,
        List<String> enumQueryStringArray,
        String enumQueryString,
        Integer enumQueryInteger,
        Double enumQueryDouble,
        List<String> enumFormStringArray,
        String enumFormString);

    /**
     * @see FakeApi#testGroupParameters
     */
    ResponseEntity<Void> testGroupParameters(Integer requiredStringGroup,
        Boolean requiredBooleanGroup,
        Long requiredInt64Group,
        Integer stringGroup,
        Boolean booleanGroup,
        Long int64Group);

    /**
     * @see FakeApi#testInlineAdditionalProperties
     */
    ResponseEntity<Void> testInlineAdditionalProperties(Map<String, String> param);

    /**
     * @see FakeApi#testJsonFormData
     */
    ResponseEntity<Void> testJsonFormData(String param,
        String param2);

    /**
     * @see FakeApi#uploadFileWithRequiredFile
     */
    ResponseEntity<ModelApiResponse> uploadFileWithRequiredFile(Long petId,
        MultipartFile requiredFile,
        String additionalMetadata);

}
