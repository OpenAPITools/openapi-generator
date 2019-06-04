package org.openapitools.api;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import org.openapitools.model.FileSchemaTestClass;
import java.time.LocalDate;
import java.util.Map;
import org.openapitools.model.ModelApiResponse;
import java.time.OffsetDateTime;
import org.openapitools.model.OuterComposite;
import org.springframework.core.io.Resource;
import org.openapitools.model.User;
import org.openapitools.model.XmlItem;
import io.swagger.annotations.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * A delegate to be called by the {@link FakeApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */

public interface FakeApiDelegate {

    default Optional<NativeWebRequest> getRequest() {
        return Optional.empty();
    }

    /**
     * @see FakeApi#createXmlItem
     */
    default ResponseEntity<Void> createXmlItem(XmlItem xmlItem) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see FakeApi#fakeOuterBooleanSerialize
     */
    default ResponseEntity<Boolean> fakeOuterBooleanSerialize(Boolean body) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see FakeApi#fakeOuterCompositeSerialize
     */
    default ResponseEntity<OuterComposite> fakeOuterCompositeSerialize(OuterComposite body) {
        getRequest().ifPresent(request -> {
            for (MediaType mediaType: MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("*/*"))) {
                    ApiUtil.setExampleResponse(request, "*/*", "{  \"my_string\" : \"my_string\",  \"my_number\" : 0.8008281904610115,  \"my_boolean\" : true}");
                    break;
                }
            }
        });
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see FakeApi#fakeOuterNumberSerialize
     */
    default ResponseEntity<BigDecimal> fakeOuterNumberSerialize(BigDecimal body) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see FakeApi#fakeOuterStringSerialize
     */
    default ResponseEntity<String> fakeOuterStringSerialize(String body) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see FakeApi#testBodyWithFileSchema
     */
    default ResponseEntity<Void> testBodyWithFileSchema(FileSchemaTestClass body) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see FakeApi#testBodyWithQueryParams
     */
    default ResponseEntity<Void> testBodyWithQueryParams(String query,
        User body) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see FakeApi#testClientModel
     */
    default ResponseEntity<Client> testClientModel(Client body) {
        getRequest().ifPresent(request -> {
            for (MediaType mediaType: MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                    ApiUtil.setExampleResponse(request, "application/json", "{  \"client\" : \"client\"}");
                    break;
                }
            }
        });
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see FakeApi#testEndpointParameters
     */
    default ResponseEntity<Void> testEndpointParameters(BigDecimal number,
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
        String paramCallback) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see FakeApi#testEnumParameters
     */
    default ResponseEntity<Void> testEnumParameters(List<String> enumHeaderStringArray,
        String enumHeaderString,
        List<String> enumQueryStringArray,
        String enumQueryString,
        Integer enumQueryInteger,
        Double enumQueryDouble,
        List<String> enumFormStringArray,
        String enumFormString) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see FakeApi#testGroupParameters
     */
    default ResponseEntity<Void> testGroupParameters(Integer requiredStringGroup,
        Boolean requiredBooleanGroup,
        Long requiredInt64Group,
        Integer stringGroup,
        Boolean booleanGroup,
        Long int64Group) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see FakeApi#testInlineAdditionalProperties
     */
    default ResponseEntity<Void> testInlineAdditionalProperties(Map<String, String> param) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see FakeApi#testJsonFormData
     */
    default ResponseEntity<Void> testJsonFormData(String param,
        String param2) {
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

    /**
     * @see FakeApi#uploadFileWithRequiredFile
     */
    default ResponseEntity<ModelApiResponse> uploadFileWithRequiredFile(Long petId,
        MultipartFile requiredFile,
        String additionalMetadata) {
        getRequest().ifPresent(request -> {
            for (MediaType mediaType: MediaType.parseMediaTypes(request.getHeader("Accept"))) {
                if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                    ApiUtil.setExampleResponse(request, "application/json", "{  \"code\" : 0,  \"type\" : \"type\",  \"message\" : \"message\"}");
                    break;
                }
            }
        });
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

}
