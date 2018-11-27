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
import io.swagger.annotations.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

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
     * @see FakeApi#fakeOuterBooleanSerialize
     */
    default Mono<ResponseEntity<Boolean>> fakeOuterBooleanSerialize(Mono<Boolean> body,
        ServerWebExchange exchange) {
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        Mono<Void> result = Mono.empty();
        return result.then(Mono.empty());

    }

    /**
     * @see FakeApi#fakeOuterCompositeSerialize
     */
    default Mono<ResponseEntity<OuterComposite>> fakeOuterCompositeSerialize(Mono<OuterComposite> outerComposite,
        ServerWebExchange exchange) {
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        Mono<Void> result = Mono.empty();
        for (MediaType mediaType : exchange.getRequest().getHeaders().getAccept()) {
            if (mediaType.isCompatibleWith(MediaType.valueOf("*/*"))) {
                result = ApiUtil.getExampleResponse(exchange, "{  \"my_string\" : \"my_string\",  \"my_number\" : 0.80082819046101150206595775671303272247314453125,  \"my_boolean\" : true}");
                break;
            }
        }
        return result.then(Mono.empty());

    }

    /**
     * @see FakeApi#fakeOuterNumberSerialize
     */
    default Mono<ResponseEntity<BigDecimal>> fakeOuterNumberSerialize(Mono<BigDecimal> body,
        ServerWebExchange exchange) {
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        Mono<Void> result = Mono.empty();
        return result.then(Mono.empty());

    }

    /**
     * @see FakeApi#fakeOuterStringSerialize
     */
    default Mono<ResponseEntity<String>> fakeOuterStringSerialize(Mono<String> body,
        ServerWebExchange exchange) {
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        Mono<Void> result = Mono.empty();
        return result.then(Mono.empty());

    }

    /**
     * @see FakeApi#testBodyWithFileSchema
     */
    default Mono<ResponseEntity<Void>> testBodyWithFileSchema(Mono<FileSchemaTestClass> fileSchemaTestClass,
        ServerWebExchange exchange) {
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        Mono<Void> result = Mono.empty();
        return result.then(Mono.empty());

    }

    /**
     * @see FakeApi#testBodyWithQueryParams
     */
    default Mono<ResponseEntity<Void>> testBodyWithQueryParams(String query,
        Mono<User> user,
        ServerWebExchange exchange) {
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        Mono<Void> result = Mono.empty();
        return result.then(Mono.empty());

    }

    /**
     * @see FakeApi#testClientModel
     */
    default Mono<ResponseEntity<Client>> testClientModel(Mono<Client> client,
        ServerWebExchange exchange) {
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        Mono<Void> result = Mono.empty();
        for (MediaType mediaType : exchange.getRequest().getHeaders().getAccept()) {
            if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                result = ApiUtil.getExampleResponse(exchange, "{  \"client\" : \"client\"}");
                break;
            }
        }
        return result.then(Mono.empty());

    }

    /**
     * @see FakeApi#testEndpointParameters
     */
    default Mono<ResponseEntity<Void>> testEndpointParameters(BigDecimal number,
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
        String paramCallback,
        ServerWebExchange exchange) {
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        Mono<Void> result = Mono.empty();
        return result.then(Mono.empty());

    }

    /**
     * @see FakeApi#testEnumParameters
     */
    default Mono<ResponseEntity<Void>> testEnumParameters(List<String> enumHeaderStringArray,
        String enumHeaderString,
        List<String> enumQueryStringArray,
        String enumQueryString,
        Integer enumQueryInteger,
        Double enumQueryDouble,
        List<String> enumFormStringArray,
        String enumFormString,
        ServerWebExchange exchange) {
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        Mono<Void> result = Mono.empty();
        return result.then(Mono.empty());

    }

    /**
     * @see FakeApi#testGroupParameters
     */
    default Mono<ResponseEntity<Void>> testGroupParameters(Integer requiredStringGroup,
        Boolean requiredBooleanGroup,
        Long requiredInt64Group,
        Integer stringGroup,
        Boolean booleanGroup,
        Long int64Group,
        ServerWebExchange exchange) {
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        Mono<Void> result = Mono.empty();
        return result.then(Mono.empty());

    }

    /**
     * @see FakeApi#testInlineAdditionalProperties
     */
    default Mono<ResponseEntity<Void>> testInlineAdditionalProperties(Mono<String> requestBody,
        ServerWebExchange exchange) {
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        Mono<Void> result = Mono.empty();
        return result.then(Mono.empty());

    }

    /**
     * @see FakeApi#testJsonFormData
     */
    default Mono<ResponseEntity<Void>> testJsonFormData(String param,
        String param2,
        ServerWebExchange exchange) {
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        Mono<Void> result = Mono.empty();
        return result.then(Mono.empty());

    }

    /**
     * @see FakeApi#uploadFileWithRequiredFile
     */
    default Mono<ResponseEntity<ModelApiResponse>> uploadFileWithRequiredFile(Long petId,
        MultipartFile requiredFile,
        String additionalMetadata,
        ServerWebExchange exchange) {
        exchange.getResponse().setStatusCode(HttpStatus.NOT_IMPLEMENTED);
        Mono<Void> result = Mono.empty();
        for (MediaType mediaType : exchange.getRequest().getHeaders().getAccept()) {
            if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                result = ApiUtil.getExampleResponse(exchange, "{  \"code\" : 0,  \"type\" : \"type\",  \"message\" : \"message\"}");
                break;
            }
        }
        return result.then(Mono.empty());

    }

}
