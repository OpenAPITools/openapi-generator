package io.swagger.api;

import java.math.BigDecimal;
import io.swagger.model.Client;
import java.time.LocalDate;
import java.time.OffsetDateTime;
import io.swagger.model.OuterComposite;

import io.swagger.annotations.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * A delegate to be called by the {@link FakeApiController}}.
 * Should be implemented as a controller but without the {@link org.springframework.stereotype.Controller} annotation.
 * Instead, use spring to autowire this class into the {@link FakeApiController}.
 */

public interface FakeApiDelegate {

    /**
     * @see FakeApi#fakeOuterBooleanSerialize
     */
    default ResponseEntity<Boolean> fakeOuterBooleanSerialize(Boolean body) {
    // do some magic!
    return new ResponseEntity<Boolean>(HttpStatus.OK);
    }

    /**
     * @see FakeApi#fakeOuterCompositeSerialize
     */
    default ResponseEntity<OuterComposite> fakeOuterCompositeSerialize(OuterComposite body) {
    // do some magic!
    return new ResponseEntity<OuterComposite>(HttpStatus.OK);
    }

    /**
     * @see FakeApi#fakeOuterNumberSerialize
     */
    default ResponseEntity<BigDecimal> fakeOuterNumberSerialize(BigDecimal body) {
    // do some magic!
    return new ResponseEntity<BigDecimal>(HttpStatus.OK);
    }

    /**
     * @see FakeApi#fakeOuterStringSerialize
     */
    default ResponseEntity<String> fakeOuterStringSerialize(String body) {
    // do some magic!
    return new ResponseEntity<String>(HttpStatus.OK);
    }

    /**
     * @see FakeApi#testClientModel
     */
    default ResponseEntity<Client> testClientModel(Client body) {
    // do some magic!
    return new ResponseEntity<Client>(HttpStatus.OK);
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
        byte[] binary,
        LocalDate date,
        OffsetDateTime dateTime,
        String password,
        String paramCallback) {
    // do some magic!
    return new ResponseEntity<Void>(HttpStatus.OK);
    }

    /**
     * @see FakeApi#testEnumParameters
     */
    default ResponseEntity<Void> testEnumParameters(List<String> enumFormStringArray,
        String enumFormString,
        List<String> enumHeaderStringArray,
        String enumHeaderString,
        List<String> enumQueryStringArray,
        String enumQueryString,
        Integer enumQueryInteger,
        Double enumQueryDouble) {
    // do some magic!
    return new ResponseEntity<Void>(HttpStatus.OK);
    }

    /**
     * @see FakeApi#testJsonFormData
     */
    default ResponseEntity<Void> testJsonFormData(String param,
        String param2) {
    // do some magic!
    return new ResponseEntity<Void>(HttpStatus.OK);
    }

}
