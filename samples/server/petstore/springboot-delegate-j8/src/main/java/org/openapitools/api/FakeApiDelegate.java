package org.openapitools.api;

import java.math.BigDecimal;
import org.openapitools.model.Client;
import java.time.LocalDate;
import java.util.Map;
import java.time.OffsetDateTime;
import org.openapitools.model.OuterComposite;
import org.springframework.core.io.Resource;
import org.openapitools.model.User;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.annotations.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;
import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Optional;

/**
 * A delegate to be called by the {@link FakeApiController}}.
 * Implement this interface with a {@link org.springframework.stereotype.Service} annotated class.
 */

public interface FakeApiDelegate {

    Logger log = LoggerFactory.getLogger(FakeApi.class);

    default Optional<ObjectMapper> getObjectMapper() {
        return Optional.empty();
    }

    default Optional<HttpServletRequest> getRequest() {
        return Optional.empty();
    }

    default Optional<String> getAcceptHeader() {
        return getRequest().map(r -> r.getHeader("Accept"));
    }

    /**
     * @see FakeApi#fakeOuterBooleanSerialize
     */
    default ResponseEntity<Boolean> fakeOuterBooleanSerialize( Boolean  booleanPostBody) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default FakeApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

    /**
     * @see FakeApi#fakeOuterCompositeSerialize
     */
    default ResponseEntity<OuterComposite> fakeOuterCompositeSerialize( OuterComposite  outerComposite) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default FakeApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

    /**
     * @see FakeApi#fakeOuterNumberSerialize
     */
    default ResponseEntity<BigDecimal> fakeOuterNumberSerialize( BigDecimal  body) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default FakeApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

    /**
     * @see FakeApi#fakeOuterStringSerialize
     */
    default ResponseEntity<String> fakeOuterStringSerialize( String  body) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default FakeApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

    /**
     * @see FakeApi#testBodyWithQueryParams
     */
    default ResponseEntity<Void> testBodyWithQueryParams( String  query,
         User  user) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default FakeApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

    /**
     * @see FakeApi#testClientModel
     */
    default ResponseEntity<Client> testClientModel( Client  client) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default FakeApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

    /**
     * @see FakeApi#testEndpointParameters
     */
    default ResponseEntity<Void> testEndpointParameters( BigDecimal  number,
         Double  _double,
         String  patternWithoutDelimiter,
         byte[]  _byte,
         Integer  integer,
         Integer  int32,
         Long  int64,
         Float  _float,
         String  string,
        MultipartFile binary,
         LocalDate  date,
         OffsetDateTime  dateTime,
         String  password,
         String  paramCallback) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default FakeApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

    /**
     * @see FakeApi#testEnumParameters
     */
    default ResponseEntity<Void> testEnumParameters( List<String>  enumHeaderStringArray,
         String  enumHeaderString,
         List<String>  enumQueryStringArray,
         String  enumQueryString,
         Integer  enumQueryInteger,
         Double  enumQueryDouble,
         List<String>  enumFormStringArray,
         String  enumFormString) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default FakeApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

    /**
     * @see FakeApi#testInlineAdditionalProperties
     */
    default ResponseEntity<Void> testInlineAdditionalProperties( String  requestBody) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default FakeApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

    /**
     * @see FakeApi#testJsonFormData
     */
    default ResponseEntity<Void> testJsonFormData( String  param,
         String  param2) {
        if(getObjectMapper().isPresent() && getAcceptHeader().isPresent()) {
        } else {
            log.warn("ObjectMapper or HttpServletRequest not configured in default FakeApi interface so no example is generated");
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);
    }

}
