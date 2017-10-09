package io.swagger.api;

import io.swagger.model.Client;

import io.swagger.annotations.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * A delegate to be called by the {@link AnotherFakeApiController}}.
 * Should be implemented as a controller but without the {@link org.springframework.stereotype.Controller} annotation.
 * Instead, use spring to autowire this class into the {@link AnotherFakeApiController}.
 */

public interface AnotherFakeApiDelegate {

    /**
     * @see AnotherFakeApi#testSpecialTags
     */
    default ResponseEntity<Client> testSpecialTags(Client body) {
    // do some magic!
    return new ResponseEntity<Client>(HttpStatus.OK);
    }

}
