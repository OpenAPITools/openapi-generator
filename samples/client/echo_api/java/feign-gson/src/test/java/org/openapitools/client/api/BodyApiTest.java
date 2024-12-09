package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import java.io.File;
import org.openapitools.client.model.Pet;
import org.openapitools.client.model.StringEnumRef;
import org.openapitools.client.model.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;

import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for BodyApi
 */
class BodyApiTest {

    private BodyApi api;

    @BeforeEach
    public void setup() {
        api = new ApiClient().buildClient(BodyApi.class);
    }

    
    /**
     * Test binary (gif) response body
     *
     * Test binary (gif) response body
     */
    @Test
    void testBinaryGifTest() {
        // File response = api.testBinaryGif();

        // TODO: test validations
    }

    
    /**
     * Test body parameter(s)
     *
     * Test body parameter(s)
     */
    @Test
    void testBodyApplicationOctetstreamBinaryTest() {
        File body = null;
        // String response = api.testBodyApplicationOctetstreamBinary(body);

        // TODO: test validations
    }

    
    /**
     * Test array of binary in multipart mime
     *
     * Test array of binary in multipart mime
     */
    @Test
    void testBodyMultipartFormdataArrayOfBinaryTest() {
        List<File> files = null;
        // String response = api.testBodyMultipartFormdataArrayOfBinary(files);

        // TODO: test validations
    }

    
    /**
     * Test single binary in multipart mime
     *
     * Test single binary in multipart mime
     */
    @Test
    void testBodyMultipartFormdataSingleBinaryTest() {
        File myFile = null;
        // String response = api.testBodyMultipartFormdataSingleBinary(myFile);

        // TODO: test validations
    }

    
    /**
     * Test body parameter(s)
     *
     * Test body parameter(s)
     */
    @Test
    void testEchoBodyAllOfPetTest() {
        Pet pet = null;
        // Pet response = api.testEchoBodyAllOfPet(pet);

        // TODO: test validations
    }

    
    /**
     * Test free form object
     *
     * Test free form object
     */
    @Test
    void testEchoBodyFreeFormObjectResponseStringTest() {
        Object body = null;
        // String response = api.testEchoBodyFreeFormObjectResponseString(body);

        // TODO: test validations
    }

    
    /**
     * Test body parameter(s)
     *
     * Test body parameter(s)
     */
    @Test
    void testEchoBodyPetTest() {
        Pet pet = null;
        // Pet response = api.testEchoBodyPet(pet);

        // TODO: test validations
    }

    
    /**
     * Test empty response body
     *
     * Test empty response body
     */
    @Test
    void testEchoBodyPetResponseStringTest() {
        Pet pet = null;
        // String response = api.testEchoBodyPetResponseString(pet);

        // TODO: test validations
    }

    
    /**
     * Test string enum response body
     *
     * Test string enum response body
     */
    @Test
    void testEchoBodyStringEnumTest() {
        String body = null;
        // StringEnumRef response = api.testEchoBodyStringEnum(body);

        // TODO: test validations
    }

    
    /**
     * Test empty json (request body)
     *
     * Test empty json (request body)
     */
    @Test
    void testEchoBodyTagResponseStringTest() {
        Tag tag = null;
        // String response = api.testEchoBodyTagResponseString(tag);

        // TODO: test validations
    }

    
}
