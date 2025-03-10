package org.openapitools.client.api;

import org.openapitools.client.ApiClient;
import org.openapitools.client.model.Client;
import java.util.UUID;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jakarta.validation.constraints.*;
import jakarta.validation.Valid;

/**
 * API tests for AnotherFakeApi
 */
public class AnotherFakeApiTest {

    private AnotherFakeApi api;

    @BeforeEach
    public void setup() {
        api = new ApiClient().createService(AnotherFakeApi.class);
    }

    /**
     * To test special tags
     *
     * To test special tags and operation ID starting with number
     */
    @Test
    public void call123testSpecialTagsTest() {
        UUID uuidTest = null;
        Client body = null;
        // Client response = api.call123testSpecialTags(uuidTest, body);

        // TODO: test validations
    }
}
