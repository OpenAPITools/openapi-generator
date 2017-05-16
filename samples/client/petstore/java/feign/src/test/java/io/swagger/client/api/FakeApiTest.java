package io.swagger.client.api;

import static org.assertj.core.api.Assertions.assertThat;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.swagger.client.ApiClient;
import io.swagger.client.model.OuterComposite;
import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import okhttp3.mockwebserver.RecordedRequest;
import org.joda.time.LocalDate;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import org.joda.time.DateTime;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * API tests for FakeApi
 */
public class FakeApiTest {

    private FakeApi api;
    private MockWebServer mockServer;

    @Before
    public void setup() throws IOException {
        mockServer = new MockWebServer();
        mockServer.start();
        api = new ApiClient().setBasePath(mockServer.url("/").toString()).buildClient(FakeApi.class);
    }

    @After
    public void teardown() throws IOException {
        mockServer.shutdown();
    }

    /**
     * Extract the HTTP request body from the mock server as a String.
     * @param request The mock server's request.
     * @return A String representation of the body of the request.
     * @throws IOException On error reading the body of the request.
     */
    private static String requestBody(RecordedRequest request) throws IOException {
        ByteArrayOutputStream body = new ByteArrayOutputStream((int) request.getBodySize());
        request.getBody().copyTo(body);
        return body.toString();
    }
    
    /**
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     *
     * Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
     */
    @Test
    public void testEndpointParametersTest() {
        BigDecimal number = null;
        Double _double = null;
        String string = null;
        byte[] _byte = null;
        Integer integer = null;
        Integer int32 = null;
        Long int64 = null;
        Float _float = null;
        byte[] binary = null;
        LocalDate date = null;
        DateTime dateTime = null;
        String password = null;
        // api.testEndpointParameters(number, _double, string, _byte, integer, int32, int64, _float, binary, date, dateTime, password);

        // TODO: test validations
    }

    @Test
    public void testOuterNumber() throws Exception {
        mockServer.enqueue(new MockResponse().setBody("5"));
        BigDecimal response = api.fakeOuterNumberSerialize(new BigDecimal(3));
        assertThat(requestBody(mockServer.takeRequest())).isEqualTo("3");
        assertThat(response).isEqualTo(new BigDecimal(5));
    }

    @Test
    public void testOuterString() throws Exception {
        mockServer.enqueue(new MockResponse().setBody("\"Hello from the server\""));
        String response = api.fakeOuterStringSerialize("Hello from the client");
        assertThat(requestBody(mockServer.takeRequest())).isEqualTo("\"Hello from the client\"");
        assertThat(response).isEqualTo("Hello from the server");
    }

    @Test
    public void testOuterBoolean() throws Exception {
        mockServer.enqueue(new MockResponse().setBody("true"));
        Boolean response = api.fakeOuterBooleanSerialize(false);
        assertThat(requestBody(mockServer.takeRequest())).isEqualTo("false");
        assertThat(response).isEqualTo(true);
    }

    @Test
    public void testOuterComposite() throws Exception {
        mockServer.enqueue(new MockResponse().setBody(
                "{\"my_number\": 5, \"my_string\": \"Hello from the server\", \"my_boolean\": true}"));
        OuterComposite compositeRequest = new OuterComposite()
                .myNumber(new BigDecimal(3))
                .myString("Hello from the client")
                .myBoolean(false);
        OuterComposite response = api.fakeOuterCompositeSerialize(compositeRequest);

        JsonNode requestJson = new ObjectMapper()
                .readValue(requestBody(mockServer.takeRequest()), JsonNode.class);
        assertThat(requestJson.fieldNames()).contains("my_number", "my_string", "my_boolean");
        assertThat(requestJson.get("my_number").intValue()).isEqualTo(3);
        assertThat(requestJson.get("my_string").textValue()).isEqualTo("Hello from the client");
        assertThat(requestJson.get("my_boolean").booleanValue()).isEqualTo(false);
        assertThat(response).isEqualTo(
                new OuterComposite()
                        .myNumber(new BigDecimal(5))
                        .myString("Hello from the server")
                        .myBoolean(true)
        );
    }
}
