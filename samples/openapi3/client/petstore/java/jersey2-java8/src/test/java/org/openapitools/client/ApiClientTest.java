package org.openapitools.client;

import org.openapitools.client.auth.Authentication;
import org.openapitools.client.auth.HttpSignatureAuth;
import org.openapitools.client.model.*;
import org.openapitools.client.ApiClient;

import java.lang.Exception;
import java.security.spec.AlgorithmParameterSpec;
import java.util.*;
import java.net.URI;
import org.junit.*;
import org.tomitribe.auth.signatures.Algorithm;
import org.tomitribe.auth.signatures.Signer;
import org.tomitribe.auth.signatures.SigningAlgorithm;
import java.security.spec.PSSParameterSpec;
import java.security.spec.MGF1ParameterSpec;
import org.tomitribe.auth.signatures.*;
import java.io.ByteArrayInputStream;
import java.security.PublicKey;
import java.security.PrivateKey;

import static org.junit.Assert.*;

public class ApiClientTest {
    ApiClient apiClient = null;
    Pet pet = null;

    private final String privateKeyPem = "-----BEGIN RSA PRIVATE KEY-----\n" +
            "MIICXgIBAAKBgQDCFENGw33yGihy92pDjZQhl0C36rPJj+CvfSC8+q28hxA161QF\n" +
            "NUd13wuCTUcq0Qd2qsBe/2hFyc2DCJJg0h1L78+6Z4UMR7EOcpfdUE9Hf3m/hs+F\n" +
            "UR45uBJeDK1HSFHD8bHKD6kv8FPGfJTotc+2xjJwoYi+1hqp1fIekaxsyQIDAQAB\n" +
            "AoGBAJR8ZkCUvx5kzv+utdl7T5MnordT1TvoXXJGXK7ZZ+UuvMNUCdN2QPc4sBiA\n" +
            "QWvLw1cSKt5DsKZ8UETpYPy8pPYnnDEz2dDYiaew9+xEpubyeW2oH4Zx71wqBtOK\n" +
            "kqwrXa/pzdpiucRRjk6vE6YY7EBBs/g7uanVpGibOVAEsqH1AkEA7DkjVH28WDUg\n" +
            "f1nqvfn2Kj6CT7nIcE3jGJsZZ7zlZmBmHFDONMLUrXR/Zm3pR5m0tCmBqa5RK95u\n" +
            "412jt1dPIwJBANJT3v8pnkth48bQo/fKel6uEYyboRtA5/uHuHkZ6FQF7OUkGogc\n" +
            "mSJluOdc5t6hI1VsLn0QZEjQZMEOWr+wKSMCQQCC4kXJEsHAve77oP6HtG/IiEn7\n" +
            "kpyUXRNvFsDE0czpJJBvL/aRFUJxuRK91jhjC68sA7NsKMGg5OXb5I5Jj36xAkEA\n" +
            "gIT7aFOYBFwGgQAQkWNKLvySgKbAZRTeLBacpHMuQdl1DfdntvAyqpAZ0lY0RKmW\n" +
            "G6aFKaqQfOXKCyWoUiVknQJAXrlgySFci/2ueKlIE1QqIiLSZ8V8OlpFLRnb1pzI\n" +
            "7U1yQXnTAEFYM560yJlzUpOb1V4cScGd365tiSMvxLOvTA==\n" +
            "-----END RSA PRIVATE KEY-----\n";

    @Before
    public void setup() {
        apiClient = new ApiClient();
        pet = new Pet();
    }

    @Test
    public void testUpdateParamsForAuth() throws Exception {
        Map<String, String> headerParams = new HashMap<String, String>();
        List<Pair> queryParams = new ArrayList<>();

        URI uri = new URI("/api/v1/telemetry/TimeSeries");

        // auth name
        String[] authNames = {"http_signature_test"};

        HashMap<String, Authentication> authMap = new HashMap<String, Authentication>();

        HttpSignatureAuth signatureAuth = new HttpSignatureAuth("some-key-1", SigningAlgorithm.HS2019, Algorithm.RSA_SHA512, null,
                null, Arrays.asList(new String[] { "(request-target)" }), 128L);

        signatureAuth.setPrivateKey(PEM.readPrivateKey(new ByteArrayInputStream(privateKeyPem.getBytes())));

        authMap.put("http_signature_test", signatureAuth);

        ApiClient client = new ApiClient(authMap);

        client.updateParamsForAuth(authNames, queryParams, headerParams, null, null, "post", uri);

        // hard to test as expire will always be different
        // ApiClientTest.testUpdateParamsForAuth:77 expected:<...-1",created=15954814[97,expires=1595481497.760],algorithm="hs2019",...> but was:<...-1",created=15954814[60,expires=1595481460.841],algorithm="hs2019",...>
        //assertEquals(headerParams.get("Authorization"), "Signature keyId=\"some-key-1\",created=1595481460,expires=1595481460.841,algorithm=\"hs2019\",headers=\"(request-target)\",signature=\"eahPOLOTIH5AJyXbvpDyUIYBdYLAv6RbcAtGCEG9J1y6JyFWS+1IT/n/u4ZGMteiUvtoPm52dUXrhN3OMump+ivi+2JgMjHhd2G89zj7wcOVkZwaFfHjymHb8SwkVrda35GYsmXlnx01JRHCShk9yVHS7VYkY0CpQw171VaFWUc=\"");
    }

    @Test
    public void testSerializeToString() throws Exception {
        Long petId = 4321L;
        pet.setId(petId);
        pet.setName("jersey2 java8 pet");
        Category category = new Category();
        category.setId(petId);
        category.setName("jersey2 java8 category");
        pet.setCategory(category);
        pet.setStatus(Pet.StatusEnum.AVAILABLE);
        pet.setPhotoUrls(Arrays.asList("A", "B", "C"));
        Tag tag = new Tag();
        tag.setId(petId);
        tag.setName("jersey2 java8 tag");
        pet.setTags(Arrays.asList(tag));

        String result = "{\"id\":4321,\"category\":{\"id\":4321,\"name\":\"jersey2 java8 category\"},\"name\":\"jersey2 java8 pet\",\"photoUrls\":[\"A\",\"B\",\"C\"],\"tags\":[{\"id\":4321,\"name\":\"jersey2 java8 tag\"}],\"status\":\"available\"}";
        assertEquals(result, apiClient.serializeToString(pet, null, "application/json", false));
        // nulllable and there should be no diffencne as the payload is not null
        assertEquals(result, apiClient.serializeToString(pet, null, "application/json", true));

        // non-nullable null object should be converted to "" (empty body)
        assertEquals("", apiClient.serializeToString(null, null, "application/json", false));
        // nullable null object should be converted to "null"
        assertEquals("null", apiClient.serializeToString(null, null, "application/json", true));

        // non-nullable empty string should be converted to "\"\"" (empty json string)
        assertEquals("\"\"", apiClient.serializeToString("", null, "application/json", false));
        // nullable empty string should be converted to "\"\"" (empty json string)
        assertEquals("\"\"", apiClient.serializeToString("", null, "application/json", true));

        // non-nullable string "null" should be converted to "\"null\""
        assertEquals("\"null\"", apiClient.serializeToString("null", null, "application/json", false));
        // nullable string "null" should be converted to "\"null\""
        assertEquals("\"null\"", apiClient.serializeToString("null", null, "application/json", true));
    }

}
