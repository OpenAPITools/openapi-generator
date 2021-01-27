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
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.client.ClientProperties;

import org.glassfish.jersey.apache.connector.ApacheConnectorProvider;
import org.tomitribe.auth.signatures.Algorithm;
import org.tomitribe.auth.signatures.Signer;
import org.tomitribe.auth.signatures.SigningAlgorithm;
import java.security.spec.PSSParameterSpec;
import java.security.spec.MGF1ParameterSpec;
import org.tomitribe.auth.signatures.*;
import java.io.ByteArrayInputStream;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.PrivateKey;

import static org.junit.Assert.*;

public class ApiClientTest {
    ApiClient apiClient = null;
    Pet pet = null;
    PrivateKey privateKey = null;
    PublicKey publicKey = null;

    @Before
    public void setup() {
        apiClient = new ApiClient();
        pet = new Pet();
        try {
            KeyPair keypair = KeyPairGenerator.getInstance("RSA").generateKeyPair();
            privateKey = keypair.getPrivate();
            publicKey = keypair.getPublic();
        } catch(NoSuchAlgorithmException e) {
            fail("No such algorithm: " + e.toString());
        }
    }

    @Test
    public void testClientConfig() {
        ApiClient testClient = new ApiClient();
        ClientConfig config = testClient.getDefaultClientConfig();
        config.connectorProvider(new ApacheConnectorProvider());
        config.property(ClientProperties.PROXY_URI, "http://localhost:8080");
        config.property(ClientProperties.PROXY_USERNAME,"proxy_user");
        config.property(ClientProperties.PROXY_PASSWORD,"proxy_password");
        testClient.setClientConfig(config);
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

        signatureAuth.setPrivateKey(privateKey);

        authMap.put("http_signature_test", signatureAuth);

        ApiClient client = new ApiClient(authMap);

        client.updateParamsForAuth(authNames, queryParams, headerParams, null, null, "post", uri);
        Signature requestSignature = Signature.fromString(headerParams.get("Authorization"), Algorithm.RSA_SHA512);
        Verifier verify = new Verifier(publicKey, requestSignature);
        assert verify.verify("post", uri.toString(), headerParams);
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
