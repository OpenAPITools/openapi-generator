package org.openapitools.client;

import org.openapitools.client.auth.Authentication;
import org.openapitools.client.auth.HttpSignatureAuth;
import org.openapitools.client.model.*;
import org.openapitools.client.ApiClient;

import java.lang.Exception;
import java.security.spec.AlgorithmParameterSpec;
import java.util.*;
import java.net.URI;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.client.ClientProperties;

import org.glassfish.jersey.apache.connector.ApacheConnectorProvider;
import org.glassfish.jersey.media.multipart.BodyPart;
import org.glassfish.jersey.media.multipart.FormDataBodyPart;
import org.glassfish.jersey.media.multipart.MultiPart;
import jakarta.ws.rs.client.Entity;
import jakarta.ws.rs.core.MediaType;
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

import org.junit.jupiter.api.*;
import static org.junit.jupiter.api.Assertions.*;

public class ApiClientTest {
    ApiClient apiClient = null;
    Pet pet = null;
    PrivateKey privateKey = null;
    PublicKey publicKey = null;

    @BeforeEach
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
        org.openapitools.client.model.Tag tag = new org.openapitools.client.model.Tag();
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

    @Test
    public void testCollectionPathParameterToString() {
        // items are escaped individually, the csv delimiter is kept raw
        assertEquals("a%2Cb,c", apiClient.collectionPathParameterToString("csv", Arrays.asList("a,b", "c")));
        assertEquals("a,b", apiClient.collectionPathParameterToString("", Arrays.asList("a", "b")));
        assertEquals("a,b", apiClient.collectionPathParameterToString("multi", Arrays.asList("a", "b")));
        assertEquals("a%7Cb", apiClient.collectionPathParameterToString("pipes", Arrays.asList("a", "b")));
        assertEquals("a%20b", apiClient.collectionPathParameterToString("ssv", Arrays.asList("a", "b")));
        assertEquals("a%09b", apiClient.collectionPathParameterToString("tsv", Arrays.asList("a", "b")));
        // leading empty or null items keep their delimiter
        assertEquals(",b", apiClient.collectionPathParameterToString("csv", Arrays.asList("", "b")));
        assertEquals(",b", apiClient.collectionPathParameterToString("csv", Arrays.asList(null, "b")));
        assertEquals("", apiClient.collectionPathParameterToString("csv", Collections.emptyList()));
        assertEquals("", apiClient.collectionPathParameterToString("csv", null));
    }

    @Test
    public void testMultipartComplexPartSerializedAsJson() throws Exception {
        Category category = new Category().id(1L).name("dogs");
        Map<String, Object> formParams = new LinkedHashMap<>();
        formParams.put("name", "doggie");
        formParams.put("category", category);
        formParams.put("tags", Arrays.asList("a", "b"));

        Entity<?> entity = apiClient.serialize(null, formParams, "multipart/form-data", false);
        List<BodyPart> parts = ((MultiPart) entity.getEntity()).getBodyParts();
        assertEquals(4, parts.size());

        // scalars stay plain text parts
        FormDataBodyPart name = (FormDataBodyPart) parts.get(0);
        assertEquals("name", name.getName());
        assertEquals(MediaType.TEXT_PLAIN_TYPE, name.getMediaType());
        assertEquals("doggie", name.getEntity());

        // complex objects are sent as JSON, not via toString()
        FormDataBodyPart categoryPart = (FormDataBodyPart) parts.get(1);
        assertEquals("category", categoryPart.getName());
        assertEquals(MediaType.APPLICATION_JSON_TYPE, categoryPart.getMediaType());
        assertEquals("{\"id\":1,\"name\":\"dogs\"}", categoryPart.getEntity());

        // list items become one part each
        assertEquals("a", ((FormDataBodyPart) parts.get(2)).getEntity());
        assertEquals("b", ((FormDataBodyPart) parts.get(3)).getEntity());
    }

    @Test
    public void testMultipartEncodingContentTypeIsHonoured() throws Exception {
        java.io.File image = java.io.File.createTempFile("upload", ".bin");
        image.deleteOnExit();
        Map<String, Object> formParams = new LinkedHashMap<>();
        formParams.put("metadata", "{\"raw\":true}");
        formParams.put("ids", Arrays.asList(1, 2));
        formParams.put("image", image);
        formParams.put("count", 5);
        formParams.put("note", "plain");
        Map<String, String> formParamContentTypes = new HashMap<>();
        formParamContentTypes.put("metadata", "application/json");
        formParamContentTypes.put("ids", "application/json");
        formParamContentTypes.put("image", "image/png");
        formParamContentTypes.put("count", "text/csv");

        Entity<?> entity = apiClient.serialize(null, formParams, formParamContentTypes, "multipart/form-data", false);
        List<BodyPart> parts = ((MultiPart) entity.getEntity()).getBodyParts();
        assertEquals(5, parts.size());

        // a string declared as JSON is sent as-is, not double encoded
        FormDataBodyPart metadata = (FormDataBodyPart) parts.get(0);
        assertEquals("metadata", metadata.getName());
        assertEquals(MediaType.APPLICATION_JSON_TYPE, metadata.getMediaType());
        assertEquals("{\"raw\":true}", metadata.getEntity());

        // an array declared as JSON is a single JSON part
        FormDataBodyPart ids = (FormDataBodyPart) parts.get(1);
        assertEquals("ids", ids.getName());
        assertEquals(MediaType.APPLICATION_JSON_TYPE, ids.getMediaType());
        assertEquals("[1,2]", ids.getEntity());

        // the declared type replaces the probed file type
        FormDataBodyPart imagePart = (FormDataBodyPart) parts.get(2);
        assertEquals(MediaType.valueOf("image/png"), imagePart.getMediaType());

        // scalars keep their text value with the declared type
        FormDataBodyPart count = (FormDataBodyPart) parts.get(3);
        assertEquals(MediaType.valueOf("text/csv"), count.getMediaType());
        assertEquals("5", count.getEntity());

        // no encoding: plain text as before
        FormDataBodyPart note = (FormDataBodyPart) parts.get(4);
        assertEquals(MediaType.TEXT_PLAIN_TYPE, note.getMediaType());
        assertEquals("plain", note.getEntity());
    }

    @Test
    public void testMultipartOverrideWithoutPartTypeIsCalled() throws Exception {
        List<String> overridden = new ArrayList<>();
        ApiClient client = new ApiClient() {
            @Override
            protected void addParamToMultipart(Object value, String key, MultiPart multiPart) throws ApiException {
                overridden.add(key);
                super.addParamToMultipart(value, key, multiPart);
            }
        };
        Map<String, Object> formParams = new LinkedHashMap<>();
        formParams.put("note", "plain");
        formParams.put("metadata", "{}");
        Map<String, String> formParamContentTypes = new HashMap<>();
        formParamContentTypes.put("metadata", "application/json");

        client.serialize(null, formParams, formParamContentTypes, "multipart/form-data", false);

        // the old overload is still called for parts without a declared type
        assertEquals(Collections.singletonList("note"), overridden);
    }
}
