package org.openapitools.client;

import org.openapitools.client.auth.Authentication;
import org.openapitools.client.auth.HttpSignatureAuth;
import org.openapitools.client.model.*;
import org.openapitools.client.ApiClient;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
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

    @Test
    public void testParseAndFormatDate() {
        // default date format
        String dateStr = "2015-11-07T03:49:09.356Z";
        assertEquals(dateStr, apiClient.formatDate(apiClient.parseDate("2015-11-07T03:49:09.356+00:00")));
        assertEquals(dateStr, apiClient.formatDate(apiClient.parseDate("2015-11-07T03:49:09.356Z")));
        assertEquals(dateStr, apiClient.formatDate(apiClient.parseDate("2015-11-07T05:49:09.356+02:00")));
        assertEquals(dateStr, apiClient.formatDate(apiClient.parseDate("2015-11-07T02:49:09.356-01:00")));

        // custom date format: without milli-seconds, custom time zone
        DateFormat format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX", Locale.ROOT);
        format.setTimeZone(TimeZone.getTimeZone("GMT+10"));
        apiClient.setDateFormat(format);
        dateStr = "2015-11-07T13:49:09+10:00";
        assertEquals(dateStr, apiClient.formatDate(apiClient.parseDate("2015-11-07T03:49:09+00:00")));
        assertEquals(dateStr, apiClient.formatDate(apiClient.parseDate("2015-11-07T03:49:09Z")));
        assertEquals(dateStr, apiClient.formatDate(apiClient.parseDate("2015-11-07T00:49:09-03:00")));
        assertEquals(dateStr, apiClient.formatDate(apiClient.parseDate("2015-11-07T13:49:09+10:00")));
    }

    @Test
    public void testIsJsonMime() {
        assertFalse(apiClient.isJsonMime(null));
        assertFalse(apiClient.isJsonMime(""));
        assertFalse(apiClient.isJsonMime("text/plain"));
        assertFalse(apiClient.isJsonMime("application/xml"));
        assertFalse(apiClient.isJsonMime("application/jsonp"));
        assertFalse(apiClient.isJsonMime("example/json"));
        assertFalse(apiClient.isJsonMime("example/foo+bar+jsonx"));
        assertFalse(apiClient.isJsonMime("example/foo+bar+xjson"));

        assertTrue(apiClient.isJsonMime("application/json"));
        assertTrue(apiClient.isJsonMime("application/json; charset=UTF8"));
        assertTrue(apiClient.isJsonMime("APPLICATION/JSON"));

        assertTrue(apiClient.isJsonMime("application/problem+json"));
        assertTrue(apiClient.isJsonMime("APPLICATION/PROBLEM+JSON"));
        assertTrue(apiClient.isJsonMime("application/json\t"));
        assertTrue(apiClient.isJsonMime("example/foo+bar+json"));
        assertTrue(apiClient.isJsonMime("example/foo+json;x;y"));
        assertTrue(apiClient.isJsonMime("example/foo+json\t;"));
        assertTrue(apiClient.isJsonMime("Example/fOO+JSON"));
    }

    @Test
    public void testSelectHeaderAccept() {
        String[] accepts = {"application/json", "application/xml"};
        assertEquals("application/json", apiClient.selectHeaderAccept(accepts));

        accepts = new String[]{"APPLICATION/XML", "APPLICATION/JSON"};
        assertEquals("APPLICATION/JSON", apiClient.selectHeaderAccept(accepts));

        accepts = new String[]{"application/xml", "application/json; charset=UTF8"};
        assertEquals("application/json; charset=UTF8", apiClient.selectHeaderAccept(accepts));

        accepts = new String[]{"text/plain", "application/xml"};
        assertEquals("text/plain,application/xml", apiClient.selectHeaderAccept(accepts));

        accepts = new String[]{};
        assertNull(apiClient.selectHeaderAccept(accepts));
    }

    @Test
    public void testSelectHeaderContentType() {
        String[] contentTypes = {"application/json", "application/xml"};
        assertEquals("application/json", apiClient.selectHeaderContentType(contentTypes));

        contentTypes = new String[]{"APPLICATION/JSON", "APPLICATION/XML"};
        assertEquals("APPLICATION/JSON", apiClient.selectHeaderContentType(contentTypes));

        contentTypes = new String[]{"application/xml", "application/json; charset=UTF8"};
        assertEquals("application/json; charset=UTF8", apiClient.selectHeaderContentType(contentTypes));

        contentTypes = new String[]{"text/plain", "application/xml"};
        assertEquals("text/plain", apiClient.selectHeaderContentType(contentTypes));

        contentTypes = new String[]{};
        assertEquals("application/json", apiClient.selectHeaderContentType(contentTypes));
    }

    @Test
    public void testGetAuthentications() {
        Map<String, Authentication> auths = apiClient.getAuthentications();

        Authentication auth = auths.get("api_key");
        assertNotNull(auth);
        assertTrue(auth instanceof ApiKeyAuth);
        ApiKeyAuth apiKeyAuth = (ApiKeyAuth) auth;
        assertEquals("header", apiKeyAuth.getLocation());
        assertEquals("api_key", apiKeyAuth.getParamName());

        auth = auths.get("petstore_auth");
        assertTrue(auth instanceof OAuth);
        assertSame(auth, apiClient.getAuthentication("petstore_auth"));

        assertNull(auths.get("unknown"));

        try {
            auths.put("my_auth", new HttpBasicAuth());
            fail("the authentications returned should not be modifiable");
        } catch (UnsupportedOperationException e) {
        }
    }

    @Test
    public void testSetUsernameAndPassword() {
        HttpBasicAuth auth = null;
        for (Authentication _auth : apiClient.getAuthentications().values()) {
            if (_auth instanceof HttpBasicAuth) {
                auth = (HttpBasicAuth) _auth;
                break;
            }
        }
        auth.setUsername(null);
        auth.setPassword(null);

        apiClient.setUsername("my-username");
        apiClient.setPassword("my-password");
        assertEquals("my-username", auth.getUsername());
        assertEquals("my-password", auth.getPassword());

        // reset values
        auth.setUsername(null);
        auth.setPassword(null);
    }

    @Test
    public void testSetApiKeyAndPrefix() {
        ApiKeyAuth auth = null;
        for (Authentication _auth : apiClient.getAuthentications().values()) {
            if (_auth instanceof ApiKeyAuth) {
                auth = (ApiKeyAuth) _auth;
                break;
            }
        }
        auth.setApiKey(null);
        auth.setApiKeyPrefix(null);

        apiClient.setApiKey("my-api-key");
        apiClient.setApiKeyPrefix("Token");
        assertEquals("my-api-key", auth.getApiKey());
        assertEquals("Token", auth.getApiKeyPrefix());

        // reset values
        auth.setApiKey(null);
        auth.setApiKeyPrefix(null);
    }

    @Test
    public void testParameterToPairsWhenNameIsInvalid() throws Exception {
        List<Pair> pairs_a = apiClient.parameterToPairs("csv", null, new Integer(1));
        List<Pair> pairs_b = apiClient.parameterToPairs("csv", "", new Integer(1));

        assertTrue(pairs_a.isEmpty());
        assertTrue(pairs_b.isEmpty());
    }

    @Test
    public void testParameterToPairsWhenValueIsNull() throws Exception {
        List<Pair> pairs = apiClient.parameterToPairs("csv", "param-a", null);

        assertTrue(pairs.isEmpty());
    }

    @Test
    public void testParameterToPairsWhenValueIsEmptyStrings() throws Exception {

        // single empty string
        List<Pair> pairs = apiClient.parameterToPairs("csv", "param-a", " ");
        assertEquals(1, pairs.size());

        // list of empty strings
        List<String> strs = new ArrayList<String>();
        strs.add(" ");
        strs.add(" ");
        strs.add(" ");

        List<Pair> concatStrings = apiClient.parameterToPairs("csv", "param-a", strs);

        assertEquals(1, concatStrings.size());
        assertFalse(concatStrings.get(0).getValue().isEmpty()); // should contain some delimiters
    }

    @Test
    public void testParameterToPairsWhenValueIsNotCollection() throws Exception {
        String name = "param-a";
        Integer value = 1;

        List<Pair> pairs = apiClient.parameterToPairs("csv", name, value);

        assertEquals(1, pairs.size());
        assertEquals(value, Integer.valueOf(pairs.get(0).getValue()));
    }

    @Test
    public void testParameterToPairsWhenValueIsCollection() throws Exception {
        Map<String, String> collectionFormatMap = new HashMap<String, String>();
        collectionFormatMap.put("csv", ",");
        collectionFormatMap.put("tsv", "\t");
        collectionFormatMap.put("ssv", " ");
        collectionFormatMap.put("pipes", "\\|");
        collectionFormatMap.put("", ","); // no format, must default to csv
        collectionFormatMap.put("unknown", ","); // all other formats, must default to csv

        String name = "param-a";

        List<Object> values = new ArrayList<Object>();
        values.add("value-a");
        values.add(123);
        values.add(new Date());

        // check for multi separately
        List<Pair> multiPairs = apiClient.parameterToPairs("multi", name, values);
        assertEquals(values.size(), multiPairs.size());

        // all other formats
        for (String collectionFormat : collectionFormatMap.keySet()) {
            List<Pair> pairs = apiClient.parameterToPairs(collectionFormat, name, values);

            assertEquals(1, pairs.size());

            String delimiter = collectionFormatMap.get(collectionFormat);
            String[] pairValueSplit = pairs.get(0).getValue().split(delimiter);

            // must equal input values
            assertEquals(values.size(), pairValueSplit.length);
        }
    }
}
