package org.openapitools.client;

import org.openapitools.client.auth.*;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;

import org.junit.*;
import org.springframework.http.MediaType;
import org.springframework.util.MultiValueMap;

import static org.junit.Assert.*;


public class ApiClientTest {
    ApiClient apiClient = null;

    @Before
    public void setup() {
        apiClient = new ApiClient();
    }

    /**
     *
     */
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
        assertFalse(apiClient.isJsonMime((String) null));
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
        assertEquals(Arrays.asList(MediaType.parseMediaType("application/json")), apiClient.selectHeaderAccept(accepts));

        accepts = new String[]{"APPLICATION/XML", "APPLICATION/JSON"};
        assertEquals(Arrays.asList(MediaType.parseMediaType("APPLICATION/JSON")), apiClient.selectHeaderAccept(accepts));

        accepts = new String[]{"application/xml", "application/json; charset=UTF8"};
        assertEquals(Arrays.asList(MediaType.parseMediaType("application/json; charset=UTF8")), apiClient.selectHeaderAccept(accepts));

        accepts = new String[]{"text/plain", "application/xml"};
        assertEquals(Arrays.asList(MediaType.parseMediaType("text/plain"),MediaType.parseMediaType("application/xml")), apiClient.selectHeaderAccept(accepts));

        accepts = new String[]{};
        assertNull(apiClient.selectHeaderAccept(accepts));
    }

    @Test
    public void testSelectHeaderContentType() {
        String[] contentTypes = {"application/json", "application/xml"};
        assertEquals(MediaType.parseMediaType("application/json"), apiClient.selectHeaderContentType(contentTypes));

        contentTypes = new String[]{"APPLICATION/JSON", "APPLICATION/XML"};
        assertEquals(MediaType.parseMediaType("APPLICATION/JSON"), apiClient.selectHeaderContentType(contentTypes));

        contentTypes = new String[]{"application/xml", "application/json; charset=UTF8"};
        assertEquals(MediaType.parseMediaType("application/json; charset=UTF8"), apiClient.selectHeaderContentType(contentTypes));

        contentTypes = new String[]{"text/plain", "application/xml"};
        assertEquals(MediaType.parseMediaType("text/plain"), apiClient.selectHeaderContentType(contentTypes));

        contentTypes = new String[]{};
        assertEquals(MediaType.parseMediaType("application/json"), apiClient.selectHeaderContentType(contentTypes));
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

    @Ignore("There is no more basic auth in petstore security definitions")
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
    public void testParameterToMultiValueMapWhenNameIsInvalid() throws Exception {
        MultiValueMap<String, String> pairs_a = apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.CSV, null, new Integer(1));
        MultiValueMap<String, String> pairs_b = apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.CSV, "", new Integer(1));

        assertTrue(pairs_a.isEmpty());
        assertTrue(pairs_b.isEmpty());
    }

    @Test
    public void testParameterToMultiValueMapWhenValueIsNull() throws Exception {
        MultiValueMap<String, String> pairs = apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.CSV, "param-a", null);

        assertTrue(pairs.isEmpty());
    }

    @Test
    public void testParameterToMultiValueMapWhenValueIsEmptyStrings() throws Exception {

        // single empty string
        MultiValueMap<String, String> pairs = apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.CSV, "param-a", " ");
        assertEquals(1, pairs.size());

        // list of empty strings
        List<String> strs = new ArrayList<String>();
        strs.add(" ");
        strs.add(" ");
        strs.add(" ");

        MultiValueMap<String, String> concatStrings = apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.CSV, "param-a", strs);

        assertEquals(1, concatStrings.get("param-a").size());
        assertFalse(concatStrings.get("param-a").isEmpty()); // should contain some delimiters
    }

    @Test
    public void testParameterToMultiValueMapWhenValueIsNotCollection() throws Exception {
        String name = "param-a";
        Integer value = 1;

        MultiValueMap<String, String> pairs = apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.CSV, name, value);

        assertEquals(1, pairs.get(name).size());
        assertEquals(value, Integer.valueOf(pairs.get(name).get(0)));
    }

    @Test
    public void testParameterToMultiValueMapWhenValueIsCollection() throws Exception {
        Map<ApiClient.CollectionFormat, String> collectionFormatMap = new HashMap<ApiClient.CollectionFormat, String>();
        collectionFormatMap.put(ApiClient.CollectionFormat.CSV, ",");
        collectionFormatMap.put(ApiClient.CollectionFormat.TSV, "\t");
        collectionFormatMap.put(ApiClient.CollectionFormat.SSV, " ");
        collectionFormatMap.put(ApiClient.CollectionFormat.PIPES, "\\|");
        collectionFormatMap.put(null, ","); // no format, must default to csv

        String name = "param-a";

        List<Object> values = new ArrayList<Object>();
        values.add("value-a");
        values.add(123);
        values.add(new Date());

        // check for multi separately
        MultiValueMap<String, String> multiValueMap = apiClient.parameterToMultiValueMap(ApiClient.CollectionFormat.MULTI, name, values);
        assertEquals(values.size(), multiValueMap.get(name).size());

        // all other formats
        for (ApiClient.CollectionFormat collectionFormat : collectionFormatMap.keySet()) {
            MultiValueMap<String, String> pairs = apiClient.parameterToMultiValueMap(collectionFormat, name, values);

            assertEquals(1, pairs.size());

            String delimiter = collectionFormatMap.get(collectionFormat);
            String[] pairValueSplit = pairs.get(name).get(0).split(delimiter);

            assertEquals(values.size(), pairValueSplit.length);
        }
    }
}
