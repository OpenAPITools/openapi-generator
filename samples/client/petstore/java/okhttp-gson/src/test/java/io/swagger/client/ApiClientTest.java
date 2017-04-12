package io.swagger.client;

import io.swagger.client.auth.*;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;
import java.util.TimeZone;

import org.junit.*;
import static org.junit.Assert.*;


public class ApiClientTest {
    ApiClient apiClient = null;

    @Before
    public void setup() {
        apiClient = new ApiClient();
    }

    @Test
    public void testParseAndFormatDatetime() {
        // default datetime format with UTC time zone
        apiClient.getDatetimeFormat().setTimeZone(TimeZone.getTimeZone("UTC"));
        String dateStr = "2015-11-07T03:49:09.356Z";
        assertTrue(apiClient.isLenientDatetimeFormat());
        assertEquals(dateStr, apiClient.formatDatetime(apiClient.parseDatetime("2015-11-07T03:49:09.356+00:00")));
        assertEquals(dateStr, apiClient.formatDatetime(apiClient.parseDatetime("2015-11-07T05:49:09.356+02:00")));
        assertEquals(dateStr, apiClient.formatDatetime(apiClient.parseDatetime("2015-11-07T02:49:09.356-01:00")));
        // support various cases
        assertEquals(dateStr, apiClient.formatDatetime(apiClient.parseDatetime("2015-11-07T03:49:09.356Z")));
        assertEquals(dateStr, apiClient.formatDatetime(apiClient.parseDatetime("2015-11-07T03:49:09.356+00")));
        assertEquals(dateStr, apiClient.formatDatetime(apiClient.parseDatetime("2015-11-07T02:49:09.356-0100")));
        dateStr = "2015-11-07T03:49:09.000Z";
        assertEquals(dateStr, apiClient.formatDatetime(apiClient.parseDatetime("2015-11-07T05:49:09+02")));

        // custom datetime format: without milli-seconds, custom time zone
        DateFormat format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssXXX");
        format.setTimeZone(TimeZone.getTimeZone("GMT+10"));
        apiClient.setDatetimeFormat(format);
        // disable support of various datetime format
        apiClient.setLenientDatetimeFormat(false);
        dateStr = "2015-11-07T13:49:09+10:00";
        assertEquals(dateStr, apiClient.formatDatetime(apiClient.parseDatetime("2015-11-07T03:49:09+00:00")));
        assertEquals(dateStr, apiClient.formatDatetime(apiClient.parseDatetime("2015-11-07T03:49:09Z")));
        assertEquals(dateStr, apiClient.formatDatetime(apiClient.parseDatetime("2015-11-07T00:49:09-03:00")));

        try {
            // invalid time zone format
            apiClient.parseDatetime("2015-11-07T03:49:09+00");
            fail("parseDatetime should fail");
        } catch (RuntimeException e) {
            // OK
        }
        try {
            // unexpected miliseconds
            apiClient.parseDatetime("2015-11-07T03:49:09.000Z");
            fail("parseDatetime should fail");
        } catch (RuntimeException e) {
            // OK
        }
    }

    @Test
    public void testParseAndFormatDate() {
        // default date format
        String dateStr = "2015-11-07";
        assertEquals(dateStr, apiClient.formatDate(apiClient.parseDatetime("2015-11-07T03:49:09.356+00:00")));
        assertEquals(dateStr, apiClient.formatDate(apiClient.parseDate("2015-11-07")));

        // custom date format: without day
        DateFormat format = new SimpleDateFormat("yyyy-MM");
        apiClient.setDateFormat(format);
        dateStr = "2015-11";
        assertEquals(dateStr, apiClient.formatDate(apiClient.parseDatetime("2015-11-07T03:49:09Z")));
        assertEquals(dateStr, apiClient.formatDate(apiClient.parseDate("2015-11")));
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

    /*
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
    */

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
    public void testGetAndSetConnectTimeout() {
        // connect timeout defaults to 10 seconds
        assertEquals(10000, apiClient.getConnectTimeout());
        assertEquals(10000, apiClient.getHttpClient().getConnectTimeout());

        apiClient.setConnectTimeout(0);
        assertEquals(0, apiClient.getConnectTimeout());
        assertEquals(0, apiClient.getHttpClient().getConnectTimeout());

        apiClient.setConnectTimeout(10000);
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

    @Test
    public void testSanitizeFilename() {
        assertEquals("sun", apiClient.sanitizeFilename("sun"));
        assertEquals("sun.gif", apiClient.sanitizeFilename("sun.gif"));
        assertEquals("sun.gif", apiClient.sanitizeFilename("../sun.gif"));
        assertEquals("sun.gif", apiClient.sanitizeFilename("/var/tmp/sun.gif"));
        assertEquals("sun.gif", apiClient.sanitizeFilename("./sun.gif"));
        assertEquals("sun.gif", apiClient.sanitizeFilename("..\\sun.gif"));
        assertEquals("sun.gif", apiClient.sanitizeFilename("\\var\\tmp\\sun.gif"));
        assertEquals("sun.gif", apiClient.sanitizeFilename("c:\\var\\tmp\\sun.gif"));
        assertEquals("sun.gif", apiClient.sanitizeFilename(".\\sun.gif"));
    }
}
