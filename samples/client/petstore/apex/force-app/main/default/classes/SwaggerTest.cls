@isTest
private class SwaggerTest {
    @isTest
    private static void Param_urlEncodeKeyValuePairUtf8() {
        String toEncodeLeft = 'Hello +%-_.!~*\'()@';
        String toEncodeRight = 'World +%-_.!~*\'()@';
        String expected = 'Hello+%2B%25-_.%21%7E*%27%28%29%40=World+%2B%25-_.%21%7E*%27%28%29%40';
        String result = new Swagger.Param(toEncodeLeft, toEncodeRight).toString();
        System.assertEquals(expected, result);
    }

    @isTest
    private static void ApiKeyHeaderAuth_keyInHeaderWithGivenName() {
        Map<String, Object> headers = new Map<String, String>();
        List<Swagger.Param> query = new List<Swagger.Param>();
        Swagger.ApiKeyHeaderAuth auth = new Swagger.ApiKeyHeaderAuth('X-Authenticate');
        auth.setApiKey('foo-bar-api-key');
        auth.apply(headers, query);

        System.assert(query.isEmpty());
        System.assertEquals(1, headers.size());
        System.assertEquals('foo-bar-api-key', headers.get('X-Authenticate'));
    }

    @isTest
    private static void ApiKeyQueryAuth_keyInQueryParamWithGivenName() {
        Map<String, Object> headers = new Map<String, String>();
        List<Swagger.Param> query = new List<Swagger.Param>();
        Swagger.ApiKeyQueryAuth auth = new Swagger.ApiKeyQueryAuth('auth_token');
        auth.setApiKey('foo-bar-api-key');
        auth.apply(headers, query);

        System.assert(headers.isEmpty());
        System.assertEquals(1, query.size());
        System.assertEquals('auth_token=foo-bar-api-key', query.get(0).toString());
    }

    @isTest
    private static void ApiClient_returnAuthenticationMatchingInput() {
        MockApiClient client = new MockApiClient();
        Swagger.ApiKeyHeaderAuth auth1 = new Swagger.ApiKeyHeaderAuth('foo');
        Swagger.ApiKeyQueryAuth auth2 = new Swagger.ApiKeyQueryAuth('foo');

        client.authentications.put('auth1', auth1);
        client.authentications.put('auth2', auth2);

        System.assertEquals(auth1, client.getAuthentication('auth1'));
        System.assertEquals(auth2, client.getAuthentication('auth2'));
    }

    @isTest
    private static void ApiClient_oneKeyValuePairForEachValueInList() {
        List<Object> values = new List<Object>{'bar', 4, false, 12.4, ''};
        Swagger.ApiClient client = new Swagger.ApiClient();
        List<Swagger.Param> params = client.makeParams('foo', values);

        System.assertEquals(5, params.size());
        System.assertEquals('foo=bar', params.get(0).toString());
        System.assertEquals('foo=4', params.get(1).toString());
        System.assertEquals('foo=false', params.get(2).toString());
        System.assertEquals('foo=12.4', params.get(3).toString());
        System.assertEquals('foo=', params.get(4).toString());
    }

    @isTest
    private static void ApiClient_nullMultiValuesListToEmptyParamsList() {
        Swagger.ApiClient client = new Swagger.ApiClient();
        List<Swagger.Param> params = client.makeParams('foo', null);

        System.assert(params.isEmpty());
    }

    @isTest
    private static void ApiClient_valuesListToSingleCsvKeyValuePair() {
        List<Object> values = new List<Object>{'bar', 4, false, 12.4, ''};
        Swagger.ApiClient client = new Swagger.ApiClient();
        List<Swagger.Param> params = client.makeParam('foo', values, 'csv');

        System.assertEquals(1, params.size());
        System.assertEquals('foo=bar%2C4%2Cfalse%2C12.4%2C', params.get(0).toString());
    }

    @isTest
    private static void ApiClient_valuesListToSingleSsvKeyValuePair() {
        List<Object> values = new List<Object>{'bar', 4, false, 12.4, ''};
        Swagger.ApiClient client = new Swagger.ApiClient();
        List<Swagger.Param> params = client.makeParam('foo', values, 'ssv');

        System.assertEquals(1, params.size());
        System.assertEquals('foo=bar+4+false+12.4+', params.get(0).toString());
    }

    @isTest
    private static void ApiClient_valuesListToSingleTsvKeyValuePair() {
        List<Object> values = new List<Object>{'bar', 4, false, 12.4, ''};
        Swagger.ApiClient client = new Swagger.ApiClient();
        List<Swagger.Param> params = client.makeParam('foo', values, 'tsv');

        System.assertEquals(1, params.size());
        System.assertEquals('foo=bar%094%09false%0912.4%09', params.get(0).toString());
    }

    @isTest
    private static void ApiClient_valuesListToSinglePipeSeparatedKeyValuePair() {
        List<Object> values = new List<Object>{'bar', 4, false, 12.4, ''};
        Swagger.ApiClient client = new Swagger.ApiClient();
        List<Swagger.Param> params = client.makeParam('foo', values, 'pipes');

        System.assertEquals(1, params.size());
        System.assertEquals('foo=bar%7C4%7Cfalse%7C12.4%7C', params.get(0).toString());
    }

    @isTest
    private static void ApiClient_nullValuesListToEmptyParamsList() {
        Swagger.ApiClient client = new Swagger.ApiClient();
        List<Swagger.Param> params = client.makeParam('foo', null, 'csv');

        System.assert(params.isEmpty());
    }

    @isTest
    private static void ApiClient_paramsFromAnyPrimitiveTypeDiscardNull() {
        Swagger.ApiClient client = new Swagger.ApiClient();
        List<Swagger.Param> params = new List<Swagger.Param>();
        params.addAll(client.makeParam('foo', 'bar'));
        params.addAll(client.makeParam('foo', 10));
        params.addAll(client.makeParam('foo', 12.6));
        params.addAll(client.makeParam('foo', true));
        params.addAll(client.makeParam('foo', ''));
        params.addAll(client.makeParam('foo', Datetime.newInstanceGmt(2017, 1, 1, 15, 0, 0)));
        params.addAll(client.makeParam('foo', null));

        System.assertEquals(6, params.size());
        System.assertEquals('foo=bar', params.get(0).toString());
        System.assertEquals('foo=10', params.get(1).toString());
        System.assertEquals('foo=12.6', params.get(2).toString());
        System.assertEquals('foo=true', params.get(3).toString());
        System.assertEquals('foo=', params.get(4).toString());
        System.assertEquals('foo=2017-01-01+15%3A00%3A00', params.get(5).toString());
    }

    @isTest
    private static void ApiClient_requiredParameterPasses() {
        Swagger.ApiClient client = new Swagger.ApiClient();
        client.assertNotNull('foo', 'bar');
    }

    @isTest
    private static void ApiClient_requiredParameterFails() {
        Swagger.ApiClient client = new Swagger.ApiClient();
        try {
            client.assertNotNull(null, 'bar');
        } catch (NullPointerException e) {
            System.assertEquals('Argument cannot be null: bar', e.getMessage());
            return;
        }
        System.assert(false);
    }

    @isTest
    private static void ApiClient_extractHeadersFromResponse() {
        HttpResponse res = new HttpResponse();
        res.setHeader('Content-Type', 'application/json');
        res.setHeader('Cache-Control', 'private, max-age=0');
        Map<String, String> headers = new MockApiClient().getHeaders(res);

        System.assertEquals(2, headers.size());
        System.assertEquals('application/json', headers.get('Content-Type'));
        System.assertEquals('private, max-age=0', headers.get('Cache-Control'));
    }

    @isTest
    private static void ApiClient_deserializeResponseBodyByContentType() {
        MockApiClient client = new MockApiClient();
        String jsonBody = '{"red":"apple","yellow":"banana","orange":"orange"}';
        Map<String, String> result1 = (Map<String, String>) client
            .toReturnValue(jsonBody, Map<String, String>.class, 'application/json');

        System.assertEquals(3, result1.size());
        System.assertEquals('apple', result1.get('red'));
        System.assertEquals('banana', result1.get('yellow'));
        System.assertEquals('orange', result1.get('orange'));

        String result2 = (String) client
            .toReturnValue('Hello, World!', String.class, 'text/plain');

        System.assertEquals('Hello, World!', result2);
    }

    @isTest
    private static void ApiClient_addStringifiedHeadersToRequest() {
        MockApiClient client = new MockApiClient();
        Map<String, Object> headers = new Map<String, Object>{
            'Content-Type' => 'application/json',
            'Max-Forwards' => 10
        };
        HttpRequest req = new HttpRequest();
        client.setHeaders(req, headers);

        System.assertEquals('application/json', req.getHeader('Content-Type'));
        System.assertEquals('10', req.getHeader('Max-Forwards'));
    }

    @isTest
    private static void ApiClient_serializeRequestBodyOrFormByContentType() {
        MockApiClient client = new MockApiClient();
        Map<String, Object> body1 = new Map<String, Object>{
            'hello' => 'world',
            'foo' => 15,
            'bar' => Datetime.newInstanceGmt(2017, 1, 1, 15, 0, 0),
            'bat' => false
        };
        Set<String> expected1 = new Set<String>{
            '"hello":"world"',
            '"foo":15',
            '"bar":"2017-01-01T15:00:00.000Z"',
            '"bat":false'
        };
        Set<String> actual1 = new Set<String>(client
            .toBody('application/json', body1, new List<Swagger.Param>())
            .removeStart('{')
            .removeEnd('}')
            .split(',')
        );
        System.assertEquals(expected1, actual1);

        String body2 = 'Hello, World!';
        String actual2 = client.toBody('text/plain', body2, new List<Swagger.Param>());
        System.assertEquals(body2, actual2);

        List<Swagger.Param> form = new List<Swagger.Param>{
            new Swagger.Param('hello', 'world'),
            new Swagger.Param('date', '2017-01-01 15:00:00')
        };
        String expected3 = 'hello=world&date=2017-01-01+15%3A00%3A00';
        String actual3 = client.toBody('application/x-www-form-urlencoded', '', form);
        System.assertEquals(expected3, actual3);
    }

    @isTest
    private static void ApiClient_usePreferredContentTypeOrFirstInList() {
        MockApiClient client = new MockApiClient();

        Map<String, Object> headers1 = new Map<String, Object>();
        List<String> types1 = new List<String>{'application/xml', 'application/json', 'text/plain'};
        String result1 = client.setContentTypeHeader(types1, headers1);
        System.assertEquals(1, headers1.size());
        System.assertEquals('application/json', headers1.get('Content-Type'));
        System.assertEquals('application/json', result1);

        Map<String, Object> headers2 = new Map<String, Object>();
        List<String> types2 = new List<String>{'application/xml', 'text/plain'};
        String result2 = client.setContentTypeHeader(types2, headers2);
        System.assertEquals(1, headers2.size());
        System.assertEquals('application/xml', headers2.get('Content-Type'));
        System.assertEquals('application/xml', result2);

        Map<String, Object> headers3 = new Map<String, Object>();
        String result3 = client.setContentTypeHeader(new List<String>(), headers3);
        System.assertEquals(1, headers3.size());
        System.assertEquals('application/json', headers3.get('Content-Type'));
        System.assertEquals('application/json', result3);
    }

    @isTest
    private static void ApiClient_usePreferredAcceptOrAllInListNoDefault() {
        MockApiClient client = new MockApiClient();

        Map<String, Object> headers1 = new Map<String, Object>();
        List<String> types1 = new List<String>{'application/xml', 'application/json', 'text/plain'};
        client.setAcceptHeader(types1, headers1);
        System.assertEquals(1, headers1.size());
        System.assertEquals('application/json', headers1.get('Accept'));

        Map<String, Object> headers2 = new Map<String, Object>();
        List<String> types2 = new List<String>{'application/xml', 'text/plain'};
        client.setAcceptHeader(types2, headers2);
        System.assertEquals(1, headers2.size());
        System.assertEquals('application/xml,text/plain', headers2.get('Accept'));

        Map<String, Object> headers3 = new Map<String, Object>();
        client.setAcceptHeader(new List<String>(), headers3);
        System.assert(headers3.isEmpty());
    }

    @isTest
    private static void ApiClient_formUrlWithQueryParamsPathParams() {
        MockApiClient client = new MockApiClient();
        String path = '/departments/{department}';
        Map<String, Object> params = new Map<String, Object>{'department' => 'finance'};
        List<Swagger.Param> queryParams = new List<Swagger.Param>{
            new Swagger.Param('foo', 'bar'),
            new Swagger.Param('bat', '123')
        };
        String expected = 'https://www.mccombs.utexas.edu/departments/finance?foo=bar&bat=123';
        String actual = client.toEndpoint(path, params, queryParams);
        System.assertEquals(expected, actual);
    }

    @isTest
    private static void ApiClient_returnParsedBody() {
        MockApiClient client = new MockApiClient();
        HttpResponse res = new HttpResponse();
        SwaggerResponseMock mock = new SwaggerResponseMock(res);
        Test.setMock(HttpCalloutMock.class, mock);

        res.setStatus('OK');
        res.setStatusCode(200);
        res.setHeader('Content-Type', 'application/json');
        res.setBody('{'
            + '"city":"Austin","country":"United States","latitude":30.28403639999999,'
            + '"longitude":-97.73789449999998,"postalCode":"78705","state":"Texas",'
            + '"street":"2110 Speedway"}');

        Address a = (Address) client.invoke(
            'GET', '/address', '',
            new List<Swagger.Param>(),
            new List<Swagger.Param>(),
            new Map<String, Object>(),
            new Map<String, Object>(),
            new List<String>{'application/json'},
            new List<String>{'text/plain'},
            new List<String>(),
            Address.class
        );

        System.assertEquals('Austin', a.getCity());
        System.assertEquals('United States', a.getCountry());
        System.assertEquals(30.28403639999999, a.getLatitude());
        System.assertEquals(-97.73789449999998, a.getLongitude());
        System.assertEquals('78705', a.getPostalCode());
        System.assertEquals('Texas', a.getState());
        System.assertEquals('2110 Speedway', a.getStreet());
    }

    @isTest
    private static void ApiClient_noReturnTypeReturnsNull() {
        MockApiClient client = new MockApiClient();
        HttpResponse res = new HttpResponse();
        SwaggerResponseMock mock = new SwaggerResponseMock(res);
        Test.setMock(HttpCalloutMock.class, mock);

        res.setStatus('OK');
        res.setStatusCode(200);

        Object o = client.invoke(
            'POST', '/address', '',
            new List<Swagger.Param>(),
            new List<Swagger.Param>(),
            new Map<String, Object>(),
            new Map<String, Object>(),
            new List<String>{'application/json'},
            new List<String>{'text/plain'},
            new List<String>(),
            null
        );

        System.assertEquals(null, o);
    }

    private class MockApiClient extends Swagger.ApiClient {
        public MockApiClient() {
            basePath = 'https://www.mccombs.utexas.edu';
        }
    }
}