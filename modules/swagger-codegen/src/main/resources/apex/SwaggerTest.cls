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
    private static void HttpBasicAuth_base64EncodeCredentials() {
        Map<String, Object> headers = new Map<String, String>();
        List<Swagger.Param> query = new List<Swagger.Param>();
        Swagger.HttpBasicAuth auth = new Swagger.HttpBasicAuth();
        auth.setCredentials('username', 'password');
        auth.apply(headers, query);

        System.assert(query.isEmpty());
        System.assertEquals(1, headers.size());
        System.assertEquals('Basic dXNlcm5hbWU6cGFzc3dvcmQ=', headers.get('Authorization'));
    }

    @isTest
    private static void HttpBasicAuth_base64EncodeUsernamePassword() {
        Map<String, Object> headers = new Map<String, String>();
        List<Swagger.Param> query = new List<Swagger.Param>();
        Swagger.HttpBasicAuth auth = new Swagger.HttpBasicAuth();
        auth.setUsername('username');
        auth.setPassword('password');
        auth.apply(headers, query);

        System.assert(query.isEmpty());
        System.assertEquals(1, headers.size());
        System.assertEquals('Basic dXNlcm5hbWU6cGFzc3dvcmQ=', headers.get('Authorization'));
    }

    @isTest
    private static void OAuth2_tokenInAuthorizationHeaderWithBearerPrefix() {
        Map<String, Object> headers = new Map<String, String>();
        List<Swagger.Param> query = new List<Swagger.Param>();
        Swagger.OAuth2 auth = new Swagger.OAuth2();
        auth.setAccessToken('foo-bar-api-key');
        auth.apply(headers, query);

        System.assert(query.isEmpty());
        System.assertEquals(1, headers.size());
        System.assertEquals('Bearer foo-bar-api-key', headers.get('Authorization'));
    }

    @isTest
    private static void ApiClient_returnAuthenticationMatchingInput() {
        MockApiClient client = new MockApiClient();
        Swagger.ApiKeyHeaderAuth auth1 = new Swagger.ApiKeyHeaderAuth('foo');
        Swagger.ApiKeyQueryAuth auth2 = new Swagger.ApiKeyQueryAuth('foo');
        Swagger.HttpBasicAuth auth3 = new Swagger.HttpBasicAuth();
        Swagger.OAuth2 auth4 = new Swagger.OAuth2();

        client.authentications.put('auth1', auth1);
        client.authentications.put('auth2', auth2);
        client.authentications.put('auth3', auth3);
        client.authentications.put('auth4', auth4);

        System.assertEquals(auth1, client.getAuthentication('auth1'));
        System.assertEquals(auth2, client.getAuthentication('auth2'));
        System.assertEquals(auth3, client.getAuthentication('auth3'));
        System.assertEquals(auth4, client.getAuthentication('auth4'));
    }

    @isTest
    private static void ApiClient_noAuthenticationsMatchInputReturnNull() {
        MockApiClient client = new MockApiClient();
        Swagger.OAuth2 auth = new Swagger.OAuth2();
        client.authentications.put('auth', auth);

        System.assertEquals(auth, client.getAuthentication('auth'));
        System.assertEquals(null, client.getAuthentication('no-auth'));
    }

    @isTest
    private static void ApiClient_setUsernamePasswordFirstBasicAuthOnly() {
        MockApiClient client = new MockApiClient();
        Swagger.OAuth2 auth1 = new Swagger.OAuth2();
        Swagger.ApiKeyQueryAuth auth2 = new Swagger.ApiKeyQueryAuth('auth2');
        Swagger.ApiKeyHeaderAuth auth3 = new Swagger.ApiKeyHeaderAuth('auth3');
        Swagger.HttpBasicAuth auth4 = new Swagger.HttpBasicAuth();
        Swagger.HttpBasicAuth auth5 = new Swagger.HttpBasicAuth();
        client.authentications.put('auth1', auth1);
        client.authentications.put('auth2', auth2);
        client.authentications.put('auth3', auth3);
        client.authentications.put('auth4', auth4);
        client.authentications.put('auth5', auth5);
        client.setUsername('username');
        client.setPassword('password');

        System.assertEquals('Bearer ', auth1.getHeaderValue());
        System.assertEquals('', auth2.getApiKey());
        System.assertEquals('', auth3.getApiKey());
        System.assertEquals('Basic dXNlcm5hbWU6cGFzc3dvcmQ=', auth4.getHeaderValue());
        System.assertEquals('Basic Og==', auth5.getHeaderValue());
    }

    @isTest
    private static void ApiClient_setUsernameExceptionNoBasicAuth() {
        Swagger.ApiClient client = new Swagger.ApiClient();
        try {
            client.setUsername('username');
        } catch (NoSuchElementException e) {
            return;
        }
        System.assert(false);
    }

    @isTest
    private static void ApiClient_setPasswordExceptionNoBasicAuth() {
        Swagger.ApiClient client = new Swagger.ApiClient();
        try {
            client.setPassword('password');
        } catch (NoSuchElementException e) {
            return;
        }
        System.assert(false);
    }

    @isTest
    private static void ApiClient_setCredentialsFirstBasicAuthOnly() {
        MockApiClient client = new MockApiClient();
        Swagger.OAuth2 auth1 = new Swagger.OAuth2();
        Swagger.ApiKeyQueryAuth auth2 = new Swagger.ApiKeyQueryAuth('auth2');
        Swagger.ApiKeyHeaderAuth auth3 = new Swagger.ApiKeyHeaderAuth('auth3');
        Swagger.HttpBasicAuth auth4 = new Swagger.HttpBasicAuth();
        Swagger.HttpBasicAuth auth5 = new Swagger.HttpBasicAuth();
        client.authentications.put('auth1', auth1);
        client.authentications.put('auth2', auth2);
        client.authentications.put('auth3', auth3);
        client.authentications.put('auth4', auth4);
        client.authentications.put('auth5', auth5);
        client.setCredentials('username', 'password');

        System.assertEquals('Bearer ', auth1.getHeaderValue());
        System.assertEquals('', auth2.getApiKey());
        System.assertEquals('', auth3.getApiKey());
        System.assertEquals('Basic dXNlcm5hbWU6cGFzc3dvcmQ=', auth4.getHeaderValue());
        System.assertEquals('Basic Og==', auth5.getHeaderValue());
    }

    @isTest
    private static void ApiClient_setCredentialsExceptionNoBasicAuth() {
        Swagger.ApiClient client = new Swagger.ApiClient();
        try {
            client.setCredentials('username', 'password');
        } catch (NoSuchElementException e) {
            return;
        }
        System.assert(false);
    }

    @isTest
    private static void ApiClient_setApiKeyFirstKeyAuthOnly() {
        MockApiClient client = new MockApiClient();
        Swagger.OAuth2 auth1 = new Swagger.OAuth2();
        Swagger.HttpBasicAuth auth2 = new Swagger.HttpBasicAuth();
        Swagger.HttpBasicAuth auth3 = new Swagger.HttpBasicAuth();
        Swagger.ApiKeyQueryAuth auth4 = new Swagger.ApiKeyQueryAuth('auth4');
        Swagger.ApiKeyHeaderAuth auth5 = new Swagger.ApiKeyHeaderAuth('auth5');
        client.authentications.put('auth1', auth1);
        client.authentications.put('auth2', auth2);
        client.authentications.put('auth3', auth3);
        client.authentications.put('auth4', auth4);
        client.authentications.put('auth5', auth5);
        client.setApiKey('foo-bar-api-key');

        System.assertEquals('Bearer ', auth1.getHeaderValue());
        System.assertEquals('Basic Og==', auth2.getHeaderValue());
        System.assertEquals('Basic Og==', auth3.getHeaderValue());
        System.assertEquals('foo-bar-api-key', auth4.getApiKey());
        System.assertEquals('', auth5.getApiKey());
    }

    @isTest
    private static void ApiClient_setApiKeyExceptionNoKeyAuth() {
        Swagger.ApiClient client = new Swagger.ApiClient();
        try {
            client.setApiKey('foo-bar-api-key');
        } catch (NoSuchElementException e) {
            return;
        }
        System.assert(false);
    }

    @isTest
    private static void ApiClient_setAccessTokenFirstOauthOnly() {
        MockApiClient client = new MockApiClient();
        Swagger.HttpBasicAuth auth1 = new Swagger.HttpBasicAuth();
        Swagger.ApiKeyQueryAuth auth2 = new Swagger.ApiKeyQueryAuth('auth2');
        Swagger.ApiKeyHeaderAuth auth3 = new Swagger.ApiKeyHeaderAuth('auth3');
        Swagger.OAuth2 auth4 = new Swagger.OAuth2();
        Swagger.OAuth2 auth5 = new Swagger.OAuth2();
        client.authentications.put('auth1', auth1);
        client.authentications.put('auth2', auth2);
        client.authentications.put('auth3', auth3);
        client.authentications.put('auth4', auth4);
        client.authentications.put('auth5', auth5);
        client.setAccessToken('foo-bar-api-key');

        System.assertEquals('Basic Og==', auth1.getHeaderValue());
        System.assertEquals('', auth2.getApiKey());
        System.assertEquals('', auth3.getApiKey());
        System.assertEquals('Bearer foo-bar-api-key', auth4.getHeaderValue());
        System.assertEquals('Bearer ', auth5.getHeaderValue());
    }

    @isTest
    private static void ApiClient_setAccessTokenExceptionNoOAuth() {
        Swagger.ApiClient client = new Swagger.ApiClient();
        try {
            client.setAccessToken('foo-bar-api-key');
        } catch (NoSuchElementException e) {
            return;
        }
        System.assert(false);
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
    private static void ApiClient_applyOnlyGivenAuthMethodsToParams() {
        MockApiClient client = new MockApiClient();
        Map<String, Object> headers = new Map<String, Object>();
        Swagger.OAuth2 auth1 = new Swagger.OAuth2();
        Swagger.ApiKeyHeaderAuth auth2 = new Swagger.ApiKeyHeaderAuth('X-Authentication-Token');
        auth1.setAccessToken('boo-bat-api-key');
        auth2.setApiKey('foo-bar-api-key');
        client.authentications.put('auth1', auth1);
        client.authentications.put('auth2', auth2);
        client.applyAuthentication(new List<String>{'auth2'}, headers, new List<Swagger.Param>());

        System.assertEquals(1, headers.size());
        System.assertEquals('foo-bar-api-key', headers.get('X-Authentication-Token'));
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
    private static void ApiClient_setupRequestWithBody() {
        MockApiClient client = new MockApiClient();
        HttpResponse res = new HttpResponse();
        SwaggerResponseMock mock = new SwaggerResponseMock(res);
        Swagger.OAuth2 auth = new Swagger.OAuth2();
        auth.setAccessToken('foo-bar-access-token');
        client.authentications.put('oauth_method', auth);
        Test.setMock(HttpCalloutMock.class, mock);

        HttpResponse returned = client.getResponse(
            'PUT', '/courses/{course}/assignments/{assignmentId}',
            new Map<String, Object> {
                'title' => 'Chapter 4 quiz',
                'timed' => true,
                'time' => 60,
                'points' => 20.5,
                'due' => Datetime.newInstanceGmt(2016, 5, 10, 23, 59, 59),
                'description' => ''
            },
            new List<Swagger.Param>(),
            new List<Swagger.Param>(),
            new Map<String, Object>{
                'course' => 'acc321',
                'assignmentId' => 5
            },
            new Map<String, Object>{
                'X-Session' => 'foo-bar-444'
            },
            new List<String>{'application/json', 'application/xml'},
            new List<String>{'application/json', 'application/xml'},
            new List<String>{'oauth_method'}
        );

        HttpRequest req = mock.getRequest();
        String expectedUrl = 'https://www.mccombs.utexas.edu/courses/acc321/assignments/5';
        Set<String> body = new Set<String>(req
            .getBody()
            .removeStart('{')
            .removeEnd('}')
            .split(',')
        );

        System.assertEquals(res, returned);
        System.assertEquals(expectedUrl, req.getEndpoint());
        System.assertEquals(6, body.size());
        System.assert(body.contains('"title":"Chapter 4 quiz"'));
        System.assert(body.contains('"timed":true'));
        System.assert(body.contains('"time":60'));
        System.assert(body.contains('"points":20.5'));
        System.assert(body.contains('"due":"2016-05-10T23:59:59.000Z"'));
        System.assert(body.contains('"description":""'));
        System.assertEquals('PUT', req.getMethod());
        System.assertEquals('Bearer foo-bar-access-token', req.getHeader('Authorization'));
        System.assertEquals('foo-bar-444', req.getHeader('X-Session'));
        System.assertEquals('application/json', req.getHeader('Accept'));
        System.assertEquals('application/json', req.getHeader('Content-Type'));
    }

    @isTest
    private static void ApiClient_setupRequestWithForm() {
        MockApiClient client = new MockApiClient();
        HttpResponse res = new HttpResponse();
        SwaggerResponseMock mock = new SwaggerResponseMock(res);
        Swagger.OAuth2 auth = new Swagger.OAuth2();
        auth.setAccessToken('foo-bar-access-token');
        client.authentications.put('oauth_method', auth);
        Test.setMock(HttpCalloutMock.class, mock);

        HttpResponse returned = client.getResponse(
            'PUT', '/courses/{course}/assignments/{assignmentId}', '',
            new List<Swagger.Param>(),
            new List<Swagger.Param>{
                new Swagger.Param('title', 'Chapter 4 quiz'),
                new Swagger.Param('timed', 'true'),
                new Swagger.Param('time', '60'),
                new Swagger.Param('points', '20.5'),
                new Swagger.Param('due', '2016-05-10 18:59:59'),
                new Swagger.Param('description', 'complete & upload \'section1: advanced\'')
            },
            new Map<String, Object>{
                'course' => 'acc321',
                'assignmentId' => 5
            },
            new Map<String, Object>{
                'X-Session' => 'foo-bar-444'
            },
            new List<String>{'text/html', 'application/xml'},
            new List<String>{'application/x-www-form-urlencoded'},
            new List<String>{'oauth_method'}
        );

        HttpRequest req = mock.getRequest();
        String expectedUrl = 'https://www.mccombs.utexas.edu/courses/acc321/assignments/5';
        Set<String> body = new Set<String>(req.getBody().split('&'));

        System.assertEquals(res, returned);
        System.assertEquals(expectedUrl, req.getEndpoint());
        System.assertEquals(6, body.size());
        System.assert(body.contains('title=Chapter+4+quiz'));
        System.assert(body.contains('timed=true'));
        System.assert(body.contains('time=60'));
        System.assert(body.contains('points=20.5'));
        System.assert(body.contains('due=2016-05-10+18%3A59%3A59'));
        System.assert(body.contains('description=complete+%26+upload+%27section1%3A+advanced%27'));
        System.assertEquals('PUT', req.getMethod());
        System.assertEquals('Bearer foo-bar-access-token', req.getHeader('Authorization'));
        System.assertEquals('foo-bar-444', req.getHeader('X-Session'));
        System.assertEquals('text/html,application/xml', req.getHeader('Accept'));
        System.assertEquals('application/x-www-form-urlencoded', req.getHeader('Content-Type'));
    }

    @isTest
    private static void ApiClient_setupRequestWithQuery() {
        MockApiClient client = new MockApiClient();
        HttpResponse res = new HttpResponse();
        SwaggerResponseMock mock = new SwaggerResponseMock(res);
        Swagger.OAuth2 auth = new Swagger.OAuth2();
        auth.setAccessToken('foo-bar-access-token');
        client.authentications.put('oauth_method', auth);
        Test.setMock(HttpCalloutMock.class, mock);

        HttpResponse returned = client.getResponse(
            'GET', '/courses/{course}/assignments', '',
            new List<Swagger.Param>{
                new Swagger.Param('title', '#chapter1:section2'),
                new Swagger.Param('due', '2016-05-10 18:59:59')
            },
            new List<Swagger.Param>(),
            new Map<String, Object>{
                'course' => 'acc321'
            },
            new Map<String, Object>(),
            new List<String>{'application/xml'},
            new List<String>{'text/plain'},
            new List<String>{'oauth_method'}
        );

        HttpRequest req = mock.getRequest();
        List<String> splitUrl = req.getEndpoint().split('\\?');
        String expectedUrl = 'https://www.mccombs.utexas.edu/courses/acc321/assignments';
        Set<String> query = new Set<String>(splitUrl.get(1).split('&'));

        System.assertEquals(res, returned);
        System.assertEquals(expectedUrl, splitUrl.get(0));
        System.assertEquals(2, query.size());
        System.assert(query.contains('title=%23chapter1%3Asection2'));
        System.assert(query.contains('due=2016-05-10+18%3A59%3A59'));
        System.assertEquals('GET', req.getMethod());
        System.assertEquals('Bearer foo-bar-access-token', req.getHeader('Authorization'));
        System.assertEquals('application/xml', req.getHeader('Accept'));
        System.assertEquals('text/plain', req.getHeader('Content-Type'));
    }

    @isTest
    private static void ApiClient_nonSuccessfulStatusCodeException() {
        MockApiClient client = new MockApiClient();
        HttpResponse res = new HttpResponse();
        SwaggerResponseMock mock = new SwaggerResponseMock(res);
        Swagger.OAuth2 auth = new Swagger.OAuth2();
        auth.setAccessToken('foo-bar-access-token');
        client.authentications.put('oauth_method', auth);
        Test.setMock(HttpCalloutMock.class, mock);

        res.setStatus('Not Found');
        res.setStatusCode(404);
        res.setHeader('X-Request-ID', '1234567890');
        res.setHeader('Content-Type', 'application/json');
        res.setBody('{"error":"the specified course does not exist"}');

        try {
            client.invoke(
	            'GET', '/courses/{course}', '',
	            new List<Swagger.Param>(),
	            new List<Swagger.Param>(),
	            new Map<String, Object>{
	                'course' => 'acc321'
	            },
	            new Map<String, Object>(),
	            new List<String>{'application/json'},
	            new List<String>{'text/plain'},
	            new List<String>{'oauth_method'},
	            null
	        );
        } catch (Swagger.ApiException e) {
            Map<String, String> headers = e.getHeaders();

            System.assertEquals('API returned HTTP 404: Not Found', e.getMessage());
            System.assertEquals(404, e.getStatusCode());
            System.assertEquals('Not Found', e.getStatus());
            System.assertEquals('{"error":"the specified course does not exist"}', e.getBody());
            System.assertEquals(2, headers.size());
            System.assertEquals('1234567890', headers.get('X-Request-ID'));
            System.assertEquals('application/json', headers.get('Content-Type'));
            return;
        }

        System.assert(false);
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