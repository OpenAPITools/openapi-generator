@isTest
private class SwagUserApiTest {
    /**
     * Create user
     *
     * This can only be done by the logged in user.
     */
    @isTest
    private static void createUserTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(201);
        res.setStatus('Created');
        Test.setMock(HttpCalloutMock.class, new SwaggerResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'body' => SwagUser.getExample()
        };

        SwagClient client;
        SwagUserApi api;

        api = new SwagUserApi(new SwagClient());

        api.createUser(params);
    }

    /**
     * Creates list of users with given input array
     *
     * 
     */
    @isTest
    private static void createUsersWithArrayInputTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new SwaggerResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'body' => new List<SwagUser>{SwagUser.getExample()}
        };

        SwagClient client;
        SwagUserApi api;

        api = new SwagUserApi(new SwagClient());

        api.createUsersWithArrayInput(params);
    }

    /**
     * Creates list of users with given input array
     *
     * 
     */
    @isTest
    private static void createUsersWithListInputTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new SwaggerResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'body' => new List<SwagUser>{SwagUser.getExample()}
        };

        SwagClient client;
        SwagUserApi api;

        api = new SwagUserApi(new SwagClient());

        api.createUsersWithListInput(params);
    }

    /**
     * Delete user
     *
     * This can only be done by the logged in user.
     */
    @isTest
    private static void deleteUserTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new SwaggerResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'username' => 'username_example'
        };

        SwagClient client;
        SwagUserApi api;

        api = new SwagUserApi(new SwagClient());

        api.deleteUser(params);
    }

    /**
     * Get user by user name
     *
     * 
     */
    @isTest
    private static void getUserByNameTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new SwaggerResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'username' => 'username_example'
        };

        SwagClient client;
        SwagUserApi api;
        SwagUser response;
        SwagUser expectedResponse;

        api = new SwagUserApi(new SwagClient());

        res.setHeader('Content-Type', 'application/json');
        res.setBody('{\n  "firstName" : "aeiou",\n  "lastName" : "aeiou",\n  "password" : "aeiou",\n  "userStatus" : 6,\n  "phone" : "aeiou",\n  "id" : 0,\n  "email" : "aeiou",\n  "username" : "aeiou"\n}');
        expectedResponse = SwagUser.getExample();
        response = (SwagUser) api.getUserByName(params);
        System.assertEquals(expectedResponse, response);
    }

    /**
     * Logs user into the system
     *
     * 
     */
    @isTest
    private static void loginUserTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new SwaggerResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'username' => 'username_example',
            'password' => 'password_example'
        };

        SwagClient client;
        SwagUserApi api;
        String response;
        String expectedResponse;

        api = new SwagUserApi(new SwagClient());

        res.setHeader('Content-Type', 'application/json');
        res.setBody('"aeiou"');
        expectedResponse = 'aeiou';
        response = (String) api.loginUser(params);
        System.assertEquals(expectedResponse, response);
    }

    /**
     * Logs out current logged in user session
     *
     * 
     */
    @isTest
    private static void logoutUserTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new SwaggerResponseMock(res));

        SwagClient client;
        SwagUserApi api;

        api = new SwagUserApi(new SwagClient());

        api.logoutUser();
    }

    /**
     * Updated user
     *
     * This can only be done by the logged in user.
     */
    @isTest
    private static void updateUserTest() {
        HttpResponse res = new HttpResponse();
        res.setStatusCode(200);
        res.setStatus('OK');
        Test.setMock(HttpCalloutMock.class, new SwaggerResponseMock(res));

        Map<String, Object> params = new Map<String, Object>{
            'username' => 'username_example',
            'body' => SwagUser.getExample()
        };

        SwagClient client;
        SwagUserApi api;

        api = new SwagUserApi(new SwagClient());

        api.updateUser(params);
    }
}