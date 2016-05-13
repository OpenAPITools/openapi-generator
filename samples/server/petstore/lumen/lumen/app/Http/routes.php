<?php
/**
 * Swagger Petstore
 * @version 1.0.0
 */

$app->get('/', function () use ($app) {
    return $app->version();
});

/**
 * POST testEndpointParameters
 * Summary: Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
 * Notes: Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
 * Output-Formats: [application/xml, application/json]
 */
$app->POST('/fake', function($null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['number'])) {
        throw new \InvalidArgumentException('Missing the required parameter $number when calling testEndpointParameters');
    }
    if ($input['number'] > 543.2) {
        throw new \InvalidArgumentException('invalid value for $number when calling FakeApi.testEndpointParameters, must be smaller than or equal to 543.2.');
    }
    if ($input['number'] < 32.1) {
        throw new \InvalidArgumentException('invalid value for $number when calling FakeApi.testEndpointParameters, must be bigger than or equal to 32.1.');
    }
    $number = $input['number']; 

    if (!isset($input['double'])) {
        throw new \InvalidArgumentException('Missing the required parameter $double when calling testEndpointParameters');
    }
    if ($input['double'] > 123.4) {
        throw new \InvalidArgumentException('invalid value for $double when calling FakeApi.testEndpointParameters, must be smaller than or equal to 123.4.');
    }
    if ($input['double'] < 67.8) {
        throw new \InvalidArgumentException('invalid value for $double when calling FakeApi.testEndpointParameters, must be bigger than or equal to 67.8.');
    }
    $double = $input['double']; 

    if (!isset($input['string'])) {
        throw new \InvalidArgumentException('Missing the required parameter $string when calling testEndpointParameters');
    }
    if (!preg_match("/[a-z]/i", $input['string'])) {
        throw new \InvalidArgumentException('invalid value for $string when calling FakeApi.testEndpointParameters, must conform to the pattern /[a-z]/i.');
    }
    $string = $input['string']; 

    if (!isset($input['byte'])) {
        throw new \InvalidArgumentException('Missing the required parameter $byte when calling testEndpointParameters');
    }
    $byte = $input['byte']; 

    if ($input['integer'] > 100.0) {
        throw new \InvalidArgumentException('invalid value for $integer when calling FakeApi.testEndpointParameters, must be smaller than or equal to 100.0.');
    }
    if ($input['integer'] < 10.0) {
        throw new \InvalidArgumentException('invalid value for $integer when calling FakeApi.testEndpointParameters, must be bigger than or equal to 10.0.');
    }
    $integer = $input['integer']; 

    if ($input['int32'] > 200.0) {
        throw new \InvalidArgumentException('invalid value for $int32 when calling FakeApi.testEndpointParameters, must be smaller than or equal to 200.0.');
    }
    if ($input['int32'] < 20.0) {
        throw new \InvalidArgumentException('invalid value for $int32 when calling FakeApi.testEndpointParameters, must be bigger than or equal to 20.0.');
    }
    $int32 = $input['int32']; 

    $int64 = $input['int64']; 

    if ($input['float'] > 987.6) {
        throw new \InvalidArgumentException('invalid value for $float when calling FakeApi.testEndpointParameters, must be smaller than or equal to 987.6.');
    }
    $float = $input['float']; 

    $binary = $input['binary']; 

    $date = $input['date']; 

    $dateTime = $input['dateTime']; 

    if (strlen($input['password']) > 64) {
        throw new \InvalidArgumentException('invalid length for $password when calling FakeApi.testEndpointParameters, must be smaller than or equal to 64.');
    }
    if (strlen($input['password']) < 10) {
        throw new \InvalidArgumentException('invalid length for $password when calling FakeApi.testEndpointParameters, must be bigger than or equal to 10.');
    }
    $password = $input['password']; 

    return response('How about implementing testEndpointParameters as a POST method ?');
});

/**
 * POST addPet
 * Summary: Add a new pet to the store
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->POST('/pet', function($null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['body'])) {
        throw new \InvalidArgumentException('Missing the required parameter $body when calling addPet');
    }
    $body = $input['body']; 

    return response('How about implementing addPet as a POST method ?');
});

/**
 * DELETE deletePet
 * Summary: Deletes a pet
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->DELETE('/pet/{petId}', function($petId, $null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['petId'])) {
        throw new \InvalidArgumentException('Missing the required parameter $petId when calling deletePet');
    }
    $petId = $input['petId']; 

    $apiKey = $input['apiKey']; 

    return response('How about implementing deletePet as a DELETE method ?');
});

/**
 * GET findPetsByStatus
 * Summary: Finds Pets by status
 * Notes: Multiple status values can be provided with comma separated strings
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/pet/findByStatus', function($null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['status'])) {
        throw new \InvalidArgumentException('Missing the required parameter $status when calling findPetsByStatus');
    }
    $status = $input['status']; 

    return response('How about implementing findPetsByStatus as a GET method ?');
});

/**
 * GET findPetsByTags
 * Summary: Finds Pets by tags
 * Notes: Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/pet/findByTags', function($null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['tags'])) {
        throw new \InvalidArgumentException('Missing the required parameter $tags when calling findPetsByTags');
    }
    $tags = $input['tags']; 

    return response('How about implementing findPetsByTags as a GET method ?');
});

/**
 * GET getPetById
 * Summary: Find pet by ID
 * Notes: Returns a single pet
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/pet/{petId}', function($petId, $null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['petId'])) {
        throw new \InvalidArgumentException('Missing the required parameter $petId when calling getPetById');
    }
    $petId = $input['petId']; 

    return response('How about implementing getPetById as a GET method ?');
});

/**
 * PUT updatePet
 * Summary: Update an existing pet
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->PUT('/pet', function($null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['body'])) {
        throw new \InvalidArgumentException('Missing the required parameter $body when calling updatePet');
    }
    $body = $input['body']; 

    return response('How about implementing updatePet as a PUT method ?');
});

/**
 * POST updatePetWithForm
 * Summary: Updates a pet in the store with form data
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->POST('/pet/{petId}', function($petId, $null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['petId'])) {
        throw new \InvalidArgumentException('Missing the required parameter $petId when calling updatePetWithForm');
    }
    $petId = $input['petId']; 

    $name = $input['name']; 

    $status = $input['status']; 

    return response('How about implementing updatePetWithForm as a POST method ?');
});

/**
 * POST uploadFile
 * Summary: uploads an image
 * Notes: 
 * Output-Formats: [application/json]
 */
$app->POST('/pet/{petId}/uploadImage', function($petId, $null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['petId'])) {
        throw new \InvalidArgumentException('Missing the required parameter $petId when calling uploadFile');
    }
    $petId = $input['petId']; 

    $additionalMetadata = $input['additionalMetadata']; 

    $file = $input['file']; 

    return response('How about implementing uploadFile as a POST method ?');
});

/**
 * DELETE deleteOrder
 * Summary: Delete purchase order by ID
 * Notes: For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
 * Output-Formats: [application/xml, application/json]
 */
$app->DELETE('/store/order/{orderId}', function($orderId, $null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['orderId'])) {
        throw new \InvalidArgumentException('Missing the required parameter $orderId when calling deleteOrder');
    }
    if ($input['orderId'] < 1.0) {
        throw new \InvalidArgumentException('invalid value for $orderId when calling StoreApi.deleteOrder, must be bigger than or equal to 1.0.');
    }
    $orderId = $input['orderId']; 

    return response('How about implementing deleteOrder as a DELETE method ?');
});

/**
 * GET getInventory
 * Summary: Returns pet inventories by status
 * Notes: Returns a map of status codes to quantities
 * Output-Formats: [application/json]
 */
$app->GET('/store/inventory', function($null = null) use ($app) {
    $input = Request::all();

    return response('How about implementing getInventory as a GET method ?');
});

/**
 * GET getOrderById
 * Summary: Find purchase order by ID
 * Notes: For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/store/order/{orderId}', function($orderId, $null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['orderId'])) {
        throw new \InvalidArgumentException('Missing the required parameter $orderId when calling getOrderById');
    }
    if ($input['orderId'] > 5.0) {
        throw new \InvalidArgumentException('invalid value for $orderId when calling StoreApi.getOrderById, must be smaller than or equal to 5.0.');
    }
    if ($input['orderId'] < 1.0) {
        throw new \InvalidArgumentException('invalid value for $orderId when calling StoreApi.getOrderById, must be bigger than or equal to 1.0.');
    }
    $orderId = $input['orderId']; 

    return response('How about implementing getOrderById as a GET method ?');
});

/**
 * POST placeOrder
 * Summary: Place an order for a pet
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->POST('/store/order', function($null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['body'])) {
        throw new \InvalidArgumentException('Missing the required parameter $body when calling placeOrder');
    }
    $body = $input['body']; 

    return response('How about implementing placeOrder as a POST method ?');
});

/**
 * POST createUser
 * Summary: Create user
 * Notes: This can only be done by the logged in user.
 * Output-Formats: [application/xml, application/json]
 */
$app->POST('/user', function($null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['body'])) {
        throw new \InvalidArgumentException('Missing the required parameter $body when calling createUser');
    }
    $body = $input['body']; 

    return response('How about implementing createUser as a POST method ?');
});

/**
 * POST createUsersWithArrayInput
 * Summary: Creates list of users with given input array
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->POST('/user/createWithArray', function($null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['body'])) {
        throw new \InvalidArgumentException('Missing the required parameter $body when calling createUsersWithArrayInput');
    }
    $body = $input['body']; 

    return response('How about implementing createUsersWithArrayInput as a POST method ?');
});

/**
 * POST createUsersWithListInput
 * Summary: Creates list of users with given input array
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->POST('/user/createWithList', function($null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['body'])) {
        throw new \InvalidArgumentException('Missing the required parameter $body when calling createUsersWithListInput');
    }
    $body = $input['body']; 

    return response('How about implementing createUsersWithListInput as a POST method ?');
});

/**
 * DELETE deleteUser
 * Summary: Delete user
 * Notes: This can only be done by the logged in user.
 * Output-Formats: [application/xml, application/json]
 */
$app->DELETE('/user/{username}', function($username, $null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['username'])) {
        throw new \InvalidArgumentException('Missing the required parameter $username when calling deleteUser');
    }
    $username = $input['username']; 

    return response('How about implementing deleteUser as a DELETE method ?');
});

/**
 * GET getUserByName
 * Summary: Get user by user name
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/user/{username}', function($username, $null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['username'])) {
        throw new \InvalidArgumentException('Missing the required parameter $username when calling getUserByName');
    }
    $username = $input['username']; 

    return response('How about implementing getUserByName as a GET method ?');
});

/**
 * GET loginUser
 * Summary: Logs user into the system
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/user/login', function($null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['username'])) {
        throw new \InvalidArgumentException('Missing the required parameter $username when calling loginUser');
    }
    $username = $input['username']; 

    if (!isset($input['password'])) {
        throw new \InvalidArgumentException('Missing the required parameter $password when calling loginUser');
    }
    $password = $input['password']; 

    return response('How about implementing loginUser as a GET method ?');
});

/**
 * GET logoutUser
 * Summary: Logs out current logged in user session
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/user/logout', function($null = null) use ($app) {
    $input = Request::all();

    return response('How about implementing logoutUser as a GET method ?');
});

/**
 * PUT updateUser
 * Summary: Updated user
 * Notes: This can only be done by the logged in user.
 * Output-Formats: [application/xml, application/json]
 */
$app->PUT('/user/{username}', function($username, $null = null) use ($app) {
    $input = Request::all();

    if (!isset($input['username'])) {
        throw new \InvalidArgumentException('Missing the required parameter $username when calling updateUser');
    }
    $username = $input['username']; 

    if (!isset($input['body'])) {
        throw new \InvalidArgumentException('Missing the required parameter $body when calling updateUser');
    }
    $body = $input['body']; 

    return response('How about implementing updateUser as a PUT method ?');
});


