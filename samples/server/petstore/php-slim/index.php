<?php
/**
 * OpenAPI Petstore
 * @version 1.0.0
 */

require_once __DIR__ . '/vendor/autoload.php';

$app = new Slim\App();


/**
 * PATCH testSpecialTags
 * Summary: To test special tags
 * Notes: To test special tags
 * Output-Formats: [application/json]
 */
$app->PATCH('/v2/another-fake/dummy', function($request, $response, $args) {
    $body = $request->getParsedBody();
    $response->write('How about implementing testSpecialTags as a PATCH method ?');
    return $response;
});


/**
 * POST fakeOuterBooleanSerialize
 * Summary: 
 * Notes: Test serialization of outer boolean types
 * Output-Formats: [*_/_*]
 */
$app->POST('/v2/fake/outer/boolean', function($request, $response, $args) {
    $body = $request->getParsedBody();
    $response->write('How about implementing fakeOuterBooleanSerialize as a POST method ?');
    return $response;
});


/**
 * POST fakeOuterCompositeSerialize
 * Summary: 
 * Notes: Test serialization of object with outer number type
 * Output-Formats: [*_/_*]
 */
$app->POST('/v2/fake/outer/composite', function($request, $response, $args) {
    $body = $request->getParsedBody();
    $response->write('How about implementing fakeOuterCompositeSerialize as a POST method ?');
    return $response;
});


/**
 * POST fakeOuterNumberSerialize
 * Summary: 
 * Notes: Test serialization of outer number types
 * Output-Formats: [*_/_*]
 */
$app->POST('/v2/fake/outer/number', function($request, $response, $args) {
    $body = $request->getParsedBody();
    $response->write('How about implementing fakeOuterNumberSerialize as a POST method ?');
    return $response;
});


/**
 * POST fakeOuterStringSerialize
 * Summary: 
 * Notes: Test serialization of outer string types
 * Output-Formats: [*_/_*]
 */
$app->POST('/v2/fake/outer/string', function($request, $response, $args) {
    $body = $request->getParsedBody();
    $response->write('How about implementing fakeOuterStringSerialize as a POST method ?');
    return $response;
});


/**
 * PUT testBodyWithQueryParams
 * Summary: 
 * Notes: 
 */
$app->PUT('/v2/fake/body-with-query-params', function($request, $response, $args) {
    $queryParams = $request->getQueryParams();
    $query = $request->getQueryParam('query');
    $body = $request->getParsedBody();
    $response->write('How about implementing testBodyWithQueryParams as a PUT method ?');
    return $response;
});


/**
 * PATCH testClientModel
 * Summary: To test \&quot;client\&quot; model
 * Notes: To test \&quot;client\&quot; model
 * Output-Formats: [application/json]
 */
$app->PATCH('/v2/fake', function($request, $response, $args) {
    $body = $request->getParsedBody();
    $response->write('How about implementing testClientModel as a PATCH method ?');
    return $response;
});


/**
 * POST testEndpointParameters
 * Summary: Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
 * Notes: Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
 */
$app->POST('/v2/fake', function($request, $response, $args) {
    $binary = $request->getUploadedFiles()['binary'];
    $response->write('How about implementing testEndpointParameters as a POST method ?');
    return $response;
});


/**
 * GET testEnumParameters
 * Summary: To test enum parameters
 * Notes: To test enum parameters
 */
$app->GET('/v2/fake', function($request, $response, $args) {
    $headers = $request->getHeaders();
    $enumHeaderStringArray = $request->hasHeader('enum_header_string_array') ? $headers['enum_header_string_array'] : null;
    $enumHeaderString = $request->hasHeader('enum_header_string') ? $headers['enum_header_string'] : null;
    $queryParams = $request->getQueryParams();
    $enumQueryStringArray = $request->getQueryParam('enum_query_string_array');
    $enumQueryString = $request->getQueryParam('enum_query_string');
    $enumQueryInteger = $request->getQueryParam('enum_query_integer');
    $enumQueryDouble = $request->getQueryParam('enum_query_double');
    $response->write('How about implementing testEnumParameters as a GET method ?');
    return $response;
});


/**
 * POST testInlineAdditionalProperties
 * Summary: test inline additionalProperties
 * Notes: 
 */
$app->POST('/v2/fake/inline-additionalProperties', function($request, $response, $args) {
    $body = $request->getParsedBody();
    $response->write('How about implementing testInlineAdditionalProperties as a POST method ?');
    return $response;
});


/**
 * GET testJsonFormData
 * Summary: test json serialization of form data
 * Notes: 
 */
$app->GET('/v2/fake/jsonFormData', function($request, $response, $args) {
    $response->write('How about implementing testJsonFormData as a GET method ?');
    return $response;
});


/**
 * PATCH testClassname
 * Summary: To test class name in snake case
 * Notes: To test class name in snake case
 * Output-Formats: [application/json]
 */
$app->PATCH('/v2/fake_classname_test', function($request, $response, $args) {
    $body = $request->getParsedBody();
    $response->write('How about implementing testClassname as a PATCH method ?');
    return $response;
});


/**
 * POST addPet
 * Summary: Add a new pet to the store
 * Notes: 
 */
$app->POST('/v2/pet', function($request, $response, $args) {
    $body = $request->getParsedBody();
    $response->write('How about implementing addPet as a POST method ?');
    return $response;
});


/**
 * GET findPetsByStatus
 * Summary: Finds Pets by status
 * Notes: Multiple status values can be provided with comma separated strings
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/v2/pet/findByStatus', function($request, $response, $args) {
    $queryParams = $request->getQueryParams();
    $status = $request->getQueryParam('status');
    $response->write('How about implementing findPetsByStatus as a GET method ?');
    return $response;
});


/**
 * GET findPetsByTags
 * Summary: Finds Pets by tags
 * Notes: Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/v2/pet/findByTags', function($request, $response, $args) {
    $queryParams = $request->getQueryParams();
    $tags = $request->getQueryParam('tags');
    $response->write('How about implementing findPetsByTags as a GET method ?');
    return $response;
});


/**
 * PUT updatePet
 * Summary: Update an existing pet
 * Notes: 
 */
$app->PUT('/v2/pet', function($request, $response, $args) {
    $body = $request->getParsedBody();
    $response->write('How about implementing updatePet as a PUT method ?');
    return $response;
});


/**
 * DELETE deletePet
 * Summary: Deletes a pet
 * Notes: 
 */
$app->DELETE('/v2/pet/{petId}', function($request, $response, $args) {
    $headers = $request->getHeaders();
    $apiKey = $request->hasHeader('api_key') ? $headers['api_key'] : null;
    $petId = $args['petId'];
    $response->write('How about implementing deletePet as a DELETE method ?');
    return $response;
});


/**
 * GET getPetById
 * Summary: Find pet by ID
 * Notes: Returns a single pet
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/v2/pet/{petId}', function($request, $response, $args) {
    $petId = $args['petId'];
    $response->write('How about implementing getPetById as a GET method ?');
    return $response;
});


/**
 * POST updatePetWithForm
 * Summary: Updates a pet in the store with form data
 * Notes: 
 */
$app->POST('/v2/pet/{petId}', function($request, $response, $args) {
    $petId = $args['petId'];
    $response->write('How about implementing updatePetWithForm as a POST method ?');
    return $response;
});


/**
 * POST uploadFile
 * Summary: uploads an image
 * Notes: 
 * Output-Formats: [application/json]
 */
$app->POST('/v2/pet/{petId}/uploadImage', function($request, $response, $args) {
    $petId = $args['petId'];
    $file = $request->getUploadedFiles()['file'];
    $response->write('How about implementing uploadFile as a POST method ?');
    return $response;
});


/**
 * GET getInventory
 * Summary: Returns pet inventories by status
 * Notes: Returns a map of status codes to quantities
 * Output-Formats: [application/json]
 */
$app->GET('/v2/store/inventory', function($request, $response, $args) {
    $response->write('How about implementing getInventory as a GET method ?');
    return $response;
});


/**
 * POST placeOrder
 * Summary: Place an order for a pet
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->POST('/v2/store/order', function($request, $response, $args) {
    $body = $request->getParsedBody();
    $response->write('How about implementing placeOrder as a POST method ?');
    return $response;
});


/**
 * DELETE deleteOrder
 * Summary: Delete purchase order by ID
 * Notes: For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
 */
$app->DELETE('/v2/store/order/{order_id}', function($request, $response, $args) {
    $orderId = $args['order_id'];
    $response->write('How about implementing deleteOrder as a DELETE method ?');
    return $response;
});


/**
 * GET getOrderById
 * Summary: Find purchase order by ID
 * Notes: For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/v2/store/order/{order_id}', function($request, $response, $args) {
    $orderId = $args['order_id'];
    $response->write('How about implementing getOrderById as a GET method ?');
    return $response;
});


/**
 * POST createUser
 * Summary: Create user
 * Notes: This can only be done by the logged in user.
 */
$app->POST('/v2/user', function($request, $response, $args) {
    $body = $request->getParsedBody();
    $response->write('How about implementing createUser as a POST method ?');
    return $response;
});


/**
 * POST createUsersWithArrayInput
 * Summary: Creates list of users with given input array
 * Notes: 
 */
$app->POST('/v2/user/createWithArray', function($request, $response, $args) {
    $body = $request->getParsedBody();
    $response->write('How about implementing createUsersWithArrayInput as a POST method ?');
    return $response;
});


/**
 * POST createUsersWithListInput
 * Summary: Creates list of users with given input array
 * Notes: 
 */
$app->POST('/v2/user/createWithList', function($request, $response, $args) {
    $body = $request->getParsedBody();
    $response->write('How about implementing createUsersWithListInput as a POST method ?');
    return $response;
});


/**
 * GET loginUser
 * Summary: Logs user into the system
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/v2/user/login', function($request, $response, $args) {
    $queryParams = $request->getQueryParams();
    $username = $request->getQueryParam('username');
    $password = $request->getQueryParam('password');
    $response->write('How about implementing loginUser as a GET method ?');
    return $response;
});


/**
 * GET logoutUser
 * Summary: Logs out current logged in user session
 * Notes: 
 */
$app->GET('/v2/user/logout', function($request, $response, $args) {
    $response->write('How about implementing logoutUser as a GET method ?');
    return $response;
});


/**
 * DELETE deleteUser
 * Summary: Delete user
 * Notes: This can only be done by the logged in user.
 */
$app->DELETE('/v2/user/{username}', function($request, $response, $args) {
    $username = $args['username'];
    $response->write('How about implementing deleteUser as a DELETE method ?');
    return $response;
});


/**
 * GET getUserByName
 * Summary: Get user by user name
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/v2/user/{username}', function($request, $response, $args) {
    $username = $args['username'];
    $response->write('How about implementing getUserByName as a GET method ?');
    return $response;
});


/**
 * PUT updateUser
 * Summary: Updated user
 * Notes: This can only be done by the logged in user.
 */
$app->PUT('/v2/user/{username}', function($request, $response, $args) {
    $username = $args['username'];
    $body = $request->getParsedBody();
    $response->write('How about implementing updateUser as a PUT method ?');
    return $response;
});



$app->run();
