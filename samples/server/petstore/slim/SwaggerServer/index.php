<?php
/**
 * Swagger Petstore
 * @version 1.0.0
 */

require_once __DIR__ . '/vendor/autoload.php';

$app = new Slim\App();


/**
 * POST createUser
 * Summary: Create user
 * Notes: This can only be done by the logged in user.
 * Output-Formats: [application/json, application/xml]
 */
$app->POST('/user', function($request, $response, $args) {
            
            
            
            $body = $request->getParsedBody();
            $response->write('How about implementing createUser as a POST method ?');
            return $response;
            });


/**
 * POST createUsersWithArrayInput
 * Summary: Creates list of users with given input array
 * Notes: 
 * Output-Formats: [application/json, application/xml]
 */
$app->POST('/user/createWithArray', function($request, $response, $args) {
            
            
            
            $body = $request->getParsedBody();
            $response->write('How about implementing createUsersWithArrayInput as a POST method ?');
            return $response;
            });


/**
 * POST createUsersWithListInput
 * Summary: Creates list of users with given input array
 * Notes: 
 * Output-Formats: [application/json, application/xml]
 */
$app->POST('/user/createWithList', function($request, $response, $args) {
            
            
            
            $body = $request->getParsedBody();
            $response->write('How about implementing createUsersWithListInput as a POST method ?');
            return $response;
            });


/**
 * GET loginUser
 * Summary: Logs user into the system
 * Notes: 
 * Output-Formats: [application/json, application/xml]
 */
$app->GET('/user/login', function($request, $response, $args) {
            
            $queryParams = $request->getQueryParams();
            $username = $queryParams['username'];    $password = $queryParams['password'];    
            
            
            $response->write('How about implementing loginUser as a GET method ?');
            return $response;
            });


/**
 * GET logoutUser
 * Summary: Logs out current logged in user session
 * Notes: 
 * Output-Formats: [application/json, application/xml]
 */
$app->GET('/user/logout', function($request, $response, $args) {
            
            
            
            
            $response->write('How about implementing logoutUser as a GET method ?');
            return $response;
            });


/**
 * GET getUserByName
 * Summary: Get user by user name
 * Notes: 
 * Output-Formats: [application/json, application/xml]
 */
$app->GET('/user/{username}', function($request, $response, $args) {
            
            
            
            
            $response->write('How about implementing getUserByName as a GET method ?');
            return $response;
            });


/**
 * PUT updateUser
 * Summary: Updated user
 * Notes: This can only be done by the logged in user.
 * Output-Formats: [application/json, application/xml]
 */
$app->PUT('/user/{username}', function($request, $response, $args) {
            
            
            
            $body = $request->getParsedBody();
            $response->write('How about implementing updateUser as a PUT method ?');
            return $response;
            });


/**
 * DELETE deleteUser
 * Summary: Delete user
 * Notes: This can only be done by the logged in user.
 * Output-Formats: [application/json, application/xml]
 */
$app->DELETE('/user/{username}', function($request, $response, $args) {
            
            
            
            
            $response->write('How about implementing deleteUser as a DELETE method ?');
            return $response;
            });


/**
 * PUT updatePet
 * Summary: Update an existing pet
 * Notes: 
 * Output-Formats: [application/json, application/xml]
 */
$app->PUT('/pet', function($request, $response, $args) {
            
            
            
            $body = $request->getParsedBody();
            $response->write('How about implementing updatePet as a PUT method ?');
            return $response;
            });


/**
 * POST addPet
 * Summary: Add a new pet to the store
 * Notes: 
 * Output-Formats: [application/json, application/xml]
 */
$app->POST('/pet', function($request, $response, $args) {
            
            
            
            $body = $request->getParsedBody();
            $response->write('How about implementing addPet as a POST method ?');
            return $response;
            });


/**
 * GET findPetsByStatus
 * Summary: Finds Pets by status
 * Notes: Multiple status values can be provided with comma seperated strings
 * Output-Formats: [application/json, application/xml]
 */
$app->GET('/pet/findByStatus', function($request, $response, $args) {
            
            $queryParams = $request->getQueryParams();
            $status = $queryParams['status'];    
            
            
            $response->write('How about implementing findPetsByStatus as a GET method ?');
            return $response;
            });


/**
 * GET findPetsByTags
 * Summary: Finds Pets by tags
 * Notes: Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.
 * Output-Formats: [application/json, application/xml]
 */
$app->GET('/pet/findByTags', function($request, $response, $args) {
            
            $queryParams = $request->getQueryParams();
            $tags = $queryParams['tags'];    
            
            
            $response->write('How about implementing findPetsByTags as a GET method ?');
            return $response;
            });


/**
 * GET getPetById
 * Summary: Find pet by ID
 * Notes: Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
 * Output-Formats: [application/json, application/xml]
 */
$app->GET('/pet/{petId}', function($request, $response, $args) {
            
            
            
            
            $response->write('How about implementing getPetById as a GET method ?');
            return $response;
            });


/**
 * POST updatePetWithForm
 * Summary: Updates a pet in the store with form data
 * Notes: 
 * Output-Formats: [application/json, application/xml]
 */
$app->POST('/pet/{petId}', function($request, $response, $args) {
            
            
            $name = $args['name'];    $status = $args['status'];    
            
            $response->write('How about implementing updatePetWithForm as a POST method ?');
            return $response;
            });


/**
 * DELETE deletePet
 * Summary: Deletes a pet
 * Notes: 
 * Output-Formats: [application/json, application/xml]
 */
$app->DELETE('/pet/{petId}', function($request, $response, $args) {
            $headers = $request->getHeaders();
            
            
            
            $response->write('How about implementing deletePet as a DELETE method ?');
            return $response;
            });


/**
 * POST uploadFile
 * Summary: uploads an image
 * Notes: 
 * Output-Formats: [application/json, application/xml]
 */
$app->POST('/pet/{petId}/uploadImage', function($request, $response, $args) {
            
            
            $additionalMetadata = $args['additionalMetadata'];    $file = $args['file'];    
            
            $response->write('How about implementing uploadFile as a POST method ?');
            return $response;
            });


/**
 * GET getPetByIdWithByteArray
 * Summary: Fake endpoint to test byte array return by &#39;Find pet by ID&#39;
 * Notes: Returns a pet when ID &lt; 10.  ID &gt; 10 or nonintegers will simulate API error conditions
 * Output-Formats: [application/json, application/xml]
 */
$app->GET('/pet/{petId}?testing_byte_array=true', function($request, $response, $args) {
            
            
            
            
            $response->write('How about implementing getPetByIdWithByteArray as a GET method ?');
            return $response;
            });


/**
 * POST addPetUsingByteArray
 * Summary: Fake endpoint to test byte array in body parameter for adding a new pet to the store
 * Notes: 
 * Output-Formats: [application/json, application/xml]
 */
$app->POST('/pet?testing_byte_array=true', function($request, $response, $args) {
            
            
            
            $body = $request->getParsedBody();
            $response->write('How about implementing addPetUsingByteArray as a POST method ?');
            return $response;
            });


/**
 * GET getInventory
 * Summary: Returns pet inventories by status
 * Notes: Returns a map of status codes to quantities
 * Output-Formats: [application/json, application/xml]
 */
$app->GET('/store/inventory', function($request, $response, $args) {
            
            
            
            
            $response->write('How about implementing getInventory as a GET method ?');
            return $response;
            });


/**
 * POST placeOrder
 * Summary: Place an order for a pet
 * Notes: 
 * Output-Formats: [application/json, application/xml]
 */
$app->POST('/store/order', function($request, $response, $args) {
            
            
            
            $body = $request->getParsedBody();
            $response->write('How about implementing placeOrder as a POST method ?');
            return $response;
            });


/**
 * GET getOrderById
 * Summary: Find purchase order by ID
 * Notes: For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions
 * Output-Formats: [application/json, application/xml]
 */
$app->GET('/store/order/{orderId}', function($request, $response, $args) {
            
            
            
            
            $response->write('How about implementing getOrderById as a GET method ?');
            return $response;
            });


/**
 * DELETE deleteOrder
 * Summary: Delete purchase order by ID
 * Notes: For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
 * Output-Formats: [application/json, application/xml]
 */
$app->DELETE('/store/order/{orderId}', function($request, $response, $args) {
            
            
            
            
            $response->write('How about implementing deleteOrder as a DELETE method ?');
            return $response;
            });



$app->run();
