<?php
/**
 * Swagger Petstore
 * @version 1.0.0
 */

$app->get('/', function () use ($app) {
    return $app->version();
});


/**
 * POST addPet
 * Summary: Add a new pet to the store
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->POST('/pet', function($null = null) use ($app) {
     
    
    

    return response('How about implementing addPet as a POST method ?');
    });


/**
 * DELETE deletePet
 * Summary: Deletes a pet
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->DELETE('/pet/{petId}', function($petId, $null = null) use ($app) {
    $headers = Request::header(); 
    
    

    return response('How about implementing deletePet as a DELETE method ?');
    });


/**
 * GET findPetsByStatus
 * Summary: Finds Pets by status
 * Notes: Multiple status values can be provided with comma separated strings
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/pet/findByStatus', function($null = null) use ($app) {
     
    $status = Request::input('status');    
    
    

    return response('How about implementing findPetsByStatus as a GET method ?');
    });


/**
 * GET findPetsByTags
 * Summary: Finds Pets by tags
 * Notes: Muliple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/pet/findByTags', function($null = null) use ($app) {
     
    $tags = Request::input('tags');    
    
    

    return response('How about implementing findPetsByTags as a GET method ?');
    });


/**
 * GET getPetById
 * Summary: Find pet by ID
 * Notes: Returns a single pet
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/pet/{petId}', function($petId, $null = null) use ($app) {
     
    
    

    return response('How about implementing getPetById as a GET method ?');
    });


/**
 * PUT updatePet
 * Summary: Update an existing pet
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->PUT('/pet', function($null = null) use ($app) {
     
    
    

    return response('How about implementing updatePet as a PUT method ?');
    });


/**
 * POST updatePetWithForm
 * Summary: Updates a pet in the store with form data
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->POST('/pet/{petId}', function($petId, $null = null) use ($app) {
     
    
    $name = Request::input('name');    $status = Request::input('status');    

    return response('How about implementing updatePetWithForm as a POST method ?');
    });


/**
 * POST uploadFile
 * Summary: uploads an image
 * Notes: 
 * Output-Formats: [application/json]
 */
$app->POST('/pet/{petId}/uploadImage', function($petId, $null = null) use ($app) {
     
    
    $additionalMetadata = Request::input('additionalMetadata');    $file = Request::input('file');    

    return response('How about implementing uploadFile as a POST method ?');
    });


/**
 * DELETE deleteOrder
 * Summary: Delete purchase order by ID
 * Notes: For valid response try integer IDs with positive integer value. Negative or non-integer values will generate API errors
 * Output-Formats: [application/xml, application/json]
 */
$app->DELETE('/store/order/{orderId}', function($orderId, $null = null) use ($app) {
     
    
    

    return response('How about implementing deleteOrder as a DELETE method ?');
    });


/**
 * GET getInventory
 * Summary: Returns pet inventories by status
 * Notes: Returns a map of status codes to quantities
 * Output-Formats: [application/json]
 */
$app->GET('/store/inventory', function($null = null) use ($app) {
     
    
    

    return response('How about implementing getInventory as a GET method ?');
    });


/**
 * GET getOrderById
 * Summary: Find purchase order by ID
 * Notes: For valid response try integer IDs with value &gt;&#x3D; 1 and &lt;&#x3D; 10. Other values will generated exceptions
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/store/order/{orderId}', function($orderId, $null = null) use ($app) {
     
    
    

    return response('How about implementing getOrderById as a GET method ?');
    });


/**
 * POST placeOrder
 * Summary: Place an order for a pet
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->POST('/store/order', function($null = null) use ($app) {
     
    
    

    return response('How about implementing placeOrder as a POST method ?');
    });


/**
 * POST createUser
 * Summary: Create user
 * Notes: This can only be done by the logged in user.
 * Output-Formats: [application/xml, application/json]
 */
$app->POST('/user', function($null = null) use ($app) {
     
    
    

    return response('How about implementing createUser as a POST method ?');
    });


/**
 * POST createUsersWithArrayInput
 * Summary: Creates list of users with given input array
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->POST('/user/createWithArray', function($null = null) use ($app) {
     
    
    

    return response('How about implementing createUsersWithArrayInput as a POST method ?');
    });


/**
 * POST createUsersWithListInput
 * Summary: Creates list of users with given input array
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->POST('/user/createWithList', function($null = null) use ($app) {
     
    
    

    return response('How about implementing createUsersWithListInput as a POST method ?');
    });


/**
 * DELETE deleteUser
 * Summary: Delete user
 * Notes: This can only be done by the logged in user.
 * Output-Formats: [application/xml, application/json]
 */
$app->DELETE('/user/{username}', function($username, $null = null) use ($app) {
     
    
    

    return response('How about implementing deleteUser as a DELETE method ?');
    });


/**
 * GET getUserByName
 * Summary: Get user by user name
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/user/{username}', function($username, $null = null) use ($app) {
     
    
    

    return response('How about implementing getUserByName as a GET method ?');
    });


/**
 * GET loginUser
 * Summary: Logs user into the system
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/user/login', function($null = null) use ($app) {
     
    $username = Request::input('username');    
    $password = Request::input('password');    
    
    

    return response('How about implementing loginUser as a GET method ?');
    });


/**
 * GET logoutUser
 * Summary: Logs out current logged in user session
 * Notes: 
 * Output-Formats: [application/xml, application/json]
 */
$app->GET('/user/logout', function($null = null) use ($app) {
     
    
    

    return response('How about implementing logoutUser as a GET method ?');
    });


/**
 * PUT updateUser
 * Summary: Updated user
 * Notes: This can only be done by the logged in user.
 * Output-Formats: [application/xml, application/json]
 */
$app->PUT('/user/{username}', function($username, $null = null) use ($app) {
     
    
    

    return response('How about implementing updateUser as a PUT method ?');
    });



