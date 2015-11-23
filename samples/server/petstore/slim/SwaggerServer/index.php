<?php
/**
 * Swagger Petstore
 * @version 1.0.0
 */

require_once __DIR__ . '/vendor/autoload.php';

$app = new Slim\App();


/**
 * POST User
 * Create user
 */
$app->POST('/user', function($request, $response, $args) {
            
            
            $response->write('How about implementing createUser as a POST method ?');
            return $response;
            });


/**
 * POST User
 * Creates list of users with given input array
 */
$app->POST('/user/createWithArray', function($request, $response, $args) {
            
            
            $response->write('How about implementing createUsersWithArrayInput as a POST method ?');
            return $response;
            });


/**
 * POST User
 * Creates list of users with given input array
 */
$app->POST('/user/createWithList', function($request, $response, $args) {
            
            
            $response->write('How about implementing createUsersWithListInput as a POST method ?');
            return $response;
            });


/**
 * GET User
 * Logs user into the system
 */
$app->GET('/user/login', function($request, $response, $args) {
            $queryParams = $request->getQueryParams();
            $username = $queryParams['username'];    $password = $queryParams['password'];    
            
            $response->write('How about implementing loginUser as a GET method ?');
            return $response;
            });


/**
 * GET User
 * Logs out current logged in user session
 */
$app->GET('/user/logout', function($request, $response, $args) {
            
            
            $response->write('How about implementing logoutUser as a GET method ?');
            return $response;
            });


/**
 * GET User
 * Get user by user name
 */
$app->GET('/user/{username}', function($request, $response, $args) {
            
            
            $response->write('How about implementing getUserByName as a GET method ?');
            return $response;
            });


/**
 * PUT User
 * Updated user
 */
$app->PUT('/user/{username}', function($request, $response, $args) {
            
            
            $response->write('How about implementing updateUser as a PUT method ?');
            return $response;
            });


/**
 * DELETE User
 * Delete user
 */
$app->DELETE('/user/{username}', function($request, $response, $args) {
            
            
            $response->write('How about implementing deleteUser as a DELETE method ?');
            return $response;
            });


/**
 * GET Store
 * Returns pet inventories by status
 */
$app->GET('/store/inventory', function($request, $response, $args) {
            
            
            $response->write('How about implementing getInventory as a GET method ?');
            return $response;
            });


/**
 * POST Store
 * Place an order for a pet
 */
$app->POST('/store/order', function($request, $response, $args) {
            
            
            $response->write('How about implementing placeOrder as a POST method ?');
            return $response;
            });


/**
 * GET Store
 * Find purchase order by ID
 */
$app->GET('/store/order/{orderId}', function($request, $response, $args) {
            
            
            $response->write('How about implementing getOrderById as a GET method ?');
            return $response;
            });


/**
 * DELETE Store
 * Delete purchase order by ID
 */
$app->DELETE('/store/order/{orderId}', function($request, $response, $args) {
            
            
            $response->write('How about implementing deleteOrder as a DELETE method ?');
            return $response;
            });


/**
 * PUT Pet
 * Update an existing pet
 */
$app->PUT('/pet', function($request, $response, $args) {
            
            
            $response->write('How about implementing updatePet as a PUT method ?');
            return $response;
            });


/**
 * POST Pet
 * Add a new pet to the store
 */
$app->POST('/pet', function($request, $response, $args) {
            
            
            $response->write('How about implementing addPet as a POST method ?');
            return $response;
            });


/**
 * GET Pet
 * Finds Pets by status
 */
$app->GET('/pet/findByStatus', function($request, $response, $args) {
            $queryParams = $request->getQueryParams();
            $status = $queryParams['status'];    
            
            $response->write('How about implementing findPetsByStatus as a GET method ?');
            return $response;
            });


/**
 * GET Pet
 * Finds Pets by tags
 */
$app->GET('/pet/findByTags', function($request, $response, $args) {
            $queryParams = $request->getQueryParams();
            $tags = $queryParams['tags'];    
            
            $response->write('How about implementing findPetsByTags as a GET method ?');
            return $response;
            });


/**
 * GET Pet
 * Find pet by ID
 */
$app->GET('/pet/{petId}', function($request, $response, $args) {
            
            
            $response->write('How about implementing getPetById as a GET method ?');
            return $response;
            });


/**
 * POST Pet
 * Updates a pet in the store with form data
 */
$app->POST('/pet/{petId}', function($request, $response, $args) {
            
            $name = $args['name'];    $status = $args['status'];    
            $response->write('How about implementing updatePetWithForm as a POST method ?');
            return $response;
            });


/**
 * DELETE Pet
 * Deletes a pet
 */
$app->DELETE('/pet/{petId}', function($request, $response, $args) {
            
            
            $response->write('How about implementing deletePet as a DELETE method ?');
            return $response;
            });


/**
 * POST Pet
 * uploads an image
 */
$app->POST('/pet/{petId}/uploadImage', function($request, $response, $args) {
            
            $additional_metadata = $args['additional_metadata'];    $file = $args['file'];    
            $response->write('How about implementing uploadFile as a POST method ?');
            return $response;
            });



$app->run();
