<?php
require_once __DIR__ . '/vendor/autoload.php';

use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Silex\Application;

$app = new Silex\Application();

    
        
            

$app->POST('/user', function(Application $app, Request $request) {
            
            
            return new Response('How about implementing createUser as a POST method ?');
            });

            

$app->POST('/user/createWithArray', function(Application $app, Request $request) {
            
            
            return new Response('How about implementing createUsersWithArrayInput as a POST method ?');
            });

            

$app->POST('/user/createWithList', function(Application $app, Request $request) {
            
            
            return new Response('How about implementing createUsersWithListInput as a POST method ?');
            });

            

$app->GET('/user/login', function(Application $app, Request $request) {
            $username = $request->get('username');    $password = $request->get('password');    
            
            return new Response('How about implementing loginUser as a GET method ?');
            });

            

$app->GET('/user/logout', function(Application $app, Request $request) {
            
            
            return new Response('How about implementing logoutUser as a GET method ?');
            });

            

$app->GET('/user/{username}', function(Application $app, Request $request, $username) {
            
            
            return new Response('How about implementing getUserByName as a GET method ?');
            });

            

$app->PUT('/user/{username}', function(Application $app, Request $request, $username) {
            
            
            return new Response('How about implementing updateUser as a PUT method ?');
            });

            

$app->DELETE('/user/{username}', function(Application $app, Request $request, $username) {
            
            
            return new Response('How about implementing deleteUser as a DELETE method ?');
            });

            
        
    
        
            

$app->PUT('/pet', function(Application $app, Request $request) {
            
            
            return new Response('How about implementing updatePet as a PUT method ?');
            });

            

$app->POST('/pet', function(Application $app, Request $request) {
            
            
            return new Response('How about implementing addPet as a POST method ?');
            });

            

$app->GET('/pet/findByStatus', function(Application $app, Request $request) {
            $status = $request->get('status');    
            
            return new Response('How about implementing findPetsByStatus as a GET method ?');
            });

            

$app->GET('/pet/findByTags', function(Application $app, Request $request) {
            $tags = $request->get('tags');    
            
            return new Response('How about implementing findPetsByTags as a GET method ?');
            });

            

$app->GET('/pet/{petId}', function(Application $app, Request $request, $pet_id) {
            
            
            return new Response('How about implementing getPetById as a GET method ?');
            });

            

$app->POST('/pet/{petId}', function(Application $app, Request $request, $pet_id) {
            
            $name = $request->get('name');    $status = $request->get('status');    
            return new Response('How about implementing updatePetWithForm as a POST method ?');
            });

            

$app->DELETE('/pet/{petId}', function(Application $app, Request $request, $pet_id) {
            
            
            return new Response('How about implementing deletePet as a DELETE method ?');
            });

            

$app->POST('/pet/{petId}/uploadImage', function(Application $app, Request $request, $pet_id) {
            
            $additional_metadata = $request->get('additional_metadata');    $file = $request->get('file');    
            return new Response('How about implementing uploadFile as a POST method ?');
            });

            

$app->GET('/pet/{petId}?testing_byte_array=true', function(Application $app, Request $request, $pet_id) {
            
            
            return new Response('How about implementing getPetByIdWithByteArray as a GET method ?');
            });

            

$app->POST('/pet?testing_byte_array=true', function(Application $app, Request $request) {
            
            
            return new Response('How about implementing addPetUsingByteArray as a POST method ?');
            });

            
        
    
        
            

$app->GET('/store/inventory', function(Application $app, Request $request) {
            
            
            return new Response('How about implementing getInventory as a GET method ?');
            });

            

$app->POST('/store/order', function(Application $app, Request $request) {
            
            
            return new Response('How about implementing placeOrder as a POST method ?');
            });

            

$app->GET('/store/order/{orderId}', function(Application $app, Request $request, $order_id) {
            
            
            return new Response('How about implementing getOrderById as a GET method ?');
            });

            

$app->DELETE('/store/order/{orderId}', function(Application $app, Request $request, $order_id) {
            
            
            return new Response('How about implementing deleteOrder as a DELETE method ?');
            });

            
        
    

$app->run();
