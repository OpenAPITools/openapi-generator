<?php

namespace Swagger\Client;

class UserApiTest extends \PHPUnit_Framework_TestCase
{

    // add a new pet (id 10005) to ensure the pet object is available for all the tests
    public static function setUpBeforeClass()
    {
        // for error reporting (need to run with php5.3 to get no warning)
        //ini_set('display_errors', 1);
        //error_reporting(~0);
    }

    // test login user
    public function testLoginUser()
    {
        // initialize the API client
        $config = (new Configuration())->setHost('http://petstore.swagger.io/v2');
        $api_client = new ApiClient($config);
        $user_api = new Api\UserApi($api_client);
        // login
        $response = $user_api->loginUser("xxxxx", "yyyyyyyy");
        
        $this->assertInternalType("string", $response);
        $this->assertRegExp(
            "/^logged in user session/",
            $response,
            "response string starts with 'logged in user session'"
        );

    }
}
