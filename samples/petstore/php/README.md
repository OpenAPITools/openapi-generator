# Sample Petstore PHP application

Here's a PHP 5 [Swagger](http://swagger.wordnik.com/) client for the Petstore
sample application. See the interactive Swagger documentation at
http://petstore.swagger.wordnik.com/ for more information on this
demonstration API.

The client contained in ```petstore``` was generated using this
```swagger-codegen``` project, with the following command:

    ./bin/runscala.sh com.wordnik.swagger.codegen.BasicPHPGenerator http://petstore.swagger.wordnik.com/api/resources.json special-key

To run the tests, you will need
[PHPUnit](https://github.com/sebastianbergmann/phpunit) installed on your
system. With the ```phpunit``` command in your path, run the tests for each
resource as follows:

    phpunit tests/PetApiTest.php
    phpunit tests/StoreApiTest.php
    phpunit tests/UserApiTest.php