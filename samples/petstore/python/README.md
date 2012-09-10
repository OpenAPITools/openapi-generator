# Sample Petstore Python application

Here's a Python 2.7 [Swagger](http://swagger.wordnik.com/) client with unit
tests for the Petstore sample application. See the interactive Swagger
documentation at http://petstore.swagger.wordnik.com/ for more information on
 this demonstration API.

The client contained in ```petstore``` was generated using this
```swagger-codegen``` project, with the following command:

    ./bin/runscala.sh com.wordnik.swagger.codegen.BasicPythonGenerator http://petstore.swagger.wordnik.com/api/resources.json special-key

To run the tests, issue the following command:

    python tests/BaseApiTest.py