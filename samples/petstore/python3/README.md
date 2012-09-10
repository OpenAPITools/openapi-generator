# Sample Petstore Python 3 application

Here's a Python 3.2 [Swagger](http://swagger.wordnik.com/) client with unit
tests for the Petstore sample application. See the interactive Swagger
documentation at http://petstore.swagger.wordnik.com/ for more information on
this demonstration API.

The client contained in ```petstore``` was generated using this
```swagger-codegen``` project, with the following command:

    ./bin/runscala.sh com.wordnik.swagger.codegen.BasicPython3Generator http://petstore.swagger.wordnik.com/api/resources.json special-key

To run the tests, issue a command like the following:

    python3.2 tests/BaseApiTest.py