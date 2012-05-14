# Swagger Client Code-Generator

## Overview
This is a project to build the Swagger code-gen library which can be used to automatically
generate client libraries from a Swagger-compliant server.  It also contains a testing
framework which allows the client library to query an API server and validate expected results 
You can find out more about both the spec and the framework at http://swagger.wordnik.com.  For 
more information about Wordnik's APIs, please visit http://developer.wordnik.com.  

### Prerequisites
You need the following installed and available in your $PATH:

<li>- Java 1.6 or greater (http://java.oracle.com)

<li>- Apache ant 1.7 or greater (http://ant.apache.org/)

<li>- Scala 2.x or greater (http://www.scala-lang.org/downloads)

You also need to set an environment variable for SCALA_HOME:

<pre>
export SCALA_HOME={PATH_TO_YOUR_SCALA_DEPLOYMENT}
</pre>

### To build the codegen library

You can build the client with the following:

````
mvn package
````

### Other languages
#### scala
<pre>
./bin/generate-scala-lib.sh http://petstore.swagger.wordnik.com/api "" "client" "generated-files"
</pre>

#### javascript
<pre>
./bin/generate-js-lib.sh http://petstore.swagger.wordnik.com/api "" "" "generated-files"
</pre>

#### actionscript
<pre>
./bin/generate-as3-lib.sh http://petstore.swagger.wordnik.com/api "" "client" "generated-files"
</pre>

#### PHP
<pre>
./bin/generate-php-lib.sh http://petstore.swagger.wordnik.com/api "" "client" "generated-files"
</pre>

#### Python
<pre>
./bin/generate-python-lib.sh http://petstore.swagger.wordnik.com/api "" "client" "generated-files"
</pre>

#### C&#35;
<pre>
bin\generate-csharp-lib.cmd http://petstore.swagger.wordnik.com/api "your_api_key" "PetStore" "generated-files"
</pre>

The main class for the generator is at src/main/java/com/wordnik/swagger/codegen/config/java/JavaLibCodeGen.java

The code-gen uses the antlr string template library for generating the output files, please look at
http://www.stringtemplate.org for details on the antlr framework.

The Wordnik team is working on generating libraries for Ruby, ActionScript 3, Android, PHP and JavaScript, which will be open-sourced in the coming weeks

### The Swagger client test framework

The testing framework helps you to test Swagger generated client libraries using declarative test scripts. The same 
scripts can be used to test client libraries in different languages.  The framework can be used for client and server
regression testing.

For Example, first build the client library from the sample app:
<pre>
./bin/generate-java-lib.sh http://petstore.swagger.wordnik.com/api/ special-key com.foo.mydriver generated-files
</pre>

Use the sample build script to build a jar from the client files:
<pre>
cp conf/java/sample/*.xml ./generated-files

cd generated-files

ant
</pre>

This creates a complete client library jar.  You can now run the tests:

<pre>
./bin/test-java-lib.sh http://petstore.swagger.wordnik.com/api/ special-key conf/java/sample/lib-test-script.json \
    conf/java/sample/lib-test-data.json com.foo.mydriver.model.TestData com.foo.mydriver.api \
    generated-files/build/swagger-sample-java-lib-1.0.jar

Summary -->  Total Test Cases: 9 Failed Test Cases: 0
Details: 
1.1 : Create User :  passed  
 
1.2 : Login User :  passed  
 
1.3 : Find user by name :  passed  
 
1.4 : Delete user by name :  passed  
 
2.1 : Add pet :  passed  
 
2.2 : Find pet by id :  passed  
 
2.3 : Find pet by status :  passed  
 
3.1 : Find order by id :  passed  
 
3.2 : Place order :  passed 
</pre>

In detail, there are two components in the test framework:

<li>- Test Script

<li>- Test Data


#### Test script details

Test script is written in JSON structure. The JSON consists of following elements:

##### Resources.  This is a list of resources considered in the test. Each resource object consists of following properties:

<li>- id: a unique test script ID

<li>- name: name of the resource, used in displaying the test result

<li>- httpMethod: HTTP method used in invoking this resource

<li>- path: path of the resource

<li>- suggested method name: By default this refers to method name of the API in resource classes

##### Test suites.  This is a logical way of grouping related test cases. Each test suite consists of following properties:

<li>- id: unique id of the test script, displayed in the test report

<li>- name: name of the test suite. Used in test report

<li>- test cases: List of test cases with in each suite. Each test case consists of following properties:

  <li>- id: unique with in the test suite. Used for reporting and tracking output data

  <li>- name: Name of the test case

  <li>- resource id: references the resource id in the resources section

  <li>- input: Input is a JSON object with each property in the object map to query, path or post parameters. 
  For POST data, the name of the property should be supplied as postData. The value for each property can refer 
  to input file or output from previous test cases or actual values.   

  <li>- assertions: list of assertions that needs to be evaluated after test case is executed. 

Each assertion contains

  <li>- actual output, specified with reference to output of the current test case using syntax similar to object graph navigation language 
  <li>- condition , support values are equal (==), not equal (!=), less than (<), lesser than or equal (<=),  greater than (>), greater than or equal (>=) 
  <li>- expected output. Specified using actual values or values referring previous outputs or input data file

Test data file is documented using a Test Data Object which is generated as part of Java client library code-gen.  This 
class provides list getters and setters for each model object available in the resource description.  It is called "TestData" 
and it is available in model package of the java library code generation output.
 
Chaining results of test cases:

<li>- Reference to data in input file is done with prefix <pre>${input.</pre>, followed by object graph navigation syntax. 
Example: to refer a first user object in test data file use the syntax <pre>${input.userList[0]}</pre> 

<li>- To refer a individual property of user object use the syntax <pre>${input.userList[0].username}</pre>

<li>- Reference to output of test cases is done using combination test case path and OGNL. Reference to test cases output 
is prefixed with <pre>${output.</pre>

<li>- To refer an output of test case 1 in test suite 2, the syntax will be <pre>${output(1.2)}</pre>.  Individual attributes can 
be accessed using OGNL syntax. Example: <pre>${output(1.1).username}</pre> 

#### Reporting Test Results

A Summary will be reported with each test run.  For instance: 

<pre>
Sample: "Summary -->  Total Test Cases: 9 Failed Test Cases: 0"
</pre>

In detail section each test case and its status (passed/failed) are reported. Failures include an exception trace.  Test case path is 
combination of test suite id and test case id separated by "."
     
