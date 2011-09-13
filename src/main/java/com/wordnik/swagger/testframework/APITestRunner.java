/**
 *  Copyright 2011 Wordnik, Inc.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.wordnik.swagger.testframework;

import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;
import com.wordnik.swagger.runtime.common.APIInvoker;
import com.wordnik.swagger.runtime.common.ApiKeyAuthTokenBasedSecurityHandler;
import com.wordnik.swagger.runtime.common.SecurityHandler;
import com.wordnik.swagger.runtime.exception.APIException;
import org.apache.commons.beanutils.MethodUtils;
import org.apache.commons.beanutils.PropertyUtils;
import org.codehaus.jackson.map.DeserializationConfig.Feature;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.SerializationConfig;
import org.codehaus.jackson.map.type.TypeFactory;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.lang.reflect.Method;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * Instance of this class is used to run the tests and assert the results based on
 * JSON based test script.
 * Created by IntelliJ IDEA.
 * User: ramesh
 * Date: 3/30/11
 * Time: 6:27 PM
 */
public class APITestRunner {

	private static String INPUT_DATA_EXPRESSION_PREFIX = "${input.";
	private static String OUTPUT_DATA_EXPRESSION_PREFIX = "${output";
    public static String POST_PARAM_NAME = "postData";

	private static String CONDITION_EQUAL = "==";
	private static String CONDITION_NOT_EQUAL = "!=";
	private static String CONDITION_GREATER = ">";
	private static String CONDITION_LESSER = "<";
	private static String CONDITION_GREATER_EQUAL = ">=";
	private static String CONDITION_LESSER_EQUAL = "<=";

	private TestOutput testCaseOutput = new TestOutput();
	private TestStatus testStatus = new TestStatus();
	private Object testData = null;
	private TestPackage aPackage = null;

    private static String JAVA = "JAVA";
    private static String SCALA = "SCALA";
    private static String PYTHON = "PYTHON";
    private static String RUBY = "RUBY";
    private static String ANDROID = "ANDROID";
    private static String OBJECTIVE_C = "OBJECTIVE_C";
    private static String AS3 = "AS3";
    private static String NET = "NET";
    private static String PHP = "PHP";
    private static String HASKEL = "HASKEL";
    private static String CLOJURE = "CLOJURE";

    private static ObjectMapper mapper = new ObjectMapper();
    static{
        mapper.getDeserializationConfig().set(Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        mapper.configure(SerializationConfig.Feature.WRITE_NULL_PROPERTIES, false);
        mapper.configure(SerializationConfig.Feature.WRITE_DATES_AS_TIMESTAMPS, false);
	}

    private CamelCaseNamingPolicyProvider namingPolicyProvider = new CamelCaseNamingPolicyProvider();

    /**
     * Follow the following argument pattern
     *
     * Arg[0] --> api server URL
     * Arg[1] --> api key
     * Arg[2] --> test script file path
     * Arg[3] --> test data file path
     * Arg[4] --> test data class name (class to which test data file will be deserialized)
     * Arg[5] --> package where API classes are available
     * Arg[6] --> Language to execute test cases
     * Arg[7] --> Library location
     * Arg[8] --> Optional test cases id. provide this if you need to execute only one test case
     *
     * @param args
     * @throws Exception
     */
	public static void main(String[] args) throws Exception {

        String apiServer = args[0];
        if(!apiServer.endsWith("/")){
            apiServer = apiServer + "/";
        }

        String apiKey = args[1];
        String testScriptLocation = args[2];
        String testDataLocation = args[3];
        String testDataClass = args[4];
        String apiPackageName = args[5];
        String libraryLocation = args[6];
        String language = args[7];

        String suiteId = "0";
        if(args.length > 8){
            suiteId = args[8];
        }

        ApiKeyAuthTokenBasedSecurityHandler securityHandler = new ApiKeyAuthTokenBasedSecurityHandler(apiKey, "");
        if(language.equals(AS3)){
            DateFormat myDateFormat = new SimpleDateFormat("EEE MMM dd HH:mm:ss Z yyyy");
            mapper.getSerializationConfig().setDateFormat(myDateFormat);
            mapper.getDeserializationConfig().setDateFormat(myDateFormat);
            APIInvoker.mapper.getSerializationConfig().setDateFormat(myDateFormat);
            APIInvoker.mapper.getDeserializationConfig().setDateFormat(myDateFormat);
        }
        APIInvoker.initialize(securityHandler, apiServer, true);
		APITestRunner runner = new APITestRunner();
        runner.initialize(testScriptLocation, testDataLocation, testDataClass);
        runner.runTests(apiServer, apiPackageName, runner.getTestPackage(), language, new Integer(suiteId), apiPackageName, securityHandler, libraryLocation);
	}

    public void initialize(String testScriptLocation, String testDataLocation, String testDataClass) throws Exception {
		//load test script
		File aFile = new File(testScriptLocation);
		BufferedReader reader = new BufferedReader(new FileReader(aFile));
		StringBuilder builder = new StringBuilder();
		while(true){
			String line = reader.readLine();
			if(line == null){
				break;
			}else{
				builder.append(line);
			}
		}
        mapper.getDeserializationConfig().set(Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        aPackage = (TestPackage) mapper.readValue(builder.toString(), TestPackage.class);

        //load test data
		aFile = new File(testDataLocation);
		reader = new BufferedReader(new FileReader(aFile));
		builder = new StringBuilder();
		while(true){
			String line = reader.readLine();
			if(line == null){
				break;
			}else{
				builder.append(line);
			}
		}
        testData =  mapper.readValue(builder.toString(), Class.forName(testDataClass));
        reader.close();
    }


    /**
     * Gets the package that is initialized for testing
     * @return
     */
    public TestPackage getTestPackage() {
        return aPackage;
    }

    /***
     * Runs individual test cases in all test suites and stored the result in output object
     * @param testPackage
     */
    private void runTests(String apiServer, String apiPackageName, TestPackage testPackage, String language, int suiteId,
                          String libraryPackageName, ApiKeyAuthTokenBasedSecurityHandler securityHandler,
                          String libraryLocation) throws Exception {
        /**
         * Logic:
         *
         * 1. Get each test case
         *
         * 2. based on the patch arrive at the method that needs to be executed
         *
         * 3. From the method get list of input parameters
         *
         * 4. Read input parameters based on input structure
         *
         * 5 Execute method by calling system command
         *
         * 6. Get the output
         *
         * 7. Store output in output object
         *
         * 8. execute assertions
         *
         * 9. Continue with next test case
         */

        Map<Integer, TestResource> resourceMap = new HashMap<Integer, TestResource>();
        for(TestResource resource: testPackage.getResources()){
            resourceMap.put(resource.getId(), resource);
        }

        for(TestSuite suite : testPackage.getTestSuites()) {
            if(suiteId != 0 && suiteId != suite.getId()){
                continue;
            }
            int testSuiteId = suite.getId();
            //1
            for(TestCase testCase : suite.getTestCases()){
                String testCasePath = testCasePath(testSuiteId , testCase.getId());

                //2
                TestResource resource = resourceMap.get(testCase.getResourceId());
                String path = resource.getPath();
                String className = namingPolicyProvider.getServiceName(path);
                String methodName = resource.getSuggestedMethodName();

                //3
                Class apiClass = Class.forName(libraryPackageName +"." + className);
                Method[] methods = apiClass.getMethods();
                Method methodToExecute = null;
                for(Method method : methods){
                    if(method.getName().equals(methodName)){
                        methodToExecute = method;
                        break;
                    }
                }
                try {
                    if(methodToExecute != null) {
                        //4
                        Map<String, Object> arguments = getArgumentsForMethodCall(methodToExecute, testCase);

                        String authToken = "\"\"";
                        String postData = "\"\"";
                        StringBuilder queryPathParameters = new StringBuilder();
                        for(String argName : arguments.keySet()){
                            Object value = arguments.get(argName);
                            if(argName.equals("authToken")){
                                authToken = value.toString();
                            }else if (argName.equals(POST_PARAM_NAME)){
                                postData = convertObjectToJSONString(value);
                            }else{
                                if(queryPathParameters.toString().length()> 0){
                                    queryPathParameters.append("~");
                                }
                                queryPathParameters.append(argName+"="+value.toString());
                            }
                        }

                        //get eternal command
                        String[] externalCommand = constructExternalCommand(apiServer, apiPackageName,
                                securityHandler.getApiKey(), authToken,
                                resource.getPath(), resource.getHttpMethod(), resource.getSuggestedMethodName(),
                                queryPathParameters.toString(), postData, language, libraryLocation);
                        //print the command
                        System.out.println("Test Case :" + testCasePath);
                        for(String arg : externalCommand){
                            System.out.print(arg + " ");
                        }
                        System.out.println("");
                        //execute and get data
                        String outputString = executeExternalTestCaseAndGetResult(externalCommand);
                        Object output = null;
                        if(outputString != null && outputString.length() > 0) {
                            output = convertJSONStringToObject(outputString, methodToExecute.getReturnType());
                        }
                        //6
                        Class returnType = methodToExecute.getReturnType();
                        if(!returnType.getName().equalsIgnoreCase("void")){
                            //7
                            testCaseOutput.getData().put(testCasePath, output);
                        }
                        //8
                        //log it as passed, if there is any failures in assertions, assertions will update the status
                        //to failed
                        testStatus.logStatus(testCase, testCasePath, true);
                        executeAssertions(testCasePath, testCase);
                    }
                }catch(Exception e){
                    boolean asserted = false;
                    if(testCase.getAssertions() != null) {
                        for(Assertion assertion : testCase.getAssertions()){
                            if(assertion.getCondition().equals("==") && assertion.getExpectedOutput().equalsIgnoreCase("Exception")){
                                testStatus.logStatus(testCase, testCasePath, true);
                                asserted = true;
                            }
                        }
                    }
                    if(!asserted){
                        testStatus.logStatus(testCase, testCasePath, false, e.getMessage(), e);
                    }
                }
            }
        }
        System.out.println(testStatus.printTestStatus());
    }


    /**
     * Populate necessayr argument values tat needs ot be populated before calling the method
     * @return
     */
    protected Map<String, Object> getArgumentsForMethodCall(Method methodToExecute, TestCase testCase) throws Exception {

        Map<String, Object> queryPathParameters = new HashMap<String, Object>();
        if(testCase.getInput() != null) {
            for(String inputParamName: testCase.getInput().keySet()){
                Object value = getParamValue(testCase.getInput().get(inputParamName));
                queryPathParameters.put(inputParamName, value);
            }
        }
        return queryPathParameters;
    }

	/**
	 * Execute all assertions in the test case. If there are nay failures test case will be amrked as failed
	 * and logged into test status object.
	 * @param testCase
	 */
	private void executeAssertions(String testCasePath, TestCase testCase) {
		List<Assertion> assertions = testCase.getAssertions();
		if(assertions != null) {
			for(Assertion assertion: assertions){
				try{
					Object actualOutPut = getParamValue(assertion.getActualOutput());
					Object expectedValue = getParamValue(assertion.getExpectedOutput());
					boolean failed = false;
					if(assertion.getCondition().equals(CONDITION_EQUAL)){
                        if(expectedValue.toString().equalsIgnoreCase("NULL") && actualOutPut == null){
                            failed = false;
                        }else{
                            if(expectedValue.getClass().isAssignableFrom(String.class)){
                                actualOutPut = actualOutPut.toString();
                            }
                            if(!actualOutPut.equals(expectedValue)){
                                failed = true;
                            }
                        }
					}else if(assertion.getCondition().equals(CONDITION_NOT_EQUAL)){
                        if(expectedValue.toString().equalsIgnoreCase("EXCEPTION")){
                            //this means user is not expecting any exception, output can be null, if we have reached
                            // here means there are no exceptions hence we can call the assertion is passed. 
                            failed = false;
                        }
						else if(actualOutPut == null || actualOutPut.equals(expectedValue)){
							failed = true;
						}
					}else{
						float actual = new Float(actualOutPut.toString());
						float expected = new Float(expectedValue.toString());
						if(assertion.getCondition().equals(CONDITION_GREATER)){
							if(!(actual > expected)){
								failed = true;
							}
						}else if(assertion.getCondition().equals(CONDITION_LESSER)){
							if(!(actual < expected)){
								failed = true;
							}
						}else if(assertion.getCondition().equals(CONDITION_LESSER_EQUAL)){
							if(!(actual <= expected)){
								failed = true;
							}
						}else if(assertion.getCondition().equals(CONDITION_GREATER_EQUAL)){
							if(!(actual >= expected)){
								failed = true;
							}
						}
					}
					if(failed) {
                        if(actualOutPut == null) {
    						testStatus.logAssertionStatus(testCasePath, false, expectedValue.toString(), null, assertion.getCondition());
                        }else{
                            testStatus.logAssertionStatus(testCasePath, false, expectedValue.toString(), actualOutPut.toString(), assertion.getCondition());
                        }
					} else{
                        if(actualOutPut == null) {
                            testStatus.logAssertionStatus(testCasePath, true, expectedValue.toString(), "null", assertion.getCondition());
                        }else{
    						testStatus.logAssertionStatus(testCasePath, true, expectedValue.toString(), actualOutPut.toString(), assertion.getCondition());
                        }
					}
				}catch(Exception e){
                    e.printStackTrace();
					testStatus.logAssertionStatus(testCasePath, false, assertion.getExpectedOutput(), assertion.getActualOutput(), assertion.getCondition(), e);
				}
			}
		}
	}
	
	/**
	 * creates the test case unique path
	 * @param suiteId
	 * @param caseId
	 * @return
	 */
	private String testCasePath(int suiteId, int caseId) {
		return suiteId + "." + caseId;
	}
	
	/**
	 * Read the values based on expression.
	 * The expression can refer the value in input data structure or outputs generated from executing the current or previous
	 * test cases or direct input. The Test script follow the syntax of ${output if it is referring output data, 
	 * ${input if it is referring input data.
	 * @param name
	 * @return
	 * @throws Exception
	 */
	private Object getParamValue(String name) throws Exception {
		//this means we should use the input form previous steps or data file. 
		if(name.startsWith("$")){
            //sample:"${input.userList[0].username}"
			if(name.startsWith(INPUT_DATA_EXPRESSION_PREFIX)){
				String expression = name.substring(INPUT_DATA_EXPRESSION_PREFIX.length(), name.length()-1);
                boolean hasSize = false;
                if(expression.endsWith("size")){
                    expression = expression.substring(0, expression.length()-5);
                    hasSize = true;
                }
                Object value =  PropertyUtils.getProperty(testData, expression);
                if(hasSize){
                    return MethodUtils.invokeMethod(value, "size", null);
                }

				return value;
			}else if(name.startsWith(OUTPUT_DATA_EXPRESSION_PREFIX)) {
                //sample: ${output(1.1.1).token}
				String expression = name.substring(OUTPUT_DATA_EXPRESSION_PREFIX.length(), name.length()-1);
                expression = "data"+expression;
				boolean hasSize = false;
				if(expression.endsWith("size")){
					expression = expression.substring(0, expression.length()-5);
					hasSize = true;
				}
				Object value =  PropertyUtils.getProperty(testCaseOutput, expression);
				if(hasSize){
					return MethodUtils.invokeMethod(value, "size", null);
				}
				return value;
			}else{
				throw new RuntimeException("Input expression for parameter " + name + "is not as per valid syntax ");
			}
		}else{
			return name;
		}
	}

    /**
     * Converts JSON string to object.
     */
    public static Object convertJSONStringToObject(String inputJSON, Class objectType) throws Exception {
        boolean isArray = false;
        boolean isList = false;
        Class className = objectType;
        String ObjectTypeName = objectType.getName();

        //identify if the input is a array
        if(ObjectTypeName.startsWith("[")){
            isArray = true;
            className = objectType.getComponentType();
        }

        //identify if the input is a list
        if(List.class.isAssignableFrom(objectType)){
            isList = true;
        }

        if(isArray || isList){
            Object responseObject = mapper.readValue(inputJSON, TypeFactory.type(objectType));
            return responseObject;
        }else{
            return APIInvoker.deserialize(inputJSON, className);
        }
    }

    /**
     * Converts JSON string to object.
     */
    public static String convertObjectToJSONString(Object input) throws Exception {
        return APIInvoker.serialize(input);
    }

    /**
     * Reads the test case results from standard out, converts that into java object and stores the value
     * in test case output data so that the same can be used in subsequent test case execution
     *
     * First line fo response should be a status line with possible values as SUCCESS, ERROR
     */
    private String executeExternalTestCaseAndGetResult(String[] command) throws Exception {

        Process p = Runtime.getRuntime().exec(command);

        BufferedReader stdInput = new BufferedReader(new
        InputStreamReader(p.getInputStream()));
        StringBuilder output = new StringBuilder();
        String s = null;
        boolean isStatusLine = true;
        String status = "";
        while ((s = stdInput.readLine()) != null) {
            System.out.println(s);
            if(isStatusLine){
                status = s;
                 if(status.equalsIgnoreCase("SUCCESS")||status.equalsIgnoreCase("OK") ) {
                    isStatusLine = false;
                }
            }else{
                output.append(s);
            }
        }
        if(status.equalsIgnoreCase("SUCCESS")||status.equalsIgnoreCase("OK") ) {
            return output.toString();
        }else{
            APIException exception = (APIException)convertJSONStringToObject(output.toString(),
                    APIException.class);
            throw exception;
        }
    }
    

    /**
     * Get the java command line that needs to be executed for runnign a test case
     */
    private String[] constructExternalCommand(String apiServer, String apiPackageName, String apiKey, String authToken,
                        String resource, String httpMethod, String suggestedMethodName, String queryAndPathParams,
                        String postData, String language, String libraryLocation) {
        List<String> command = new ArrayList<String>();
        if(language.equals(JAVA)){
            command.add("./bin/runjavaTestCase.sh");
            command.add("com.wordnik.swagger.testframework.JavaTestCaseExecutor");
            command.add( libraryLocation );
        }else if (language.equals(PYTHON)){
            command.add("../python/runtest.py ");
        }else if (language.equals(ANDROID)){
            command.add("../android/driver-test/bin/runandroid.sh");
            command.add("com.wordnik.swagger.testframework.JavaTestCaseExecutor");
        }else if (language.equals(AS3)){
            command.add("./bin/runas3TestCase.sh");
            command.add("com.wordnik.swagger.testframework.AS3TestCaseExecutor");

            if(postData == null){
                postData = "\"\"";
            }
            else{
                postData = "\"" + postData + "\"";
            }

            if(queryAndPathParams == null || queryAndPathParams.equals("")){
                queryAndPathParams = "\"\"";
            }
        }

        command.addAll(getCommandInputs(apiServer, apiPackageName, apiKey, authToken, resource, httpMethod,
                                        suggestedMethodName, queryAndPathParams, postData,
                                        language));
        String[] commandArray = new String[command.size()];
        command.toArray(commandArray);
        return commandArray;
    }

    private List<String> getCommandInputs(String apiServer, String apiPackageName, String apiKey, String authToken, String resource, String httpMethod,
                                            String suggestedMethodName, String queryAndPathParams, String postData,
                                            String language) {
        List<String> inputs = new ArrayList<String>();
        inputs.add(apiServer);
        inputs.add(apiPackageName);
        inputs.add(apiKey);
        inputs.add(authToken);
        inputs.add(resource);
        inputs.add(httpMethod);
        inputs.add(suggestedMethodName);
        inputs.add(queryAndPathParams);
        if(postData.equals("\"\"")){
            inputs.add(postData);
        }else{
            inputs.add(postData);
        }
        return inputs;
    }

}
