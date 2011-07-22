package com.wordnik.test;

import java.io.*;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.wordnik.codegen.config.CodeGenConfig;
import com.wordnik.codegen.java.JavaCodeGenConfig;
import com.wordnik.common.*;
import com.wordnik.exception.APIException;
import org.apache.commons.beanutils.MethodUtils;
import org.apache.commons.beanutils.PropertyUtils;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.DeserializationConfig.Feature;

import com.wordnik.api.WordAPI;
import com.wordnik.codegen.resource.Resource;
import com.wordnik.model.TestData;
import org.codehaus.jackson.map.SerializationConfig;
import org.codehaus.jackson.map.type.TypeFactory;
import org.codehaus.jettison.json.JSONObject;


/**
 * Instance of this class is used to run the tests and assert the results based on
 * JSON based test script.
 * Created by IntelliJ IDEA.
 * User: ramesh
 * Date: 3/30/11
 * Time: 6:27 PM
 */
public class APITestRunner {

	private static String INPUT_DATA_EXPRESSION_PREFIX = "${data";
	private static String OUTPUT_DATA_EXPRESSION_PREFIX = "${output";
	
	private static String CONDITION_EQUAL = "==";
	private static String CONDITION_NOT_EQUAL = "!=";
	private static String CONDITION_GREATER = ">";
	private static String CONDITION_LESSER = "<";
	private static String CONDITION_GREATER_EQUAL = ">=";
	private static String CONDITION_LESSER_EQUAL = "<=";
	
	private TestOutput testCaseOutput = new TestOutput();
	private TestStatus testStatus = new TestStatus();
	private TestData testData = null;
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

    private CodeGenConfig config = new JavaCodeGenConfig();

    /**
     * Follow the following argument pattern
     * First argument should be execte full test suite or single test case
     *  -ts means execute execute full test suite
     *  -tc means execute individual test case
     * Second argument is api <apiKey>
     * third argument is auth token <auth token>
     * forth argument will be name of resource
     * fifth argument will be HTTP method name
     * sixth argument will be suggested methdo name
     * 7 and 8th argument is optional and specify while calling single test case
     *  -qp ~ seperated name=value pairs Example: username=test~password=password123
     * 9 and 10th  argument is optional and specified while calling individual test case
     *  -postData -- object tht should be used for sending post data (this is a json representation of post data)
     *
     * @param args
     * @throws Exception
     */
	public static void main(String[] args) throws Exception {
		WordAPI.initialize("c1431550c97c589f2bc1012d963142422a4270d8018aad4b0", "http://beta.wordnik.com/v4/",false);
		APITestRunner runner = new APITestRunner();
        String language = args[0];
        String suiteId = "0";
        if(args.length > 1){
            suiteId = args[1];
        }
        runner.initialize();
        runner.runTests(language, new Integer(suiteId));
	}

    public void initialize() throws Exception {
		//load test script
		File aFile = new File("../conf/driver_regression_testscript.json");
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
		aFile = new File("../conf/driver_regression_testdata.json");
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
        testData = (TestData) mapper.readValue(builder.toString(), TestData.class);
        reader.close();
    }


    /**
     * Gets the package that is initialized for testing
     * @return
     */
    public TestPackage getTestPackage() {
        return aPackage;
    }

	/**
	 * Run the tests after reading test script and test data files. 
	 * @throws Exception
	 */
	public void runTests(String language, int suiteId) throws Exception {
		runTestPackageExternal(aPackage, language, suiteId);
	}
	
    /***
     * Runs individual test cases in all test suites and stored the result in output object
     * @param testPackage
     */
    private void runTestPackageExternal(TestPackage testPackage, String language, int suiteId) throws Exception {
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

        int testPackageId = testPackage.getId();

        for(TestSuite suite : testPackage.getTestSuites()) {
            if(suiteId != 0 && suiteId != suite.getId()){
                continue;
            }
            int testSuiteId = suite.getId();
            //1
            for(TestCase testCase : suite.getTestCases()){
                String testCasePath = testCasePath(testPackageId , testSuiteId , testCase.getId());

                //2
                TestResource resource = resourceMap.get(testCase.getResourceId());
                String path = resource.getPath();
                String className = config.getNameGenerator().getServiceName(path);
                String methodName = resource.getSuggestedMethodName();

                //3
                Class apiClass = Class.forName("com.wordnik.api." + className);
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
                            }else if (argName.equals("postObject")){
                                postData = convertObjectToJSONString(value);
                            }else{
                                if(queryPathParameters.toString().length()> 0){
                                    queryPathParameters.append("~");
                                }
                                queryPathParameters.append(argName+"="+value.toString());
                            }
                        }

                        //get eternal command
                        String[] externalCommand = constructExternalCommand(WordnikAPI.getApiKey(), authToken,
                                resource.getPath(), resource.getHttpMethod(), resource.getSuggestedMethodName(),
                                queryPathParameters.toString(), postData, language);
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
                            if(assertion.getExpectedOutput().equalsIgnoreCase("Exception")){
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
	 * @param packageId
	 * @param suiteId
	 * @param caseId
	 * @return
	 */
	private String testCasePath(int packageId, int suiteId, int caseId) {
		return packageId + "." + suiteId + "." + caseId;
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
			if(name.startsWith(INPUT_DATA_EXPRESSION_PREFIX)){
				String expression = name.substring(7, name.length()-1);
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
				String expression = name.substring(9, name.length()-1);
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
            return WordnikAPI.deserialize(inputJSON, className);
        }
    }

    /**
     * Converts JSON string to object.
     */
    public static String convertObjectToJSONString(Object input) throws Exception {
        return WordnikAPI.serialize(input);
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
    private String[] constructExternalCommand(String apiKey, String authToken, String resource, String httpMethod,
                                            String suggestedMethodName, String queryAndPathParams, String postData,
                                            String language) {
        List<String> command = new ArrayList<String>();
        if(language.equals(JAVA)){
            command.add("./bin/runjava.sh");
            command.add("com.wordnik.test.TestCaseExecutor");
        }else if (language.equals(PYTHON)){
            command.add("../python/runtest.py ");
        }else if (language.equals(ANDROID)){
            command.add("../android/driver-test/bin/runandroid.sh");
            command.add("com.wordnik.test.TestCaseExecutor");
        }

        command.addAll(getCommandInputs(apiKey, authToken, resource, httpMethod,
                                        suggestedMethodName, queryAndPathParams, postData,
                                        language));
        String[] commandArray = new String[command.size()];
        command.toArray(commandArray);
        return commandArray;
    }

    private List<String> getCommandInputs(String apiKey, String authToken, String resource, String httpMethod,
                                            String suggestedMethodName, String queryAndPathParams, String postData,
                                            String language) {
        List<String> inputs = new ArrayList<String>();
        inputs.add(apiKey);
        inputs.add(authToken);
        inputs.add(resource);
        inputs.add(httpMethod);
        inputs.add(suggestedMethodName);
        inputs.add(queryAndPathParams);
        if(postData.equals("\"\"")){
            inputs.add(postData);
        }else{
            //inputs.add(JSONObject.quote(postData));
            inputs.add(postData);
        }
        return inputs;
    }

}
