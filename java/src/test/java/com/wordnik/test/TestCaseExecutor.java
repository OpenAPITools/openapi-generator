package com.wordnik.test;

import com.wordnik.annotations.MethodArgumentNames;
import com.wordnik.api.WordAPI;
import com.wordnik.swagger.codegen.config.ApiConfiguration;
import com.wordnik.swagger.codegen.config.*;
import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;
import com.wordnik.swagger.codegen.config.java.JavaCodeGenRulesProvider;
import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;
import com.wordnik.exception.APIException;
import com.wordnik.exception.APIExceptionCodes;
import com.wordnik.swagger.codegen.config.ApiConfiguration;
import com.wordnik.swagger.codegen.config.NamingPolicyProvider;
import com.wordnik.swagger.codegen.config.RulesProvider;
import com.wordnik.swagger.codegen.config.java.JavaCodeGenRulesProvider;
import org.apache.commons.beanutils.BeanUtils;
import org.codehaus.jettison.json.JSONObject;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

/**
 * Instance of this class runs single test case
 * User: ramesh
 * Date: 4/22/11
 * Time: 7:32 AM
 */
public class TestCaseExecutor {

    public ApiConfiguration config = new com.wordnik.swagger.codegen.config.ApiConfiguration();
    private NamingPolicyProvider namingPolicyProvider = new CamelCaseNamingPolicyProvider();
    private RulesProvider rulesProvider = new JavaCodeGenRulesProvider();

    /**
     * Follow the following argument pattern
     * First argument is api <apiKey>
     * Second argument is auth token <auth token>
     * Third argument will be name of resource
     * Fourth argument will be HTTP method name
     * Fifth argument will be suggested methdo name
     * 6th argument is for query and path parameters, if not available then gets empty string
     * 7th argument is for post data, if not available get empty string
     *
     * @param args
     * @throws Exception
     */
	public static void main(String[] args) throws Exception {
		WordAPI.initialize("c23b746d074135dc9500c0a61300a3cb7647e53ec2b9b658e", "http://beta.wordnik.com/v4/", false);
		TestCaseExecutor runner = new TestCaseExecutor();
        runner.config.setModelBaseClass("WordnikObject");
        String apiKey = args[0];
        String authToken = args[1];
        String resource = args[2];
        String method = args[3];
        String suggestedMethodName = args[4];
        Map<String, String> queryAndPathParameters = new HashMap<String, String>();
        String postData = null;
        if(args.length > 5 && args[5].length() > 0){
            String[] qpTuple = args[5].split("~");
            for(String tuple: qpTuple){
                String[] nameValue = tuple.split("=");
                queryAndPathParameters.put(nameValue[0], nameValue[1]);
            }
        }
        if(args.length > 6 ){
            postData = args[6];
        }
        queryAndPathParameters.put("authToken", authToken);
        runner.executeTestCase(resource, method, suggestedMethodName, apiKey, authToken, queryAndPathParameters,
                postData);

	}

   private void executeTestCase(String resourceName, String httpMethod, String suggestedName, String apiKey,
                                 String authToken, Map<String, String> queryAndPathParameters, String postData) {

        String className = namingPolicyProvider.getServiceName(resourceName);
        String methodName = suggestedName;

        //3
        try {
            Class apiClass = Class.forName("com.wordnik.api." + className);
            Method[] methods = apiClass.getMethods();
            Method methodToExecute = null;
            for(Method method : methods){
                if(method.getName().equals(methodName)){
                    methodToExecute = method;
                    break;
                }
            }

            if(methodToExecute != null) {
                //4
                Object[] arguments = populateArgumentsForTestCaseExecution(methodToExecute, queryAndPathParameters,
                                                           postData);
                Object output = null;
                if(arguments != null && arguments.length > 0){
                    //5
                    output = methodToExecute.invoke(null, arguments);
                }else{
                    //5
                    output = methodToExecute.invoke(null);
                }
                //6
                System.out.println("SUCCESS");
                System.out.println(APITestRunner.convertObjectToJSONString(output));

            }
        }catch(APIException e){
            StringWriter sWriter = new StringWriter();
            PrintWriter writer = new PrintWriter(sWriter);
            e.printStackTrace(writer);
            System.out.println(sWriter.getBuffer().toString());
            System.out.println(e.getMessage());
            System.out.println("ERROR");
            try{
                System.out.println(APITestRunner.convertObjectToJSONString(e));
            }catch(Exception ex){
                ex.printStackTrace();
            }
        } catch(Exception e){
            StringWriter sWriter = new StringWriter();
            PrintWriter writer = new PrintWriter(sWriter);
            e.printStackTrace(writer);
            System.out.println(sWriter.getBuffer().toString());
            e.printStackTrace();
            System.out.println("ERROR");
            try{
                APIException apiException = new APIException(APIExceptionCodes.SYSTEM_EXCEPTION,
                        e.getMessage());
                System.out.println(APITestRunner.convertObjectToJSONString(apiException));
            }catch(Exception ex){
                ex.printStackTrace();
            }
        }
    }

    /**
     * Gets the list of input query and path parameters and post data vlues and covenrt them to arguments that
     * can be used for calling the method. This logic will be different in each driver language depends on how method
     * input arguments are created.
     */
    private Object[] populateArgumentsForTestCaseExecution(Method methodToExecute, Map<String, String> queryAndPathParameters,
                                                           String postData) throws Exception {
        MethodArgumentNames argNames = methodToExecute.getAnnotation(MethodArgumentNames.class);
        String[] argNamesArray = null;
        if(argNames != null && argNames.value().length() > 0) {
            argNamesArray = argNames.value().split(",");
        }
        Class[] argTypesArray = methodToExecute.getParameterTypes();
        Object output = null;

        if(argNamesArray != null && argNamesArray.length > 0){
            Object[] arguments = new Object[argNamesArray.length];

            for(int i=0; i < argNamesArray.length; i++){
                Object argument = null;
                String canonicalName = argTypesArray[i].getCanonicalName();
                Class superclass = (Class)argTypesArray[i].getGenericSuperclass();
                //if the input argument is of type wordnik object then it is posisble that the object could be either
                // post data or input wrapper object created by code generator. If it is wrpper object then use the
                // individual query and path parameters to create the wrapper object.  If it is post data directly
                // convert input JSON string to post data object
                if(superclass != null && superclass.getSimpleName().equalsIgnoreCase(config.getModelBaseClass())){
                    if(argNamesArray[i].trim().equals("postObject")){
                        argument = APITestRunner.convertJSONStringToObject(postData, argTypesArray[i]);
                    }else{
                        argument = populateWordnikInputModelObject(argTypesArray[i], queryAndPathParameters);
                    }
                }else{
                    //the aruments can be primitive types for query and path data and for post data it could be either
                    //a object or collection of objects. Hence we need to identify the input is single or colection
                    //based on that un-marshal the string
                    if(argNamesArray[i].trim().equals("postObject")){
                        argument = APITestRunner.convertJSONStringToObject(postData, argTypesArray[i]);
                    }else{
                        argument = queryAndPathParameters.get(argNamesArray[i].trim());
                    }
                }
                arguments[i] = argument;
            }
            return arguments;
        }
        return null;
    }

	/**
	 * Populates the wordnik inout object.
	 * The definitions for the input will refer the attribute name directly as the input object is more of java driver concept
	 * hence the test script can not create the input with reference to input object. Test scirpt will only use attribute name.
	 * Example: If we are looking for a attribute called limit inside an WordExampleInput the input definitions in test script
	 * will have an entry as "input":10 (please note that there is no reference to input object)
	 * @param inputDefinitions
	 * @return
	 */
	private Object populateWordnikInputModelObject(Class wordnikClass, Map<String, String> inputDefinitions) throws Exception {
		Object object =  wordnikClass.getConstructor().newInstance();
		Method[] methods = wordnikClass.getMethods();
		for(Method method : methods){
			if(method.getName().startsWith("get")){
				String methodName = method.getName();
				String fieldName = methodName.substring(3);
				fieldName = namingPolicyProvider.applyMethodNamingPolicy(fieldName);
				Object value = inputDefinitions.get(fieldName);
				BeanUtils.setProperty(object, fieldName, value);
			}
		}
		return object;
	}    

}
