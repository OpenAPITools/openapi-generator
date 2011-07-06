package com.wordnik.test;

import com.wordnik.annotations.MethodArgumentNames;
import com.wordnik.api.WordAPI;
import com.wordnik.common.WordnikAPI;
import com.wordnik.exception.WordnikAPIException;
import com.wordnik.exception.WordnikExceptionCodes;
import org.apache.commons.beanutils.BeanUtils;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.type.TypeFactory;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Instance of this class runs single test case
 * User: ramesh
 * Date: 4/22/11
 * Time: 7:32 AM
 */
public class TestCaseExecutor {

    private static ObjectMapper mapper = new ObjectMapper();

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

        String className = getAPIClassName(resourceName);
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
                System.out.println(convertObjectToJSONString(output));

            }
        }catch(WordnikAPIException e){
            System.out.println("ERROR");
            try{
                System.out.println(convertObjectToJSONString(e));
            }catch(Exception ex){
                ex.printStackTrace();
            }
        } catch(Exception e){
            System.out.println("ERROR");
            try{
                WordnikAPIException apiException = new WordnikAPIException(WordnikExceptionCodes.SYSTEM_EXCEPTION,
                        e.getMessage());
                System.out.println(convertObjectToJSONString(apiException));
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
                if(superclass != null && superclass.getSimpleName().equalsIgnoreCase("WordnikObject")){
                    if(argNamesArray[i].trim().equals("postData")){
                        argument = convertJSONStringToObject(postData, argTypesArray[i]);
                    }else{
                        argument = populateWordnikInputModelObject(argTypesArray[i], queryAndPathParameters);
                    }
                }else{
                    //the aruments can be primitive types for query and path data and for post data it could be either
                    //a object or collection of objects. Hence we need to identify the input is single or colection
                    //based on that un-marshal the string
                    if(argNamesArray[i].trim().equals("postData")){
                        argument = convertJSONStringToObject(postData, argTypesArray[i]);
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
				fieldName = convertFirstCharToSmall(fieldName);
				Object value = inputDefinitions.get(fieldName);
				BeanUtils.setProperty(object, fieldName, value);
			}
		}
		return object;
	}

    /**
     * Converts the first character of the input into string.
     * Example: If the input is word, the return value will be Word
     * @param input
     * @return
     */
    public static String convertFirstCharToCaps(String input) {
    	if(input != null && input.length() > 0) {
    		return input.substring(0,1).toUpperCase() + input.substring(1);
    	} else {
    		throw new RuntimeException("Error converting input to first letter caps becuase of null input");
    	}
    }

    /**
     * Converts the first character of the input into string.
     * Example: If the input is word, the return value will be Word
     * @param input
     * @return
     */
    public static String convertFirstCharToSmall(String input) {
    	if(input != null && input.length() > 0) {
    		return input.substring(0,1).toLowerCase() + input.substring(1);
    	}else{
    		throw new RuntimeException("Error converting input to first letter to lower because of null input");
    	}
    }

    public static String getAPIClassName(String resourcePath) {
        String className = null;
        int index = resourcePath.indexOf(".");
        if(index >= 0) {
            String resourceName = resourcePath.substring(1,index);
            className = convertFirstCharToCaps(resourceName)+"API";
        }else{
            String[] paths = resourcePath.split("/");
            for(String path : paths) {
                if(path != null && path.length() > 0) {
                    className = convertFirstCharToCaps(path)+"API";
                    break;
                }
            }
        }
        return className;
    }

    /**
     * Converts JSON string to object.
     */
    public Object convertJSONStringToObject(String inputJSON, Class objectType) throws Exception {
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

}
