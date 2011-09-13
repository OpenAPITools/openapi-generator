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

import com.wordnik.swagger.codegen.config.java.JavaDataTypeMappingProvider;
import com.wordnik.swagger.runtime.annotations.MethodArgumentNames;
import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;
import com.wordnik.swagger.runtime.common.APIInvoker;
import com.wordnik.swagger.runtime.common.ApiKeyAuthTokenBasedSecurityHandler;
import com.wordnik.swagger.runtime.exception.APIException;
import com.wordnik.swagger.runtime.exception.APIExceptionCodes;
import org.apache.commons.beanutils.BeanUtils;

import java.io.PrintWriter;
import java.io.StringWriter;
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
public class JavaTestCaseExecutor {

    private CamelCaseNamingPolicyProvider namingPolicyProvider = new CamelCaseNamingPolicyProvider();
    private JavaDataTypeMappingProvider datatypeMppingProvider = new JavaDataTypeMappingProvider();

    /**
     * Follow the following argument pattern
     * Arguments in calling this method:
     * ApiServerURL
     *
     * @param args
     * @throws Exception
     */
	public static void main(String[] args) throws Exception {


		JavaTestCaseExecutor runner = new JavaTestCaseExecutor();
        String apiServer = args[1];
        String servicePackageName = args[2];
        String apiKey = args[3];
        String authToken = args[4];
        String resource = args[5];
        String httpMethod = args[6];
        String suggestedMethodName = args[7];
        Map<String, String> queryAndPathParameters = new HashMap<String, String>();
        String postData = null;
        if(args.length > 8 && args[8].length() > 0){
            String[] qpTuple = args[8].split("~");
            for(String tuple: qpTuple){
                String[] nameValue = tuple.split("=");
                queryAndPathParameters.put(nameValue[0], nameValue[1]);
            }
        }
        if(args.length > 9 ){
            postData = args[9];
        }
        queryAndPathParameters.put("authToken", authToken);

        ApiKeyAuthTokenBasedSecurityHandler securityHandler = new ApiKeyAuthTokenBasedSecurityHandler(apiKey, authToken);
        APIInvoker aAPIInvoker = APIInvoker.initialize(securityHandler, apiServer, true);

        runner.executeTestCase(resource, servicePackageName, suggestedMethodName, queryAndPathParameters, postData);

	}

    private void executeTestCase(String resourceName, String servicePackageName, String suggestedName,
                                 Map<String, String> queryAndPathParameters, String postData) {

        String className = namingPolicyProvider.getServiceName(resourceName);
        String methodName = suggestedName;
        //3
        try {
            Class apiClass = Class.forName(servicePackageName + "." + className);
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
                                                           postData, className, resourceName);
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
                                                           String postData, String serviceName, String resourcePath) throws Exception {
        MethodArgumentNames argNames = methodToExecute.getAnnotation(MethodArgumentNames.class);
        String[] argNamesArray = null;
        if(argNames != null && argNames.value().length() > 0) {
            argNamesArray = argNames.value().split(",");
        }
        Class[] argTypesArray = methodToExecute.getParameterTypes();
        Object output = null;
        String inputClassName = namingPolicyProvider.getInputObjectName(serviceName, resourcePath);

        if(argNamesArray != null && argNamesArray.length > 0){
            Object[] arguments = new Object[argNamesArray.length];

            for(int i=0; i < argNamesArray.length; i++){
                Object argument = null;
                //if the method takes input model instead of individual arguments, convert individual arguments into input model object
                if(argTypesArray[i].getName().equalsIgnoreCase(inputClassName)){
                    argument = populateInputModelObject(argTypesArray[i], queryAndPathParameters);
                }else if(datatypeMppingProvider.isPrimitiveType(argTypesArray[i].getName())){
                    argument = queryAndPathParameters.get(argNamesArray[i].trim());
                }else if (argNamesArray[i].trim().equals(APITestRunner.POST_PARAM_NAME)){
                    argument = APITestRunner.convertJSONStringToObject(postData, argTypesArray[i]);
                }else{
                    //some times input can be list of primitives for supporting multivalued values. however test case sends the input as comma separated values
                    //so we need to convert comma separated string into JSON list format
                    if(List.class.isAssignableFrom(argTypesArray[i]) && !queryAndPathParameters.get(argNamesArray[i].trim()).startsWith("[")){
                        String listInput= "[";
                        int x = 0;
                        String[] values = queryAndPathParameters.get(argNamesArray[i].trim()).split(",");
                        for(String value : values){
                            if(x > 0){listInput = listInput + ",";}
                            listInput = listInput + "\""+ value + "\"";
                            x++;
                        }
                        listInput = listInput + "]";
                        argument = APITestRunner.convertJSONStringToObject(listInput, argTypesArray[i]);
                    }else{
                        argument = APITestRunner.convertJSONStringToObject(queryAndPathParameters.get(argNamesArray[i].trim()), argTypesArray[i]);
                    }
                }
                arguments[i] = argument;
            }
            return arguments;
        }
        return null;
    }

	/**
	 * Populates the swagger input model object.
     *
     * Input model is created when number of inputs to a method exceed certain limit.
	 * @param inputDefinitions
	 * @return
	 */
	private Object populateInputModelObject(Class swaggerInputClass, Map<String, String> inputDefinitions) throws Exception {
		Object object =  swaggerInputClass.getConstructor().newInstance();
		Method[] methods = swaggerInputClass.getMethods();
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
