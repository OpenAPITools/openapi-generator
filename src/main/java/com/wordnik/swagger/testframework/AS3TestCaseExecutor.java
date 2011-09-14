package com.wordnik.swagger.testframework;


import org.codehaus.jackson.JsonFactory;
import org.codehaus.jackson.JsonGenerationException;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.type.TypeReference;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * User: deepakmichael
 * Date: 08/09/11
 * Time: 10:34 AM
 */
public class AS3TestCaseExecutor {

    /**
     * Follow the following argument pattern
     * Arguments in calling this method:
     * ApiServerURL
     *
     * @param args
     * @throws Exception
     */
    public static void main(String[] args) throws Exception {


        AS3TestCaseExecutor runner = new AS3TestCaseExecutor();
        String apiServer = args[0];
        String servicePackageName = args[1];
        String apiKey = args[2];
        String authToken = args[3];
        String resource = args[4];
        String httpMethod = args[5];
        String suggestedMethodName = args[6];
        Map<String, String> queryAndPathParameters = new HashMap<String, String>();
        String postData = null;
        if(args.length > 7 && args[7].length() > 0){
            String[] qpTuple = args[7].split("~");
            for(String tuple: qpTuple){
                String[] nameValue = tuple.split("=");
                if (nameValue.length == 2 ) {
                    queryAndPathParameters.put(nameValue[0], nameValue[1]);
                }
            }

        }
        if(args.length > 8 ){
            postData = args[8];
        }
        queryAndPathParameters.put("authToken", authToken);
        servicePackageName = args[9];
        String testAppConfigPath = args[10];
        String flexHome = args[11];

        runner.writeJSONTestData(apiServer, apiKey, authToken, httpMethod, resource, servicePackageName,
                suggestedMethodName, queryAndPathParameters, postData, testAppConfigPath);

        runner.executeTestCase(testAppConfigPath, flexHome);

    }

    private void executeTestCase(String testAppConfigPath, String flexHome) throws Exception {
        String[] externalCommand = constructExternalCommand(testAppConfigPath, flexHome);

        executeExternalTestCaseAndGetResult(externalCommand);

    }

    private void writeJSONTestData(String apiServer, String apiKey, String authToken, String httpMethod, String resource,
                                   String servicePackageName, String suggestedMethodName, Map<String,
            String> queryAndPathParameters, String postData, String testAppConfigPath) throws IOException {
        //write JSON file
        HashMap<String, Object> testInfo = new HashMap<String, Object>();
        testInfo.put("apiUrl",apiServer);
        testInfo.put("apiPackageName", servicePackageName);
        testInfo.put("apiKey", apiKey);
        testInfo.put("authToken", authToken);
        testInfo.put("resource", resource);
        testInfo.put("httpMethod", httpMethod);
        testInfo.put("methodName", suggestedMethodName);
        testInfo.put("queryAndPathParams",queryAndPathParameters);
        testInfo.put("postData", postData);
        testInfo.put("language","AS3");
        JsonFactory factory = new JsonFactory();
        ObjectMapper mapper = new ObjectMapper(factory);
        File aFileInOutputPath = new File(testAppConfigPath);
        String parentDirectory = aFileInOutputPath.getParent();
        mapper.writeValue(new File(parentDirectory+"/testData.json"), testInfo);
    }

    private void executeExternalTestCaseAndGetResult(String[] command) throws Exception {

        Process p = Runtime.getRuntime().exec(command);

        BufferedReader stdInput = new BufferedReader(new
                InputStreamReader(p.getInputStream()));
        StringBuilder output = new StringBuilder();
        String s = null;
        boolean isStatusLine = true;
        String status = "";
        while ((s = stdInput.readLine()) != null) {
            //System.out.println(s);
            if(isStatusLine){
                status = s;
                if(status.equalsIgnoreCase("SUCCESS")||status.equalsIgnoreCase("OK") ) {
                    isStatusLine = false;
                }
            }else{
                output.append(s);
            }
            //System.out.println(s);
        }

        String userDirectory = System.getProperty("user.home")+ File.separator+"Documents";
        String outputFile = userDirectory + File.separator+"testOutput.json";
        JsonFactory factory = new JsonFactory();
        ObjectMapper mapper = new ObjectMapper(factory);
        File from = new File(outputFile);
        TypeReference<HashMap<String,Object>> typeRef
                = new TypeReference<
                HashMap<String,Object>
                >() {};
        HashMap<String,Object> o
                = mapper.readValue(from, typeRef);
        Boolean isSuccess = false;
        if(o.containsKey("isSuccess")){
            isSuccess = (Boolean) o.get("isSuccess");
        }
        System.out.println(isSuccess ? "SUCCESS" : "FAILURE");
        if(isSuccess){
            if ( o.get("payload") != null ) {
                mapper.writeValue(System.out, o.get("payload"));
            }
        }
        else{
            mapper.writeValue(System.out, o.get("errorMessage"));
        }
        from.delete();
    }

    /**
     * Get the java command line that needs to be executed for runnign a test case
     * @param testAppConfigPath
     * @param flexHome
     */
    private String[] constructExternalCommand(String testAppConfigPath, String flexHome) {
        List<String> command = new ArrayList<String>();

        command.add(flexHome + "/bin/adl");
        command.add(testAppConfigPath);

        String[] commandArray = new String[command.size()];
        command.toArray(commandArray);
        return commandArray;
    }


}
