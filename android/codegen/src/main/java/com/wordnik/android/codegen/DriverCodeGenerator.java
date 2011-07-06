package com.wordnik.codegen;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.wordnik.exception.CodeGenerationException;

import org.antlr.stringtemplate.StringTemplate;
import org.antlr.stringtemplate.StringTemplateGroup;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.DeserializationConfig.Feature;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

/**
 * Created by IntelliJ IDEA.
 * User: ramesh
 * Date: 3/30/11
 * Time: 6:59 PM
 * To change this template use File | Settings | File Templates.
 */
public class DriverCodeGenerator {
	
	private static String HEADER_NAME_API_VERSION = "Wordnik-Api-Version";
	private static String VERSION_OBJECT_TEMPLATE = "VersionChecker";
	private static String MODEL_OBJECT_TEMPLATE = "ModelObject";
	private static String API_OBJECT_TEMPLATE = "ResourceObject";
	
    public static void main(String[] args) {
        DriverCodeGenerator codeGenerator = new DriverCodeGenerator();
        codeGenerator.generateCode();
    }

    /**
     * Generate classes needed for the model and API invocation
     */
    public void generateCode()	{
    	//read resources and get their documentation
        List<Resource> resources = this.readResourceDocumentation(
                "http://beta.wordnik.com/v4/", "word.json,words.json,wordList.json,wordLists.json,account.json");
        StringTemplateGroup aTemplateGroup = new StringTemplateGroup("templates","conf/templates");
        if(resources.size() > 0) {
        	generateVersionHelper(resources.get(0).getVersion(), aTemplateGroup);
        }
        generateModelClasses(resources, aTemplateGroup);
        generateAssemblerClassesForOutput(resources, aTemplateGroup);         
        generateModelClassesForInput(resources, aTemplateGroup);      
        generateAPIClasses(resources, aTemplateGroup);
    }

    /**
     * Reads the documentation of the resources and constructs the resource object that can be used
     * for generating the driver related classes. The resource list string should be "," separated
     */
    private List<Resource> readResourceDocumentation(String baseUrl, String resourceList) {

        List<Resource> resourceDocs = new ArrayList<Resource>();

        //valid for input
        if (baseUrl == null || resourceList == null ||
                baseUrl.trim().length() == 0 ||
                resourceList.trim().length() == 0) {
            throw new CodeGenerationException("Base URL or Resource list input is null");
        }


        //create list of resource URL
        String[] resources = resourceList.split(",");
        List<String> resourceURLs = new ArrayList<String>();
        for (String resource : resources) {
            resourceURLs.add(baseUrl + resource);
        }

        //make connection to resource and get the documentation
        for (String resourceURL : resourceURLs) {
            Client apiClient = Client.create();
            WebResource aResource = apiClient.resource(resourceURL);
            ClientResponse clientResponse =  aResource.get(ClientResponse.class);
            String version = clientResponse.getHeaders().get(HEADER_NAME_API_VERSION).get(0);
            String response = clientResponse.getEntity(String.class);
            try {
                ObjectMapper mapper = new ObjectMapper();
                mapper.getDeserializationConfig().set(Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
                Resource aResourceDoc = (Resource) mapper.readValue(response, Resource.class);
                aResourceDoc.setVersion(version);
                resourceDocs.add(aResourceDoc);
            } catch (IOException ioe) {
                throw new CodeGenerationException("Erro in coversting response json value to java object");
            }
        }
        return resourceDocs;

    }
    
    /**
     * Generates version file based on the version number received from the doc calls. This version file is used
     * while making the API calls to make sure Client and back end are compatible. 
     * @param version
     */
    private void generateVersionHelper(String version, StringTemplateGroup templateGroup) {
    	StringTemplate template = templateGroup.getInstanceOf(VERSION_OBJECT_TEMPLATE);
    	template.setAttribute("apiVersion", version);
    	File aFile = new File("../driver/src/main/java/com/wordnik/api/VersionChecker.java");
    	try{
	    	FileWriter aWriter = new FileWriter(aFile);
	    	BufferedWriter bufWriter = new BufferedWriter(aWriter); 
	    	bufWriter.write(template.toString());
	    	bufWriter.close();
    	}catch(IOException ioe){
            throw new CodeGenerationException("Error generating the versioned file: " +  ioe.getMessage());
    	}
    }
    
    /**
     * Generates model classes. If the class is already generated then ignores the same.
     */
    private void generateModelClasses(List<Resource> resources, StringTemplateGroup templateGroup) {
    	List<String> generatedClassNames = new ArrayList();
    	
    	for(Resource resource: resources) {
    		for(Model model : resource.getModels()){
    			if(!generatedClassNames.contains(model.getName())){
    				List<String> imports = new ArrayList<String>();
    				imports.add("com.wordnik.common.WordListType");
    				for(Parameter param : model.getFields()){
    					for(String importDef : param.getAttributeDefinition().getImportDefinitions()){
    						if(!imports.contains(importDef)){
    							imports.add(importDef);
    						}
    					}
    				}
    		    	StringTemplate template = templateGroup.getInstanceOf(MODEL_OBJECT_TEMPLATE);
    		    	template.setAttribute("fields", model.getFields());
    		    	template.setAttribute("imports", imports);
    		    	template.setAttribute("className", model.getGenratedClassName());
    		    	File aFile = new File("../driver/src/main/java/com/wordnik/model/"+model.getGenratedClassName()+".java");
    		    	try{
    			    	FileWriter aWriter = new FileWriter(aFile);
    			    	BufferedWriter bufWriter = new BufferedWriter(aWriter); 
    			    	bufWriter.write(template.toString());
    			    	bufWriter.close();
    		    	}catch(IOException ioe){
    		            throw new CodeGenerationException("Error generating the model classes : " + ioe.getMessage());
    		    	}
    				generatedClassNames.add(model.getName());
    			}
    		}
    	}
    	
    	generateWrapperClassForTestData(generatedClassNames, templateGroup);
    }    

    /**
     * Generates assembler classes if the API returns more than one objects. 
     * @param resources
     * @param templateGroup
     */
    private void generateAssemblerClassesForOutput(List<Resource> resources, StringTemplateGroup templateGroup) {
    	List<String> generatedClasses = new ArrayList<String>();
    	for(Resource resource : resources) {
    		if(resource.getEndPoints() != null) {
    			for(Endpoint endpoint : resource.getEndPoints()){
    				if(endpoint.getOperations() != null) {
    					for(EndpointOperation operation : endpoint.getOperations()){
    						Model model = operation.getModelObjectForAggregateObject(endpoint);
    						if(model != null){
    							if(!generatedClasses.contains(model.getName())) {
	    		    				List<String> imports = new ArrayList<String>();
	    		    				imports.add("com.wordnik.common.WordListType");
	    		    				for(Parameter param : model.getFields()){
	    		    					for(String importDef : param.getAttributeDefinition().getImportDefinitions()){
	    		    						if(!imports.contains(importDef)){
	    		    							imports.add(importDef);
	    		    						}
	    		    					}
	    		    				}
	    		    		    	StringTemplate template = templateGroup.getInstanceOf(MODEL_OBJECT_TEMPLATE);
	    		    		    	template.setAttribute("fields", model.getFields());
	    		    		    	template.setAttribute("imports", imports);
	    		    		    	template.setAttribute("className", model.getGenratedClassName());
	    		    		    	File aFile = new File("../driver/src/main/java/com/wordnik/model/"+model.getGenratedClassName()+".java");
	    		    		    	try{
	    		    			    	FileWriter aWriter = new FileWriter(aFile);
	    		    			    	BufferedWriter bufWriter = new BufferedWriter(aWriter); 
	    		    			    	bufWriter.write(template.toString());
	    		    			    	bufWriter.close();
	    		    		    	}catch(IOException ioe){
	    		    		            throw new CodeGenerationException("Error generating the assemble classes : " + ioe.getMessage());
	    		    		    	}
	    		    		    	generatedClasses.add(model.getName());
    							}    							
    						}
    					}
    				}
    			}
    		}
    	}
    }
    
    /**
     * Generates assembler classes if the API returns more than one objects. 
     * @param resources
     * @param templateGroup
     */
    private void generateModelClassesForInput(List<Resource> resources, StringTemplateGroup templateGroup) {
    	List<String> generatedClasses = new ArrayList<String>();
    	for(Resource resource : resources) {
    		if(resource.getEndPoints() != null) {
    			for(Endpoint endpoint : resource.getEndPoints()){
    				if(endpoint.getOperations() != null) {
    					for(EndpointOperation operation : endpoint.getOperations()){
    						Method method = operation.generateMethod(endpoint, resource);;
    						if(method.getInputModel() != null) {
	    						Model model = method.getInputModel();
	    						if(model != null){
	    							if(!generatedClasses.contains(model.getName())) {
		    		    				List<String> imports = new ArrayList<String>();
		    		    				imports.add("com.wordnik.common.WordListType");
		    		    				for(Parameter param : model.getFields()){
		    		    					for(String importDef : param.getAttributeDefinition().getImportDefinitions()){
		    		    						if(!imports.contains(importDef)){
		    		    							imports.add(importDef);
		    		    						}
		    		    					}
		    		    				}
		    		    		    	StringTemplate template = templateGroup.getInstanceOf(MODEL_OBJECT_TEMPLATE);

		    		    		    	template.setAttribute("fields", model.getFields());
		    		    		    	template.setAttribute("imports", imports);
		    		    		    	template.setAttribute("className", model.getGenratedClassName());
		    		    		    	File aFile = new File("../driver/src/main/java/com/wordnik/model/"+model.getGenratedClassName()+".java");
		    		    		    	try{
		    		    			    	FileWriter aWriter = new FileWriter(aFile);
		    		    			    	BufferedWriter bufWriter = new BufferedWriter(aWriter); 
		    		    			    	bufWriter.write(template.toString());
		    		    			    	bufWriter.close();
		    		    		    	}catch(IOException ioe){
		    		    		            throw new CodeGenerationException("Error generating the input model classes  " + ioe.getMessage());
		    		    		    	}
		    		    		    	generatedClasses.add(model.getName());
	    							}    							
	    						}
    						}
    					}
    				}
    			}
    		}
    	}
    }    
    
    /**
     * Generates one API class for each resource and each end point in the resource is translated as method. 
     * @param resources
     * @param templateGroup
     */
    private void generateAPIClasses(List<Resource> resources, StringTemplateGroup templateGroup) {
    	
    	for(Resource resource : resources) {
    		List<Method> methods = new ArrayList<Method>();
    		methods = resource.generateMethods(resource);
	    	StringTemplate template = templateGroup.getInstanceOf(API_OBJECT_TEMPLATE);
            String className = resource.generateClassName();
            List<Method> filteredMethods = new ArrayList<Method>();
            for(Method method:methods){
                if(!CodeGenOverridingRules.isMethodIgnored(className, method.getName())){
                    filteredMethods.add(method);
                }
            }
	    	template.setAttribute("resource", className);
	    	template.setAttribute("methods", filteredMethods);
            template.setAttribute("extends", CodeGenOverridingRules.getServiceExtendingClass(className));

	    	File aFile = new File("../driver/src/main/java/com/wordnik/api/"+ resource.generateClassName() +".java");
	    	try{
		    	FileWriter aWriter = new FileWriter(aFile);
		    	BufferedWriter bufWriter = new BufferedWriter(aWriter); 
		    	bufWriter.write(template.toString());
		    	bufWriter.close();
	    	}catch(IOException ioe){
	            throw new CodeGenerationException("Error generating the API classes : " + ioe.getMessage());
	    	}    		
    	}
    }
    
    /**
     * Creates a wrapper model class that contains all model classes as list of objects.
     * This class is used for storing test data
     */
    private void generateWrapperClassForTestData(List<String> generatedClassNames, StringTemplateGroup templateGroup) {
    	Model model = new Model();
    	model.setName("TestData");
    	model.setDescription("CLass used to store all the test data. Thsi should not be used for any development");
    	List<Parameter> parameters = new ArrayList<Parameter>();
    	model.setFields(parameters);
    	for(String className : generatedClassNames){
    		Parameter aParam = new Parameter();
    		aParam.setName(convertFirstCharToSmall(className)+"List");
    		aParam.setParamType("List["+className+"]");
    		parameters.add(aParam);
    	}

        //add missing class from models
        Parameter aParam = new Parameter();
        aParam.setName("StringValueList");
        aParam.setParamType("List[StringValue]");
        parameters.add(aParam);

		List<String> imports = new ArrayList<String>();
		imports.add("com.wordnik.common.WordListType");
        imports.add("com.wordnik.common.StringValue");

		for(Parameter param : model.getFields()){
			for(String importDef : param.getAttributeDefinition().getImportDefinitions()){
				if(!imports.contains(importDef)){
					imports.add(importDef);
				}
			}
		}
    	StringTemplate template = templateGroup.getInstanceOf(MODEL_OBJECT_TEMPLATE);
    	template.setAttribute("fields", model.getFields());
    	template.setAttribute("imports", imports);
    	template.setAttribute("className", model.getGenratedClassName());
    	File aFile = new File("../driver/src/main/java/com/wordnik/model/"+model.getGenratedClassName()+".java");
    	try{
	    	FileWriter aWriter = new FileWriter(aFile);
	    	BufferedWriter bufWriter = new BufferedWriter(aWriter); 
	    	bufWriter.write(template.toString());
	    	bufWriter.close();
    	}catch(IOException ioe){
            throw new CodeGenerationException("Error generating the wrapper classes for test data file : " + ioe.getMessage());
    	}
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
    	}else{
    		throw new CodeGenerationException("Error converting input to first letter caps becuase of null input");
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
    		throw new CodeGenerationException("Error converting input to first letter to lower because of null input");
    	}
    }    
}
