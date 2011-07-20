package com.wordnik.codegen;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.wordnik.codegen.config.CodeGenConfig;
import com.wordnik.codegen.resource.*;
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
import java.util.Map;

/**
 * User: ramesh
 * Date: 3/30/11
 * Time: 6:59 PM
 */
public class DriverCodeGenerator {
	
	private static String HEADER_NAME_API_VERSION = "Wordnik-Api-Version";
	private static String VERSION_OBJECT_TEMPLATE = "VersionChecker";
	private static String MODEL_OBJECT_TEMPLATE = "ModelObject";
	private static String API_OBJECT_TEMPLATE = "ResourceObject";

	private CodeGenConfig config = null;

    public CodeGenConfig getConfig() {
        return config;
    }

    public void setConfig(CodeGenConfig config) {
        this.config = config;
    }

    /**
     * Generate classes needed for the model and API invocation
     */
    public void generateCode()	{
    	//read resources and get their documentation
        //TODO - temporary change until new API server is up
        Boolean isNewApi = true;
        List<Resource> resources = this.readResourceDocumentation(
                "http://swagr.api.wordnik.com/v4/", "word.json", isNewApi);//word.json,words.json,wordList.json,wordLists.json,
        StringTemplateGroup aTemplateGroup = new StringTemplateGroup("templates",config.getTemplateLocation());
        if(resources.size() > 0) {
        	generateVersionHelper(resources.get(0).getVersion(), aTemplateGroup);
        }
        generateModelClasses(resources, aTemplateGroup);
        generateAssemblerClassesForOutput(resources, aTemplateGroup, config);
        generateModelClassesForInput(resources, aTemplateGroup);      
        generateAPIClasses(resources, aTemplateGroup);
    }

    /**
     * Reads the documentation of the resources and constructs the resource object that can be used
     * for generating the driver related classes. The resource list string should be "," separated
     */
    private List<Resource> readResourceDocumentation(String baseUrl, String resourceList, Boolean newApi) {

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
        Client apiClient = Client.create();
        for (String resourceURL : resourceURLs) {
            WebResource aResource = apiClient.resource(resourceURL);
            ClientResponse clientResponse =  aResource.get(ClientResponse.class);
            String version = clientResponse.getHeaders().get(HEADER_NAME_API_VERSION).get(0);
            String response = clientResponse.getEntity(String.class);
            try {
                ObjectMapper mapper = new ObjectMapper();
                mapper.getDeserializationConfig().set(Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
                Resource aResourceDoc = deserializeResource(response, mapper, newApi);
                aResourceDoc.setVersion(version);
                resourceDocs.add(aResourceDoc);
            } catch (IOException ioe) {
                throw new CodeGenerationException("Error in coverting resource json documentation to java object");
            }
        }
        return resourceDocs;

    }

    /**
     * Deserializes the response and returns a Response object
     * @param response
     * @param mapper
     * @param newApi
     * @return
     * @throws IOException
     */
    private Resource deserializeResource(String response, ObjectMapper mapper, Boolean newApi) throws IOException {
        Resource resource;
        if(!newApi) {
            resource = (Resource) mapper.readValue(response, Resource.class);
        }
        else{
            ApiResource apiResource = mapper.readValue(response, ApiResource.class);
            //convert apiResource to resource
            resource = new Resource();
            Model model;
            List<Model> models = new ArrayList<Model>();
            String modelName;
            ApiModelDefn modelDefn;
            if (apiResource.getModels() != null) {
                for (Map.Entry<String, ApiModelDefn> entry : apiResource.getModels().getModelList().entrySet()) {
                    modelName = entry.getKey();
                    modelDefn = entry.getValue();
                    model = new Model();
                    model.setName(modelName);
                    model.setDescription(modelDefn.getDescription());
                    model.setFields( modelDefn.getProperties().toFieldList( this.config ) );
                    models.add( model );
                }
            }
            resource.setModels( models );
            resource.setEndPoints( apiResource.getEndPoints() );
        }

        return resource;
    }

    /**
     * Generates version file based on the version number received from the doc calls. This version file is used
     * while making the API calls to make sure Client and back end are compatible. 
     * @param version
     */
    private void generateVersionHelper(String version, StringTemplateGroup templateGroup) {
    	StringTemplate template = templateGroup.getInstanceOf(VERSION_OBJECT_TEMPLATE);
    	template.setAttribute("apiVersion", version);
    	File aFile = new File(config.getResourceClassLocation() + config.getNameGenerator().getVersionCheckerClassName()
                + config.getClassFileExtension());
        writeFile(aFile, template.toString(), "Version checker class");
    }
    
    /**
     * Generates model classes. If the class is already generated then ignores the same.
     */
    private void generateModelClasses(List<Resource> resources, StringTemplateGroup templateGroup) {
    	List<String> generatedClassNames = new ArrayList();
    	
    	for(Resource resource: resources) {
    		for(Model model : resource.getModels()){
    			if(!generatedClassNames.contains(model.getName()) && !config.getCodeGenOverridingRules().isModelIgnored(model.getName())){
    				List<String> imports = new ArrayList<String>();
    				imports.addAll(this.config.getDefaultModelImports());
    				for(Parameter param : model.getFields()){
    					for(String importDef : param.getAttributeDefinition(config.getDataTypeMapper()).getImportDefinitions()){
    						if(!imports.contains(importDef)){
    							imports.add(importDef);
    						}
    					}
    				}
    		    	StringTemplate template = templateGroup.getInstanceOf(MODEL_OBJECT_TEMPLATE);
    		    	template.setAttribute("fields", model.getFields());
    		    	template.setAttribute("imports", imports);
    		    	template.setAttribute("className", model.getGenratedClassName());
    		    	File aFile = new File(config.getModelClassLocation()+model.getGenratedClassName()+config.getClassFileExtension());
                    writeFile(aFile, template.toString(), "Model class");
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
    private void generateAssemblerClassesForOutput(List<Resource> resources, StringTemplateGroup templateGroup,
                                                   CodeGenConfig config) {
    	List<String> generatedClasses = new ArrayList<String>();
    	for(Resource resource : resources) {
    		if(resource.getEndPoints() != null) {
    			for(Endpoint endpoint : resource.getEndPoints()){
    				if(endpoint.getOperations() != null) {
    					for(EndpointOperation operation : endpoint.getOperations()){
    						Model model = operation.getModelObjectForAggregateObject(endpoint, config);
    						if(model != null){
    							if(!generatedClasses.contains(model.getName())) {
	    		    				List<String> imports = new ArrayList<String>();
                                    imports.addAll(this.config.getDefaultModelImports());
	    		    				for(Parameter param : model.getFields()){
	    		    					for(String importDef : param.getAttributeDefinition(config.getDataTypeMapper()).getImportDefinitions()){
	    		    						if(!imports.contains(importDef)){
	    		    							imports.add(importDef);
	    		    						}
	    		    					}
	    		    				}
	    		    		    	StringTemplate template = templateGroup.getInstanceOf(MODEL_OBJECT_TEMPLATE);
	    		    		    	template.setAttribute("fields", model.getFields());
	    		    		    	template.setAttribute("imports", imports);
	    		    		    	template.setAttribute("className", model.getGenratedClassName());
	    		    		    	File aFile = new File(config.getModelClassLocation()+model.getGenratedClassName()+config.getClassFileExtension());
                                    writeFile(aFile, template.toString(), "Assemble class");
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
    						Method method = operation.generateMethod(endpoint, resource, config);
    						if(method.getInputModel() != null) {
	    						Model model = method.getInputModel();
	    						if(model != null){
	    							if(!generatedClasses.contains(model.getName())) {
		    		    				List<String> imports = new ArrayList<String>();
                                        imports.addAll(this.config.getDefaultModelImports());
		    		    				for(Parameter param : model.getFields()){
		    		    					for(String importDef : param.getAttributeDefinition(config.getDataTypeMapper()).getImportDefinitions()){
		    		    						if(!imports.contains(importDef)){
		    		    							imports.add(importDef);
		    		    						}
		    		    					}
		    		    				}
		    		    		    	StringTemplate template = templateGroup.getInstanceOf(MODEL_OBJECT_TEMPLATE);

		    		    		    	template.setAttribute("fields", model.getFields());
		    		    		    	template.setAttribute("imports", imports);
		    		    		    	template.setAttribute("className", model.getGenratedClassName());
		    		    		    	File aFile = new File(config.getModelClassLocation()+model.getGenratedClassName()+config.getClassFileExtension());
                                        writeFile(aFile, template.toString(), "Input model class");
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
            List<String> imports = new ArrayList<String>();
            imports.addAll(this.config.getDefaultServiceImports());
    		methods = resource.generateMethods(resource, config);
	    	StringTemplate template = templateGroup.getInstanceOf(API_OBJECT_TEMPLATE);
            String className = resource.generateClassName(config);
            List<Method> filteredMethods = new ArrayList<Method>();
            for(Method method:methods){
                if(!config.getCodeGenOverridingRules().isMethodIgnored(className, method.getName())){
                    filteredMethods.add(method);
                }
            }
	    	template.setAttribute("imports", imports);
	    	template.setAttribute("resource", className);
	    	template.setAttribute("methods", filteredMethods);
            template.setAttribute("extends", config.getCodeGenOverridingRules().getServiceExtendingClass(className));

	    	File aFile = new File(config.getResourceClassLocation()+ resource.generateClassName(config) +config.getClassFileExtension());
            writeFile(aFile, template.toString(), "API CLasses");
    	}
    }
    
    /**
     * Creates a wrapper model class that contains all model classes as list of objects.
     * This class is used for storing test data
     */
    private void generateWrapperClassForTestData(List<String> generatedClassNames, StringTemplateGroup templateGroup) {
    	Model model = new Model();
    	model.setName("TestData");
    	model.setDescription("Class used to store all the test data. This should not be used for any development");
    	List<Parameter> parameters = new ArrayList<Parameter>();
    	model.setFields(parameters);
    	for(String className : generatedClassNames){
    		Parameter aParam = new Parameter();
    		aParam.setName(config.getNameGenerator().convertToMethodNameFormat(className)+"List");
    		aParam.setParamType(config.getDataTypeMapper().getListReturnType(className));
    		parameters.add(aParam);
    	}

        //add missing class from models
        Parameter aParam = new Parameter();
        aParam.setName("StringValueList");
        aParam.setParamType(config.getDataTypeMapper().getListReturnType("StringValue"));
        parameters.add(aParam);
        
		List<String> imports = new ArrayList<String>();
        imports.addAll(this.config.getDefaultModelImports());
        imports.addAll(this.config.getDataTypeMapper().getListImports());
		for(Parameter param : model.getFields()){
			for(String importDef : param.getAttributeDefinition(config.getDataTypeMapper()).getImportDefinitions()){
				if(!imports.contains(importDef)){
					imports.add(importDef);
				}
			}
		}
    	StringTemplate template = templateGroup.getInstanceOf(MODEL_OBJECT_TEMPLATE);
    	template.setAttribute("fields", model.getFields());
    	template.setAttribute("imports", imports);
    	template.setAttribute("className", model.getGenratedClassName());
    	File aFile = new File(config.getModelClassLocation()+model.getGenratedClassName()+config.getClassFileExtension());
        writeFile(aFile, template.toString(), "Wrapper class for test data file");
    }

    private void writeFile(File aFile, String content, String classType){
    	try{
	    	FileWriter aWriter = new FileWriter(aFile);
	    	BufferedWriter bufWriter = new BufferedWriter(aWriter);
	    	bufWriter.write(content);
	    	bufWriter.close();
    	}catch(IOException ioe){
            throw new CodeGenerationException("Error generating " + classType + " : " + ioe.getMessage());
    	}
    }
}
