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
package com.wordnik.swagger.codegen;

import com.wordnik.swagger.codegen.api.SwaggerResourceDocReader;
import com.wordnik.swagger.codegen.config.*;
import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;
import com.wordnik.swagger.codegen.exception.CodeGenerationException;
import com.wordnik.swagger.codegen.resource.*;

import org.antlr.stringtemplate.StringTemplate;
import org.antlr.stringtemplate.StringTemplateGroup;
import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * User: ramesh
 * Date: 3/30/11
 * Time: 6:59 PM
 */
public class LibraryCodeGenerator {
	private static String VERSION_OBJECT_TEMPLATE = "VersionChecker";
	private static String MODEL_OBJECT_TEMPLATE = "ModelObject";
    private static String API_OBJECT_TEMPLATE = "ResourceObject";
    private static final String ENUM_OBJECT_TEMPLATE = "EnumObject";
    private static final String WRAPPER_OBJECT_TEMPLATE = "WrapperObject";

    protected static final String PACKAGE_NAME = "packageName";
    protected ApiConfiguration config = null;
    protected LanguageConfiguration languageConfig = null;
    protected ReservedWordMapper reservedWordMapper = new DefaultReservedWordMapper();

    private SwaggerResourceDocReader apiMarshaller;
    protected DataTypeMappingProvider dataTypeMappingProvider;
    protected RulesProvider codeGenRulesProvider;
    protected NamingPolicyProvider nameGenerator;

    Logger logger = LoggerFactory.getLogger(LibraryCodeGenerator.class);

    public LibraryCodeGenerator(){}

    public LibraryCodeGenerator(String configPath){
    	initializeWithConfigPath(configPath);
    }
    
    protected void initializeWithConfigPath(String configPath){
        final ObjectMapper mapper = new ObjectMapper();
        mapper.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        final File configFile = new File(configPath);
        this.setApiConfig(readApiConfiguration(configPath, mapper, configFile));
        this.setCodeGenRulesProvider(readRulesProviderConfig(configPath, mapper, configFile));
        this.setLanguageConfig( initializeLangConfig(readLanguageConfiguration(configPath, mapper, configFile)) );
    }

    public LibraryCodeGenerator(String apiServerURL, String apiKey, String modelPackageName, String apiPackageName,
                                String classOutputDir, String libraryHome){
        initialize(apiServerURL, apiKey, modelPackageName, apiPackageName, classOutputDir, libraryHome);
    }
    
    protected void initialize(String apiServerURL, String apiKey, String modelPackageName, String apiPackageName,
                                String classOutputDir, String libraryHome){
        final ObjectMapper mapper = new ObjectMapper();
        mapper.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        ApiConfiguration aApiConfiguration = new ApiConfiguration();
        aApiConfiguration.setApiKey(apiKey);
        aApiConfiguration.setApiPackageName(apiPackageName);
        aApiConfiguration.setModelPackageName(modelPackageName);
        aApiConfiguration.setApiUrl(apiServerURL);
        this.setApiConfig(aApiConfiguration);
        CodeGenRulesProvider codeGenRules = new CodeGenRulesProvider();
        this.setCodeGenRulesProvider(codeGenRules);
        LanguageConfiguration aLanguageConfiguration = new LanguageConfiguration();
        aLanguageConfiguration.setOutputDirectory(classOutputDir);
        aLanguageConfiguration.setLibraryHome(libraryHome);
        initializeLangConfig(aLanguageConfiguration);
        this.setLanguageConfig(aLanguageConfiguration);
    }
    
    /**
     * Generate classes needed for the model and API invocation
     */
    public void generateCode()	{
        apiMarshaller = new SwaggerResourceDocReader(this.config, this.getDataTypeMappingProvider(), this.getNameGenerator());
    	//read resources and get their documentation
        List<Resource> resources = apiMarshaller.readResourceDocumentation();
        preprocess(resources);
        StringTemplateGroup aTemplateGroup = new StringTemplateGroup(languageConfig.getTemplateLocation());
        if(resources.size() > 0) {
        	generateVersionHelper(resources.get(0).getApiVersion(), aTemplateGroup);
        }
        generateModelClasses(resources, aTemplateGroup);
        generateModelClassesForInput(resources, aTemplateGroup);
        if(languageConfig.isModelEnumRequired()){
            generateEnumForAllowedValues(resources, aTemplateGroup);
        }

        if(languageConfig.isOutputWrapperRequired()) {
            generateOutputWrappers(resources, aTemplateGroup);
        }
        generateAPIClasses(resources, aTemplateGroup);
        generateMiscClasses(resources, aTemplateGroup);
    }

    /**
     * prepares the model for template generation
     * @param resources
     */
    protected void preprocess(List<Resource> resources) {
    	for(Resource resource: resources) {
    		for(Model model : resource.getModels()){
    			//	apply keyword mapping
    			for(ModelField modelField : model.getFields()){
    				modelField.setName(reservedWordMapper.translate(modelField.getName()));
    			}
    		}
    	}
	}

	/**
     * Generates version file based on the version number received from the doc calls. This version file is used
     * while making the API calls to make sure Client and back end are compatible.
     * @param version
     */
    private void generateVersionHelper(String version, StringTemplateGroup templateGroup) {
    	StringTemplate template = templateGroup.getInstanceOf(languageConfig.getTemplateLocation()+"/"+VERSION_OBJECT_TEMPLATE);
    	template.setAttribute("apiVersion", version);
    	template.setAttribute(PACKAGE_NAME, config.getApiPackageName());
    	File aFile = new File(languageConfig.getResourceClassLocation() + this.getNameGenerator().getVersionCheckerClassName()
                + languageConfig.getClassFileExtension());
        writeFile(aFile, template.toString(), "Version checker class");
    }

    /**
     * Generates model classes. If the class is already generated then ignores the same.
     */
    private void generateModelClasses(List<Resource> resources, StringTemplateGroup templateGroup) {
    	List<String> generatedClassNames = new ArrayList();

    	for(Resource resource: resources) {
    		for(Model model : resource.getModels()){
    			if(!generatedClassNames.contains(model.getName()) && !this.getCodeGenRulesProvider().isModelIgnored(model.getName())){
    				List<String> imports = new ArrayList<String>();
    				imports.addAll(this.config.getDefaultModelImports());
                    if(null == model.getFields() || model.getFields().size() == 0){
                        logger.warn("Model " + model.getName() + " doesn't have any properties");
                    } else {
                        for(ModelField param : model.getFields()){
                            param.setName(reservedWordMapper.translate(param.getName()));
                            for(String importDef : param.getFieldDefinition(this.getDataTypeMappingProvider(), config, nameGenerator, reservedWordMapper).getImportDefinitions()){
                                if(!imports.contains(importDef)){
                                    imports.add(importDef);
                                }
                            }
                        }
                        StringTemplate template = templateGroup.getInstanceOf(languageConfig.getTemplateLocation()+"/"+MODEL_OBJECT_TEMPLATE);
                        template.setAttribute("model", model);
                        template.setAttribute("fields", model.getFields());
                        template.setAttribute("imports", imports);
                        template.setAttribute("annotationPackageName", languageConfig.getAnnotationPackageName());
                        template.setAttribute("extends", config.getDefaultModelBaseClass());
                        template.setAttribute("className", model.getGenratedClassName());
                        template.setAttribute(PACKAGE_NAME, config.getModelPackageName());
                        File aFile = new File(languageConfig.getModelClassLocation()+model.getGenratedClassName()+languageConfig.getClassFileExtension());
                        writeFile(aFile, template.toString(), "Model class");
                        generatedClassNames.add(model.getName());
                    }
    			}
    		}
    	}

    	generateWrapperClassForTestData(generatedClassNames, templateGroup);
    }

    /**
     * Generates assembler classes if the API returns more than one object.
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
    						ResourceMethod method = operation.generateMethod(endpoint, resource, dataTypeMappingProvider, nameGenerator);
    						if(method.getInputModel() != null) {
	    						Model model = method.getInputModel();
	    						if(model != null){
	    							if(!generatedClasses.contains(model.getName())) {
		    		    				List<String> imports = new ArrayList<String>();
                                        imports.addAll(this.config.getDefaultModelImports());
		    		    				for(ModelField param : model.getFields()){
		    		    					param.setName(reservedWordMapper.translate(param.getName()));
		    		    					for(String importDef : param.getFieldDefinition(this.getDataTypeMappingProvider(), config, nameGenerator, reservedWordMapper).getImportDefinitions()){
		    		    						if(!imports.contains(importDef)){
		    		    							imports.add(importDef);
		    		    						}
		    		    					}
		    		    				}
		    		    		    	StringTemplate template = templateGroup.getInstanceOf(languageConfig.getTemplateLocation()+"/"+MODEL_OBJECT_TEMPLATE);

		    		    		    	template.setAttribute("fields", model.getFields());
		    		    		    	template.setAttribute("imports", imports);
                                        template.setAttribute("extends", config.getDefaultModelBaseClass());
                                        template.setAttribute("annotationPackageName", languageConfig.getAnnotationPackageName());
		    		    		    	template.setAttribute("className", model.getGenratedClassName());
                                        template.setAttribute(PACKAGE_NAME, config.getModelPackageName());
		    		    		    	File aFile = new File(languageConfig.getModelClassLocation()+model.getGenratedClassName()+languageConfig.getClassFileExtension());
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
     * Generates an Enum class for method params that have an allowed values list.
     * @param resources
     * @param templateGroup
     */
    private void generateEnumForAllowedValues(List<Resource> resources, StringTemplateGroup templateGroup) {
        List<String> generatedEnums = new ArrayList<String>();
        StringTemplate template;
        String valuePrefix, valueSuffix = "";
        String enumName;
        for(Resource resource: resources) {
            if(resource.getEndPoints() != null) {
                for(Endpoint endpoint : resource.getEndPoints()){
                    if(endpoint.getOperations() != null) {
                        for(EndpointOperation operation : endpoint.getOperations()){
                            //ResourceMethod method = operation.generateMethod(endpoint, resource, config);
                            if(operation.getParameters() != null){
                                for(ModelField operationParam : operation.getParameters()){
                                    //skipping the case where there is just one item - TODO process case of allowableValue like '0 to 1000'
                                    if(operationParam.getAllowableValues() != null && operationParam.getAllowableValues().getClass().isAssignableFrom(AllowableListValues.class)) {
                                        if(!generatedEnums.contains(operationParam.getName())){
                                            //generate enum
                                            template = templateGroup.getInstanceOf(languageConfig.getTemplateLocation()+"/"+ENUM_OBJECT_TEMPLATE);
                                            List<String> imports = new ArrayList<String>();
                                            imports.addAll(this.config.getDefaultModelImports());
                                            enumName = this.getNameGenerator().getEnumName(operationParam.getName());
                                            template.setAttribute("className", enumName);
                                            template.setAttribute("description", operationParam.getDescription());
                                            template.setAttribute("enumValueType", this.getDataTypeMappingProvider().getClassType(operationParam.getDataType(), true));
                                            for (String allowableValue : ((AllowableListValues)operationParam.getAllowableValues()).getValues()) {
                                                if(operationParam.getDataType().equalsIgnoreCase("string")){
                                                    valuePrefix = valueSuffix = "\"";
                                                }
                                                else{
                                                    valuePrefix = valueSuffix = "";
                                                };
                                                String namePrefix = "";
                                                if((isNameStartsWithInteger(allowableValue) && !canEnumNameStartsWithNumber()) || isEnumNumber(allowableValue) ){
                                                    namePrefix = "ENUM_";
                                                }
                                                template.setAttribute("values.{name,value}",
                                                        namePrefix+this.getNameGenerator().applyClassNamingPolicy(allowableValue.replaceAll("-","_")),
                                                        this.getNameGenerator().applyMethodNamingPolicy(valuePrefix.concat(allowableValue).concat(valueSuffix)));
                                            }
                                            template.setAttribute(PACKAGE_NAME, config.getModelPackageName());
		    		    		    	    File aFile = new File(languageConfig.getModelClassLocation() + enumName + languageConfig.getClassFileExtension());
                                            writeFile(aFile, template.toString(), "Enum class");
                                            generatedEnums.add(operationParam.getName());
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

        }
    }

    private boolean isNameStartsWithInteger(String name) {
        for(int i=0; i <= 9 ; i++){
            if(name.startsWith(i+"")){
                return true;
            }
        }
        return false;
    }

    private boolean isEnumNumber(String name) {
        try{
            new Integer(name);
            return true;
        }catch(Throwable t){
            return false;
        }
    }
    
    private void generateOutputWrappers(List<Resource> resources, StringTemplateGroup templateGroup) {
        List<String> generatedClasses = new ArrayList<String>();
        StringTemplate template = templateGroup.getInstanceOf(languageConfig.getTemplateLocation()+"/"+WRAPPER_OBJECT_TEMPLATE);
        if(template == null){
            System.out.println("WrapperObject template not found to generate output wrappers");
            return;
        }

        for(Resource resource: resources) {
            if(resource.getEndPoints() != null) {
                for(Endpoint endpoint : resource.getEndPoints()){
                    if(endpoint.getOperations() != null) {
                        for(EndpointOperation operation : endpoint.getOperations()){
                            ResourceMethod method = operation.generateMethod(endpoint, resource, dataTypeMappingProvider, nameGenerator);
                            if(codeGenRulesProvider.isModelIgnored( nameGenerator.applyMethodNamingPolicy( method.getReturnClassName() ))){
                                continue;
                            }
                            if(method.getListWrapperModel() != null) {
                                Model model = method.getListWrapperModel();
                                method.setReturnClassName(model.getName());
                                if(model != null){
                                    if(!generatedClasses.contains(model.getName())) {
                                        List<String> imports = new ArrayList<String>();
                                        imports.addAll(this.config.getDefaultModelImports());
                                        for(ModelField param : model.getFields()){
                                            param.setName(reservedWordMapper.translate(param.getName()));
                                            for(String importDef : param.getFieldDefinition(this.getDataTypeMappingProvider(), config, nameGenerator, reservedWordMapper).getImportDefinitions()){
                                                if(!imports.contains(importDef)){
                                                    imports.add(importDef);
                                                }
                                            }
                                        }
                                        template = templateGroup.getInstanceOf(WRAPPER_OBJECT_TEMPLATE);

                                        template.setAttribute("fields", model.getFields());
                                        template.setAttribute("imports", imports);
                                        template.setAttribute("extends", config.getDefaultModelBaseClass());
                                        template.setAttribute("annotationPackageName", languageConfig.getAnnotationPackageName());
                                        template.setAttribute("className", model.getGenratedClassName());
                                        template.setAttribute(PACKAGE_NAME, config.getModelPackageName());
                                        File aFile = new File(languageConfig.getModelClassLocation()+model.getGenratedClassName()+languageConfig.getClassFileExtension());
                                        writeFile(aFile, template.toString(), "List wrapper model class");
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
            try{
                List<ResourceMethod> methods = new ArrayList<ResourceMethod>();
                List<String> imports = new ArrayList<String>();
                imports.addAll(this.config.getDefaultServiceImports());
                methods = resource.generateMethods(resource, dataTypeMappingProvider, nameGenerator, languageConfig);
                StringTemplate template = templateGroup.getInstanceOf(languageConfig.getTemplateLocation()+"/"+API_OBJECT_TEMPLATE);
                String className = resource.generateClassName(nameGenerator);

                if(className != null){
	                List<ResourceMethod> filteredMethods = new ArrayList<ResourceMethod>();
	                for(ResourceMethod method:methods){
                        if(method.getArguments() != null){
                            for(MethodArgument methodArg: method.getArguments()){
                                methodArg.setName(reservedWordMapper.translate(methodArg.getName()));
                            }
                        }
	                    if(!this.getCodeGenRulesProvider().isMethodIgnored(className, method.getName())){
	                        filteredMethods.add(method);
	                    }
	                }
	                template.setAttribute("imports", imports);
	                template.setAttribute(PACKAGE_NAME, config.getApiPackageName());
	                template.setAttribute("annotationPackageName", languageConfig.getAnnotationPackageName());
	                template.setAttribute("modelPackageName", config.getModelPackageName());
	                template.setAttribute("exceptionPackageName", languageConfig.getExceptionPackageName());
	                template.setAttribute("resource", className);
	                template.setAttribute("methods", filteredMethods);
	                template.setAttribute("extends", config.getServiceBaseClass(className));
	
	                File aFile = new File(languageConfig.getResourceClassLocation()+ resource.generateClassName(nameGenerator) +languageConfig.getClassFileExtension());
	                writeFile(aFile, template.toString(), "API Classes");
                }
            }catch(RuntimeException t){
                System.out.println("Failed generating api class for the resource : " + resource.getResourcePath());
                throw t;
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
    	model.setDescription("Class used to store all the test data. This should not be used for any development");
    	List<ModelField> modelFields = new ArrayList<ModelField>();
    	model.setFields(modelFields);
    	for(String className : generatedClassNames){
    		ModelField aParam = new ModelField();
    		aParam.setName(this.getNameGenerator().applyMethodNamingPolicy(className)+"List");
    		aParam.setParamType(this.getDataTypeMappingProvider().getListReturnTypeSignature(className));
    		modelFields.add(aParam);
    	}

		List<String> imports = new ArrayList<String>();
        imports.addAll(this.config.getDefaultModelImports());
        imports.addAll(this.getDataTypeMappingProvider().getListIncludes());
		for(ModelField param : model.getFields()){
			for(String importDef : param.getFieldDefinition(this.getDataTypeMappingProvider(), config, nameGenerator, reservedWordMapper).getImportDefinitions()){
				if(!imports.contains(importDef)){
					imports.add(importDef);
				}
			}
		}
    	StringTemplate template = templateGroup.getInstanceOf(languageConfig.getTemplateLocation()+"/"+MODEL_OBJECT_TEMPLATE);
    	template.setAttribute("fields", model.getFields());
    	template.setAttribute("imports", imports);
        template.setAttribute("annotationPackageName", languageConfig.getAnnotationPackageName());
        template.setAttribute("extends", config.getDefaultModelBaseClass());
        template.setAttribute(PACKAGE_NAME, config.getModelPackageName());
    	template.setAttribute("className", model.getGenratedClassName());
    	File aFile = new File(languageConfig.getModelClassLocation()+model.getGenratedClassName()+languageConfig.getClassFileExtension());
        writeFile(aFile, template.toString(), "Wrapper class for test data file");
    }

    /**
     * Override this method in the derived classes to generate language specific classes
     * @param resources
     * @param aTemplateGroup
     */
    protected void generateMiscClasses(List<Resource> resources, StringTemplateGroup aTemplateGroup) {
        //nothing here in the base class
    }

    protected void writeFile(File aFile, String content, String classType){
    	try{
            System.out.println("Writing to the file " + aFile.getAbsolutePath());
	    	FileWriter aWriter = new FileWriter(aFile);
	    	BufferedWriter bufWriter = new BufferedWriter(aWriter);
	    	bufWriter.write(content);
	    	bufWriter.close();
    	}catch(IOException ioe){
            throw new CodeGenerationException("Error generating " + classType + " : " + ioe.getMessage());
    	}
    }
    
    public ApiConfiguration getConfig() {
        return config;
    }

    public void setApiConfig(ApiConfiguration config) {
        this.config = config;
    }

    public LanguageConfiguration getLanguageConfig() {
        return languageConfig;
    }

    public void setLanguageConfig(LanguageConfiguration config) {
        this.languageConfig = config;
    }

    public void setDataTypeMappingProvider(DataTypeMappingProvider dataTypeMappingProvider) {
        this.dataTypeMappingProvider = dataTypeMappingProvider;
    }

    public void setCodeGenRulesProvider(RulesProvider codeGenRulesProvider) {
        this.codeGenRulesProvider = codeGenRulesProvider;
    }

    public void setNameGenerator(NamingPolicyProvider nameGenerator) {
        this.nameGenerator = nameGenerator;
    }

    public DataTypeMappingProvider getDataTypeMappingProvider() {
        return dataTypeMappingProvider;
    }

    public RulesProvider getCodeGenRulesProvider() {
        return codeGenRulesProvider;
    }

    public NamingPolicyProvider getNameGenerator() {
        return nameGenerator;
    }

    /**
     * In java enum names can't start with number so if the enums are numbers then we prefix them with ENUM_
     * @return
     */
    protected boolean canEnumNameStartsWithNumber() {
        return true;
    }

    protected CodeGenRulesProvider readRulesProviderConfig(String rulesProviderLocation, ObjectMapper mapper, File configFile) {
        CodeGenRulesProvider codeGenRules = null;
        try {
            codeGenRules = mapper.readValue(configFile, CodeGenRulesProvider.class);
        } catch (IOException e) {
            throw new CodeGenerationException("Java codegen rules configuration could not be read from the location : " + rulesProviderLocation);
        }

        return codeGenRules;
    }

    protected ApiConfiguration readApiConfiguration(String apiConfigLocation, ObjectMapper mapper, File configFile) {
        ApiConfiguration configuration = null;
        try {
            configuration = mapper.readValue(configFile, ApiConfiguration.class);
        } catch (IOException e) {
            throw new CodeGenerationException("Api configuration could not be read from the location : " + apiConfigLocation);
        }

        return configuration;
    }

    protected LanguageConfiguration readLanguageConfiguration(String langConfigLocation, ObjectMapper mapper, File configFile) {
        LanguageConfiguration langConfig = null;
        try {
            langConfig = mapper.readValue(configFile, LanguageConfiguration.class);
        } catch (IOException e) {
            throw new CodeGenerationException("Language configuration value could not be read from the location : " + langConfigLocation);
        }

        return langConfig;
    }

    protected LanguageConfiguration initializeLangConfig(LanguageConfiguration configuration) {
        return configuration;
    }

}
