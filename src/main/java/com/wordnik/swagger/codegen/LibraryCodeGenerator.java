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
import com.wordnik.swagger.codegen.config.ApiConfiguration;
import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;
import com.wordnik.swagger.codegen.config.java.JavaDataTypeMappingProvider;
import com.wordnik.swagger.codegen.exception.CodeGenerationException;
import com.wordnik.swagger.codegen.resource.*;
import com.wordnik.swagger.codegen.util.FileUtil;
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

    private static final String PACKAGE_NAME = "packageName";
    private ApiConfiguration config = null;
    private LanguageConfiguration languageConfig = null;

    private SwaggerResourceDocReader apiMarshaller;
    protected DataTypeMappingProvider dataTypeMappingProvider;
    protected RulesProvider codeGenRulesProvider;
    protected NamingPolicyProvider nameGenerator;

    public LibraryCodeGenerator(String configPath){

        final ObjectMapper mapper = new ObjectMapper();
        mapper.configure(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES, false);

        final File configFile = new File(configPath);
        this.setApiConfig(readApiConfiguration(configPath, mapper, configFile));
        this.setCodeGenRulesProvider(readRulesProviderConfig(configPath, mapper, configFile));
        this.setLanguageConfig( initializeLangConfig(readLanguageConfiguration(configPath, mapper, configFile)) );

        this.setDataTypeMappingProvider(new JavaDataTypeMappingProvider());
        this.setNameGenerator(new CamelCaseNamingPolicyProvider());
    }

    public LibraryCodeGenerator(String apiServerURL, String apiKey, String modelPackageName, String apiPackageName,
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

        this.setDataTypeMappingProvider(new JavaDataTypeMappingProvider());
        this.setNameGenerator(new CamelCaseNamingPolicyProvider());
    }

    /**
     * Generate classes needed for the model and API invocation
     */
    public void generateCode()	{
        apiMarshaller = new SwaggerResourceDocReader(this.config, this.getDataTypeMappingProvider(), this.getNameGenerator());
    	//read resources and get their documentation
        List<Resource> resources = apiMarshaller.readResourceDocumentation();
        StringTemplateGroup aTemplateGroup = new StringTemplateGroup("templates", languageConfig.getTemplateLocation());
        if(resources.size() > 0) {
        	generateVersionHelper(resources.get(0).getApiVersion(), aTemplateGroup);
        }
        generateModelClasses(resources, aTemplateGroup);
        generateModelClassesForInput(resources, aTemplateGroup);
        if(languageConfig.isGenerateHelperEnums()){
            generateEnumForAllowedValues(resources, aTemplateGroup);
        }

        if(languageConfig.isGenerateOutputWrappers()) {
            generateOutputWrappers(resources, aTemplateGroup);
        }
        generateAPIClasses(resources, aTemplateGroup);
    }

    /**
     * Generates version file based on the version number received from the doc calls. This version file is used
     * while making the API calls to make sure Client and back end are compatible.
     * @param version
     */
    private void generateVersionHelper(String version, StringTemplateGroup templateGroup) {
    	StringTemplate template = templateGroup.getInstanceOf(VERSION_OBJECT_TEMPLATE);
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

        //remove old generated files
        FileUtil.clearFolder(languageConfig.getModelClassLocation());

    	for(Resource resource: resources) {
    		for(Model model : resource.getModels()){
    			if(!generatedClassNames.contains(model.getName()) && !this.getCodeGenRulesProvider().isModelIgnored(model.getName())){
    				List<String> imports = new ArrayList<String>();
    				imports.addAll(this.config.getDefaultModelImports());
    				for(ModelField param : model.getFields()){
    					for(String importDef : param.getFieldDefinition(this.getDataTypeMappingProvider(), config, nameGenerator).getImportDefinitions()){
    						if(!imports.contains(importDef)){
    							imports.add(importDef);
    						}
    					}
    				}
    		    	StringTemplate template = templateGroup.getInstanceOf(MODEL_OBJECT_TEMPLATE);
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

    	generateWrapperClassForTestData(generatedClassNames, templateGroup);
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
    						ResourceMethod method = operation.generateMethod(endpoint, resource, dataTypeMappingProvider, nameGenerator);
    						if(method.getInputModel() != null) {
	    						Model model = method.getInputModel();
	    						if(model != null){
	    							if(!generatedClasses.contains(model.getName())) {
		    		    				List<String> imports = new ArrayList<String>();
                                        imports.addAll(this.config.getDefaultModelImports());
		    		    				for(ModelField param : model.getFields()){
		    		    					for(String importDef : param.getFieldDefinition(this.getDataTypeMappingProvider(), config, nameGenerator).getImportDefinitions()){
		    		    						if(!imports.contains(importDef)){
		    		    							imports.add(importDef);
		    		    						}
		    		    					}
		    		    				}
		    		    		    	StringTemplate template = templateGroup.getInstanceOf(MODEL_OBJECT_TEMPLATE);

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
                                    if(operationParam.getAllowableValues() != null && operationParam.getAllowableValues().size() > 1) {
                                        if(!generatedEnums.contains(operationParam.getName())){
                                            //generate enum
                                            template = templateGroup.getInstanceOf(ENUM_OBJECT_TEMPLATE);
                                            List<String> imports = new ArrayList<String>();
                                            imports.addAll(this.config.getDefaultModelImports());
                                            enumName = this.getNameGenerator().getEnumName(operationParam.getName());
                                            template.setAttribute("className", enumName);
                                            template.setAttribute("description", operationParam.getDescription());
                                            template.setAttribute("enumValueType", this.getDataTypeMappingProvider().getClassType(operationParam.getDataType(), true));
                                            for (String allowableValue : operationParam.getAllowableValues()) {
                                                if(operationParam.getDataType().equalsIgnoreCase("string")){
                                                    valuePrefix = valueSuffix = "\"";
                                                }
                                                else{
                                                    valuePrefix = valueSuffix = "";
                                                };
                                                template.setAttribute("values.{name,value}",
                                                        this.getNameGenerator().applyClassNamingPolicy(allowableValue.replaceAll("-","_")),
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

    private void generateOutputWrappers(List<Resource> resources, StringTemplateGroup templateGroup) {
        List<String> generatedClasses = new ArrayList<String>();
        StringTemplate template = templateGroup.getInstanceOf(WRAPPER_OBJECT_TEMPLATE);
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
                            if(method.getOutputWrapperModel() != null) {
                                Model model = method.getOutputWrapperModel();
                                method.setReturnClassName(model.getName());
                                if(model != null){
                                    if(!generatedClasses.contains(model.getName())) {
                                        List<String> imports = new ArrayList<String>();
                                        imports.addAll(this.config.getDefaultModelImports());
                                        for(ModelField param : model.getFields()){
                                            for(String importDef : param.getFieldDefinition(this.getDataTypeMappingProvider(), config, nameGenerator).getImportDefinitions()){
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
                                        writeFile(aFile, template.toString(), "Output wrapper model class");
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

        //delete previously generated files
        FileUtil.clearFolder(languageConfig.getResourceClassLocation());

    	for(Resource resource : resources) {
            try{
                List<ResourceMethod> methods = new ArrayList<ResourceMethod>();
                List<String> imports = new ArrayList<String>();
                imports.addAll(this.config.getDefaultServiceImports());
                methods = resource.generateMethods(resource, dataTypeMappingProvider, nameGenerator);
                StringTemplate template = templateGroup.getInstanceOf(API_OBJECT_TEMPLATE);
                String className = resource.generateClassName(nameGenerator);
                List<ResourceMethod> filteredMethods = new ArrayList<ResourceMethod>();
                for(ResourceMethod method:methods){
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
                writeFile(aFile, template.toString(), "API CLasses");
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
			for(String importDef : param.getFieldDefinition(this.getDataTypeMappingProvider(), config, nameGenerator).getImportDefinitions()){
				if(!imports.contains(importDef)){
					imports.add(importDef);
				}
			}
		}
    	StringTemplate template = templateGroup.getInstanceOf(MODEL_OBJECT_TEMPLATE);
    	template.setAttribute("fields", model.getFields());
    	template.setAttribute("imports", imports);
        template.setAttribute("annotationPackageName", languageConfig.getAnnotationPackageName());
        template.setAttribute("extends", config.getDefaultModelBaseClass());
        template.setAttribute(PACKAGE_NAME, config.getModelPackageName());
    	template.setAttribute("className", model.getGenratedClassName());
    	File aFile = new File(languageConfig.getModelClassLocation()+model.getGenratedClassName()+languageConfig.getClassFileExtension());
        writeFile(aFile, template.toString(), "Wrapper class for test data file");
    }

    private void writeFile(File aFile, String content, String classType){
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
