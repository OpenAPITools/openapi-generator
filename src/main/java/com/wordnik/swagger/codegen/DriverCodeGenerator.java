package com.wordnik.swagger.codegen;

import com.wordnik.swagger.codegen.api.SwaggerResourceDocReader;
import com.wordnik.swagger.codegen.config.*;
import com.wordnik.swagger.codegen.config.ApiConfiguration;
import com.wordnik.swagger.codegen.resource.*;
import com.wordnik.swagger.exception.CodeGenerationException;
import org.antlr.stringtemplate.StringTemplate;
import org.antlr.stringtemplate.StringTemplateGroup;

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
public class DriverCodeGenerator {
	
	private static String VERSION_OBJECT_TEMPLATE = "VersionChecker";
	private static String MODEL_OBJECT_TEMPLATE = "ModelObject";
    private static String API_OBJECT_TEMPLATE = "ResourceObject";
    private static final String ENUM_OBJECT_TEMPLATE = "EnumObject";

    private static final String PACKAGE_NAME = "packageName";
    private ApiConfiguration config = null;
    private LanguageConfiguration languageConfig = null;

    private SwaggerResourceDocReader apiMarshaller;
    protected DataTypeMappingProvider dataTypeMappingProvider;
    protected RulesProvider codeGenRulesProvider;
    protected NamingPolicyProvider nameGenerator;

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
        generateEnumForAllowedValues(resources, aTemplateGroup);
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

    	for(Resource resource: resources) {
    		for(Model model : resource.getModels()){
    			if(!generatedClassNames.contains(model.getName()) && !this.getCodeGenRulesProvider().isModelIgnored(model.getName())){
    				List<String> imports = new ArrayList<String>();
    				imports.addAll(this.config.getDefaultModelImports());
    				for(ModelField param : model.getFields()){
    					for(String importDef : param.getFieldDefinition(this.getDataTypeMappingProvider()).getImportDefinitions()){
    						if(!imports.contains(importDef)){
    							imports.add(importDef);
    						}
    					}
    				}
    		    	StringTemplate template = templateGroup.getInstanceOf(MODEL_OBJECT_TEMPLATE);
    		    	template.setAttribute("fields", model.getFields());
    		    	template.setAttribute("imports", imports);
                    template.setAttribute("annotationPackageName", languageConfig.getAnnotationPackageName());
                    template.setAttribute("extends", config.getModelBaseClass());
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
		    		    					for(String importDef : param.getFieldDefinition(this.getDataTypeMappingProvider()).getImportDefinitions()){
		    		    						if(!imports.contains(importDef)){
		    		    							imports.add(importDef);
		    		    						}
		    		    					}
		    		    				}
		    		    		    	StringTemplate template = templateGroup.getInstanceOf(MODEL_OBJECT_TEMPLATE);

		    		    		    	template.setAttribute("fields", model.getFields());
		    		    		    	template.setAttribute("imports", imports);
                                        template.setAttribute("extends", config.getModelBaseClass());
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
                                            template.setAttribute("enumValueType", this.getDataTypeMappingProvider().getObjectType(operationParam.getDataType(), true));
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

    /**
     * Generates one API class for each resource and each end point in the resource is translated as method.
     * @param resources
     * @param templateGroup
     */
    private void generateAPIClasses(List<Resource> resources, StringTemplateGroup templateGroup) {

    	for(Resource resource : resources) {
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

        //add missing class from models
        ModelField aParam = new ModelField();
        aParam.setName("StringValueList");
        aParam.setParamType(this.getDataTypeMappingProvider().getListReturnTypeSignature("StringValue"));
        modelFields.add(aParam);
        
		List<String> imports = new ArrayList<String>();
        imports.addAll(this.config.getDefaultModelImports());
        imports.addAll(this.getDataTypeMappingProvider().getListImportPackages());
		for(ModelField param : model.getFields()){
			for(String importDef : param.getFieldDefinition(this.getDataTypeMappingProvider()).getImportDefinitions()){
				if(!imports.contains(importDef)){
					imports.add(importDef);
				}
			}
		}
    	StringTemplate template = templateGroup.getInstanceOf(MODEL_OBJECT_TEMPLATE);
    	template.setAttribute("fields", model.getFields());
    	template.setAttribute("imports", imports);
        template.setAttribute("annotationPackageName", languageConfig.getAnnotationPackageName());
        template.setAttribute("extends", config.getModelBaseClass());
        template.setAttribute(PACKAGE_NAME, config.getModelPackageName());
    	template.setAttribute("className", model.getGenratedClassName());
    	File aFile = new File(languageConfig.getModelClassLocation()+model.getGenratedClassName()+languageConfig.getClassFileExtension());
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
}
