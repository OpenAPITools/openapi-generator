package com.wordnik.swagger.codegen.config.as3;

import com.wordnik.swagger.codegen.LibraryCodeGenerator;
import com.wordnik.swagger.codegen.config.LanguageConfiguration;
import com.wordnik.swagger.codegen.exception.CodeGenerationException;
import com.wordnik.swagger.codegen.resource.Model;
import com.wordnik.swagger.codegen.resource.ModelField;
import com.wordnik.swagger.codegen.resource.Resource;
import com.wordnik.swagger.codegen.util.FileUtil;
import org.antlr.stringtemplate.StringTemplate;
import org.antlr.stringtemplate.StringTemplateGroup;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * User: deepakmichael
 * Date: 17/08/11
 * Time: 5:02 PM
 */
public class As3LibCodeGen extends LibraryCodeGenerator{
    protected static final String MANIFEST_OBJECT_TEMPLATE = "ReferencesObject";
    private static final String DEFAULT_SERVICE_BASE_CLASS = "SwaggerApi";

    public static void main(String[] args) {
        if(args.length < 1){
            throw new CodeGenerationException("Invalid number of arguments passed: No command line argument was passed to the program for config json");
        }
        if(args.length == 1) {
            String configPath = args[0];
            As3LibCodeGen codeGenerator = new As3LibCodeGen(configPath);
            codeGenerator.generateCode();
        }
        if(args.length == 4) {
            String apiServerURL = args[0];
            if(!apiServerURL.endsWith("/")){
                apiServerURL = apiServerURL + "/";
            }
            String apiKey = args[1];
            String packageName = args[2];
            String libraryHome = args[3];
            if(libraryHome.endsWith("/")){
                libraryHome = libraryHome.substring(0, libraryHome.length()-1);
            }
            String modelPackageName = packageName+".model";
            String apiPackageName = packageName+".api";
            String classOutputDir = libraryHome + "/src/main/as3/" + packageName.replace(".","/");
            As3LibCodeGen codeGenerator = new As3LibCodeGen(apiServerURL, apiKey, modelPackageName,
                    apiPackageName, classOutputDir, libraryHome);
            codeGenerator.getConfig().setDefaultServiceBaseClass(DEFAULT_SERVICE_BASE_CLASS);
            codeGenerator.generateCode();
        }

    }

    public As3LibCodeGen(String apiServerURL, String apiKey, String modelPackageName, String apiPackageName,
                          String classOutputDir, String libraryHome){
        super(apiServerURL, apiKey, modelPackageName, apiPackageName, classOutputDir, libraryHome);
        this.setDataTypeMappingProvider(new As3DataTypeMappingProvider());
        this.setNameGenerator(new As3NamingPolicyProvider());
    }

    public As3LibCodeGen(String configPath){
        super(configPath);
        this.setDataTypeMappingProvider(new As3DataTypeMappingProvider());
        this.setNameGenerator(new As3NamingPolicyProvider());
    }

    @Override
    protected LanguageConfiguration initializeLangConfig(LanguageConfiguration as3Configuration) {
        as3Configuration.setClassFileExtension(".as");
        as3Configuration.setTemplateLocation("conf/as3/templates");
        as3Configuration.setStructureLocation("conf/as3/structure");
        as3Configuration.setExceptionPackageName("com.wordnik.swagger.exception");
        as3Configuration.setAnnotationPackageName("com.wordnik.swagger.annotations");

        //create ouput directories
        FileUtil.createOutputDirectories(as3Configuration.getModelClassLocation(), as3Configuration.getClassFileExtension());
        FileUtil.createOutputDirectories(as3Configuration.getResourceClassLocation(), as3Configuration.getClassFileExtension());
        //delete previously generated files
        FileUtil.clearFolder(as3Configuration.getModelClassLocation());
        FileUtil.clearFolder(as3Configuration.getResourceClassLocation());
        FileUtil.clearFolder(as3Configuration.getLibraryHome() + "/src/main/as3/com/wordnik/swagger/common");
        FileUtil.clearFolder(as3Configuration.getLibraryHome() + "/src/main/as3/com/wordnik/swagger/exception");
        FileUtil.clearFolder(as3Configuration.getLibraryHome() + "/src/main/as3/com/wordnik/swagger/event");
        FileUtil.copyDirectoryFromUrl(this.getClass().getClassLoader().getResource(as3Configuration.getStructureLocation()), new File(as3Configuration.getLibraryHome()));

        as3Configuration.setModelEnumRequired(false);
        as3Configuration.setOutputWrapperRequired(true);
        as3Configuration.setMethodOverloadingSupported(false);
        return as3Configuration;
    }

    protected void generateMiscClasses(List<Resource> resources, StringTemplateGroup aTemplateGroup) {
        generateReferencesObject(resources, aTemplateGroup);
    }

    private void generateReferencesObject(List<Resource> resources, StringTemplateGroup templateGroup) {
        StringTemplate template = templateGroup.getInstanceOf(MANIFEST_OBJECT_TEMPLATE);
        if(template == null){
            System.out.println("WrapperObject template not found to generate output wrappers");
            return;
        }
        Model referencesModel = new Model();
        List<ModelField> refFields = new ArrayList<ModelField>();
        ModelField refModelField;
        for(Resource resource: resources) {
            for(Model model : resource.getModels()){

                for(ModelField modelField : model.getFields()){
                    if (modelField.getFieldDefinition() != null) {
                        final String collectionItemType = modelField.getFieldDefinition().getCollectionItemType();
                        if(collectionItemType != null){
                            refModelField = new ModelField();
                            refModelField.setName(modelField.getName() + model.getName());
                            refModelField.setParamType(collectionItemType);
                            refFields.add(refModelField);
                        }
                    }
                }
            }

            refModelField = new ModelField();
            refModelField.setName( nameGenerator.applyMethodNamingPolicy( resource.generateClassName(nameGenerator) ) );
            refModelField.setParamType( resource.generateClassName(nameGenerator) );
            refFields.add(refModelField);
        }
        List<String> imports = new ArrayList<String>();
        imports.addAll(this.config.getDefaultModelImports());
        referencesModel.setFields(refFields);
        referencesModel.setName("LibraryReferences");
        template = templateGroup.getInstanceOf(MANIFEST_OBJECT_TEMPLATE);
        template.setAttribute("model", referencesModel);
        template.setAttribute("fields", referencesModel.getFields());
        template.setAttribute("imports", imports);
        template.setAttribute("annotationPackageName", languageConfig.getAnnotationPackageName());
        template.setAttribute("extends", config.getDefaultModelBaseClass());
        template.setAttribute("className", referencesModel.getGenratedClassName());
        template.setAttribute(PACKAGE_NAME, config.getModelPackageName());
        File aFile = new File(languageConfig.getModelClassLocation()+referencesModel.getGenratedClassName()+languageConfig.getClassFileExtension());
        writeFile(aFile, template.toString(), "Model class");
    }
}
