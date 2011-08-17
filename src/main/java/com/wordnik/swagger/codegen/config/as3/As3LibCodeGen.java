package com.wordnik.swagger.codegen.config.as3;

import com.wordnik.swagger.codegen.LibraryCodeGenerator;
import com.wordnik.swagger.codegen.config.LanguageConfiguration;
import com.wordnik.swagger.codegen.config.common.CamelCaseNamingPolicyProvider;
import com.wordnik.swagger.codegen.exception.CodeGenerationException;
import com.wordnik.swagger.codegen.util.FileUtil;

import java.io.File;

/**
 * User: deepakmichael
 * Date: 17/08/11
 * Time: 5:02 PM
 */
public class As3LibCodeGen extends LibraryCodeGenerator{
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
/*
        FileUtil.clearFolder(as3Configuration.getLibraryHome() + "/src/main/java/com/wordnik/swagger/runtime");
        FileUtil.createOutputDirectories(as3Configuration.getLibraryHome() + "/src/main/java/com/wordnik/swagger/runtime", "as");
*/
        FileUtil.clearFolder(as3Configuration.getLibraryHome() + "/src/main/as3/com/wordnik/swagger/common");
        FileUtil.clearFolder(as3Configuration.getLibraryHome() + "/src/main/as3/com/wordnik/swagger/exception");
        FileUtil.clearFolder(as3Configuration.getLibraryHome() + "/src/main/as3/com/wordnik/swagger/event");
        FileUtil.copyDirectory(new File(as3Configuration.getStructureLocation()), new File(as3Configuration.getLibraryHome()));

        as3Configuration.setGenerateHelperEnums(false);
        as3Configuration.setGenerateOutputWrappers(true);
        return as3Configuration;
    }

}
