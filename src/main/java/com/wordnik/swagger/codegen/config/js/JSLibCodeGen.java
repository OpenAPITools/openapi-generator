package com.wordnik.swagger.codegen.config.js;

import com.wordnik.swagger.codegen.LibraryCodeGenerator;
import com.wordnik.swagger.codegen.config.LanguageConfiguration;
import com.wordnik.swagger.codegen.exception.CodeGenerationException;
import com.wordnik.swagger.codegen.util.FileUtil;

import java.io.*;

/**
 * @author ayush
 * @since 10/24/11 7:47 PM
 */
public class JSLibCodeGen extends LibraryCodeGenerator {
    private static final String DEFAULT_SERVICE_BASE_CLASS = "SwaggerApi";

    public static void main(String[] args) {
        if (args.length < 1) {
            throw new CodeGenerationException("Invalid number of arguments passed: No command line argument was passed to the program for config json");
        }
        String libraryHome = null;

        if (args.length == 1) {
            String configPath = args[0];
            JSLibCodeGen codeGenerator = new JSLibCodeGen(configPath);
            codeGenerator.generateCode();

            libraryHome = codeGenerator.getLanguageConfig().getLibraryHome();
        }
        if (args.length == 3) {
            String apiServerURL = args[0];
            if (!apiServerURL.endsWith("/")) {
                apiServerURL = apiServerURL + "/";
            }
            String apiKey = args[1];
            String packageName = args[2];
            libraryHome = args[2];
            if (libraryHome.endsWith("/")) {
                libraryHome = libraryHome.substring(0, libraryHome.length() - 1);
            }
            String modelPackageName = "";
            String apiPackageName = "";
            String classOutputDir = libraryHome + "/src/main/js/";
            JSLibCodeGen codeGenerator = new JSLibCodeGen(apiServerURL, apiKey, modelPackageName,
                    apiPackageName, classOutputDir, libraryHome);
            codeGenerator.getConfig().setDefaultServiceBaseClass(DEFAULT_SERVICE_BASE_CLASS);
            codeGenerator.generateCode();
        }

        try {
            if (libraryHome != null) {
                concatinateFiles(libraryHome + "/api-lib.js", libraryHome + "/src/main/js/");
            }
        } catch (IOException e) {
            System.out.println("Unable to combine files");
            e.printStackTrace();
        }
    }

    private static void concatinateFiles(String outFile, String sourcePath) throws IOException {
        final PrintWriter pw = new PrintWriter(new FileOutputStream(outFile));
        final File file = new File(sourcePath);

        System.out.println("Scanning " + file);
        appendFiles(pw, file.listFiles());

        final File modelSource = new File(file, "model");
        System.out.println("Scanning " + modelSource.getAbsolutePath());
        appendFiles(pw, modelSource.listFiles());

        final File apiSource = new File(file, "api");
        System.out.println("Scanning " + apiSource.getAbsolutePath());
        appendFiles(pw, apiSource.listFiles());

        pw.close();
        System.out.println("Concatenated to " + outFile);
    }

    private static void appendFiles(PrintWriter pw, File[] files) throws IOException {
        for (int i = 0; i < files.length; i++) {
            System.out.println("Processing " + files[i].getPath() + "...");

            if(!files[i].isDirectory()) {
                BufferedReader br = new BufferedReader(new FileReader(files[i]
                        .getPath()));
                String line = br.readLine();
                while (line != null) {
                    pw.println(line);
                    line = br.readLine();
                }
                br.close();
            }
        }
    }

    public JSLibCodeGen(String apiServerURL, String apiKey, String modelPackageName, String apiPackageName,
                        String classOutputDir, String libraryHome) {
        super(apiServerURL, apiKey, modelPackageName, apiPackageName, classOutputDir, libraryHome);
        this.setDataTypeMappingProvider(new JSDataTypeMappingProvider());
        this.setNameGenerator(new JSNamingPolicyProvider());
    }

    public JSLibCodeGen(String configPath) {
        super(configPath);
        this.setDataTypeMappingProvider(new JSDataTypeMappingProvider());
        this.setNameGenerator(new JSNamingPolicyProvider());
    }

    @Override
    protected LanguageConfiguration initializeLangConfig(LanguageConfiguration jsConfiguration) {
        jsConfiguration.setClassFileExtension(".js");
        jsConfiguration.setTemplateLocation("conf/js/templates");
        jsConfiguration.setStructureLocation("conf/js/structure");
        jsConfiguration.setExceptionPackageName("com.wordnik.swagger.exception");
        jsConfiguration.setAnnotationPackageName("com.wordnik.swagger.annotations");

        //create ouput directories
        FileUtil.createOutputDirectories(jsConfiguration.getModelClassLocation(), jsConfiguration.getClassFileExtension());
        FileUtil.createOutputDirectories(jsConfiguration.getResourceClassLocation(), jsConfiguration.getClassFileExtension());
        //delete previously generated files
        FileUtil.clearFolder(jsConfiguration.getModelClassLocation());
        FileUtil.clearFolder(jsConfiguration.getResourceClassLocation());
        FileUtil.clearFolder(jsConfiguration.getLibraryHome() + "/src/main/js/com/wordnik/swagger/common");
        FileUtil.clearFolder(jsConfiguration.getLibraryHome() + "/src/main/js/com/wordnik/swagger/exception");
        FileUtil.clearFolder(jsConfiguration.getLibraryHome() + "/src/main/js/com/wordnik/swagger/event");
        FileUtil.copyDirectoryFromUrl(this.getClass().getClassLoader().getResource(jsConfiguration.getStructureLocation()), new File(jsConfiguration.getLibraryHome()));

        jsConfiguration.setModelEnumRequired(false);
        jsConfiguration.setOutputWrapperRequired(true);
        return jsConfiguration;
    }
}
