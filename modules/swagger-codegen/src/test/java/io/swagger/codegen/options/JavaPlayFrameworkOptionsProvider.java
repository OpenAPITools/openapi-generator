package io.swagger.codegen.options;

import io.swagger.codegen.languages.JavaPlayFrameworkCodegen;

import java.util.HashMap;
import java.util.Map;

public class JavaPlayFrameworkOptionsProvider extends JavaOptionsProvider {
    public static final String TITLE = "swagger";
    public static final String CONFIG_PACKAGE_VALUE = "configPackage";
    public static final String BASE_PACKAGE_VALUE = "basePackage";
    public static final String CONTROLLER_ONLY = "false";
    public static final String USE_BEANVALIDATION = "true";
    public static final String USE_INTERFACES = "true";
    public static final String HANDLE_EXCEPTIONS = "true";
    public static final String WRAP_CALLS = "true";
    public static final String USE_SWAGGER_UI = "true";

    @Override
    public String getLanguage() {
        return "java-play-framework";
    }

    @Override
    public Map<String, String> createOptions() {
        Map<String, String> options = new HashMap<String, String>(super.createOptions());
        options.put(JavaPlayFrameworkCodegen.TITLE, TITLE);
        options.put(JavaPlayFrameworkCodegen.CONFIG_PACKAGE, CONFIG_PACKAGE_VALUE);
        options.put(JavaPlayFrameworkCodegen.BASE_PACKAGE, BASE_PACKAGE_VALUE);
        options.put(JavaPlayFrameworkCodegen.CONTROLLER_ONLY, CONTROLLER_ONLY);
        options.put(JavaPlayFrameworkCodegen.USE_BEANVALIDATION, USE_BEANVALIDATION);
        options.put(JavaPlayFrameworkCodegen.USE_INTERFACES, USE_INTERFACES);
        options.put(JavaPlayFrameworkCodegen.HANDLE_EXCEPTIONS, HANDLE_EXCEPTIONS);
        options.put(JavaPlayFrameworkCodegen.WRAP_CALLS, WRAP_CALLS);
        options.put(JavaPlayFrameworkCodegen.USE_SWAGGER_UI, USE_SWAGGER_UI);

        return options;
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
