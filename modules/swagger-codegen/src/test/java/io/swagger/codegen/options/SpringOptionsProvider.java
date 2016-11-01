package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.SpringCodegen;

import java.util.HashMap;
import java.util.Map;

public class SpringOptionsProvider extends JavaOptionsProvider {
    public static final String TITLE = "swagger";
    public static final String CONFIG_PACKAGE_VALUE = "configPackage";
    public static final String BASE_PACKAGE_VALUE = "basePackage";
    public static final String LIBRARY_VALUE = "spring-mvc"; //FIXME hidding value from super class
    public static final String INTERFACE_ONLY = "true";
    public static final String SINGLE_CONTENT_TYPES = "true";
    public static final String JAVA_8 = "true";
    public static final String ASYNC = "true";
    public static final String RESPONSE_WRAPPER = "Callable";

    @Override
    public String getLanguage() {
        return "spring";
    }

    @Override
    public Map<String, String> createOptions() {
        Map<String, String> options = new HashMap<String, String>(super.createOptions());
        options.put(SpringCodegen.TITLE, TITLE);
        options.put(SpringCodegen.CONFIG_PACKAGE, CONFIG_PACKAGE_VALUE);
        options.put(SpringCodegen.BASE_PACKAGE, BASE_PACKAGE_VALUE);
        options.put(CodegenConstants.LIBRARY, LIBRARY_VALUE);
        options.put(SpringCodegen.INTERFACE_ONLY, INTERFACE_ONLY);
        options.put(SpringCodegen.SINGLE_CONTENT_TYPES, SINGLE_CONTENT_TYPES);
        options.put(SpringCodegen.JAVA_8, JAVA_8);
        options.put(SpringCodegen.ASYNC, ASYNC);
        options.put(SpringCodegen.RESPONSE_WRAPPER, RESPONSE_WRAPPER);

        return options;
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
