package io.swagger.codegen.options;

import io.swagger.codegen.languages.JavaPlayFrameworkCodegen;

import java.util.HashMap;
import java.util.Map;

public class JavaPlayFrameworkOptionsProvider extends JavaOptionsProvider {
    public static final String TITLE = "swagger";
    public static final String CONFIG_PACKAGE_VALUE = "configPackage";
    public static final String BASE_PACKAGE_VALUE = "basePackage";
    public static final String CONTROLLER_ONLY = "true";
    public static final String SINGLE_CONTENT_TYPES = "true";
    public static final String RESPONSE_WRAPPER = "Callable";
    public static final String USE_TAGS = "useTags";
    public static final String USE_BEANVALIDATION = "true";

    @Override
    public String getLanguage() {
        return "javaPlayFramework";
    }

    @Override
    public Map<String, String> createOptions() {
        Map<String, String> options = new HashMap<String, String>(super.createOptions());
        options.put(JavaPlayFrameworkCodegen.TITLE, TITLE);
        options.put(JavaPlayFrameworkCodegen.CONFIG_PACKAGE, CONFIG_PACKAGE_VALUE);
        options.put(JavaPlayFrameworkCodegen.BASE_PACKAGE, BASE_PACKAGE_VALUE);
        options.put(JavaPlayFrameworkCodegen.CONTROLLER_ONLY, CONTROLLER_ONLY);
        options.put(JavaPlayFrameworkCodegen.SINGLE_CONTENT_TYPES, SINGLE_CONTENT_TYPES);
        options.put(JavaPlayFrameworkCodegen.RESPONSE_WRAPPER, RESPONSE_WRAPPER);
        options.put(JavaPlayFrameworkCodegen.USE_TAGS, USE_TAGS);
        options.put(JavaPlayFrameworkCodegen.USE_BEANVALIDATION, USE_BEANVALIDATION);

        return options;
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
