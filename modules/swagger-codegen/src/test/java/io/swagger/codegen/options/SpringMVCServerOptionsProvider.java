package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.SpringMVCServerCodegen;

import java.util.HashMap;
import java.util.Map;

public class SpringMVCServerOptionsProvider extends JavaOptionsProvider {
    public static final String CONFIG_PACKAGE_VALUE = "configPackage";
    public static final String LIBRARY_VALUE = "j8-async"; //FIXME hidding value from super class

    @Override
    public String getLanguage() {
        return "spring-mvc";
    }

    @Override
    public Map<String, String> createOptions() {
        Map<String, String> options = new HashMap<String, String>(super.createOptions());
        options.put(SpringMVCServerCodegen.CONFIG_PACKAGE, CONFIG_PACKAGE_VALUE);
        options.put(CodegenConstants.LIBRARY, LIBRARY_VALUE);
        return options;
    }

    @Override
    public boolean isServer() {
        return true;
    }
}
