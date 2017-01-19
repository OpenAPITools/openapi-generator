package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.BashClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class BashClientOptionsProvider implements OptionsProvider {

    public static final String CURL_OPTIONS = "-k --tlsv1.2";
    public static final String PROCESS_MARKDOWN = "true";
    public static final String SCRIPT_NAME = "petstore-cli";
    public static final String GENERATE_BASH_COMPLETION = "true";
    public static final String GENERATE_ZSH_COMPLETION = "false";
    public static final String HOST_ENVIRONMENT_VARIABLE_NAME 
                                = "PETSTORE_HOSTNAME";
    public static final String BASIC_AUTH_ENVIRONMENT_VARIABLE_NAME 
                                = "PETSTORE_BASIC_AUTH";
    public static final String APIKEY_AUTH_ENVIRONMENT_VARIABLE_NAME 
                                = "PETSTORE_APIKEY";

    @Override
    public String getLanguage() {
        return "bash";
    }

    @Override
    public Map<String, String> createOptions() {
        
        ImmutableMap.Builder<String, String> builder 
            = new ImmutableMap.Builder<String, String>();

        return builder
                .put(BashClientCodegen.CURL_OPTIONS, CURL_OPTIONS)
                .put(BashClientCodegen.SCRIPT_NAME, SCRIPT_NAME)
                .put(BashClientCodegen.PROCESS_MARKDOWN, PROCESS_MARKDOWN)
                .put(BashClientCodegen.GENERATE_BASH_COMPLETION, 
                        GENERATE_BASH_COMPLETION)
                .put(BashClientCodegen.GENERATE_ZSH_COMPLETION, 
                        GENERATE_ZSH_COMPLETION)
                .put(BashClientCodegen.HOST_ENVIRONMENT_VARIABLE_NAME, 
                        HOST_ENVIRONMENT_VARIABLE_NAME)
                .put(BashClientCodegen.BASIC_AUTH_ENVIRONMENT_VARIABLE_NAME, 
                        BASIC_AUTH_ENVIRONMENT_VARIABLE_NAME)
                .put(BashClientCodegen.APIKEY_AUTH_ENVIRONMENT_VARIABLE_NAME, 
                        APIKEY_AUTH_ENVIRONMENT_VARIABLE_NAME)
                .put(CodegenConstants.SORT_PARAMS_BY_REQUIRED_FLAG, "false")
                .put(CodegenConstants.ENSURE_UNIQUE_PARAMS, "false")
                .build();
    
    }

    @Override
    public boolean isServer() {
    
        return false;
    
    }
}
