package io.swagger.codegen.options;

import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.languages.CsharpDotNet2ClientCodegen;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class CsharpDotNet2ClientOptionsProvider implements OptionsProvider {
    public static final String PACKAGE_NAME_VALUE = "swagger_client_csharp_dotnet";
    public static final String PACKAGE_VERSION_VALUE = "1.0.0-SNAPSHOT";
    public static final String CLIENT_PACKAGE_VALUE = "IO.Swagger.Client.Test";

    @Override
    public String getLanguage() {
        return "CsharpDotNet2";
    }

    @Override
    public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put(CodegenConstants.PACKAGE_NAME, PACKAGE_NAME_VALUE)
                .put(CodegenConstants.PACKAGE_VERSION, PACKAGE_VERSION_VALUE)
                .put(CsharpDotNet2ClientCodegen.CLIENT_PACKAGE, CLIENT_PACKAGE_VALUE)
                .build();
    }

    @Override
    public boolean isServer() {
        return false;
    }
}
