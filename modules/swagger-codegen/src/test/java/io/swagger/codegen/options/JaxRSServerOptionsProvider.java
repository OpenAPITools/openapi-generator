package io.swagger.codegen.options;

import java.util.Map;

import com.google.common.collect.ImmutableMap;

import io.swagger.codegen.CodegenConstants;

public class JaxRSServerOptionsProvider extends JavaOptionsProvider {
    public static final String IMPL_FOLDER_VALUE = "src/main/java/impl";
	
    @Override
	public Map<String, String> createOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.putAll(super.createOptions())
                .put(CodegenConstants.IMPL_FOLDER, IMPL_FOLDER_VALUE)
                .build();
	}

	@Override
    public boolean isServer() {
        return true;
    }

    @Override
    public String getLanguage() {
        return "jaxrs";
    }
}
