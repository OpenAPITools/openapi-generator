package org.openapitools.codegen.online.configuration;

import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConfigLoader;
import org.openapitools.codegen.CodegenType;
import org.springframework.stereotype.Component;
import springfox.documentation.service.AllowableListValues;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spi.service.ParameterBuilderPlugin;
import springfox.documentation.spi.service.contexts.ParameterContext;

import java.util.ArrayList;
import java.util.List;

@Component
public class ParameterAllowableValuesPlugin implements ParameterBuilderPlugin {

    private static List<String> clients = new ArrayList<>();
    private static List<String> servers = new ArrayList<>();

    static {
        List<CodegenConfig> extensions = CodegenConfigLoader.getAll();
        for (CodegenConfig config : extensions) {
            if (config.getTag().equals(CodegenType.CLIENT)
                    || config.getTag().equals(CodegenType.DOCUMENTATION)) {
                clients.add(config.getName());
            } else if (config.getTag().equals(CodegenType.SERVER)) {
                servers.add(config.getName());
            }
        }

        clients.sort(String.CASE_INSENSITIVE_ORDER);
        servers.sort(String.CASE_INSENSITIVE_ORDER);
    }

    @Override
    public void apply(ParameterContext parameterContext) {
        String name = parameterContext.getOperationContext().getName();
        switch (name) {
            case "getClientOptions":
            case "generateClient":
                parameterContext.parameterBuilder().allowableValues(new AllowableListValues(clients, "string"));
                break;
            case "getServerOptions":
            case "generateServerForLanguage":
                parameterContext.parameterBuilder().allowableValues(new AllowableListValues(servers, "string"));
        }
    }

    @Override
    public boolean supports(DocumentationType documentationType) {
        return true;
    }
}
