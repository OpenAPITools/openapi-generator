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
        processCodegenExtensions(extensions);
        sortClientsAndServers();
    }

    @Override
    public void apply(ParameterContext parameterContext) {
        String parameterName = parameterContext.getOperationContext().getName();
        switch (parameterName) {
            case "getClientOptions":
            case "generateClient":
                parameterContext.parameterBuilder().allowableValues(new AllowableListValues(clients, "string"));
                break;
            case "getServerOptions":
            case "generateServerForLanguage":
                parameterContext.parameterBuilder().allowableValues(new AllowableListValues(servers, "string"));
                break;
            default:
                throw new IllegalStateException("Unexpected value: " + parameterName);
        }
    }

    @Override
    public boolean supports(DocumentationType documentationType) {
        return true;
    }


    private static void processCodegenExtensions(List<CodegenConfig> extensions) {
        for (CodegenConfig config : extensions) {
            processCodegenConfig(config);
        }
    }

    private static void processCodegenConfig(CodegenConfig config) {
        if (isClientOrDocumentation(config)) {
            clients.add(config.getName());
        } else if (isServer(config)) {
            servers.add(config.getName());
        }
    }

    private static boolean isClientOrDocumentation(CodegenConfig config) {
        return config.getTag().equals(CodegenType.CLIENT) || config.getTag().equals(CodegenType.DOCUMENTATION);
    }

    private static boolean isServer(CodegenConfig config) {
        return config.getTag().equals(CodegenType.SERVER);
    }

    private static void sortClientsAndServers() {
        clients.sort(String.CASE_INSENSITIVE_ORDER);
        servers.sort(String.CASE_INSENSITIVE_ORDER);
    }
}