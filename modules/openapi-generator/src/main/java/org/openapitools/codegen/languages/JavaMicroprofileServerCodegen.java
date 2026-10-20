package org.openapitools.codegen.languages;

import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenOperation;
import org.openapitools.codegen.CodegenResponse;
import org.openapitools.codegen.CodegenType;
import org.openapitools.codegen.VendorExtension;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.ModelsMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;

import java.io.File;
import java.util.List;
import java.util.Map;

import static org.openapitools.codegen.CodegenConstants.TYPE_INFO_DEFAULT_IMPLS;

public class JavaMicroprofileServerCodegen extends JavaClientCodegen {
    public static final String PROJECT_NAME = "projectName";
    public static final String MICROPROFILE_SERVER = "microprofileServer";
    protected boolean microprofileServer = true;

    public JavaMicroprofileServerCodegen() {
        super();

        outputFolder = "generated-code" + File.separator + "microprofile";
        invokerPackage = "org.openapitools.server";
        artifactId = "openapi-microprofile-server";
        apiPackage = "org.openapitools.server.api";
        modelPackage = "org.openapitools.server.model";
        setLibrary("microprofile");

        // The microprofile library renders oneOf interfaces through its own model.mustache, but does
        // not support Jackson's @JsonTypeInfo(defaultImpl = ...) on them, so typeInfoDefaultImpls and
        // x-jackson-default-impl have no meaningful effect here.
        cliOptions.removeIf(opt -> TYPE_INFO_DEFAULT_IMPLS.equals(opt.getOpt()));
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.SERVER;
    }

    @Override
    public String getName() {
        return "java-microprofile";
    }

    @Override
    public String getHelp() {
        return "Generates a microprofile server.";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        additionalProperties.put(MICROPROFILE_SERVER, microprofileServer);
        // Clear any typeInfoDefaultImpls that the parent may have read; the microprofile
        // model template does not support Jackson defaultImpl on oneOf interfaces.
        typeInfoDefaultImpls.clear();
    }

    @Override
    public Map<String, ModelsMap> postProcessAllModels(Map<String, ModelsMap> objs) {
        // The inherited resolution in AbstractJavaCodegen would otherwise turn a schema-level
        // x-jackson-default-impl into a defaultImpl = ... annotation, which the microprofile
        // templates do not support. Strip the opt-in extension before that resolution runs.
        for (CodegenModel cm : getAllModels(objs).values()) {
            cm.vendorExtensions.remove(VendorExtension.X_JACKSON_DEFAULT_IMPL.getName());
        }
        return super.postProcessAllModels(objs);
    }

    @Override
    public List<VendorExtension> getSupportedVendorExtensions() {
        List<VendorExtension> extensions = super.getSupportedVendorExtensions();
        extensions.remove(VendorExtension.X_JACKSON_DEFAULT_IMPL);
        return extensions;
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        super.postProcessOperationsWithModels(objs, allModels);
        OperationMap operations = objs.getOperations();
        if (operations != null) {
            List<CodegenOperation> ops = operations.getOperation();
            for (CodegenOperation operation : ops) {
                List<CodegenResponse> responses = operation.responses;
                if (multiple2xxResponsesSpecified(responses)) {
                    operation.vendorExtensions.put("x-multiple-2xx-response-operation", true);
                }
            }
        }
        return objs;
    }

    private boolean multiple2xxResponsesSpecified(List<CodegenResponse> responses) {
        int responseIs2xxCount = 0;
        for (CodegenResponse response : responses) {
            if (response.is2xx) {
                responseIs2xxCount++;
            }
        }
        return responseIs2xxCount > 1;
    }
}
