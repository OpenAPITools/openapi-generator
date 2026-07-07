package org.openapitools.codegen.languages;

import com.samskivert.mustache.Mustache;
import org.openapitools.codegen.*;
import org.openapitools.codegen.model.ModelMap;
import org.openapitools.codegen.model.OperationMap;
import org.openapitools.codegen.model.OperationsMap;
import org.openapitools.codegen.templating.mustache.GoHttpStatusLambda;
import org.openapitools.codegen.templating.mustache.HttpStatusNameLambda;
import org.openapitools.codegen.templating.mustache.SpringHttpStatusLambda;

import java.io.File;
import java.util.List;

public class GoRestyClient extends GoGinServer2Codegen {

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "go-resty-client";
    }

    @Override
    public String getHelp() {
        return "Generates a go resty client.";
    }

    public GoRestyClient() {
        super();

        outputFolder = "generated-code" + File.separator + "go-resty-client";
        modelTemplateFiles.put("model.mustache", ".go");
        apiTemplateFiles.put("api.mustache", ".go");
        embeddedTemplateDir = templateDir = "go-resty-client";

        replacePath = false;

        // go-resty-client uses strfmt.DateTime (not time.Time) for date-time; strfmt import handled by parent
        typeMapping.put("DateTime", "strfmt.DateTime");

        // map[string]any and any are Go builtins mapped from object/AnyType — not model types
        languageSpecificPrimitives.add("map[string]any");
        languageSpecificPrimitives.add("map[string]interface{}");
        languageSpecificPrimitives.add("any");
    }

    @Override
    protected void addSupportingFiles() {
        supportingFiles.add(new SupportingFile("client.mustache", path, "client.go"));
        supportingFiles.add(new SupportingFile("constants.mustache", path + "/common", "constants.go"));
        supportingFiles.add(new SupportingFile("request_options.mustache", path + "/common", "request_options.go"));
        supportingFiles.add(new SupportingFile("utils.mustache", path + "/common", "utils.go"));
        supportingFiles.add(new SupportingFile("README.mustache", path, "README.md"));
    }

    @Override
    public void processOpts() {
        super.processOpts();
        additionalProperties.put("lambda.append-package", (Mustache.Lambda) (fragment, writer) -> writer.write(appendPackage(fragment.execute())));
        additionalProperties.put("lambda.to-package", (Mustache.Lambda) (fragment, writer) -> writer.write(toPackage(fragment.execute())));
        additionalProperties.put("goHttpStatus", new GoHttpStatusLambda());
        additionalProperties.put("goHttpStatusName", new HttpStatusNameLambda());
    }

    @Override
    public OperationsMap postProcessOperationsWithModels(OperationsMap objs, List<ModelMap> allModels) {
        objs = super.postProcessOperationsWithModels(objs, allModels);

        OperationMap objectMap = objs.getOperations();
        List<CodegenOperation> operations = objectMap.getOperation();

        boolean hasModels = false;
        outer:
        for (CodegenOperation op : operations) {
            if (op.returnType != null && !op.returnTypeIsPrimitive && !op.isMap) {
                hasModels = true;
                break;
            }
            for (CodegenResponse r : op.responses) {
                if (r.isModel) {
                    hasModels = true;
                    break outer;
                }
            }
            if (op.bodyParam != null && !op.bodyParam.isPrimitiveType) {
                hasModels = true;
                break;
            }
        }

        objs.put("hasModels", hasModels);
        return objs;
    }

    private String appendPackage(String content) {
        content = content.trim();
        if (content.startsWith("[]")) {
            String itemType = content.substring(2);
            // Only add models. prefix for unqualified model/enum names (uppercase, no existing package dot)
            if (!itemType.isEmpty() && Character.isUpperCase(itemType.charAt(0)) && !itemType.contains(".")) {
                return "[]models." + itemType;
            }
            return content;
        }
        return content.replace("[]", "[]models.");
    }

    private String toPackage(String content) {
        return content.trim().replace("-", "").toLowerCase();
    }
}
