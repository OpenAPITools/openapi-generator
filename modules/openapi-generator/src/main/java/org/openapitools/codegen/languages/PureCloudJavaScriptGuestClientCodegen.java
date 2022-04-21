package org.openapitools.codegen.languages;

public class PureCloudJavaScriptGuestClientCodegen extends PureCloudJavaScriptClientCodegen {

    public PureCloudJavaScriptGuestClientCodegen() {
        super();
        apiDocTemplateFiles.put("api_json.mustache", ".json");
        operationTemplateFiles.put("operation_example.mustache", "-example.txt");
    }

    @Override
    public String getName() { return "purecloudjavascript-guest"; }

}
