package io.swagger.codegen.python;

import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.CodegenOperation;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.PythonClientCodegen;
import io.swagger.models.Operation;
import io.swagger.models.Swagger;
import io.swagger.parser.SwaggerParser;

import org.testng.Assert;
import org.testng.annotations.Test;

@SuppressWarnings("static-method")
public class PythonTest {

    @Test(description = "convert a python model with dots")
    public void modelTest() {
        final Swagger swagger = new SwaggerParser().read("src/test/resources/2_0/v1beta3.json");
        final DefaultCodegen codegen = new PythonClientCodegen();

        final CodegenModel simpleName = codegen.fromModel("v1beta3.Binding", swagger.getDefinitions().get("v1beta3.Binding"));
        Assert.assertEquals(simpleName.name, "v1beta3.Binding");
        Assert.assertEquals(simpleName.classname, "V1beta3Binding");
        Assert.assertEquals(simpleName.classVarName, "v1beta3_binding");

        final CodegenModel compoundName = codegen.fromModel("v1beta3.ComponentStatus", swagger.getDefinitions().get("v1beta3.ComponentStatus"));
        Assert.assertEquals(compoundName.name, "v1beta3.ComponentStatus");
        Assert.assertEquals(compoundName.classname, "V1beta3ComponentStatus");
        Assert.assertEquals(compoundName.classVarName, "v1beta3_component_status");

        final String path = "/api/v1beta3/namespaces/{namespaces}/bindings";
        final Operation operation = swagger.getPaths().get(path).getPost();
        final CodegenOperation codegenOperation = codegen.fromOperation(path, "get", operation, swagger.getDefinitions());
        Assert.assertEquals(codegenOperation.returnType, "V1beta3Binding");
        Assert.assertEquals(codegenOperation.returnBaseType, "V1beta3Binding");
    }
}
