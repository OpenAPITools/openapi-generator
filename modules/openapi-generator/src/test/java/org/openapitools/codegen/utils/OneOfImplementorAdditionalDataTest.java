package org.openapitools.codegen.utils;

import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.languages.GoClientExperimentalCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class OneOfImplementorAdditionalDataTest {
    @Test
    public void testGeneralUsage() {
        OneOfImplementorAdditionalData o = new OneOfImplementorAdditionalData("Implementor");

        // set up all the necessary inputs for `o.addFromInterfaceModel`
        CodegenModel oneOfModel = new CodegenModel();
        oneOfModel.classname = "OneOfModel";
        oneOfModel.vars = new ArrayList<>();
        CodegenProperty cp1 = new CodegenProperty();
        cp1.baseName = "OneOfModelProperty";
        oneOfModel.vars.add(cp1);
        CodegenProperty cp2 = new CodegenProperty();
        cp2.baseName = "InterfaceModelProperty";
        oneOfModel.vars.add(cp2);
        // if the OneOfModel has interface models, we want to verify that their properties don't get
        // added to the oneOf-implementing model
        CodegenModel interfaceModel = new CodegenModel();
        interfaceModel.vars.add(cp2.clone());
        oneOfModel.interfaceModels = new ArrayList<>();
        oneOfModel.interfaceModels.add(interfaceModel);

        List<Map<String, String>> interfaceModelImports = new ArrayList<>();
        interfaceModelImports.add(new HashMap<String, String>(){{ put("import", "foo"); }});

        o.addFromInterfaceModel(oneOfModel, interfaceModelImports);

        // set up all the necessary inputs for `o.addToImplementor`
        CodegenModel implModel = new CodegenModel();
        implModel.vars = new ArrayList<>();
        CodegenProperty cp3 = new CodegenProperty();
        cp3.baseName = "OtherProperty";
        cp3.hasMore = false;
        implModel.vars.add(cp3);
        List<Map<String, String>> implModelImports = new ArrayList<>();
        GoClientExperimentalCodegen cc = new GoClientExperimentalCodegen();
        cc.setModelPackage("openapi");

        o.addToImplementor(cc, implModel, implModelImports, false);

        // make sure all the additions were done correctly
        Assert.assertEquals(implModel.getVendorExtensions().get("x-implements"), new ArrayList<String>(){{add(oneOfModel.classname);}});
        Assert.assertEquals(implModelImports, interfaceModelImports);
        Assert.assertEquals(implModel.vars, new ArrayList<CodegenProperty>(){{add(cp3); add(cp1);}});
        Assert.assertTrue(implModel.vars.get(0).hasMore);
    }
}
