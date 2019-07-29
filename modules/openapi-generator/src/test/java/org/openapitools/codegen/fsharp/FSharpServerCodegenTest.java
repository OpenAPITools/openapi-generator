/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.fsharp;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.CodegenProperty;
import org.openapitools.codegen.languages.AbstractFSharpCodegen;
import org.openapitools.codegen.languages.FsharpGiraffeServerCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

@SuppressWarnings("static-method")
public class FSharpServerCodegenTest {

  @Test(description = "sort models according to dependency order")
  public void testModelsAreSortedAccordingToDependencyOrder() throws Exception {
        final AbstractFSharpCodegen codegen = new P_AbstractFSharpCodegen();

        // parent
        final CodegenModel parent = new CodegenModel();
        CodegenProperty childProp = new CodegenProperty();
        childProp.complexType = "child";
        childProp.name = "child";
        parent.setVars(Collections.singletonList(childProp));

        final CodegenModel child = new CodegenModel();
        CodegenProperty carProp = new CodegenProperty();
        carProp.complexType = "car";
        carProp.name = "car";
        child.setVars(Collections.singletonList(carProp));

        // child
        final CodegenModel car = new CodegenModel();
        CodegenProperty modelProp = new CodegenProperty();
        modelProp.name = "model";
        car.setVars(Collections.singletonList(modelProp));

        Map<String, Object> models = new HashMap<String,Object>();
        models.put("parent", Collections.singletonMap("models", Collections.singletonList(Collections.singletonMap("model", parent))));
        models.put("child", Collections.singletonMap("models", Collections.singletonList(Collections.singletonMap("model", child))));
        models.put("car", Collections.singletonMap("models", Collections.singletonList(Collections.singletonMap("model", car))));

        Map<String,Object> sorted = codegen.postProcessDependencyOrders(models);
        
        Object[] keys = sorted.keySet().toArray();

        Assert.assertEquals(keys[0], "car");
        Assert.assertEquals(keys[1], "child");
        Assert.assertEquals(keys[2], "parent");
    }

    @Test(description = "modify model imports to explicit set namespace and package name")
    public void testModelImportsSpecifyNamespaceAndPackageName() throws Exception {
          final AbstractFSharpCodegen codegen = new FsharpGiraffeServerCodegen();
          codegen.setPackageName("MyNamespace");
          codegen.setModelPackage("Model");
          String modified = codegen.toModelImport("Foo");
          Assert.assertEquals(modified, "MyNamespace.Model.Foo");          
    }
    
    private static class P_AbstractFSharpCodegen extends AbstractFSharpCodegen {
     
    }
}
