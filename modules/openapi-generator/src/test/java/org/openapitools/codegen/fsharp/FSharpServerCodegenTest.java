/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
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
import java.util.HashSet;
import java.util.Map;
import java.util.Arrays;
import java.lang.Exception;

@SuppressWarnings("static-method")
public class FSharpServerCodegenTest {

  @Test(description = "sort models according to dependency order")
  public void testModelsAreSortedAccordingToDependencyOrder() throws Exception {
        final AbstractFSharpCodegen codegen = new P_AbstractFSharpCodegen();

        final CodegenModel wheel = new CodegenModel();
        wheel.setImports(new HashSet<String>(Arrays.asList()));
        wheel.setClassname("wheel");

        final CodegenModel bike = new CodegenModel();
        bike.setImports(new HashSet<String>(Arrays.asList("wheel")));
        bike.setClassname("bike");

        final CodegenModel parent = new CodegenModel();
        parent.setImports(new HashSet<String>(Arrays.asList("bike", "car")));
        parent.setClassname("parent");

        final CodegenModel car = new CodegenModel();
        car.setImports(new HashSet<String>(Arrays.asList("wheel")));
        car.setClassname("car");

        final CodegenModel child = new CodegenModel();
        child.setImports(new HashSet<String>(Arrays.asList("car", "bike", "parent")));
        child.setClassname("child");

        Map<String, Object> models = new HashMap<String,Object>();
        models.put("parent", Collections.singletonMap("models", Collections.singletonList(Collections.singletonMap("model", parent))));
        models.put("child", Collections.singletonMap("models", Collections.singletonList(Collections.singletonMap("model", child))));
        models.put("car", Collections.singletonMap("models", Collections.singletonList(Collections.singletonMap("model", car))));
        models.put("bike", Collections.singletonMap("models", Collections.singletonList(Collections.singletonMap("model", bike))));
        models.put("wheel", Collections.singletonMap("models", Collections.singletonList(Collections.singletonMap("model", wheel))));

        Map<String,Object> sorted = codegen.postProcessDependencyOrders(models);
        
        Object[] keys = sorted.keySet().toArray();
        
        Assert.assertTrue(keys[0] == "wheel");
        Assert.assertTrue(keys[1] == "bike" || keys[1] == "car");
        Assert.assertTrue(keys[2] == "bike" || keys[2] == "car");
        Assert.assertEquals(keys[3], "parent");
        Assert.assertEquals(keys[4], "child");
        
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
