package org.openapitools.codegen.typescript.typescriptnode;

import org.openapitools.codegen.languages.TypeScriptNodeClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

public class TypeScriptNodeClientCodegenTest {

    @Test
    public void convertVarName() throws Exception {
       TypeScriptNodeClientCodegen codegen = new TypeScriptNodeClientCodegen();
       Assert.assertEquals(codegen.toVarName("name"), "name");
       Assert.assertEquals(codegen.toVarName("$name"), "$name");
       Assert.assertEquals(codegen.toVarName("nam$$e"), "nam$$e");
       Assert.assertEquals(codegen.toVarName("user-name"), "userName");
       Assert.assertEquals(codegen.toVarName("user_name"), "userName");
       Assert.assertEquals(codegen.toVarName("user|name"), "userName");
       Assert.assertEquals(codegen.toVarName("user !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~name"), "user$Name");
   }

   @Test
   public void testSnapshotVersion() {
      TypeScriptNodeClientCodegen codegen = new TypeScriptNodeClientCodegen();
      codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
      codegen.additionalProperties().put("snapshot", true);
      codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
      codegen.processOpts();

      org.junit.Assert.assertTrue(codegen.getNpmVersion().matches("^1.0.0-SNAPSHOT.[0-9]{12}$"));

      codegen = new TypeScriptNodeClientCodegen();
      codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
      codegen.additionalProperties().put("snapshot", true);
      codegen.additionalProperties().put("npmVersion", "3.0.0-M1");
      codegen.processOpts();

      org.junit.Assert.assertTrue(codegen.getNpmVersion().matches("^3.0.0-M1-SNAPSHOT.[0-9]{12}$"));

   }

   @Test
   public void testWithoutSnapshotVersion() {
      TypeScriptNodeClientCodegen codegen = new TypeScriptNodeClientCodegen();
      codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
      codegen.additionalProperties().put("snapshot", false);
      codegen.additionalProperties().put("npmVersion", "1.0.0-SNAPSHOT");
      codegen.processOpts();

      org.junit.Assert.assertTrue(codegen.getNpmVersion().matches("^1.0.0-SNAPSHOT$"));

      codegen = new TypeScriptNodeClientCodegen();
      codegen.additionalProperties().put("npmName", "@openapi/typescript-angular-petstore");
      codegen.additionalProperties().put("snapshot", false);
      codegen.additionalProperties().put("npmVersion", "3.0.0-M1");
      codegen.processOpts();

      org.junit.Assert.assertTrue(codegen.getNpmVersion().matches("^3.0.0-M1$"));

   }

}
