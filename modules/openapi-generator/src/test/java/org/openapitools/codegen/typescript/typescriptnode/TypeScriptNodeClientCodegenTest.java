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
   }
}
