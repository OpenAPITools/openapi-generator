package org.openapitools.codegen.debug;

import org.junit.Test;
import org.openapitools.codegen.OpenAPIGenerator;

/***
 * This test allows you to easily launch your code generation software under a debugger.
 * Then run this test under debug mode.  You will be able to step through your java code 
 * and then see the results in the out directory. 
 *
 * To experiment with debugging your code generator:
 * 1) Set a break point in MyclientcodegenGenerator.java in the postProcessOperationsWithModels() method.
 * 2) To launch this test in Eclipse: right-click | Debug As | JUnit Test
 *
 */
public class DebugCodegenLauncher 
{
  @Test
  public void launchCodeGeneratorInDebugMode()
  {
    // use this test to launch you code generator in the debugger.
    // this allows you to easily set break points in MyclientcodegenGenerator.
    String commandLineParams =
        "generate "+              
        "-i https://raw.githubusercontent.com/openapitools/openapi-generator/master/modules/openapi-generator/src/test/resources/2_0/petstore.yaml "+ // sample swagger 
        "-t ./src/main/resources/myClientCodegen "+          // template directory
        "-o out/myClientCodegen "+                           // output directory
        "-g myClientCodegen ";                               // use this codegen library

    try{
      OpenAPIGenerator.main( commandLineParams.split(" ") );
    }
    catch(Exception ex) {
      System.err.println(ex.toString());
    }
    catch(Error er) {
      System.err.println(er.toString());
    }
  }
}