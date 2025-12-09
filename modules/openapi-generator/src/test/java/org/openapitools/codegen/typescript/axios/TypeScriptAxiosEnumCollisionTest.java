package org.openapitools.codegen.typescript.axios;

import org.openapitools.codegen.languages.TypeScriptAxiosClientCodegen;
import org.openapitools.codegen.typescript.TypeScriptGroups;
import org.testng.annotations.Test;

import static org.testng.Assert.assertNotEquals;
import static org.testng.Assert.assertEquals;

@Test(groups = {TypeScriptGroups.TYPESCRIPT, TypeScriptGroups.TYPESCRIPT_AXIOS})
public class TypeScriptAxiosEnumCollisionTest {

    @Test
    public void testTimezoneEnumsDontCollide() {
        TypeScriptAxiosClientCodegen codegen = new TypeScriptAxiosClientCodegen();
        codegen.processOpts();

        // Test the problematic timezone values from issue #22074
        String plusTwelve = codegen.toEnumVarName("Etc/GMT+12", "string");
        String minusTwelve = codegen.toEnumVarName("Etc/GMT-12", "string");
        
        // These should produce different enum variable names to avoid TS1117 error
        assertNotEquals(plusTwelve, minusTwelve, 
            "Enum variable names for 'Etc/GMT+12' and 'Etc/GMT-12' should be different to avoid TS1117 error");
        
        // Test a few more cases to ensure we handle the pattern correctly
        String plusOne = codegen.toEnumVarName("Etc/GMT+1", "string");
        String minusOne = codegen.toEnumVarName("Etc/GMT-1", "string");
        
        assertNotEquals(plusOne, minusOne, 
            "Enum variable names for 'Etc/GMT+1' and 'Etc/GMT-1' should be different");

        // Verify the expected output format with our fix (using default PascalCase naming)
        // The + and - should be converted to descriptive names
        assertEquals(plusTwelve, "EtcGmtPlus12", "Etc/GMT+12 should become EtcGmtPlus12");
        assertEquals(minusTwelve, "EtcGmtMinus12", "Etc/GMT-12 should become EtcGmtMinus12");
        assertEquals(plusOne, "EtcGmtPlus1", "Etc/GMT+1 should become EtcGmtPlus1");
        assertEquals(minusOne, "EtcGmtMinus1", "Etc/GMT-1 should become EtcGmtMinus1");
    }
}
