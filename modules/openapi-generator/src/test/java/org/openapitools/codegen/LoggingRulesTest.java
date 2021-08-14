package org.openapitools.codegen;

import com.tngtech.archunit.core.domain.JavaClasses;
import com.tngtech.archunit.core.importer.ClassFileImporter;
import com.tngtech.archunit.lang.ArchRule;
import org.junit.Test;
import org.slf4j.Logger;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.fields;

public class LoggingRulesTest  {

    @Test
    public void testLoggersAreNotPublicFinalAndNotStatic() {
        final JavaClasses importedClasses = new ClassFileImporter()
                .importPackages("org.openapitools.codegen.languages");

        LoggingRulesTest.LOGGERS_SHOULD_BE_NOT_PUBLIC_NOT_STATIC_AND_FINAL.check(importedClasses);
    }

    /**
     * Making loggers not static decreases memory consumption when running generator:
     * https://github.com/OpenAPITools/openapi-generator/pull/8799
     */
    public static final ArchRule LOGGERS_SHOULD_BE_NOT_PUBLIC_NOT_STATIC_AND_FINAL = fields().that()
            .haveRawType(Logger.class)
            .should().notBePublic()
            .andShould().notBeStatic()
            .andShould().beFinal()
            .because("Code generators are most often used once per program lifetime, " +
                    "so making them all static will cause higher memory consumption, " +
                    "see #8799");

}
