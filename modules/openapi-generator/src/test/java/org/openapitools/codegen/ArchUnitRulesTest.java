package org.openapitools.codegen;

import com.tngtech.archunit.core.domain.JavaClasses;
import com.tngtech.archunit.core.domain.JavaModifier;
import com.tngtech.archunit.core.importer.ClassFileImporter;
import com.tngtech.archunit.lang.ArchRule;
import org.junit.Test;

import static com.tngtech.archunit.lang.syntax.ArchRuleDefinition.*;
import static com.tngtech.archunit.library.GeneralCodingRules.NO_CLASSES_SHOULD_ACCESS_STANDARD_STREAMS;
import static com.tngtech.archunit.library.GeneralCodingRules.NO_CLASSES_SHOULD_USE_JAVA_UTIL_LOGGING;

public class ArchUnitRulesTest {
    private static final JavaClasses CLASSES = new ClassFileImporter()
                                              .importPackages("org.openapitools.codegen.languages");

    @Test
    public void testLoggersAreNotPublicFinalAndNotStatic() {
        ArchUnitRulesTest.LOGGERS_SHOULD_BE_NOT_PUBLIC_NOT_STATIC_AND_FINAL.check(CLASSES);
    }

    @Test
    public void classesNotAllowedToUseStandardStreams() {
        NO_CLASSES_SHOULD_ACCESS_STANDARD_STREAMS.check(CLASSES);
    }

    @Test
    public void disallowJavaUtilLogging() {
        NO_CLASSES_SHOULD_USE_JAVA_UTIL_LOGGING.check(CLASSES);
    }

    @Test
    public void abstractClassesAreAbstract() {
        ArchUnitRulesTest.ABSTRACT_CLASS_MUST_BE_ABSTRACT.check(CLASSES);
    }

    /**
     * Making loggers not static decreases memory consumption when running generator:
     * https://github.com/OpenAPITools/openapi-generator/pull/8799
     */
    public static final ArchRule LOGGERS_SHOULD_BE_NOT_PUBLIC_NOT_STATIC_AND_FINAL =
            fields()
            .that()
            .haveRawType(org.slf4j.Logger.class)
            .should().notBePublic()
            .andShould().notBeStatic()
            .andShould().beFinal()
            .because("Code generators are most often used once per program lifetime, " +
                    "so making them all static will cause higher memory consumption. " +
                    "See PR #8799");


    public static final ArchRule ABSTRACT_CLASS_MUST_BE_ABSTRACT =
            classes()
            .that()
            .haveSimpleNameContaining("Abstract").or().haveSimpleNameContaining("abstract")
            .should()
            .haveModifier(JavaModifier.ABSTRACT);
}
