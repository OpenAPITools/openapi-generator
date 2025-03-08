package org.openapitools.codegen.kotlin;

import lombok.Getter;
import org.jetbrains.kotlin.com.intellij.openapi.util.text.Strings;

@Getter
enum ClientLibrary {
    JVM_KTOR("main/kotlin"),
    JVM_OKHTTP4("main/kotlin"),
    JVM_SPRING_WEBCLIENT("main/kotlin"),
    JVM_SPRING_RESTCLIENT("main/kotlin"),
    JVM_RETROFIT2("main/kotlin"),
    MULTIPLATFORM("commonMain/kotlin"),
    JVM_VOLLEY("gson", "main/java"),
    JVM_VERTX("main/kotlin");
    private final String serializationLibrary;
    private final String libraryName;
    private final String sourceRoot;

    ClientLibrary(String serializationLibrary, String sourceRoot) {
        this.serializationLibrary = serializationLibrary;
        this.sourceRoot = sourceRoot;
        this.libraryName = Strings.toLowerCase(this.name()).replace("_", "-");
    }

    ClientLibrary(String sourceRoot) {
        this("jackson", sourceRoot);
    }
}
