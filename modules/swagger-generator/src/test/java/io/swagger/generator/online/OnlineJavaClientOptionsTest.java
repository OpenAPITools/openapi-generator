package io.swagger.generator.online;

import com.google.common.collect.ImmutableMap;

import java.util.Map;

public class OnlineJavaClientOptionsTest extends OnlineGeneratorOptionsTest {

    public OnlineJavaClientOptionsTest() {
        super("java", false);
    }

    protected OnlineJavaClientOptionsTest(String language, boolean isServer) {
       super(language, isServer);
    }

    @Override
    protected Map<String, String> getOptions() {
        ImmutableMap.Builder<String, String> builder = new ImmutableMap.Builder<String, String>();
        return builder.put("modelPackage", "package")
                .put("apiPackage", "apiPackage")
                .put("sortParamsByRequiredFlag", "false")
                .put("invokerPackage", "io.swagger.client.test")
                .put("groupId", "io.swagger.test")
                .put("artifactId", "swagger-java-client-test")
                .put("artifactVersion", "1.0.0-SNAPSHOT")
                .put("sourceFolder", "src/main/java/test")
                .put("localVariablePrefix", "tst")
                .put("serializableModel", "false")
                .put("fullJavaUtil", "true")
                .put("library", "jersey2")
                .build();
    }
}
