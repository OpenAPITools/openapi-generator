package org.openapitools.codegen.typescript.axios;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.typescript.TypeScriptGroups;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.Consumer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertTrue;

@Test(groups = {TypeScriptGroups.TYPESCRIPT, TypeScriptGroups.TYPESCRIPT_AXIOS})
public class TypeScriptAxiosSlimParityTest {
    private static final String EDGE_CASE_SPEC = "src/test/resources/3_0/typescript-axios-slim/identity-edge-cases.yaml";

    private static final Pattern REQUEST_INTERFACE_PATTERN = Pattern.compile("export interface (\\w+Request) \\{([\\s\\S]*?)\\n\\}");
    private static final Pattern API_INTERFACE_PATTERN = Pattern.compile("export interface (\\w+Interface) \\{([\\s\\S]*?)\\n\\}");
    private static final Pattern API_INTERFACE_METHOD_PATTERN = Pattern.compile("^\\s*(\\w+)\\((.*)\\):\\s*(.+);\\s*$", Pattern.MULTILINE);
    private static final Pattern ENUM_DECL_PATTERN = Pattern.compile("export enum (\\w+) \\{([\\s\\S]*?)\\n\\}");
    private static final Pattern CONST_ENUM_DECL_PATTERN = Pattern.compile("export const (\\w+) = \\{([\\s\\S]*?)\\} as const;");
    private static final Pattern WRAPPED_RETURN_TYPE_PATTERN = Pattern.compile("^(AxiosPromise|Promise|AxiosResponse)\\s*<\\s*(.+)\\s*>$");

    private static final List<String> API_DIR_CANDIDATES = Arrays.asList("api", "apis");
    private static final List<String> MODEL_DIR_CANDIDATES = Arrays.asList("model", "models");

    private static final Set<String> EXPECTED_EDGE_METHODS = new TreeSet<>(Arrays.asList(
            "getUserByCompany",
            "createUserByCompany",
            "deleteUserByCompany",
            "aliasLookup",
            "listReports",
            "submitForm",
            "uploadEvidence",
            "healthCheck",
            "searchUsers",
            "getUnionPayload",
            "getWithQueryApiKey"
    ));

    private static final Consumer<CodegenConfigurator> NO_CUSTOMIZER = cfg -> {
    };

    @Test(description = "identity: comprehensive edge-case spec across option matrix")
    public void shouldKeepIdentityForComprehensiveEdgeCaseSpecAcrossOptionMatrix() throws Exception {
        List<OptionScenario> scenarios = Arrays.asList(
                new OptionScenario("default", NO_CUSTOMIZER),
                new OptionScenario("string-enums", cfg -> cfg.addAdditionalProperty("stringEnums", true)),
                new OptionScenario("node-imports", cfg -> cfg.addAdditionalProperty("withNodeImports", true)),
                new OptionScenario("aws-v4-signature", cfg -> cfg.addAdditionalProperty("withAWSV4Signature", true)),
                new OptionScenario("separate-models-and-api", cfg -> cfg
                        .addAdditionalProperty("withSeparateModelsAndApi", true)
                        .addAdditionalProperty("apiPackage", "api")
                        .addAdditionalProperty("modelPackage", "model")),
                new OptionScenario("import-js-extension", cfg -> cfg.addAdditionalProperty("importFileExtension", ".js")),
                new OptionScenario("square-bracket-form-arrays", cfg -> cfg.addAdditionalProperty("useSquareBracketsInArrayNames", true))
        );

        for (OptionScenario scenario : scenarios) {
            IdentitySurface axiosSurface = generateIdentity("typescript-axios", EDGE_CASE_SPEC, scenario.customizer);
            IdentitySurface slimSurface = generateIdentity("typescript-axios-slim", EDGE_CASE_SPEC, scenario.customizer);

            assertIdentitySurfaceEquals(scenario.name, axiosSurface, slimSurface);
            assertTrue(axiosSurface.allMethodNames().containsAll(EXPECTED_EDGE_METHODS),
                    "Scenario " + scenario.name + " did not generate the expected operation set");
        }
    }

    @Test(description = "identity: known regression fixtures (req/res surface)")
    public void shouldKeepIdentityAcrossKnownRegressionFixtures() throws Exception {
        List<SpecScenario> scenarios = Arrays.asList(
                new SpecScenario("petstore", "src/test/resources/3_0/petstore.yaml", NO_CUSTOMIZER),
                new SpecScenario("nullable-required", "src/test/resources/3_0/petstore-with-nullable-required.yaml", NO_CUSTOMIZER),
                new SpecScenario("multiple-2xx", "src/test/resources/3_0/petstore-multiple-2xx-responses.yaml", NO_CUSTOMIZER),
                new SpecScenario("query-form", "src/test/resources/3_0/query-param-form.yaml", NO_CUSTOMIZER),
                new SpecScenario("query-deep-object", "src/test/resources/3_0/query-param-deep-object.yaml", NO_CUSTOMIZER),
                new SpecScenario("deepobject", "src/test/resources/3_0/deepobject.yaml", NO_CUSTOMIZER),
                new SpecScenario("parameter-name-mapping", "src/test/resources/3_0/name-parameter-mappings.yaml", NO_CUSTOMIZER),
                new SpecScenario("shared-parameters-3_1", "src/test/resources/3_1/common-parameters.yaml", NO_CUSTOMIZER),
                new SpecScenario("multipart-enum-3_1", "src/test/resources/3_1/enum-in-multipart.yaml", NO_CUSTOMIZER),
                new SpecScenario("map-array-inner-enum", "src/test/resources/3_0/issue_19393_map_of_inner_enum.yaml", NO_CUSTOMIZER),
                new SpecScenario("generic-type-mapping", "src/test/resources/3_1/issue_21317.yaml", cfg -> cfg
                        .addTypeMapping("UserSummary", "Pick<User, \"email\">")
                        .addTypeMapping("object", "Record<string,unknown>"))
        );

        for (SpecScenario scenario : scenarios) {
            IdentitySurface axiosSurface = generateIdentity("typescript-axios", scenario.specPath, scenario.customizer);
            IdentitySurface slimSurface = generateIdentity("typescript-axios-slim", scenario.specPath, scenario.customizer);
            assertIdentitySurfaceEquals(scenario.name, axiosSurface, slimSurface);
        }
    }

    @Test(description = "identity: mapped generic response signature is preserved")
    public void shouldPreserveMappedGenericResponseSignature() throws Exception {
        Consumer<CodegenConfigurator> mappedTypeCustomizer = cfg -> cfg
                .addTypeMapping("UserSummary", "Pick<User, \"email\">")
                .addTypeMapping("object", "Record<string,unknown>");

        IdentitySurface axiosSurface = generateIdentity("typescript-axios", "src/test/resources/3_1/issue_21317.yaml", mappedTypeCustomizer);
        IdentitySurface slimSurface = generateIdentity("typescript-axios-slim", "src/test/resources/3_1/issue_21317.yaml", mappedTypeCustomizer);

        assertIdentitySurfaceEquals("generic-type-mapping-signature", axiosSurface, slimSurface);
    }

    @Test(description = "slim: object-oriented methods return payload data directly")
    public void shouldReturnPayloadDataFromObjectOrientedMethods() throws Exception {
        IdentitySurface slimSurface = generateIdentity("typescript-axios-slim", EDGE_CASE_SPEC, NO_CUSTOMIZER);
        String apiSource = String.join(" ", slimSurface.apiFiles.values());

        assertTrue(apiSource.contains("): Promise<"), "Slim API methods should return Promise payload types");
        assertFalse(apiSource.contains("AxiosPromise<"), "Slim API interface should not expose AxiosPromise return wrappers");
        assertFalse(apiSource.contains("Promise<AxiosResponse<"), "Slim API class should not return Promise<AxiosResponse<T>>");
        assertTrue(apiSource.contains("return localVarResponse.data;"), "Slim API class should resolve axios response data directly");
    }

    private IdentitySurface generateIdentity(String generatorName, String specPath, Consumer<CodegenConfigurator> customizer) throws Exception {
        File output = Files.createTempDirectory("typescript_axios_identity_").toFile().getCanonicalFile();
        output.deleteOnExit();

        CodegenConfigurator configurator = new CodegenConfigurator()
                .setGeneratorName(generatorName)
                .setInputSpec(specPath)
                .setOutputDir(output.getAbsolutePath())
                .addAdditionalProperty("withInterfaces", true)
                .addAdditionalProperty("useSingleRequestParameter", true);
        customizer.accept(configurator);

        ClientOptInput clientOptInput = configurator.toClientOptInput();
        new DefaultGenerator().opts(clientOptInput).generate();

        return extractIdentity(output.toPath());
    }

    private IdentitySurface extractIdentity(Path outputDir) throws IOException {
        IdentitySurface surface = new IdentitySurface();

        List<Path> apiFiles = collectApiFiles(outputDir);
        assertFalse(apiFiles.isEmpty(), "No API source files were generated under " + outputDir);

        for (Path apiFile : apiFiles) {
            String content = Files.readString(apiFile);
            String relativePath = normalizePath(outputDir.relativize(apiFile));
            surface.apiFiles.put(relativePath, normalize(content));

            extractRequestInterfaces(surface.requestInterfaces, content);
            extractApiInterfaceMethods(surface.apiInterfaceMethods, content);
            extractOperationEnums(surface.operationEnums, content);
        }

        List<Path> modelFiles = collectModelFiles(outputDir);
        for (Path modelFile : modelFiles) {
            String relativePath = normalizePath(outputDir.relativize(modelFile));
            surface.modelFiles.put(relativePath, normalize(Files.readString(modelFile)));
        }

        assertTrue(!surface.requestInterfaces.isEmpty() || !surface.apiInterfaceMethods.isEmpty(),
                "No comparable request/response surface was extracted from generated sources under " + outputDir);

        return surface;
    }

    private List<Path> collectApiFiles(Path outputDir) throws IOException {
        LinkedHashSet<Path> files = new LinkedHashSet<>();
        Path rootApi = outputDir.resolve("api.ts");
        if (Files.exists(rootApi)) {
            files.add(rootApi);
        }

        for (String dirName : API_DIR_CANDIDATES) {
            Path dir = outputDir.resolve(dirName);
            if (Files.isDirectory(dir)) {
                Files.walk(dir)
                        .filter(Files::isRegularFile)
                        .filter(path -> path.getFileName().toString().endsWith(".ts"))
                        .forEach(files::add);
            }
        }

        List<Path> sorted = new ArrayList<>(files);
        Collections.sort(sorted);
        return sorted;
    }

    private List<Path> collectModelFiles(Path outputDir) throws IOException {
        LinkedHashSet<Path> files = new LinkedHashSet<>();
        for (String dirName : MODEL_DIR_CANDIDATES) {
            Path dir = outputDir.resolve(dirName);
            if (Files.isDirectory(dir)) {
                Files.walk(dir)
                        .filter(Files::isRegularFile)
                        .filter(path -> path.getFileName().toString().endsWith(".ts"))
                        .forEach(files::add);
            }
        }

        List<Path> sorted = new ArrayList<>(files);
        Collections.sort(sorted);
        return sorted;
    }

    private void extractRequestInterfaces(Map<String, String> target, String apiSource) {
        Matcher matcher = REQUEST_INTERFACE_PATTERN.matcher(apiSource);
        while (matcher.find()) {
            target.put(matcher.group(1), normalize(matcher.group(2)));
        }
    }

    private void extractApiInterfaceMethods(Map<String, Set<String>> target, String apiSource) {
        Matcher interfaceMatcher = API_INTERFACE_PATTERN.matcher(apiSource);
        while (interfaceMatcher.find()) {
            String interfaceName = interfaceMatcher.group(1);
            String interfaceBody = interfaceMatcher.group(2);
            Matcher methodMatcher = API_INTERFACE_METHOD_PATTERN.matcher(interfaceBody);
            while (methodMatcher.find()) {
                String methodName = methodMatcher.group(1);
                String params = normalize(methodMatcher.group(2));
                String returnType = normalizeComparableReturnType(methodMatcher.group(3));
                target.computeIfAbsent(interfaceName, ignored -> new TreeSet<>())
                        .add(methodName + "(" + params + "):" + returnType);
            }
        }
    }

    private String normalizeComparableReturnType(String returnType) {
        String current = normalize(returnType);
        while (true) {
            Matcher wrapperMatcher = WRAPPED_RETURN_TYPE_PATTERN.matcher(current);
            if (!wrapperMatcher.matches()) {
                return current;
            }

            String wrapperType = wrapperMatcher.group(1);
            String innerType = wrapperMatcher.group(2).trim();
            if ("AxiosResponse".equals(wrapperType)) {
                innerType = firstTopLevelTypeArgument(innerType);
            }

            if (innerType.equals(current)) {
                return innerType;
            }
            current = innerType;
        }
    }

    private String firstTopLevelTypeArgument(String typeArguments) {
        int depth = 0;
        for (int i = 0; i < typeArguments.length(); i++) {
            char current = typeArguments.charAt(i);
            if (current == '<') {
                depth++;
            } else if (current == '>') {
                depth = Math.max(0, depth - 1);
            } else if (current == ',' && depth == 0) {
                return typeArguments.substring(0, i).trim();
            }
        }

        return typeArguments.trim();
    }

    private void extractOperationEnums(Map<String, String> target, String apiSource) {
        Matcher enumMatcher = ENUM_DECL_PATTERN.matcher(apiSource);
        while (enumMatcher.find()) {
            target.put("enum:" + enumMatcher.group(1), normalize(enumMatcher.group(2)));
        }

        Matcher constEnumMatcher = CONST_ENUM_DECL_PATTERN.matcher(apiSource);
        while (constEnumMatcher.find()) {
            target.put("const:" + constEnumMatcher.group(1), normalize(constEnumMatcher.group(2)));
        }
    }

    private void assertIdentitySurfaceEquals(String scenarioName, IdentitySurface expectedAxios, IdentitySurface actualSlim) {
        assertEquals(actualSlim.requestInterfaces, expectedAxios.requestInterfaces,
                scenarioName + ": request interface identity mismatch");
        assertEquals(actualSlim.apiInterfaceMethods, expectedAxios.apiInterfaceMethods,
                scenarioName + ": API interface method identity mismatch");
        assertEquals(actualSlim.operationEnums, expectedAxios.operationEnums,
                scenarioName + ": enum identity mismatch");
        assertEquals(actualSlim.modelFiles, expectedAxios.modelFiles,
                scenarioName + ": model file identity mismatch");
    }

    private static String normalize(String content) {
        return content
                .replace("\r\n", "\n")
                .replace('\r', '\n')
                .replaceAll("\\s+", " ")
                .trim();
    }

    private static String normalizePath(Path path) {
        return path.toString().replace('\\', '/');
    }

    private static final class IdentitySurface {
        private final Map<String, String> requestInterfaces = new TreeMap<>();
        private final Map<String, Set<String>> apiInterfaceMethods = new TreeMap<>();
        private final Map<String, String> operationEnums = new TreeMap<>();
        private final Map<String, String> modelFiles = new TreeMap<>();
        private final Map<String, String> apiFiles = new TreeMap<>();

        private Set<String> allMethodNames() {
            Set<String> names = new TreeSet<>();

            for (Set<String> signatures : apiInterfaceMethods.values()) {
                for (String signature : signatures) {
                    names.add(methodNameFromSignature(signature));
                }
            }
            return names;
        }

        private String methodNameFromSignature(String signature) {
            int index = signature.indexOf('(');
            return index >= 0 ? signature.substring(0, index) : signature;
        }
    }

    private static final class OptionScenario {
        private final String name;
        private final Consumer<CodegenConfigurator> customizer;

        private OptionScenario(String name, Consumer<CodegenConfigurator> customizer) {
            this.name = name;
            this.customizer = customizer;
        }
    }

    private static final class SpecScenario {
        private final String name;
        private final String specPath;
        private final Consumer<CodegenConfigurator> customizer;

        private SpecScenario(String name, String specPath, Consumer<CodegenConfigurator> customizer) {
            this.name = name;
            this.specPath = specPath;
            this.customizer = customizer;
        }
    }
}
