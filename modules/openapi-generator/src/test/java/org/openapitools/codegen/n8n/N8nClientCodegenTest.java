package org.openapitools.codegen.n8n;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConfigLoader;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.N8nClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class N8nClientCodegenTest {

    private static final String PETSTORE_SPEC = "src/test/resources/3_0/petstore.yaml";

    @Test
    public void generatorIsDiscoverableViaSpi() {
        CodegenConfig config = CodegenConfigLoader.forName("n8n");
        Assert.assertTrue(config instanceof N8nClientCodegen);
        Assert.assertEquals(config.getName(), "n8n");
    }

    /**
     * Regression test for a bug caught by actually loading generated output into a real n8n
     * instance (podman-based manual verification against a live spec): n8n's
     * {@code CustomDirectoryLoader}/{@code loadClass} derives the expected exported class name
     * directly from the filename (e.g. {@code ApiCredentials.credentials.js} must export a class
     * literally named {@code ApiCredentials}) - a mismatch is silently swallowed as a logged
     * warning ("failed to load") rather than a hard error, so the node then fails at runtime with
     * "Unknown credential name" instead of failing generation or compilation.
     */
    @Test
    public void credentialsClassNameMatchesItsFilename() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            generate(target);
            String credentialsSource = readCredentialsSource(target);
            Assert.assertTrue(
                    credentialsSource.contains("export class ApiCredentials "),
                    "the exported class in ApiCredentials.credentials.ts must be named exactly "
                            + "'ApiCredentials' to match the filename, per n8n's directory loader "
                            + "convention (DirectoryLoader.loadClass)");
        } finally {
            deleteRecursively(target);
        }
    }

    @Test
    public void generatesCompletePackageFromPetstoreSpec() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            generate(target);

            List<Path> nodeFiles = find(target, "Api.node.ts");
            List<Path> credentialFiles = find(target, "ApiCredentials.credentials.ts");

            Assert.assertTrue(Files.exists(target.resolve("package.json")), "package.json should be generated");
            Assert.assertTrue(Files.exists(target.resolve("README.md")), "README.md should be generated");
            Assert.assertFalse(nodeFiles.isEmpty(), "a node .node.ts file should be generated");
            Assert.assertFalse(credentialFiles.isEmpty(), "a credentials .credentials.ts file should be generated");
        } finally {
            deleteRecursively(target);
        }
    }

    @Test
    public void groupsPetstoreOperationsByTagAsResources() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            generate(target);
            String nodeSource = readNodeSource(target);

            // Petstore declares the "pet", "store", and "user" tags; each must appear
            // as a Resource option value (sanitized/PascalCased by the framework's
            // standard tag handling, same as every other generator), and each tag's
            // operations must be grouped under it (spec.md US2 Acceptance Scenario 1).
            for (String tag : new String[]{"Pet", "Store", "User"}) {
                Assert.assertTrue(nodeSource.contains("value: '" + tag + "'"),
                        "expected a Resource option for tag '" + tag + "'");
            }
            Assert.assertTrue(nodeSource.contains("resource: ['Pet']"));
            Assert.assertTrue(nodeSource.contains("resource: ['Store']"));
            Assert.assertTrue(nodeSource.contains("resource: ['User']"));
        } finally {
            deleteRecursively(target);
        }
    }

    @Test
    public void generatesApiKeyCredentialAsThePrimaryScheme() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            generate(target);
            String credentialsSource = readCredentialsSource(target);

            // Petstore declares both an apiKey scheme ("api_key", header) and an oauth2
            // scheme ("petstore_auth"); apiKey has priority, so it must be the scheme
            // that's actually wired into the generated authenticate block
            // (spec.md US3 Acceptance Scenario 1 / data-model.md CredentialDefinition).
            Assert.assertTrue(credentialsSource.contains("apiKey"));
            Assert.assertTrue(credentialsSource.contains("'api_key'"));
            Assert.assertTrue(credentialsSource.contains("headers"));
        } finally {
            deleteRecursively(target);
        }
    }

    @Test
    public void documentsUnsupportedOrSecondarySchemesInsteadOfFailing() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            generate(target);
            String readme = Files.readString(target.resolve("README.md"));

            // The oauth2 scheme ("petstore_auth") isn't the auto-wired primary scheme,
            // so generation must still succeed and the limitation must be documented
            // (FR-010), rather than generation failing or the limitation being silent.
            Assert.assertTrue(readme.toLowerCase(java.util.Locale.ROOT).contains("oauth2"));
            Assert.assertTrue(readme.toLowerCase(java.util.Locale.ROOT).contains("limitation"));
        } finally {
            deleteRecursively(target);
        }
    }

    private static void generate(Path target) {
        ClientOptInput clientOptInput = new CodegenConfigurator()
                .setGeneratorName("n8n")
                .setInputSpec(PETSTORE_SPEC)
                .setOutputDir(target.toAbsolutePath().toString())
                .toClientOptInput();

        new DefaultGenerator().opts(clientOptInput).generate();
    }

    private static List<Path> find(Path root, String filenameSuffix) throws IOException {
        try (Stream<Path> stream = Files.walk(root)) {
            return stream.filter(Files::isRegularFile)
                    .filter(p -> p.getFileName().toString().equals(filenameSuffix))
                    .collect(Collectors.toList());
        }
    }

    private static String readNodeSource(Path target) throws IOException {
        List<Path> files = find(target, "Api.node.ts");
        Assert.assertFalse(files.isEmpty(), "expected a generated Api.node.ts file");
        return Files.readString(files.get(0));
    }

    private static String readCredentialsSource(Path target) throws IOException {
        List<Path> files = find(target, "ApiCredentials.credentials.ts");
        Assert.assertFalse(files.isEmpty(), "expected a generated ApiCredentials.credentials.ts file");
        return Files.readString(files.get(0));
    }

    private static void deleteRecursively(Path root) throws IOException {
        if (!Files.exists(root)) {
            return;
        }
        try (Stream<Path> stream = Files.walk(root)) {
            stream.sorted(java.util.Comparator.reverseOrder()).forEach(p -> {
                try {
                    Files.deleteIfExists(p);
                } catch (IOException ignored) {
                }
            });
        }
    }
}
