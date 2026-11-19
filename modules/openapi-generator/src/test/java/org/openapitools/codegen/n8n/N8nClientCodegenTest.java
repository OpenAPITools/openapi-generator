package org.openapitools.codegen.n8n;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConfigLoader;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.languages.N8nClientCodegen;
import org.testng.Assert;
import org.testng.SkipException;
import org.testng.annotations.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
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
            for (Path credentialFile : find(target, "ApiKeyCredentials.credentials.ts")) {
                String source = Files.readString(credentialFile);
                Assert.assertTrue(
                        source.contains("export class ApiKeyCredentials "),
                        "the exported class in " + credentialFile.getFileName()
                                + " must be named exactly 'ApiKeyCredentials' to match the filename, "
                                + "per n8n's directory loader convention (DirectoryLoader.loadClass)");
            }
            for (Path credentialFile : find(target, "PetstoreAuthCredentials.credentials.ts")) {
                String source = Files.readString(credentialFile);
                Assert.assertTrue(
                        source.contains("export class PetstoreAuthCredentials "),
                        "the exported class in " + credentialFile.getFileName()
                                + " must be named exactly 'PetstoreAuthCredentials' to match the "
                                + "filename, per n8n's directory loader convention (DirectoryLoader.loadClass)");
            }
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
            List<Path> credentialFiles = find(target, "ApiKeyCredentials.credentials.ts");

            Assert.assertTrue(Files.exists(target.resolve("package.json")), "package.json should be generated");
            Assert.assertTrue(Files.exists(target.resolve("README.md")), "README.md should be generated");
            Assert.assertFalse(nodeFiles.isEmpty(), "a node .node.ts file should be generated");
            Assert.assertFalse(credentialFiles.isEmpty(), "a credentials .credentials.ts file should be generated");
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Regression test for T049 (convergence): a spec declaring more than one security scheme
     * must get one usable credential definition PER declared scheme, not only the single
     * highest-priority one. Petstore declares both an apiKey scheme ("api_key") and an oauth2
     * scheme ("petstore_auth"), so both must produce their own, correctly-shaped credential file.
     */
    @Test
    public void generatesOneCredentialPerDeclaredScheme() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            generate(target);

            List<Path> apiKeyFiles = find(target, "ApiKeyCredentials.credentials.ts");
            List<Path> oauthFiles = find(target, "PetstoreAuthCredentials.credentials.ts");

            Assert.assertEquals(apiKeyFiles.size(), 1, "expected exactly one ApiKeyCredentials.credentials.ts file");
            Assert.assertEquals(oauthFiles.size(), 1, "expected exactly one PetstoreAuthCredentials.credentials.ts file");

            String apiKeySource = Files.readString(apiKeyFiles.get(0));
            Assert.assertTrue(apiKeySource.contains("apiKeyCredentials"), "apiKey credential should use the apiKeyCredentials type name");
            Assert.assertTrue(apiKeySource.contains("API Key"), "apiKey credential should collect an API Key field");

            String oauthSource = Files.readString(oauthFiles.get(0));
            Assert.assertTrue(oauthSource.contains("petstoreAuthCredentials"), "oauth2 credential should use the petstoreAuthCredentials type name");
            Assert.assertTrue(oauthSource.contains("Access Token"), "oauth2 credential should collect an Access Token field");

            String packageJson = Files.readString(target.resolve("package.json"));
            Assert.assertTrue(packageJson.contains("dist/credentials/ApiKeyCredentials.credentials.js"));
            Assert.assertTrue(packageJson.contains("dist/credentials/PetstoreAuthCredentials.credentials.js"));

            String nodeSource = readNodeSource(target);
            Assert.assertTrue(nodeSource.contains("name: 'apiKeyCredentials'"));
            Assert.assertTrue(nodeSource.contains("name: 'petstoreAuthCredentials'"));
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
            // scheme ("petstore_auth"); apiKey has priority, so its correctly-shaped
            // credential definition must exist (spec.md US3 Acceptance Scenario 1 /
            // data-model.md CredentialDefinition) ...
            Assert.assertTrue(credentialsSource.contains("apiKey"));
            Assert.assertTrue(credentialsSource.contains("'api_key'"));
            Assert.assertTrue(credentialsSource.contains("headers"));

            // ... and getPetById, which declares `security: [api_key: []]` on itself in the
            // spec, must authenticate with that exact credential (spec.md Edge Cases #4).
            String nodeSource = readNodeSource(target);
            Assert.assertEquals(operationCredentialName(nodeSource, "getPetById"), "apiKeyCredentials");
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Regression test for T052 (convergence): the generated node used to apply only the single
     * highest-priority credential to every outgoing request, regardless of what security an
     * individual operation actually declared. Petstore's own spec declares `addPet`'s security
     * as `petstore_auth` (oauth2) and `getPetById`'s as `api_key`, so each must authenticate
     * with its OWN matching credential within the same generated file, per spec.md Edge Cases
     * #4 (2nd clause: "let the generated node reference the one appropriate per operation").
     */
    @Test
    public void appliesDifferentCredentialsToDifferentOperationsPerTheirOwnSecurityScheme() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            generate(target);
            String nodeSource = readNodeSource(target);

            Assert.assertEquals(operationCredentialName(nodeSource, "addPet"), "petstoreAuthCredentials");
            Assert.assertEquals(operationCredentialName(nodeSource, "updatePet"), "petstoreAuthCredentials");
            Assert.assertEquals(operationCredentialName(nodeSource, "getPetById"), "apiKeyCredentials");
            Assert.assertEquals(operationCredentialName(nodeSource, "getInventory"), "apiKeyCredentials");
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Extracts the credential name passed as apiRequest's 5th argument within the given
     * operation's `if (operation === '...') { ... }` branch in the generated node source.
     */
    private static String operationCredentialName(String nodeSource, String operationId) {
        String marker = "operation === '" + operationId + "'";
        int branchStart = nodeSource.indexOf(marker);
        Assert.assertTrue(branchStart >= 0, "expected an execute() branch for operation '" + operationId + "'");
        int callStart = nodeSource.indexOf("apiRequest.call(", branchStart);
        Assert.assertTrue(callStart >= 0, "expected an apiRequest.call(...) within the '" + operationId + "' branch");
        int callEnd = nodeSource.indexOf(";", callStart);
        String callText = nodeSource.substring(callStart, callEnd);
        int lastQuote = callText.lastIndexOf('\'');
        int secondLastQuote = callText.lastIndexOf('\'', lastQuote - 1);
        return callText.substring(secondLastQuote + 1, lastQuote);
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

    /**
     * Regression test for T047 (convergence): credentials.mustache's HTTP-basic branch -
     * including the mustache delimiter-switching needed to emit a literal
     * {@code {{$credentials.x}}} n8n expression - had never been exercised by any generation
     * run or test before this, since Petstore only declares apiKey+oauth2.
     */
    @Test
    public void generatesHttpBasicCredentialFromHttpBasicTestSpec() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            generate(target, "src/test/resources/3_0/http-basic-test.yaml");

            List<Path> credentialFiles = find(target, "HttpBasicCredentials.credentials.ts");
            Assert.assertEquals(credentialFiles.size(), 1, "expected exactly one HttpBasicCredentials.credentials.ts file");

            String source = Files.readString(credentialFiles.get(0));
            Assert.assertTrue(source.contains("export class HttpBasicCredentials "));
            Assert.assertTrue(source.contains("name: 'username'"));
            Assert.assertTrue(source.contains("name: 'password'"));
            Assert.assertTrue(source.contains("auth: {"));
            Assert.assertTrue(source.contains("username: '={{$credentials.username}}'"));
            Assert.assertTrue(source.contains("password: '={{$credentials.password}}'"));
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Regression test for T047 (convergence): credentials.mustache's HTTP-bearer branch had
     * never been exercised by any generation run or test before this.
     */
    @Test
    public void generatesHttpBearerCredentialFromPingBearerAuthSpec() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            generate(target, "src/test/resources/3_0/pingBearerAuth.yaml");

            List<Path> credentialFiles = find(target, "BearerAuthCredentials.credentials.ts");
            Assert.assertEquals(credentialFiles.size(), 1, "expected exactly one BearerAuthCredentials.credentials.ts file");

            String source = Files.readString(credentialFiles.get(0));
            Assert.assertTrue(source.contains("export class BearerAuthCredentials "));
            Assert.assertTrue(source.contains("name: 'token'"));
            Assert.assertTrue(source.contains("Authorization: '=Bearer {{$credentials.token}}'"));
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Regression test for T045 (convergence): an untagged operation used to be grouped under
     * the framework's literal fallback tag "default" (rendered as Resource "Default"), rather
     * than a Resource derived from its own path, per FR-003 / spec.md US2 Acceptance Scenario 3.
     */
    @Test
    public void groupsUntaggedOperationsByPathDerivedResource() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            generate(target, "src/test/resources/3_0/ping.yaml");
            String nodeSource = readNodeSource(target);

            Assert.assertFalse(nodeSource.contains("value: 'Default'"),
                    "an untagged operation must not fall back to the literal 'Default' Resource");
            Assert.assertTrue(nodeSource.contains("value: 'Ping'"),
                    "the untagged /ping operation should be grouped into a Resource derived from its path");
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Regression test for T045, covering the case the single-operation ping.yaml fixture can't:
     * two untagged operations under two DIFFERENT paths must become two DIFFERENT Resources
     * (path-derived), not all merged into one catch-all, per FR-003 / US2/AC3.
     */
    @Test
    public void groupsDifferentUntaggedPathsIntoDifferentResources() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            Path spec = writeTempSpec(target, "untagged-multi-path.yaml", ""
                    + "openapi: 3.0.1\n"
                    + "info:\n"
                    + "  title: untagged multi path test\n"
                    + "  version: '1.0'\n"
                    + "servers:\n"
                    + "  - url: 'http://localhost:8000/'\n"
                    + "paths:\n"
                    + "  /widgets:\n"
                    + "    get:\n"
                    + "      operationId: getWidgets\n"
                    + "      responses:\n"
                    + "        '200':\n"
                    + "          description: OK\n"
                    + "  /gadgets:\n"
                    + "    get:\n"
                    + "      operationId: getGadgets\n"
                    + "      responses:\n"
                    + "        '200':\n"
                    + "          description: OK\n");

            Path outputDir = target.resolve("out");
            generate(outputDir, spec.toAbsolutePath().toString());
            String nodeSource = readNodeSource(outputDir);

            Assert.assertTrue(nodeSource.contains("value: 'Widgets'"));
            Assert.assertTrue(nodeSource.contains("value: 'Gadgets'"));
            Assert.assertFalse(nodeSource.contains("value: 'Default'"));
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Regression test for T046 (convergence): two distinct OpenAPI tags that sanitize to the
     * same Resource label used to silently merge into one Resource; they must instead be
     * disambiguated into two distinct, selectable Resources, per FR-011 / spec.md Edge Cases #3.
     */
    @Test
    public void disambiguatesCollidingSanitizedTagNames() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            Path spec = writeTempSpec(target, "colliding-tags.yaml", ""
                    + "openapi: 3.0.1\n"
                    + "info:\n"
                    + "  title: colliding tags test\n"
                    + "  version: '1.0'\n"
                    + "servers:\n"
                    + "  - url: 'http://localhost:8000/'\n"
                    + "tags:\n"
                    + "  - name: Pet Store\n"
                    + "  - name: pet_store\n"
                    + "paths:\n"
                    + "  /a:\n"
                    + "    get:\n"
                    + "      operationId: getA\n"
                    + "      tags: ['Pet Store']\n"
                    + "      responses:\n"
                    + "        '200':\n"
                    + "          description: OK\n"
                    + "  /b:\n"
                    + "    get:\n"
                    + "      operationId: getB\n"
                    + "      tags: ['pet_store']\n"
                    + "      responses:\n"
                    + "        '200':\n"
                    + "          description: OK\n");

            Path outputDir = target.resolve("out");
            generate(outputDir, spec.toAbsolutePath().toString());
            String nodeSource = readNodeSource(outputDir);

            // Both tags camelize/sanitize to "PetStore"; the second one to be processed must be
            // disambiguated rather than silently merging into the first's Resource.
            long petStoreResourceOptions = Stream.of(nodeSource.split("\n"))
                    .filter(line -> line.contains("value: 'PetStore'") || line.matches(".*value: 'PetStore_[0-9a-f]+'.*"))
                    .count();
            Assert.assertEquals(petStoreResourceOptions, 2, "expected two distinct, disambiguated Resources for the colliding tags");
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Regression test for T048 (convergence): until now, the only proof that generated output
     * actually compiles was a human running `tsc --noEmit` by hand in an ephemeral sandbox
     * during this feature's development - there was no automated, repeatable check anywhere in
     * the project's test/build pipeline (plan.md Testing section / FR-009 / SC-002). This test
     * runs that same check as part of the normal `mvn test` run. It skips (rather than fails)
     * when npm/npx aren't on PATH, or when installing the type-only dependencies fails (e.g. no
     * network access) - those are environment limitations, not evidence the generated code is
     * broken - but FAILS the build if `tsc --noEmit` itself reports compile errors.
     */
    @Test
    public void generatedSampleCompilesWithTypeScript() throws IOException, InterruptedException {
        if (!isCommandAvailable("npm") || !isCommandAvailable("npx")) {
            throw new SkipException("npm/npx not available in this environment; skipping the generated-TypeScript compile check (FR-009/SC-002)");
        }

        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            generate(target);

            int installExit = runProcess(target, "npm", "install", "--no-audit", "--no-fund", "n8n-workflow@latest", "typescript@latest");
            if (installExit != 0) {
                throw new SkipException("could not install n8n-workflow/typescript (no network access?); skipping the generated-TypeScript compile check");
            }

            int tscExit = runProcess(target, "npx", "tsc", "--noEmit");
            Assert.assertEquals(tscExit, 0, "the generated package's TypeScript must compile cleanly with `tsc --noEmit` (FR-009/SC-002)");
        } finally {
            deleteRecursively(target);
        }
    }

    private static boolean isCommandAvailable(String command) {
        try {
            Process process = new ProcessBuilder(command, "--version").redirectErrorStream(true).start();
            return process.waitFor() == 0;
        } catch (IOException e) {
            return false;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return false;
        }
    }

    private static int runProcess(Path cwd, String... command) throws IOException, InterruptedException {
        Process process = new ProcessBuilder(command)
                .directory(cwd.toFile())
                .redirectErrorStream(true)
                .start();
        String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        int exitCode = process.waitFor();
        if (exitCode != 0) {
            System.out.println("Command `" + String.join(" ", command) + "` exited " + exitCode + ":\n" + output);
        }
        return exitCode;
    }

    /**
     * Regression test for T050 (convergence): the generic-JSON fallback for shapes the
     * generator can't map precisely (FR-006 / spec.md Edge Cases #2) had previously only ever
     * been exercised incidentally, via a plain object request body, never an actual
     * oneOf/anyOf shape. Uses the existing oneOf_twoPrimitives.yaml fixture (a request body
     * that is oneOf two primitive string variants).
     */
    @Test
    public void mapsOneOfRequestBodyToGenericJsonFallback() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            generate(target, "src/test/resources/3_0/oneOf_twoPrimitives.yaml");
            String nodeSource = readNodeSource(target);

            Assert.assertTrue(nodeSource.contains("name: 'MyExamplePostRequest'"),
                    "expected a field for the oneOf request body");
            int fieldIndex = nodeSource.indexOf("name: 'MyExamplePostRequest'");
            String fieldBlock = nodeSource.substring(fieldIndex, Math.min(nodeSource.length(), fieldIndex + 200));
            Assert.assertTrue(fieldBlock.contains("type: 'json'"),
                    "a oneOf request body the generator can't map precisely must fall back to the generic JSON field type, not fail generation or silently drop the field");
        } finally {
            deleteRecursively(target);
        }
    }

    private static Path writeTempSpec(Path dir, String filename, String content) throws IOException {
        Path file = dir.resolve(filename);
        Files.createDirectories(dir);
        Files.write(file, content.getBytes(StandardCharsets.UTF_8));
        return file;
    }

    private static void generate(Path target) {
        generate(target, PETSTORE_SPEC);
    }

    private static void generate(Path target, String inputSpec) {
        ClientOptInput clientOptInput = new CodegenConfigurator()
                .setGeneratorName("n8n")
                .setInputSpec(inputSpec)
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
        List<Path> files = find(target, "ApiKeyCredentials.credentials.ts");
        Assert.assertFalse(files.isEmpty(), "expected a generated ApiKeyCredentials.credentials.ts file");
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
