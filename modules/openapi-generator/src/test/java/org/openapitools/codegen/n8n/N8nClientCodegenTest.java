package org.openapitools.codegen.n8n;

import org.openapitools.codegen.ClientOptInput;
import org.openapitools.codegen.CodegenConfig;
import org.openapitools.codegen.CodegenConfigLoader;
import org.openapitools.codegen.DefaultGenerator;
import org.openapitools.codegen.config.CodegenConfigurator;
import org.openapitools.codegen.config.GlobalSettings;
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
                String expectedClass = expectedClassNameOf(credentialFile, ".credentials.ts");
                Assert.assertTrue(
                        source.contains("export class " + expectedClass + " "),
                        "the exported class in " + credentialFile.getFileName()
                                + " must be named exactly '" + expectedClass + "' to match the filename, "
                                + "per n8n's directory loader convention (DirectoryLoader.loadClass)");
            }
            for (Path credentialFile : find(target, "PetstoreAuthCredentials.credentials.ts")) {
                String source = Files.readString(credentialFile);
                String expectedClass = expectedClassNameOf(credentialFile, ".credentials.ts");
                Assert.assertTrue(
                        source.contains("export class " + expectedClass + " "),
                        "the exported class in " + credentialFile.getFileName()
                                + " must be named exactly '" + expectedClass + "' to match the "
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
            Assert.assertTrue(apiKeySource.contains("ApiKeyCredentials"), "apiKey credential should use the *ApiKeyCredentials type name");
            Assert.assertTrue(apiKeySource.contains("API Key"), "apiKey credential should collect an API Key field");

            String oauthSource = Files.readString(oauthFiles.get(0));
            Assert.assertTrue(oauthSource.contains("PetstoreAuthCredentials"), "oauth2 credential should use the *PetstoreAuthCredentials type name");
            Assert.assertTrue(oauthSource.contains("Access Token"), "oauth2 credential should collect an Access Token field");

            String packageJson = Files.readString(target.resolve("package.json"));
            Assert.assertTrue(packageJson.contains("ApiKeyCredentials.credentials.js"));
            Assert.assertTrue(packageJson.contains("PetstoreAuthCredentials.credentials.js"));

            String nodeSource = readNodeSource(target);
            Assert.assertTrue(nodeSource.contains("ApiKeyCredentials'"));
            Assert.assertTrue(nodeSource.contains("PetstoreAuthCredentials'"));
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
            Assert.assertTrue(operationCredentialName(nodeSource, "getPetById").endsWith("ApiKeyCredentials"));
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

            Assert.assertTrue(operationCredentialName(nodeSource, "addPet").endsWith("PetstoreAuthCredentials"));
            Assert.assertTrue(operationCredentialName(nodeSource, "updatePet").endsWith("PetstoreAuthCredentials"));
            Assert.assertTrue(operationCredentialName(nodeSource, "getPetById").endsWith("ApiKeyCredentials"));
            Assert.assertTrue(operationCredentialName(nodeSource, "getInventory").endsWith("ApiKeyCredentials"));
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Regression test for T056 (convergence): operations that genuinely need no authentication
     * - either because they declare no `security:` key at all with no spec-wide default to
     * inherit, or because they explicitly opt out with `security: []` - must send an
     * unauthenticated request, not be force-authenticated with the node's fallback credential.
     * Petstore's own spec exercises the first case directly: placeOrder, getOrderById,
     * getUserByName, loginUser, and deleteOrder declare no security at all (confirmed against
     * the live API, which accepts these unauthenticated).
     */
    @Test
    public void leavesOperationsWithNoDeclaredSecurityUnauthenticated() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            generate(target);
            String nodeSource = readNodeSource(target);

            for (String operationId : new String[]{"placeOrder", "getOrderById", "getUserByName", "loginUser", "deleteOrder"}) {
                Assert.assertEquals(operationCredentialName(nodeSource, operationId), "",
                        "operation '" + operationId + "' declares no security in the spec and must send an unauthenticated request");
            }
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Regression test for T056 (convergence), the explicit-opt-out variant: an operation with
     * `security: []` must also be unauthenticated, even when the spec declares a global default
     * security scheme that would otherwise apply to every other operation.
     */
    @Test
    public void leavesExplicitlyUnsecuredOperationUnauthenticatedEvenWithGlobalDefault() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            Path spec = writeTempSpec(target, "explicit-no-security.yaml", ""
                    + "openapi: 3.0.1\n"
                    + "info:\n"
                    + "  title: explicit no security test\n"
                    + "  version: '1.0'\n"
                    + "servers:\n"
                    + "  - url: 'http://localhost:8000/'\n"
                    + "security:\n"
                    + "  - apiKeyAuth: []\n"
                    + "paths:\n"
                    + "  /secured:\n"
                    + "    get:\n"
                    + "      operationId: getSecured\n"
                    + "      responses:\n"
                    + "        '200':\n"
                    + "          description: OK\n"
                    + "  /public:\n"
                    + "    get:\n"
                    + "      operationId: getPublic\n"
                    + "      security: []\n"
                    + "      responses:\n"
                    + "        '200':\n"
                    + "          description: OK\n"
                    + "components:\n"
                    + "  securitySchemes:\n"
                    + "    apiKeyAuth:\n"
                    + "      type: apiKey\n"
                    + "      name: x-api-key\n"
                    + "      in: header\n");

            Path outputDir = target.resolve("out");
            generate(outputDir, spec.toAbsolutePath().toString());
            String nodeSource = readNodeSource(outputDir);

            Assert.assertTrue(operationCredentialName(nodeSource, "getSecured").endsWith("ApiKeyAuthCredentials"),
                    "getSecured inherits the spec-wide default security scheme");
            Assert.assertEquals(operationCredentialName(nodeSource, "getPublic"), "",
                    "getPublic explicitly opts out with security: [] and must stay unauthenticated");
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

    /**
     * Regression test for T065 (convergence): a security scheme this generator cannot map at
     * all (e.g. {@code openIdConnect} - lands in {@code schemePriority()}'s unsupported/
     * {@code MAX_VALUE} bucket, unlike Petstore's own oauth2 scheme which DOES get a credential
     * via the manual fallback) used to be indistinguishable, in the generated README, from a
     * spec that declares no security at all: the "no security scheme" message rendered
     * regardless, actively claiming something false and hiding the real limitation, contrary to
     * FR-007/FR-010 and spec.md US3 Acceptance Scenario 3.
     */
    @Test
    public void documentsGenuinelyUnmappableSchemeInsteadOfClaimingNoSecurity() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            Path spec = writeTempSpec(target, "unmappable-scheme.yaml", ""
                    + "openapi: 3.0.1\n"
                    + "info:\n"
                    + "  title: unmappable scheme test\n"
                    + "  version: '1.0'\n"
                    + "servers:\n"
                    + "  - url: 'http://localhost:8000/'\n"
                    + "paths:\n"
                    + "  /widgets:\n"
                    + "    get:\n"
                    + "      operationId: getWidgets\n"
                    + "      security:\n"
                    + "        - openIdAuth: []\n"
                    + "      responses:\n"
                    + "        '200':\n"
                    + "          description: OK\n"
                    + "components:\n"
                    + "  securitySchemes:\n"
                    + "    openIdAuth:\n"
                    + "      type: openIdConnect\n"
                    + "      openIdConnectUrl: 'https://auth.example.com/.well-known/openid-configuration'\n");

            Path outputDir = target.resolve("out");
            generate(outputDir, spec.toAbsolutePath().toString());

            Assert.assertTrue(find(outputDir, ".credentials.ts").isEmpty(),
                    "an entirely unmappable scheme must not produce a credential file");

            String readme = Files.readString(outputDir.resolve("README.md"));
            String lowerReadme = readme.toLowerCase(java.util.Locale.ROOT);
            Assert.assertFalse(lowerReadme.contains("declares no security scheme"),
                    "the README must not claim no security scheme was declared when one actually was, just unmappable");
            Assert.assertTrue(lowerReadme.contains("openidauth"),
                    "the README must name the unmappable scheme");
            Assert.assertTrue(lowerReadme.contains("limitation"),
                    "the README must document this as a limitation, per FR-010");
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
            String expectedClass = expectedClassNameOf(credentialFiles.get(0), ".credentials.ts");
            Assert.assertTrue(source.contains("export class " + expectedClass + " "));
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
            String expectedClass = expectedClassNameOf(credentialFiles.get(0), ".credentials.ts");
            Assert.assertTrue(source.contains("export class " + expectedClass + " "));
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

    /**
     * Regression test for T062 (convergence): a plain array-of-strings parameter - Petstore's
     * own {@code findPetsByStatus} {@code status} query parameter (array of enum strings) - used
     * to fall into the exact same generic-JSON fallback as a genuinely-unmappable {@code oneOf}
     * shape (see {@link #mapsOneOfRequestBodyToGenericJsonFallback}), even though it is not such
     * a shape: FR-006 requires "array" to map to an "appropriate" node field type. It must now
     * render as n8n's {@code collection} type (with {@code multipleValues: true}), not {@code
     * json}.
     */
    @Test
    public void mapsArrayParameterToCollectionFieldType() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            generate(target);
            String nodeSource = readNodeSource(target);

            Assert.assertTrue(nodeSource.contains("name: 'status'"), "expected a field for findPetsByStatus's 'status' parameter");
            int fieldIndex = nodeSource.indexOf("name: 'status'");
            String fieldBlock = nodeSource.substring(fieldIndex, Math.min(nodeSource.length(), fieldIndex + 400));
            Assert.assertTrue(fieldBlock.contains("type: 'collection'"),
                    "a plain array-of-strings parameter must map to n8n's 'collection' field type, not the generic JSON fallback reserved for genuinely-unmappable shapes");
            Assert.assertTrue(fieldBlock.contains("multipleValues: true"),
                    "the collection field must allow adding more than one value");
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Regression test for T067 (convergence): a parameter's spec-declared OpenAPI {@code
     * default} must pre-fill the generated field, not a generic type-based filler -
     * {@code data-model.md}'s "Field" entity documents this mapping explicitly, but before this
     * fix a boolean param declared {@code default: true} rendered {@code default: false}, an
     * integer declared {@code default: 25} rendered {@code default: 0}, and a string declared
     * {@code default: 'desc'} rendered {@code default: ''}.
     */
    @Test
    public void rendersSpecDeclaredDefaultForScalarFields() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            Path spec = writeTempSpec(target, "scalar-defaults.yaml", ""
                    + "openapi: 3.0.1\n"
                    + "info:\n"
                    + "  title: scalar defaults test\n"
                    + "  version: '1.0'\n"
                    + "servers:\n"
                    + "  - url: 'http://localhost:8000/'\n"
                    + "paths:\n"
                    + "  /widgets:\n"
                    + "    get:\n"
                    + "      operationId: getWidgets\n"
                    + "      parameters:\n"
                    + "        - name: includeArchived\n"
                    + "          in: query\n"
                    + "          schema:\n"
                    + "            type: boolean\n"
                    + "            default: true\n"
                    + "        - name: pageSize\n"
                    + "          in: query\n"
                    + "          schema:\n"
                    + "            type: integer\n"
                    + "            default: 25\n"
                    + "        - name: sortOrder\n"
                    + "          in: query\n"
                    + "          schema:\n"
                    + "            type: string\n"
                    + "            default: 'desc'\n"
                    + "      responses:\n"
                    + "        '200':\n"
                    + "          description: OK\n");

            Path outputDir = target.resolve("out");
            generate(outputDir, spec.toAbsolutePath().toString());
            String nodeSource = readNodeSource(outputDir);

            Assert.assertTrue(fieldDefaultBlock(nodeSource, "includeArchived").contains("default: true"),
                    "a boolean parameter's declared default must be used, not the generic 'false' filler");
            Assert.assertTrue(fieldDefaultBlock(nodeSource, "pageSize").contains("default: 25"),
                    "a number parameter's declared default must be used, not the generic '0' filler");
            Assert.assertTrue(fieldDefaultBlock(nodeSource, "sortOrder").contains("default: 'desc'"),
                    "a string parameter's declared default must be used, not the generic '' filler");
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Regression test for T067 (convergence), the enum variant: before this fix, an {@code
     * options}-typed field's default always used the FIRST enum choice regardless of what the
     * spec actually declared as the default, e.g. an enum declared {@code default: published}
     * (not its first choice) rendered {@code default: "draft"} instead.
     */
    @Test
    public void rendersSpecDeclaredDefaultForEnumFieldNotFirstChoice() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            Path spec = writeTempSpec(target, "enum-default.yaml", ""
                    + "openapi: 3.0.1\n"
                    + "info:\n"
                    + "  title: enum default test\n"
                    + "  version: '1.0'\n"
                    + "servers:\n"
                    + "  - url: 'http://localhost:8000/'\n"
                    + "paths:\n"
                    + "  /widgets:\n"
                    + "    get:\n"
                    + "      operationId: getWidgets\n"
                    + "      parameters:\n"
                    + "        - name: status\n"
                    + "          in: query\n"
                    + "          schema:\n"
                    + "            type: string\n"
                    + "            enum: [draft, published, archived]\n"
                    + "            default: published\n"
                    + "      responses:\n"
                    + "        '200':\n"
                    + "          description: OK\n");

            Path outputDir = target.resolve("out");
            generate(outputDir, spec.toAbsolutePath().toString());
            String nodeSource = readNodeSource(outputDir);

            Assert.assertTrue(fieldDefaultBlock(nodeSource, "status").contains("default: \"published\""),
                    "an enum parameter's declared default must be used even when it isn't the first enum choice");
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Extracts the text from a field's {@code name: '<paramName>'} line up to its {@code
     * default:} line, so a test can assert on the rendered default without depending on the
     * field's other property ordering.
     */
    private static String fieldDefaultBlock(String nodeSource, String paramName) {
        String marker = "name: '" + paramName + "'";
        int fieldIndex = nodeSource.indexOf(marker);
        Assert.assertTrue(fieldIndex >= 0, "expected a field for parameter '" + paramName + "'");
        int defaultIndex = nodeSource.indexOf("default:", fieldIndex);
        Assert.assertTrue(defaultIndex >= 0, "expected a default: entry for parameter '" + paramName + "'");
        int lineEnd = nodeSource.indexOf("\n", defaultIndex);
        return nodeSource.substring(defaultIndex, lineEnd < 0 ? nodeSource.length() : lineEnd);
    }

    /**
     * Regression test for T063 (convergence): an OAuth2 security scheme declaring exactly one
     * of the "common flows" (clientCredentials/authorizationCode) that research.md §3 / spec.md
     * Assumptions commit to automating must get n8n's generic {@code oAuth2Api} credential
     * wiring (pre-filled with the spec's declared authorization/token URLs and scopes), not the
     * manual-paste-an-access-token fallback used for every other OAuth2 flow.
     */
    @Test
    public void autoWiresOAuth2CredentialForClientCredentialsFlow() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            Path spec = writeTempSpec(target, "oauth2-client-credentials.yaml", ""
                    + "openapi: 3.0.1\n"
                    + "info:\n"
                    + "  title: oauth2 client credentials test\n"
                    + "  version: '1.0'\n"
                    + "servers:\n"
                    + "  - url: 'http://localhost:8000/'\n"
                    + "paths:\n"
                    + "  /widgets:\n"
                    + "    get:\n"
                    + "      operationId: getWidgets\n"
                    + "      security:\n"
                    + "        - clientAuth: ['read:widgets']\n"
                    + "      responses:\n"
                    + "        '200':\n"
                    + "          description: OK\n"
                    + "components:\n"
                    + "  securitySchemes:\n"
                    + "    clientAuth:\n"
                    + "      type: oauth2\n"
                    + "      flows:\n"
                    + "        clientCredentials:\n"
                    + "          tokenUrl: 'https://auth.example.com/token'\n"
                    + "          scopes:\n"
                    + "            'read:widgets': read widgets\n");

            Path outputDir = target.resolve("out");
            generate(outputDir, spec.toAbsolutePath().toString());

            List<Path> credentialFiles = find(outputDir, "ClientAuthCredentials.credentials.ts");
            Assert.assertEquals(credentialFiles.size(), 1, "expected exactly one ClientAuthCredentials.credentials.ts file");
            String source = Files.readString(credentialFiles.get(0));

            Assert.assertTrue(source.contains("extends = ['oAuth2Api']"),
                    "a single-flow clientCredentials OAuth2 scheme must auto-wire n8n's generic oAuth2Api credential");
            Assert.assertTrue(source.contains("default: 'clientCredentials'"), "the grant type must be pre-filled as clientCredentials");
            Assert.assertTrue(source.contains("https://auth.example.com/token"), "the token URL must be pre-filled from the spec");
            Assert.assertTrue(source.contains("'read:widgets'") || source.contains("read:widgets"), "the declared scope must be pre-filled from the spec");
            Assert.assertFalse(source.contains("name: 'accessToken'"),
                    "an auto-wired OAuth2 credential must not also collect a manually-pasted access token");
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Regression test for T063 (convergence), the ambiguous-multi-flow variant: a single OAuth2
     * security scheme declaring BOTH clientCredentials and authorizationCode at once (the
     * framework builds one CodegenSecurity per declared flow, all sharing the same scheme name)
     * is ambiguous about which flow n8n should actually perform, so it must keep the
     * manual-paste-an-access-token fallback rather than guessing - and must still produce exactly
     * one credential file for the scheme, not one per flow.
     */
    @Test
    public void keepsManualOAuth2CredentialWhenSchemeDeclaresMultipleFlows() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            Path spec = writeTempSpec(target, "oauth2-multi-flow.yaml", ""
                    + "openapi: 3.0.1\n"
                    + "info:\n"
                    + "  title: oauth2 multi flow test\n"
                    + "  version: '1.0'\n"
                    + "servers:\n"
                    + "  - url: 'http://localhost:8000/'\n"
                    + "paths:\n"
                    + "  /widgets:\n"
                    + "    get:\n"
                    + "      operationId: getWidgets\n"
                    + "      security:\n"
                    + "        - multiAuth: ['read:widgets']\n"
                    + "      responses:\n"
                    + "        '200':\n"
                    + "          description: OK\n"
                    + "components:\n"
                    + "  securitySchemes:\n"
                    + "    multiAuth:\n"
                    + "      type: oauth2\n"
                    + "      flows:\n"
                    + "        clientCredentials:\n"
                    + "          tokenUrl: 'https://auth.example.com/token'\n"
                    + "          scopes:\n"
                    + "            'read:widgets': read widgets\n"
                    + "        authorizationCode:\n"
                    + "          authorizationUrl: 'https://auth.example.com/authorize'\n"
                    + "          tokenUrl: 'https://auth.example.com/token'\n"
                    + "          scopes:\n"
                    + "            'read:widgets': read widgets\n");

            Path outputDir = target.resolve("out");
            generate(outputDir, spec.toAbsolutePath().toString());

            List<Path> credentialFiles = find(outputDir, "MultiAuthCredentials.credentials.ts");
            Assert.assertEquals(credentialFiles.size(), 1, "an ambiguous multi-flow scheme must still produce exactly one credential file");
            String source = Files.readString(credentialFiles.get(0));

            Assert.assertFalse(source.contains("extends = ['oAuth2Api']"),
                    "a scheme declaring more than one flow at once must not auto-wire n8n's generic oAuth2Api credential (ambiguous which flow to perform)");
            Assert.assertTrue(source.contains("name: 'accessToken'"),
                    "an ambiguous multi-flow scheme must keep the manual-paste-an-access-token fallback");
        } finally {
            deleteRecursively(target);
        }
    }

    /**
     * Regression test for T066 (convergence): contracts/generator-cli-options.md's Failure
     * Contract documents, as a MUST, that a spec with zero operations still produces a valid,
     * loadable package rather than the generator throwing - this had no automated test despite
     * being a documented contract clause.
     */
    @Test
    public void generatesValidPackageFromZeroOperationSpec() throws IOException, InterruptedException {
        if (!isCommandAvailable("npm") || !isCommandAvailable("npx")) {
            throw new SkipException("npm/npx not available in this environment; skipping the zero-operations compile check");
        }

        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            Path spec = writeTempSpec(target, "zero-operations.yaml", ""
                    + "openapi: 3.0.1\n"
                    + "info:\n"
                    + "  title: zero operations test\n"
                    + "  version: '1.0'\n"
                    + "servers:\n"
                    + "  - url: 'http://localhost:8000/'\n"
                    + "paths: {}\n");

            Path outputDir = target.resolve("out");
            generate(outputDir, spec.toAbsolutePath().toString());

            Assert.assertTrue(Files.exists(outputDir.resolve("package.json")));
            Assert.assertTrue(Files.exists(outputDir.resolve("README.md")));
            Assert.assertFalse(find(outputDir, ".node.ts").isEmpty(), "a node file should still be generated for a zero-operation spec");

            int installExit = runProcess(outputDir, "npm", "install", "--no-audit", "--no-fund", "n8n-workflow@latest", "typescript@latest");
            if (installExit != 0) {
                throw new SkipException("could not install n8n-workflow/typescript (no network access?); skipping the zero-operations compile check");
            }
            int tscExit = runProcess(outputDir, "npx", "tsc", "--noEmit");
            Assert.assertEquals(tscExit, 0, "the generated package for a zero-operation spec must still compile cleanly");
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

    /**
     * Regression test for T060 (convergence): confirmed by actually running
     * `generate -g n8n ... --dry-run` that {@code writeCredentialFile}'s direct
     * {@code Files.write} call bypassed the framework's dry-run mode entirely, writing real
     * credential files to disk despite `--dry-run` - and the framework's own "Dry Run Results"
     * report didn't even list them, since they never went through the tracked
     * {@code templateProcessor}. No credential file (or any file at all) should be written to
     * the target directory when dry-run is enabled, per FR-002. Extended for T061: the node and
     * GenericFunctions files moved to the same manual-write path as credentials, so they must
     * honor dry-run too.
     */
    @Test
    public void writesNoCredentialFileInDryRunMode() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            generateDryRun(target);
            Assert.assertTrue(find(target, "ApiKeyCredentials.credentials.ts").isEmpty(),
                    "no credential file should be written to disk in dry-run mode");
            Assert.assertTrue(find(target, "PetstoreAuthCredentials.credentials.ts").isEmpty(),
                    "no credential file should be written to disk in dry-run mode");
            Assert.assertTrue(find(target, "Api.node.ts").isEmpty(),
                    "no node file should be written to disk in dry-run mode");
            Assert.assertTrue(find(target, "GenericFunctions.ts").isEmpty(),
                    "no GenericFunctions file should be written to disk in dry-run mode");
        } finally {
            GlobalSettings.clearProperty("dryRun");
            deleteRecursively(target);
        }
    }

    private static void generateDryRun(Path target) {
        ClientOptInput clientOptInput = new CodegenConfigurator()
                .setGeneratorName("n8n")
                .setInputSpec(PETSTORE_SPEC)
                .setOutputDir(target.toAbsolutePath().toString())
                .toClientOptInput();

        new DefaultGenerator(true).opts(clientOptInput).generate();
    }

    /**
     * Regression test for T061: the generated node's class name, file name, and
     * {@code INodeTypeDescription.name} field used to be hardcoded ({@code Api} /
     * {@code Api.node.ts} / {@code 'api'}) regardless of the source spec, so installing two
     * generated packages side-by-side in the same n8n instance caused the second one's
     * registration to silently fail (the same filename/classname loader convention that caught
     * the bug fixed under T049's work). Each must now be derived from the spec's own title via
     * nodePrefix (contracts/generator-cli-options.md), so two differently-titled specs produce
     * distinct, non-colliding identifiers.
     */
    @Test
    public void differentlyTitledSpecsProduceDistinctNodeIdentifiers() throws IOException {
        Path target = Files.createTempDirectory("n8n-client-codegen-test");
        try {
            Path specA = writeTempSpec(target, "spec-a.yaml", ""
                    + "openapi: 3.0.1\n"
                    + "info:\n"
                    + "  title: First Example API\n"
                    + "  version: '1.0'\n"
                    + "servers:\n"
                    + "  - url: 'http://localhost:8000/'\n"
                    + "paths:\n"
                    + "  /widgets:\n"
                    + "    get:\n"
                    + "      operationId: getWidgets\n"
                    + "      responses:\n"
                    + "        '200':\n"
                    + "          description: OK\n");
            Path specB = writeTempSpec(target, "spec-b.yaml", ""
                    + "openapi: 3.0.1\n"
                    + "info:\n"
                    + "  title: Second Example API\n"
                    + "  version: '1.0'\n"
                    + "servers:\n"
                    + "  - url: 'http://localhost:8000/'\n"
                    + "paths:\n"
                    + "  /gadgets:\n"
                    + "    get:\n"
                    + "      operationId: getGadgets\n"
                    + "      responses:\n"
                    + "        '200':\n"
                    + "          description: OK\n");

            Path outA = target.resolve("out-a");
            Path outB = target.resolve("out-b");
            generate(outA, specA.toAbsolutePath().toString());
            generate(outB, specB.toAbsolutePath().toString());

            List<Path> nodeFilesA = find(outA, "Api.node.ts");
            List<Path> nodeFilesB = find(outB, "Api.node.ts");
            Assert.assertEquals(nodeFilesA.size(), 1, "expected exactly one node file for spec A");
            Assert.assertEquals(nodeFilesB.size(), 1, "expected exactly one node file for spec B");

            String nodeClassA = expectedClassNameOf(nodeFilesA.get(0), ".node.ts");
            String nodeClassB = expectedClassNameOf(nodeFilesB.get(0), ".node.ts");
            Assert.assertNotEquals(nodeClassA, nodeClassB,
                    "two differently-titled specs must produce different node class/file names");

            String nodeSourceA = Files.readString(nodeFilesA.get(0));
            String nodeSourceB = Files.readString(nodeFilesB.get(0));
            Assert.assertTrue(nodeSourceA.contains("export class " + nodeClassA + " "));
            Assert.assertTrue(nodeSourceB.contains("export class " + nodeClassB + " "));

            String nodeTypeNameA = lowerFirstForTest(nodeClassA);
            String nodeTypeNameB = lowerFirstForTest(nodeClassB);
            Assert.assertTrue(nodeSourceA.contains("name: '" + nodeTypeNameA + "'"));
            Assert.assertTrue(nodeSourceB.contains("name: '" + nodeTypeNameB + "'"));
            Assert.assertNotEquals(nodeTypeNameA, nodeTypeNameB,
                    "two differently-titled specs must produce different INodeTypeDescription.name values");
        } finally {
            deleteRecursively(target);
        }
    }

    private static String lowerFirstForTest(String value) {
        if (value == null || value.isEmpty()) {
            return value;
        }
        return Character.toLowerCase(value.charAt(0)) + value.substring(1);
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

    /**
     * Matches by filename SUFFIX, not exact equality: every node/credential file name is now
     * prefixed with the spec-derived {@code nodePrefix} (T061), so e.g. "Api.node.ts" still
     * finds "OpenapiPetstoreApi.node.ts" without the test needing to know the exact prefix.
     */
    private static List<Path> find(Path root, String filenameSuffix) throws IOException {
        try (Stream<Path> stream = Files.walk(root)) {
            return stream.filter(Files::isRegularFile)
                    .filter(p -> p.getFileName().toString().endsWith(filenameSuffix))
                    .collect(Collectors.toList());
        }
    }

    private static String readNodeSource(Path target) throws IOException {
        List<Path> files = find(target, "Api.node.ts");
        Assert.assertFalse(files.isEmpty(), "expected a generated *Api.node.ts file");
        return Files.readString(files.get(0));
    }

    private static String readCredentialsSource(Path target) throws IOException {
        List<Path> files = find(target, "ApiKeyCredentials.credentials.ts");
        Assert.assertFalse(files.isEmpty(), "expected a generated *ApiKeyCredentials.credentials.ts file");
        return Files.readString(files.get(0));
    }

    /** Derives the class name a generated file must export, per n8n's filename-matching
     * loader convention, directly from the file's own name (not a hardcoded expectation),
     * so this works regardless of the spec-derived nodePrefix. */
    private static String expectedClassNameOf(Path file, String extensionSuffix) {
        String filename = file.getFileName().toString();
        return filename.substring(0, filename.length() - extensionSuffix.length());
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
