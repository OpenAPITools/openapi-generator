/*
 * Copyright 2020 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.plugin;

import org.apache.commons.io.FileUtils;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.execution.DefaultMavenExecutionRequest;
import org.apache.maven.execution.MavenExecutionRequest;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingRequest;
import org.apache.maven.repository.internal.MavenRepositorySystemUtils;
import org.eclipse.aether.DefaultRepositorySystemSession;
import org.eclipse.aether.internal.impl.DefaultLocalPathComposer;
import org.eclipse.aether.internal.impl.SimpleLocalRepositoryManagerFactory;
import org.eclipse.aether.repository.LocalRepository;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class CodeGenMojoTest extends BaseTestCase {
    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    public void testCommonConfigurationWithFileInputSpec() throws Exception {
        testCommonConfiguration("file");
    }

    public void testCommonConfigurationWithResourceInputSpec() throws Exception {
        testCommonConfiguration("resource");
    }

    public void testCommonConfigurationWithResourceExternalRefInputSpec() throws Exception {
        testCommonConfiguration("resource-external-ref");
    }

    public void testCommonConfigurationWithJARInputSpec() throws Exception {
        testCommonConfiguration("jar");
    }

    @SuppressWarnings("unchecked")
    private void testCommonConfiguration(String profile) throws Exception {
        CodeGenMojo mojo = loadMojo(newTempFolder(), "src/test/resources/default", profile);
        mojo.execute();
        assertEquals("java", getVariableValueFromObject(mojo, "generatorName"));
        assertEquals("jersey2", getVariableValueFromObject(mojo, "library"));
        assertEquals("Suffix", getVariableValueFromObject(mojo, "apiNameSuffix"));
        assertEquals("remote.org.openapitools.client.api", getVariableValueFromObject(mojo, "apiPackage"));
        assertEquals("remote.org.openapitools.client.model", getVariableValueFromObject(mojo, "modelPackage"));
        assertEquals("remote.org.openapitools.client", getVariableValueFromObject(mojo, "invokerPackage"));

        Map<String, Object> configOptions = (Map<String, Object>) getVariableValueFromObject(mojo, "configOptions");
        assertNotNull(configOptions);
        assertEquals("joda", configOptions.get("dateLibrary"));
    }

    public void testHashGenerationFileContainsExecutionId() throws Exception {
        // GIVEN
        final Path tempDir = newTempFolder();
        CodeGenMojo mojo = loadMojo(tempDir, "src/test/resources/default", "file", "executionId");

        // WHEN
        mojo.execute();

        // THEN
        assertTrue(Files.exists(tempDir.resolve(
                "target/generated-sources/common-maven/remote-openapi/.openapi-generator/petstore.yaml-executionId.sha256"
        )));
    }

    /**
     * For a Pom file which refers to an input file which will be on the classpath, as opposed to a file path,
     * test that the spec is not regenerated when the hash has not changed.
     */
    public void testSkipRegenerationForClasspathSpecFileNoChange() throws Exception {
        //GIVEN
        /* Set up the mojo */
        final Path tempDir = newTempFolder();
        final CodeGenMojo mojo = loadMojo(tempDir, "src/test/resources/classpath", null, "executionId");

        /* Perform an initial generation */
        mojo.execute();

        /* Check the hash file was created */
        final Path generatedDir = tempDir.resolve("target/generated-sources/common-maven/remote-openapi");
        assertTrue(Files.exists(
                generatedDir.resolve(".openapi-generator/petstore-on-classpath.yaml-executionId.sha256")
        ));

        /* Remove the generated source */
        try (Stream<Path> files = Files.walk(generatedDir.resolve("src"))) {
            //noinspection ResultOfMethodCallIgnored
            files.sorted(Comparator.reverseOrder()).map(Path::toFile).forEach(File::delete);
        }

        // WHEN
        /* Execute the mojo again */
        mojo.execute();

        // THEN
        /* Verify that the source directory has not been repopulated. If it has then we generated code again */
        assertFalse("src directory should not have been regenerated", Files.exists(generatedDir.resolve("src")));
    }

    /**
     * For a Pom file which refers to an input file which will be on the classpath, as opposed to a file path,
     * test that the generated source is regenerated when the hash has changed.
     */
    public void testSkipRegenerationForClasspathSpecFileWithChange() throws Exception {
        //GIVEN
        /* Set up the mojo */
        final Path tempDir = newTempFolder();
        final CodeGenMojo mojo = loadMojo(tempDir, "src/test/resources/classpath", null, "executionId");

        /* Perform an initial generation */
        mojo.execute();

        /* Check the hash file was created, proving a generation occurred */
        final Path generatedDir = tempDir.resolve("target/generated-sources/common-maven/remote-openapi");
        assertTrue(Files.exists(generatedDir.resolve(".openapi-generator/petstore-on-classpath.yaml-executionId.sha256")));

        /* Update the hash contents to be a different value, simulating a spec change */
        Files.write(
                generatedDir.resolve(".openapi-generator/petstore-on-classpath.yaml-executionId.sha256"),
                List.of("bd1bf4a953c858f9d47b67ed6029daacf1707e5cbd3d2e4b01383ba30363366f")
        );

        /* Remove the generated source */
        try (Stream<Path> files = Files.walk(generatedDir.resolve("src"))) {
            //noinspection ResultOfMethodCallIgnored
            files.sorted(Comparator.reverseOrder()).map(Path::toFile).forEach(File::delete);
        }


        // WHEN
        /* Execute the mojo again */
        mojo.execute();

        // THEN
        /* Verify that the source directory has been repopulated. */
        assertTrue("src directory should have been regenerated", Files.exists(generatedDir.resolve("src")));
    }

    public void testCollapsedSpecProduced() throws Exception {
        // GIVEN
        final Path tempDir = newTempFolder();
        CodeGenMojo mojo = loadMojo(tempDir, "src/test/resources/default", "file", "executionId");

        // WHEN
        mojo.execute();

        // THEN
        assertTrue(Files.exists(
                tempDir.resolve("target/generated-sources/common-maven/remote-openapi/petstore-full-spec.yaml")
        ));
    }

    public void testCollapsedSpecAddedToArtifacts() throws Exception {
        // GIVEN
        CodeGenMojo mojo = loadMojo(newTempFolder(), "src/test/resources/default", "file", "executionId");

        // WHEN
        mojo.execute();

        // THEN
        List<Artifact> matchingArtifacts = mojo.mavenProject.getAttachedArtifacts().stream()
                .filter(artifact -> artifact.getFile().getName().equals("petstore-full-spec.yaml"))
                .collect(Collectors.toList());
        assertEquals(1, matchingArtifacts.size());
    }

    public void testAnyInputSpecMustBeProvided() throws Exception {
        // GIVEN
        CodeGenMojo mojo = loadMojo(newTempFolder(), "src/test/resources/default", "file", "executionId");
        mojo.inputSpec = null;
        mojo.inputSpecRootDirectory = null;

        // WHEN
        MojoExecutionException e = assertThrows(MojoExecutionException.class, mojo::execute);

        // THEN
        assertEquals("inputSpec or inputSpecRootDirectory must be specified", e.getMessage());
    }

    public void testInputSpecRootDirectoryDoesNotRequireInputSpec() throws Exception {
        // GIVEN
        final Path tempDir = newTempFolder();
        CodeGenMojo mojo = loadMojo(tempDir, "src/test/resources/default", "file", "executionId");
        mojo.inputSpec = null;
        mojo.inputSpecRootDirectory = "src/test/resources/default";

        // WHEN
        mojo.execute();

        // THEN
        /* Check the hash file was created */
        final Path hashFolder = tempDir.resolve("target/generated-sources/common-maven/remote-openapi/.openapi-generator");
        assertTrue(Files.exists(hashFolder.resolve("_merged_spec.yaml-executionId.sha256")));
    }

    /**
     * Regression test for <a href="https://github.com/OpenAPITools/openapi-generator/issues/16489">#16489</a>
     */
    public void test_skipIfSpecIsUnchanged_recognizesUpdatesInExternalReferencedFile() throws Exception {

        //GIVEN
        final Path tempDir = newTempFolder();
        final Path generatedDir = tempDir.resolve("target/generated-sources/issue-16489");
        final Path hashFile = generatedDir.resolve(".openapi-generator/petstore.yaml-default.sha256");
        final CodeGenMojo mojo = loadMojo(tempDir, "src/test/resources/issue-16489", null);
        mojo.execute(); // Perform an initial generation 
        var currentHash = Files.readString(hashFile);   // read hash
        FileUtils.deleteDirectory(generatedDir.resolve("src").toFile());    // Remove the generated source
        Files.writeString(  // change schema definition in external file
                tempDir.resolve("schemas/Pet.yaml"), "\n  wrapped: true", StandardOpenOption.APPEND
        );

        // WHEN
        mojo.execute(); // Execute the mojo again

        // THEN
        assertNotEquals(
                Files.readString(hashFile), currentHash, "Checksum should not be the same after external file change"
        );
        assertTrue("Src directory should have been regenerated", Files.exists(generatedDir.resolve("src")));
    }

    protected CodeGenMojo loadMojo(Path temporaryFolder, String projectRoot, String profile) throws Exception {
        return loadMojo(temporaryFolder, projectRoot, profile, "default");
    }

    protected CodeGenMojo loadMojo(Path temporaryFolder, String projectRoot, String profile, String executionId) throws Exception {
        FileUtils.copyDirectory(new File(projectRoot), temporaryFolder.toFile());
        MavenProject project = readMavenProject(temporaryFolder, profile);
        MavenSession session = newMavenSession(project);
        MojoExecution execution = newMojoExecution("generate");
        MojoExecution executionWithId = copyWithExecutionId(executionId, execution);
        return (CodeGenMojo) lookupConfiguredMojo(session, executionWithId);
    }

    private MojoExecution copyWithExecutionId(String executionId, MojoExecution execution) {
        MojoExecution executionWithId = new MojoExecution(execution.getMojoDescriptor(), executionId);
        executionWithId.setConfiguration(execution.getConfiguration());
        return executionWithId;
    }

    protected MavenProject readMavenProject(Path basedir, String profile) throws Exception {
        LocalRepository localRepo = new LocalRepository(basedir.resolve("local-repo").toFile());
        DefaultRepositorySystemSession session = MavenRepositorySystemUtils.newSession();
        session.setLocalRepositoryManager(
                new SimpleLocalRepositoryManagerFactory(new DefaultLocalPathComposer()).newInstance(session, localRepo)
        );
        MavenExecutionRequest request = new DefaultMavenExecutionRequest().setBaseDirectory(basedir.toFile());
        if (profile != null) {
            request.addActiveProfile(profile);
        }
        ProjectBuildingRequest configuration = request.getProjectBuildingRequest()
                .setRepositorySession(session)
                .setResolveDependencies(true);
        MavenProject project = lookup(ProjectBuilder.class)
                .build(basedir.resolve("pom.xml").toFile(), configuration)
                .getProject();
        assertNotNull(project);
        return project;
    }

    static private Path newTempFolder() throws IOException {
        final Path tempDir = Files.createTempDirectory("test");
        tempDir.toFile().deleteOnExit();
        return tempDir;
    }
}
