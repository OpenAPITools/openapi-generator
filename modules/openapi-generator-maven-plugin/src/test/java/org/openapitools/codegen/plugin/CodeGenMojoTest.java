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

import lombok.SneakyThrows;
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
import org.eclipse.aether.internal.impl.SimpleLocalRepositoryManagerFactory;
import org.eclipse.aether.repository.LocalRepository;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

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

    public void testCommonConfigurationWithURLInputSpec() throws Exception {
        testCommonConfiguration("url");
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
        Path hashFolder = tempDir.resolve("target/generated-sources/common-maven/remote-openapi/.openapi-generator");
        assertTrue(hashFolder.resolve("petstore.yaml-executionId.sha256").toFile().exists());
    }

    /**
     * For a Pom file which refers to a input file which will be on the classpath, as opposed to a file path,
     * test that the spec is not regenerated when the hash has not changed. 
     * 
     * @throws Exception
     */
    public void testSkipRegenerationForClasspathSpecFileNoChange() throws Exception {
        //GIVEN
        /* Setup the mojo */
        final Path tempDir = newTempFolder();
        final CodeGenMojo mojo = loadMojo(tempDir, "src/test/resources/classpath", null, "executionId");

        /* Perform an initial generation */
        mojo.execute();

        /* Check the hash file was created */
        final Path hashFolder = tempDir.resolve("target/generated-sources/common-maven/remote-openapi/.openapi-generator");
        assertTrue(hashFolder.resolve("petstore-on-classpath.yaml-executionId.sha256").toFile().exists());

        /* Remove the generated source */
        Files.walk(tempDir.resolve("target/generated-sources/common-maven/remote-openapi/src"))
            .sorted(Comparator.reverseOrder())
            .map(Path::toFile)
            .forEach(File::delete);


        // WHEN
        /* Execute the mojo again */
        mojo.execute();

        // THEN
        /* Verify that the source directory has not been repopulated. If it has then we generated code again */
        assertFalse("src directory should not have been regenerated", 
            tempDir.resolve("target/generated-sources/common-maven/remote-openapi/src").toFile().exists());

    }

    /**
     * For a Pom file which refers to a input file which will be on the classpath, as opposed to a file path,
     * test that the generated source is regenerated when the hash has changed. 
     * 
     * @throws Exception
     */
    public void testSkipRegenerationForClasspathSpecFileWithChange() throws Exception {
        //GIVEN
        /* Setup the mojo */
        final Path tempDir = newTempFolder();
        final CodeGenMojo mojo = loadMojo(tempDir, "src/test/resources/classpath", null, "executionId");

        /* Perform an initial generation */
        mojo.execute();

        /* Check the hash file was created, proving a generation occurred */
        final Path hashFolder = tempDir.resolve("target/generated-sources/common-maven/remote-openapi/.openapi-generator");
        assertTrue(hashFolder.resolve("petstore-on-classpath.yaml-executionId.sha256").toFile().exists());

        /* Update the hash contents to be a different value, simulating a spec change */
        Files.write(
            hashFolder.resolve("petstore-on-classpath.yaml-executionId.sha256"), 
            Arrays.asList("bd1bf4a953c858f9d47b67ed6029daacf1707e5cbd3d2e4b01383ba30363366f"));

        /* Remove the generated source */
        Files.walk(tempDir.resolve("target/generated-sources/common-maven/remote-openapi/src"))
            .sorted(Comparator.reverseOrder())
            .map(Path::toFile)
            .forEach(File::delete);


        // WHEN
        /* Execute the mojo again */
        mojo.execute();

        // THEN
        /* Verify that the source directory has not been repopulated. If it has then we generated code again */
        assertTrue("src directory should have been regenerated", 
            tempDir.resolve("target/generated-sources/common-maven/remote-openapi/src").toFile().exists());

    }

    public void testCollapsedSpecProduced() throws Exception {
        // GIVEN
        final Path tempDir = newTempFolder();
        CodeGenMojo mojo = loadMojo(tempDir, "src/test/resources/default", "file", "executionId");

        // WHEN
        mojo.execute();

        // THEN
        File collapseSpecFile = tempDir.resolve("target/generated-sources/common-maven/remote-openapi/petstore-full-spec.yaml").toFile();
        assertTrue(collapseSpecFile.exists());
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
        assertTrue(hashFolder.resolve("_merged_spec.yaml-executionId.sha256").toFile().exists());
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
        MojoExecution executionWithId  = new MojoExecution(execution.getMojoDescriptor(), executionId);
        executionWithId.setConfiguration(execution.getConfiguration());
        return executionWithId;
    }

    protected MavenProject readMavenProject(Path basedir, String profile) throws Exception {
        Path pom = basedir.resolve("pom.xml");
        LocalRepository localRepo = new LocalRepository(basedir.resolve("local-repo").toFile());
        DefaultRepositorySystemSession session = MavenRepositorySystemUtils.newSession();
        session.setLocalRepositoryManager(new SimpleLocalRepositoryManagerFactory().newInstance(session, localRepo));
        MavenExecutionRequest request = new DefaultMavenExecutionRequest();
        request.setBaseDirectory(basedir.toFile());
        if (profile != null) {
            request.addActiveProfile(profile);
        }
        ProjectBuildingRequest configuration = request.getProjectBuildingRequest();
        configuration.setRepositorySession(session);
        configuration.setResolveDependencies(true);
        MavenProject project = lookup(ProjectBuilder.class).build(pom.toFile(), configuration).getProject();
        assertNotNull(project);
        return project;
    }

    @SneakyThrows
    static private Path newTempFolder() {
        var tempDir = Files.createTempDirectory("test");
        tempDir.toFile().deleteOnExit();
        return tempDir;
    }
}
