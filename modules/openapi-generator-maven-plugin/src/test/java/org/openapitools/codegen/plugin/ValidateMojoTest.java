/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.plugin;

import java.nio.file.Paths;
import org.apache.commons.io.FileUtils;
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

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Unit and integration tests for ValidateMojo to guarantee enforcement 
 * of validation precedence gates and multi-file tracking matrices.
 */
public class ValidateMojoTest extends BaseTestCase {

  private static final String HARNESS_PATH = "src/test/resources/validate-harness";

  @Override
  protected void setUp() throws Exception {
    super.setUp();
  }

  /**
   * Test 1: Happy Path. Verifies a completely valid specification passes
   * seamlessly without raising exceptions.
   */
  public void testExecute_WithValidSpec_ShouldPassNatively() throws Exception {
    // GIVEN
    final Path tempDir = newTempFolder();
    ValidateMojo mojo = loadMojo(tempDir, "default");

    // Point specifically to a fully compliant valid spec
    mojo.inputSpec = new String[]{ tempDir.resolve("valid_api.yaml").toString() };

    // WHEN & THEN (Expect no exception)
    mojo.execute();
    assertFalse("Mojo state tracking shouldn't flag global error", mojo.hasError);
    assertFalse("Mojo state tracking shouldn't flag global warning", mojo.hasWarning);
  }

  /**
   * Test 2: Multi-File Error Isolation. Verifies that if multiple files have errors,
   * a failure in the first file does not crash the loop before checking the remaining files.
   */
  public void testExecute_WithMultipleSpecs_ShouldCollectAllBeforeFailing() throws Exception {
    // GIVEN
    final Path tempDir = newTempFolder();
    ValidateMojo mojo = loadMojo(tempDir, "default");

    mojo.inputSpec = new String[]{
        tempDir.resolve("broken_syntax_api.yaml").toString(),
        tempDir.resolve("unused_model_api.yaml").toString()
    };

    // WHEN & THEN (Should collect across files, then throw execution crash at termination)
    assertThrows(MojoExecutionException.class, mojo::execute,
        "Should fail build globally due to aggregated specifications errors.");

    assertTrue("Mojo must have flagged the error from file 1", mojo.hasError);
    assertTrue("Mojo must have flagged the warning from file 2", mojo.hasWarning);
  }

  /**
   * Test 3: Strict Spec Mode Gates. Verifies recommendations behave as non-blocking
   * items when strictSpec is false, but trigger hard failures when strictSpec is turned on.
   */
  public void testExecute_WithWarningsOnly_ShouldEnforceStrictSpecGates() throws Exception {
    // GIVEN
    final Path tempDir = newTempFolder();

    // Scenario A: strictSpec is false -> Warnings should be logged, but build passes
    ValidateMojo softMojo = loadMojo(tempDir, "default");
    setVariableValueToObject(softMojo, "strictSpec", Boolean.FALSE);
    softMojo.inputSpec = new String[]{ tempDir.resolve("unused_model_api.yaml").toString() };

    softMojo.execute();
    assertTrue("Should catch warning state", softMojo.hasWarning);
    assertFalse("Should not mark hard error state", softMojo.hasError);

    // Scenario B: strictSpec is true -> Same warnings must result in a hard build crash
    ValidateMojo strictMojo = loadMojo(tempDir, "default");
    setVariableValueToObject(strictMojo, "strictSpec", Boolean.TRUE);
    strictMojo.inputSpec = new String[]{ tempDir.resolve("unused_model_api.yaml").toString() };

    assertThrows(MojoExecutionException.class, strictMojo::execute,
        "Strict compliance configuration must fail build if specs contain recommendations.");
  }

  /**
   * Test 4: Dry Run Override Supremacy. Verifies that if dryRun is active, the build
   * passes completely despite severe specification errors and strict flags being active.
   */
  public void testExecute_WithErrorsInDryRunMode_ShouldPassNatively() throws Exception {
    // GIVEN
    final Path tempDir = newTempFolder();
    ValidateMojo mojo = loadMojo(tempDir, "default");

    // Activate both strict configurations and point to broken specification layouts
    setVariableValueToObject(mojo, "dryRun", Boolean.TRUE);
    setVariableValueToObject(mojo, "strictSpec", Boolean.TRUE);
    mojo.inputSpec = new String[]{ tempDir.resolve("broken_syntax_api.yaml").toString() };

    // WHEN & THEN (The dryRun shield guarantees no MojoExecutionException is thrown)
    mojo.execute();

    assertTrue("Mojo must successfully record that severe errors were found", mojo.hasError);
  }

  /**
   * Test 5: Comma-Separated Input Processing. Verifies inline comma-delimited definitions
   * work seamlessly for CLI invocation overrides (-Dopenapi.generator.maven.plugin.inputSpec=a,b).
   */
  public void testExecute_WithCommaSeparatedInputSpec_ShouldParseIndividualFiles() throws Exception {
    // GIVEN
    final Path tempDir = newTempFolder();
    ValidateMojo mojo = loadMojo(tempDir, "default");

    // Pass a single string containing comma-separated paths mirroring direct terminal arguments
    String path1 = tempDir.resolve("valid_api.yaml").toString();
    String path2 = tempDir.resolve("unused_model_api.yaml").toString();
    mojo.inputSpec = new String[]{ path1 + " , " + path2 };

    // WHEN
    mojo.execute();

    // THEN
    assertTrue("Must successfully split string elements and traverse to check file 2", mojo.hasWarning);
  }

  /**
   * Test 6: Input Validation Edge Case. Verifies fallback errors raise correctly
   * if parameters are left completely blank.
   */
  public void testExecute_WithMissingConfigurationInputs_ShouldThrowExplicitException() throws Exception {
    // GIVEN
    final Path tempDir = newTempFolder();
    ValidateMojo mojo = loadMojo(tempDir, "default");
    mojo.inputSpec = new String[]{ "" };
    mojo.inputSpecRootDirectory = null;

    // WHEN & THEN
    MojoExecutionException e = assertThrows(MojoExecutionException.class, mojo::execute);
    assertEquals("inputSpec, inputSpecRootDirectory, or inputSpecFiles must be specified", e.getMessage());
  }

  /**
   * Test 7: Directory Merging Path. Verifies that configuring inputSpecRootDirectory
   * triggers MergedSpecBuilder, cleanly overrides the inputSpec array, and runs validation on the result.
   */
  public void testExecute_WithInputSpecRootDirectory_ShouldMergeAndValidate() throws Exception {
    // GIVEN
    final Path tempDir = newTempFolder();
    ValidateMojo mojo = loadMojo(tempDir, "default");

    // Point to our subfolder containing files to combine, and clear out explicit inputSpec
    setVariableValueToObject(mojo, "inputSpecRootDirectory", tempDir.resolve("specs-to-merge").toString());
    mojo.inputSpec = new String[]{ "" };

    // WHEN
    mojo.execute();

    // THEN
    assertNotNull("inputSpec should have been automatically populated via the merge engine", mojo.inputSpec);
    assertEquals("Should contain exactly 1 merged path pointer", 1, mojo.inputSpec.length);
    assertTrue("The path should point to the generated merged spec file", mojo.inputSpec[0].contains("_merged_spec"));
    assertFalse("The aggregated spec should pass validation cleanly", mojo.hasError);
  }

  /**
   * Test 8: Collapsed Spec Generation (YAML flavor). Verifies that when collapsedSpec is set,
   * the plugin successfully resolves inline $refs, compiles a single flat representation,
   * and dumps it directly into the project's build target output path.
   */
  public void testExecute_WithCollapsedSpec_ShouldCreateFlatRepresentationFile() throws Exception {
    // GIVEN
    final Path tempDir = newTempFolder();
    ValidateMojo mojo = loadMojo(tempDir, "default");

    // Target an explicit singular input file and configure a collapse name definition
    mojo.inputSpec = new String[]{ tempDir.resolve("valid_api.yaml").toString() };
    setVariableValueToObject(mojo, "collapsedSpec", "flattened-output-api");

    // Mock the maven project output path structure so createCollapsedSpec knows where to save it
    String mockOutputDir = tempDir.resolve("target/classes").toString();
    Files.createDirectories(Paths.get(mockOutputDir));
    mojo.project.getBuild().setOutputDirectory(mockOutputDir);

    // WHEN
    mojo.execute();

    // THEN
    Path expectedFile = Paths.get(mockOutputDir, "flattened-output-api.yaml");
    assertTrue("The Mojo must physically write out the collapsed flat definition to disk", Files.exists(expectedFile));

    String content = Files.readString(expectedFile);
    assertTrue("The written file must be a real parsed OpenAPI file", content.contains("openapi: 3.0.3"));
  }

  /**
   * Test 9: Collapsed Spec Attached to Maven Artifact Matrix. Verifies that when
   * includeCollapsedSpecInArtifacts is flagged true, the plugin cleanly calls Maven's ProjectHelper
   * to bind the new file to the lifecycle deployment queue.
   */
  public void testExecute_WithIncludeCollapsedSpecInArtifacts_ShouldRegisterMavenArtifact() throws Exception {
    // GIVEN
    final Path tempDir = newTempFolder();
    ValidateMojo mojo = loadMojo(tempDir, "default");

    mojo.inputSpec = new String[]{ tempDir.resolve("valid_api.yaml").toString() };
    setVariableValueToObject(mojo, "collapsedSpec", "artifact-bound-spec");
    setVariableValueToObject(mojo, "includeCollapsedSpecInArtifacts", Boolean.TRUE);

    // Standard build output paths setups
    String mockOutputDir = tempDir.resolve("target/classes").toString();
    Files.createDirectories(Paths.get(mockOutputDir));
    mojo.project.getBuild().setOutputDirectory(mockOutputDir);

    // WHEN
    mojo.execute();

    // THEN
    // Dig into the mocked MavenProject artifact array to verify our binding exists
    java.util.List<org.apache.maven.artifact.Artifact> attachedArtifacts = mojo.mavenProject.getAttachedArtifacts();
    boolean artifactFound = attachedArtifacts.stream()
        .anyMatch(artifact -> artifact.getClassifier().equals("artifact-bound-spec")
            && artifact.getType().equals("yaml"));

    assertTrue("The collapsed specification file must be registered directly into the Maven artifact attachment queue",
        artifactFound);
  }

  /**
   * Test 10: Global Skip Toggle. Verifies that when codegen.skip (skip) is set to true,
   * the Mojo exits immediately and logs that validation is skipped, without running any file parsing.
   */
  public void testExecute_WithGlobalSkipTrue_ShouldReturnImmediately() throws Exception {
    // GIVEN
    final Path tempDir = newTempFolder();
    ValidateMojo mojo = loadMojo(tempDir, "default");

    // Explicitly set the global skip configuration to true
    setVariableValueToObject(mojo, "skip", Boolean.TRUE);

    // Point to a completely broken file path that would normally throw an exception
    mojo.inputSpec = new String[]{ "non_existent_file_that_would_crash.yaml" };

    // WHEN & THEN
    // If the skip gate works perfectly, this will execute smoothly without throwing any MojoExecutionException
    mojo.execute();

    assertFalse("Mojo should not have processed any files to find errors", mojo.hasError);
  }

  /**
   * Test 11: Goal-Specific Skip Toggle. Verifies that when skipValidateSpec is true,
   * the plugin behaves identically to the global skip, bypassing execution rules cleanly.
   */
  public void testExecute_WithSkipValidateSpecTrue_ShouldReturnImmediately() throws Exception {
    // GIVEN
    final Path tempDir = newTempFolder();
    ValidateMojo mojo = loadMojo(tempDir, "default");

    // Activate the plugin-specific skip flag instead of the global one
    setVariableValueToObject(mojo, "skipValidateSpec", Boolean.TRUE);
    mojo.inputSpec = new String[]{ "another_broken_path.yaml" };

    // WHEN & THEN
    mojo.execute();

    assertFalse("Mojo should have exited before evaluating any file loops", mojo.hasError);
  }

  /**
   * Test 12: Delta Build Incremental Skip. Verifies that when running in an incremental
   * workspace build context, if the input spec file exists but has no change modifications (no delta),
   * the execution skips parsing and optimization steps gracefully.
   */
  public void testExecute_WithIncrementalBuildContextAndNoDelta_ShouldSkipFile() throws Exception {
    // GIVEN
    final Path tempDir = newTempFolder();
    ValidateMojo mojo = loadMojo(tempDir, "default");

    File validFile = tempDir.resolve("valid_api.yaml").toFile();
    mojo.inputSpec = new String[]{ validFile.getAbsolutePath() };

    // Create a mocked BuildContext that simulates an active incremental IDE compilation
    // where the file in question has *not* changed.
    org.sonatype.plexus.build.incremental.BuildContext mockContext =
        mock(org.sonatype.plexus.build.incremental.BuildContext.class);

    when(mockContext.isIncremental()).thenReturn(true);
    when(mockContext.hasDelta(validFile)).thenReturn(false);

    // Inject our mock context directly into the Mojo
    setVariableValueToObject(mojo, "buildContext", mockContext);

    // WHEN
    mojo.execute();

    // THEN
    // Since it skipped validation inside the delta gate loop, the validation stats will stay completely untouched
    assertFalse("Mojo should have skipped validation tracking loops due to matching cache delta rules", mojo.hasError);
  }
  // --- Harness Infrastructure Factories adapted from CodeGenMojoTest ---

  protected ValidateMojo loadMojo(Path temporaryFolder, String profile) throws Exception {
    FileUtils.copyDirectory(new File(HARNESS_PATH), temporaryFolder.toFile());
    MavenProject project = readMavenProject(temporaryFolder, profile);
    MavenSession session = newMavenSession(project);
    // 1. Generate the base mojo execution frame
    MojoExecution baseExecution = newMojoExecution("validate");

    // 2. Clone it safely with the execution id using the helper
    MojoExecution executionWithId = copyWithExecutionId("default", baseExecution);
    return (ValidateMojo) lookupConfiguredMojo(session, executionWithId);
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
    return lookup(ProjectBuilder.class)
        .build(basedir.resolve("pom.xml").toFile(), configuration)
        .getProject();
  }

  private static Path newTempFolder() throws IOException {
    final Path tempDir = Files.createTempDirectory("validate-test");
    tempDir.toFile().deleteOnExit();
    return tempDir;
  }
}