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
import org.apache.maven.execution.DefaultMavenExecutionRequest;
import org.apache.maven.execution.MavenExecutionRequest;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingRequest;
import org.eclipse.aether.DefaultRepositorySystemSession;
import org.openapitools.codegen.plugin.stubs.StubUtility;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

public class CodeGenMojoTest extends BaseTestCase {
    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @SuppressWarnings("unchecked")
    public void testCommonConfiguration() throws Exception {
        File folder = Files.createTempDirectory("test").toFile();
        CodeGenMojo mojo = loadMojo(folder, "src/test/resources/default");
        mojo.execute();
        assertEquals("java", getVariableValueFromObject(mojo, "generatorName"));
        assertEquals("jersey2", getVariableValueFromObject(mojo, "library"));
        assertEquals("remote.org.openapitools.client.api", getVariableValueFromObject(mojo, "apiPackage"));
        assertEquals("remote.org.openapitools.client.model", getVariableValueFromObject(mojo, "modelPackage"));
        assertEquals("remote.org.openapitools.client", getVariableValueFromObject(mojo, "invokerPackage"));

        Map<String, Object> configOptions = (Map<String, Object>) getVariableValueFromObject(mojo, "configOptions");
        assertNotNull(configOptions);
        assertEquals("joda", configOptions.get("dateLibrary"));
    }

    public void testHashGenerationFileContainsExecutionId() throws Exception {
        // GIVEN
        Path folder = Files.createTempDirectory("test");
        CodeGenMojo mojo = loadMojo(folder.toFile(), "src/test/resources/default", "executionId");

        // WHEN
        mojo.execute();

        // THEN
        Path hashFolder = folder.resolve("target/generated-sources/common-maven/remote-openapi/.openapi-generator");
        assertTrue(hashFolder.resolve("petstore.yaml-executionId.sha256").toFile().exists());
    }

    protected CodeGenMojo loadMojo(File temporaryFolder, String projectRoot) throws Exception {
        return loadMojo(temporaryFolder, projectRoot, "default");
    }

    protected CodeGenMojo loadMojo(File temporaryFolder, String projectRoot, String executionId) throws Exception {
        File file = new File(projectRoot);
        FileUtils.copyDirectory(file, temporaryFolder);
        MavenProject project = readMavenProject(temporaryFolder);
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

    protected MavenProject readMavenProject(File basedir)
            throws Exception {
        File pom = new File(basedir, "pom.xml");
        MavenExecutionRequest request = new DefaultMavenExecutionRequest();
        request.setBaseDirectory(basedir);
        ProjectBuildingRequest configuration = request.getProjectBuildingRequest();
        configuration.setRepositorySession(new DefaultRepositorySystemSession());
        MavenProject project = lookup(ProjectBuilder.class).build(pom, configuration).getProject();
        assertNotNull(project);
        return project;
    }
}