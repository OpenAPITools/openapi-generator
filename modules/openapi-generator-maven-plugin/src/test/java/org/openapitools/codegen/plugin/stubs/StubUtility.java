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

package org.openapitools.codegen.plugin.stubs;

import org.apache.maven.model.Build;
import org.apache.maven.model.Model;
import org.apache.maven.model.io.xpp3.MavenXpp3Reader;
import org.apache.maven.plugin.testing.stubs.ArtifactStub;
import org.apache.maven.plugin.testing.stubs.DefaultArtifactHandlerStub;
import org.apache.maven.plugin.testing.stubs.MavenProjectStub;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public abstract class StubUtility {
    private StubUtility() {
        throw new UnsupportedOperationException("Utility class");
    }

    /**
     * Configures a stub to conventional directories based on test name and pom file name.
     * <p>
     * Taken largely from PMD plugin:
     * https://github.com/apache/maven-pmd-plugin/blob/d766fdb0c93a6630ad7a788f260746b05c804a71/src/test/java/org/apache/maven/plugins/pmd/stubs/CustomConfigurationMavenProjectStub.java
     * License: Apache 2.0
     *
     * @param configure   The stub to configure
     * @param testName    A name used to identify this stub, used in resource lookup
     * @param pomFileName The filename and extension, e.g. "my-test-pom.xml"
     */
    public static void configureStub(MavenProjectStub configure, String testName, String pomFileName) {
        MavenXpp3Reader pomReader = new MavenXpp3Reader();
        Model model = null;
        try {
            File pomFile = basedPath(
                    configure.getBasedir(), "src", "test", "resources", "unit", testName, pomFileName
            ).toFile();
            model = pomReader.read(new InputStreamReader(new FileInputStream(pomFile), StandardCharsets.UTF_8));
            configure.setModel(model);
        } catch (Exception ignored) {

        }

        configure.setGroupId(model.getGroupId());
        configure.setArtifactId(model.getArtifactId());
        configure.setVersion(model.getVersion());
        configure.setName(model.getName());
        configure.setUrl(model.getUrl());
        configure.setPackaging(model.getPackaging());

        Build build = new Build();
        build.setFinalName(model.getBuild().getFinalName());
        build.setDirectory(basedPath(configure.getBasedir(), "target", "test", "unit", testName, "target").toString());
        build.setSourceDirectory(basedPath(configure.getBasedir(), "src", "test", "resources", "unit", testName).toString());
        configure.setBuild(build);

        List<String> compileSourceRoots = new ArrayList<>();
        compileSourceRoots.add(basedPath(configure.getBasedir(), "src", "test", "resources", "unit", testName, "src").toString());
        configure.setCompileSourceRoots(compileSourceRoots);

        ArtifactStub artifactStub = new ArtifactStub();
        artifactStub.setGroupId(configure.getGroupId());
        artifactStub.setArtifactId(configure.getArtifactId());
        artifactStub.setVersion(configure.getVersion());
        artifactStub.setArtifactHandler(new DefaultArtifactHandlerStub("jar"));
        configure.setArtifact(artifactStub);

        configure.setFile(configure.getBasedir().toPath().resolve(pomFileName).toFile());
    }

    public static Path basedPath(File baseDir, String first, String... more) {
        return baseDir.toPath().resolve(Paths.get(first, more)).normalize().toAbsolutePath();
    }
}
