/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

package org.openapitools.codegen.config;

import org.testng.annotations.Test;

import java.nio.file.Paths;
import java.util.Map;

import static org.testng.Assert.*;

public class WorkflowSettingsTest {
    @Test
    public void defaultValuesNotOverriddenByNulls() {
        WorkflowSettings settings = WorkflowSettings.newBuilder()
                .withOutputDir(null)
                .withVerbose(null)
                .withSkipOverwrite(null)
                .withRemoveOperationIdPrefix(null)
                .withLogToStderr(null)
                .withValidateSpec(null)
                .withEnablePostProcessFile(null)
                .withEnableMinimalUpdate(null)
                .withStrictSpecBehavior(null)
                .build();

        assertEquals(settings.getOutputDir(), ".");
        assertFalse(settings.isVerbose());
        assertFalse(settings.isSkipOverwrite());
        assertFalse(settings.isRemoveOperationIdPrefix());
        assertFalse(settings.isLogToStderr());
        assertTrue(settings.isValidateSpec());
        assertFalse(settings.isEnablePostProcessFile());
        assertFalse(settings.isEnableMinimalUpdate());
        assertTrue(settings.isStrictSpecBehavior());
    }

    @Test
    public void newBuilderFromCopyShouldMutateGlobalProperties() {
        WorkflowSettings original = WorkflowSettings.newBuilder()
                .withOutputDir("output")
                .withVerbose(true)
                .withSkipOverwrite(false)
                .withGlobalProperty("first", "1st")
                .build();

        WorkflowSettings modified = WorkflowSettings.newBuilder(original)
                .withGlobalProperty("second", "2nd")
                .build();

        Map<String, String> globalProperties = modified.getGlobalProperties();
        assertEquals(globalProperties.size(), 2, "System Properties map should allow mutation when invoked via copy constructor");
        assertEquals(globalProperties.getOrDefault("first", ""), "1st");
        assertEquals(globalProperties.getOrDefault("second", ""), "2nd");
    }

    private void assertOnChangesToDefaults(WorkflowSettings defaultSettings) {
        WorkflowSettings newSettings = WorkflowSettings.newBuilder()
                .withOutputDir("output")
                .withVerbose(true)
                .withSkipOverwrite(true)
                .withRemoveOperationIdPrefix(true)
                .withLogToStderr(true)
                .withValidateSpec(false)
                .withEnablePostProcessFile(true)
                .withEnableMinimalUpdate(true)
                .withStrictSpecBehavior(false)
                .build();

        assertNotEquals(defaultSettings.getOutputDir(), newSettings.getOutputDir());
        assertEquals(newSettings.getOutputDir(), Paths.get("output").toAbsolutePath().toString());

        assertNotEquals(defaultSettings.isVerbose(), newSettings.isVerbose());
        assertTrue(newSettings.isVerbose());

        assertNotEquals(defaultSettings.isSkipOverwrite(), newSettings.isSkipOverwrite());
        assertTrue(newSettings.isSkipOverwrite());

        assertNotEquals(defaultSettings.isRemoveOperationIdPrefix(), newSettings.isRemoveOperationIdPrefix());
        assertTrue(newSettings.isRemoveOperationIdPrefix());

        assertNotEquals(defaultSettings.isLogToStderr(), newSettings.isLogToStderr());
        assertTrue(newSettings.isLogToStderr());

        assertNotEquals(defaultSettings.isValidateSpec(), newSettings.isValidateSpec());
        assertFalse(newSettings.isValidateSpec());

        assertNotEquals(defaultSettings.isEnablePostProcessFile(), newSettings.isEnablePostProcessFile());
        assertTrue(newSettings.isEnablePostProcessFile());

        assertNotEquals(defaultSettings.isEnableMinimalUpdate(), newSettings.isEnableMinimalUpdate());
        assertTrue(newSettings.isEnableMinimalUpdate());

        assertNotEquals(defaultSettings.isStrictSpecBehavior(), newSettings.isStrictSpecBehavior());
        assertFalse(newSettings.isStrictSpecBehavior());
    }

    @Test
    public void defaultValuesCanBeChangedClassConstructor() {
        WorkflowSettings defaults = new WorkflowSettings();
        assertOnChangesToDefaults(defaults);
    }

    @Test
    public void defaultValuesCanBeChangedBuilder() {
        WorkflowSettings defaults = WorkflowSettings.newBuilder().build();
        assertOnChangesToDefaults(defaults);
    }

    @Test
    public void customOutputDirIsSet() {
        WorkflowSettings settings = WorkflowSettings.newBuilder()
                .withOutputDir("custom/output/directory")
                .build();

        assertEquals(settings.getOutputDir(), Paths.get("custom/output/directory").toAbsolutePath().toString());
    }

    @Test
    public void customGlobalPropertiesAreSet() {
        WorkflowSettings settings = WorkflowSettings.newBuilder()
                .withGlobalProperty("customKey", "customValue")
                .build();

        Map<String, String> properties = settings.getGlobalProperties();
        assertEquals(properties.size(), 1, "Global Properties map should allow custom entries");
        assertEquals(properties.getOrDefault("customKey", ""), "customValue");
    }
}