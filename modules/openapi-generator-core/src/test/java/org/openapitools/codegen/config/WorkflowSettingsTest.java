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
    public void defaultValuesNotOverriddenByNulls(){
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
    public void newBuilderFromCopyShouldMutateGlobalProperties(){
        WorkflowSettings original = WorkflowSettings.newBuilder()
                .withOutputDir("output")
                .withVerbose(true)
                .withSkipOverwrite(false)
                .withGlobalProperty("first", "1st")
                .build();

        WorkflowSettings modified = WorkflowSettings.newBuilder(original)
                .withGlobalProperty("second", "2nd")
                .build();

        Map<String, String> properties = modified.getGlobalProperties();
        assertEquals(properties.size(), 2, "System Properties map should allow mutation when invoked via copy constructor");
        assertEquals(properties.getOrDefault("first", ""), "1st");
        assertEquals(properties.getOrDefault("second", ""), "2nd");
    }

    private void assertOnChangesToDefaults(WorkflowSettings defaults) {
        WorkflowSettings settings = WorkflowSettings.newBuilder()
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

        assertNotEquals(defaults.getOutputDir(), settings.getOutputDir());
        assertEquals(settings.getOutputDir(), Paths.get("output").toAbsolutePath().toString());

        assertNotEquals(defaults.isVerbose(), settings.isVerbose());
        assertTrue(settings.isVerbose());

        assertNotEquals(defaults.isSkipOverwrite(), settings.isSkipOverwrite());
        assertTrue(settings.isSkipOverwrite());

        assertNotEquals(defaults.isRemoveOperationIdPrefix(), settings.isRemoveOperationIdPrefix());
        assertTrue(settings.isRemoveOperationIdPrefix());

        assertNotEquals(defaults.isLogToStderr(), settings.isLogToStderr());
        assertTrue(settings.isLogToStderr());

        assertNotEquals(defaults.isValidateSpec(), settings.isValidateSpec());
        assertFalse(settings.isValidateSpec());

        assertNotEquals(defaults.isEnablePostProcessFile(), settings.isEnablePostProcessFile());
        assertTrue(settings.isEnablePostProcessFile());

        assertNotEquals(defaults.isEnableMinimalUpdate(), settings.isEnableMinimalUpdate());
        assertTrue(settings.isEnableMinimalUpdate());

        assertNotEquals(defaults.isStrictSpecBehavior(), settings.isStrictSpecBehavior());
        assertFalse(settings.isStrictSpecBehavior());
    }

    @Test
    public void defaultValuesCanBeChangedClassConstructor(){
        WorkflowSettings defaults = new WorkflowSettings();
        assertOnChangesToDefaults(defaults);
    }

    @Test
    public void defaultValuesCanBeChangedBuilder(){
        WorkflowSettings defaults = WorkflowSettings.newBuilder().build();
        assertOnChangesToDefaults(defaults);
    }
}