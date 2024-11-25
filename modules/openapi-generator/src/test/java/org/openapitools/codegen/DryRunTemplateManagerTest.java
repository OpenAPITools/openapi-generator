package org.openapitools.codegen;

import org.openapitools.codegen.templating.TemplateManagerOptions;
import org.testng.annotations.Test;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

import static org.testng.Assert.*;

public class DryRunTemplateManagerTest {
    @Test
    public void testError() throws IOException {
        final DryRunTemplateManager templateManager = getTemplateManager(false, false);
        final File tempFile = File.createTempFile("dryrun-test", ".txt");
        tempFile.deleteOnExit();

        templateManager.error(tempFile.toPath(), "errored");
        final Map<String, DryRunStatus> result = templateManager.getDryRunStatusMap();

        assertEquals(result.size(), 1);
        assertDryRunStatus(
                result.get(tempFile.getAbsolutePath()),
                tempFile.toPath(),
                DryRunStatus.State.Error,
                "errored"
        );
    }

    @Test
    public void testIgnore() throws IOException {
        final DryRunTemplateManager templateManager = getTemplateManager(false, false);
        final File tempFile = File.createTempFile("dryrun-test", ".txt");
        tempFile.deleteOnExit();

        templateManager.ignore(tempFile.toPath(), "ignored");
        final Map<String, DryRunStatus> result = templateManager.getDryRunStatusMap();

        assertEquals(result.size(), 1);
        assertDryRunStatus(
                result.get(tempFile.getAbsolutePath()),
                tempFile.toPath(),
                DryRunStatus.State.Ignored,
                "ignored"
        );
    }

    @Test
    public void testWrite() throws IOException {
        DryRunTemplateManager templateManager = getTemplateManager(false, false);
        final File tempFile = File.createTempFile("dryrun-test", ".txt");
        final Map<String, Object> dummyData = new HashMap<>();

        templateManager.write(dummyData, "dummy", tempFile);
        Map<String, DryRunStatus> result = templateManager.getDryRunStatusMap();

        assertEquals(result.size(), 1);
        assertDryRunStatus(
                result.get(tempFile.getAbsolutePath()),
                tempFile.toPath(),
                DryRunStatus.State.Write,
                "File will be written."
        );

        templateManager = getTemplateManager(true, false);
        templateManager.write(dummyData, "dummy", tempFile);
        result = templateManager.getDryRunStatusMap();

        assertEquals(result.size(), 1);
        assertDryRunStatus(
                result.get(tempFile.getAbsolutePath()),
                tempFile.toPath(),
                DryRunStatus.State.WriteIfNewer,
                "File will be written only if it is new or if contents differ from an existing file."
        );

        templateManager = getTemplateManager(true, true);
        templateManager.write(dummyData, "dummy", tempFile);
        result = templateManager.getDryRunStatusMap();

        assertEquals(result.size(), 1);
        assertDryRunStatus(
                result.get(tempFile.getAbsolutePath()),
                tempFile.toPath(),
                DryRunStatus.State.SkippedOverwrite,
                "File exists and skip overwrite option is enabled."
        );

        tempFile.delete();

        templateManager = getTemplateManager(true, false);
        templateManager.write(dummyData, "dummy", tempFile);
        result = templateManager.getDryRunStatusMap();

        assertEquals(result.size(), 1);
        assertDryRunStatus(
                result.get(tempFile.getAbsolutePath()),
                tempFile.toPath(),
                DryRunStatus.State.WriteIfNewer,
                "File will be written only if it is new or if contents differ from an existing file."
        );

        templateManager = getTemplateManager(false, false);
        templateManager.write(dummyData, "dummy", tempFile);
        result = templateManager.getDryRunStatusMap();

        assertEquals(result.size(), 1);
        assertDryRunStatus(
                result.get(tempFile.getAbsolutePath()),
                tempFile.toPath(),
                DryRunStatus.State.Write,
                "File will be written."
        );
    }

    @Test
    public void testSkip() throws IOException {
        DryRunTemplateManager templateManager = getTemplateManager(false, false);
        final File tempFile = File.createTempFile("dryrun-test", ".txt");
        tempFile.deleteOnExit();

        templateManager.skip(tempFile.toPath(), "skipped");
        Map<String, DryRunStatus> result = templateManager.getDryRunStatusMap();

        assertEquals(result.size(), 1);
        assertDryRunStatus(
                result.get(tempFile.getAbsolutePath()),
                tempFile.toPath(),
                DryRunStatus.State.Skipped,
                "skipped"
        );

        templateManager = getTemplateManager(false, true);

        templateManager.skip(tempFile.toPath(), "skipped");
        result = templateManager.getDryRunStatusMap();

        assertEquals(result.size(), 1);
        assertDryRunStatus(
                result.get(tempFile.getAbsolutePath()),
                tempFile.toPath(),
                DryRunStatus.State.SkippedOverwrite,
                "File is configured not to overwrite an existing file of the same name."
        );
    }

    private DryRunTemplateManager getTemplateManager(final boolean minimalUpdate, final boolean skipOverwrite) {
        return new DryRunTemplateManager(
                new TemplateManagerOptions(minimalUpdate, skipOverwrite)
        );
    }

    private void assertDryRunStatus(
            final DryRunStatus dryRunStatus,
            final Path path,
            final DryRunStatus.State state,
            final String reason
    ) {
        assertEquals(dryRunStatus.getPath().toString(), path.toString());
        assertEquals(dryRunStatus.getState().toString(), state.toString());
        assertEquals(dryRunStatus.getReason(), reason);
    }
}
