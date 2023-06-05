package org.openapitools.codegen;

import org.openapitools.codegen.api.TemplateProcessor;
import org.openapitools.codegen.templating.TemplateManagerOptions;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Manages templates for a generator "dry run"
 */
public class DryRunTemplateManager implements TemplateProcessor {
    private final TemplateManagerOptions options;
    private final Map<String, DryRunStatus> dryRunStatusMap = new HashMap<>();

    /**
     * Constructs a new instance of {@link DryRunTemplateManager} for the provided options
     *
     * @param options Options pertaining to templates (reads and writes)
     */
    public DryRunTemplateManager(TemplateManagerOptions options) {
        this.options = options;
    }

    /**
     * Gets the full status of this dry run.
     *
     * @return An immutable copy of the dry run status.
     */
    public Map<String, DryRunStatus> getDryRunStatusMap() {
        return Collections.unmodifiableMap(dryRunStatusMap);
    }

    /**
     * Writes data to a compiled template
     *
     * @param data     Input data
     * @param template Input template location
     * @param target   The targeted file output location
     * @return The actual file
     */
    @Override
    public File write(Map<String, Object> data, String template, File target) throws IOException {
        return writeToFile(target.getAbsolutePath(), "dummy".getBytes(StandardCharsets.UTF_8));
    }

    @Override
    public File writeToFile(String filename, byte[] contents) throws IOException {
        final Path path = Paths.get(filename);
        final File outputFile = path.toFile();
        DryRunStatus status = new DryRunStatus(path);

        if (outputFile.exists()) {
            if (this.options.isSkipOverwrite()) {
                status = new DryRunStatus(
                    path,
                    DryRunStatus.State.SkippedOverwrite,
                    "File exists and skip overwrite option is enabled."
                );
            } else if (this.options.isMinimalUpdate()) {
                status.setState(DryRunStatus.State.WriteIfNewer);
            } else {
                status.setState(DryRunStatus.State.Write);
            }
        } else if (this.options.isMinimalUpdate()) {
            status.setState(DryRunStatus.State.WriteIfNewer);
        } else {
            status.setState(DryRunStatus.State.Write);
        }
        dryRunStatusMap.put(filename, status);

        return outputFile;
    }

    @Override
    public void ignore(Path path, String context) {
        dryRunStatusMap.put(path.toString(),
                new DryRunStatus(
                        path,
                        DryRunStatus.State.Ignored,
                        context
                ));
    }

    @Override
    public void skip(Path path, String context) {
        final DryRunStatus status = new DryRunStatus(path, DryRunStatus.State.Skipped, context);
        if (this.options.isSkipOverwrite() && path.toFile().exists()) {
            status.setState(DryRunStatus.State.SkippedOverwrite);
        }
        dryRunStatusMap.put(path.toString(), status);
    }

    @Override
    public void error(Path path, String context) {
        dryRunStatusMap.put(path.toString(), new DryRunStatus(path, DryRunStatus.State.Error, context));
    }
}
