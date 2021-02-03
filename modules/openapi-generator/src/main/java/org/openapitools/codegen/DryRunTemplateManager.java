package org.openapitools.codegen;

import com.google.common.collect.ImmutableMap;
import org.openapitools.codegen.api.TemplateProcessor;
import org.openapitools.codegen.templating.TemplateManagerOptions;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
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
        return ImmutableMap.copyOf(dryRunStatusMap);
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
        if (this.options.isSkipOverwrite() && target.exists()) {
            dryRunStatusMap.put(target.toString(),
                    new DryRunStatus(
                            target.toPath(),
                            DryRunStatus.State.SkippedOverwrite,
                            "File exists and skip overwrite option is enabled."
                    ));
        }

        return target;
    }

    @Override
    public File writeToFile(String filename, byte[] contents) throws IOException {
        Path path = java.nio.file.Paths.get(filename);
        DryRunStatus status = new DryRunStatus(path);
        if (this.options.isMinimalUpdate()) {
            status.setState(DryRunStatus.State.WriteIfNewer);
        } else {
            status.setState(DryRunStatus.State.Write);
        }
        dryRunStatusMap.put(filename, status);
        return path.toFile();
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
        if (this.options.isSkipOverwrite() && path.toFile().exists()) {
            dryRunStatusMap.put(path.toString(),
                    new DryRunStatus(
                            path,
                            DryRunStatus.State.SkippedOverwrite,
                            context
                    ));
            return;
        }

        dryRunStatusMap.put(path.toString(),
                new DryRunStatus(
                        path,
                        DryRunStatus.State.Skipped,
                        context
                ));
    }

    @Override
    public void error(Path path, String context) {
        dryRunStatusMap.put(path.toString(), new DryRunStatus(path, DryRunStatus.State.Error));
    }
}
