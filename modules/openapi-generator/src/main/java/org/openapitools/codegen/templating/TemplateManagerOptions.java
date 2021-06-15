package org.openapitools.codegen.templating;

/**
 * Holds the options relevant to template management and execution.
 */
public class TemplateManagerOptions {
    private final boolean minimalUpdate;
    private final boolean skipOverwrite;

    /**
     * Constructs a new instance of {@link TemplateManagerOptions}
     *
     * @param minimalUpdate See {@link #isMinimalUpdate()}
     * @param skipOverwrite See {@link #isSkipOverwrite()}
     */
    public TemplateManagerOptions(boolean minimalUpdate, boolean skipOverwrite) {
        this.minimalUpdate = minimalUpdate;
        this.skipOverwrite = skipOverwrite;
    }

    /**
     * Determines whether the template should minimally update a target file.
     *
     * A minimal update means the template manager is requested to update a file only if it is newer.
     *
     * This option avoids "touching" a file and causing the last modification time (mtime) to change.
     *
     * @return true to prefer updating only changed files, false to disable that suggestion
     */
    public boolean isMinimalUpdate() {
        return minimalUpdate;
    }

    /**
     * Determines whether the template manager should avoid overwriting an existing file.
     *
     * This differs from requesting {@link #isMinimalUpdate()} which evaluates contents, while this option only
     * evaluates whether the file exists.
     *
     * @return true to avoid overwriting existing files (where supported), false to disable that suggestion.
     */
    public boolean isSkipOverwrite() {
        return skipOverwrite;
    }
}
