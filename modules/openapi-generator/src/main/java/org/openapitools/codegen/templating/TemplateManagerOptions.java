package org.openapitools.codegen.templating;

import lombok.Getter;

/**
 * Holds the options relevant to template management and execution.
 */
@Getter
public class TemplateManagerOptions {
    /**
     * Determines whether the template should minimally update a target file.
     * A minimal update means the template manager is requested to update a file only if it is newer.
     * This option avoids "touching" a file and causing the last modification time (mtime) to change.
     */
    private final boolean minimalUpdate;
    /**
     * -- GETTER --
     * Determines whether the template manager should avoid overwriting an existing file.
     * This differs from requesting which evaluates contents, while this option only
     * evaluates whether the file exists.
     */
    private final boolean skipOverwrite;

    /**
     * Constructs a new instance of {@link TemplateManagerOptions}
     *
     * @param minimalUpdate Minimal update
     * @param skipOverwrite Skip overwrite
     */
    public TemplateManagerOptions(boolean minimalUpdate, boolean skipOverwrite) {
        this.minimalUpdate = minimalUpdate;
        this.skipOverwrite = skipOverwrite;
    }

}
