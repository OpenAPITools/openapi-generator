package org.openapitools.codegen;

import java.io.IOException;
import java.nio.file.Path;
import java.util.Locale;

/**
 * Holds details about a file's write status for display via the --dry-run option of CLI
 */
class DryRunStatus {
    private Path path;
    private State state;
    private String reason;

    /**
     * Constructs a new instance of {@link DryRunStatus} for a given path and status of {@link State#Write}
     *
     * @param path The target path where the file would write
     */
    public DryRunStatus(Path path) {
        this(path, State.Write);
    }

    /**
     * Constructs a new instance of {@link DryRunStatus} for a path and a target state
     *
     * @param path  The target path where the file would write
     * @param state The evaluated state as determined by the generation workflow
     */
    public DryRunStatus(Path path, State state) {
        this.path = path;
        setState(state);
    }

    /**
     * Constructs a new instance of {@link DryRunStatus} for a path, target state, and presenting a specific reason for that state
     *
     * @param path   The target path where the file would write
     * @param state  The evaluated state as determined by the generation workflow
     * @param reason A reason for the state, beyond any generic reason
     */
    public DryRunStatus(Path path, State state, String reason) {
        this.path = path;
        this.state = state;
        this.reason = reason;
    }

    /**
     * Append a user display text to the {@link Appendable} instance
     *
     * @param appendable An object implementing {@link Appendable} (such as {@link StringBuilder}
     * @throws IOException via contract of {@link Path#toAbsolutePath()}
     */
    public void appendTo(Appendable appendable) throws IOException {
        appendable.append(String.format(Locale.ROOT, "%s %s", this.state.getShortDisplay(), this.path.toAbsolutePath()));
    }

    /**
     * Gets the target path of the file write operation
     *
     * @return a {@link Path} instance
     */
    public Path getPath() {
        return path;
    }

    /**
     * Gets the reason for the file's {@link State}
     *
     * @return A human-readable string which explains why this file's dry-run resulted in the defined {@link State}
     */
    public String getReason() {
        return reason;
    }

    /**
     * Gets the {@link State} as determined by the generator's workflow
     *
     * @return A {@link State} enum detailing the expected operation of the generator's workflow
     */
    public State getState() {
        return state;
    }

    /**
     * Sets the {@link State} as determined by the generator's workflow.
     * <p>
     * This method will provide a default reason. To explicitly provide a reason for the {@link State}, use {@link DryRunStatus#DryRunStatus(Path, State, String)}
     *
     * @param state A {@link State} enum detailing the expected operation of the generator's workflow
     */
    public void setState(State state) {
        if (state != this.state) {
            switch (state) {
                case Write:
                    this.reason = "File will be written.";
                    break;
                case WriteIfNewer:
                    this.reason = "File will be written only if it is new or if contents differ from an existing file.";
                    break;
                case Ignored:
                    this.reason = "Ignored via rules defined in codegen ignore file.";
                    break;
                case SkippedOverwrite:
                    this.reason = "File is configured not to overwrite an existing file of the same name.";
                    break;
                case Error:
                    this.reason = "File error: template does not exist, or file is not accessible.";
                    break;
            }
        }

        this.state = state;
    }

    /**
     * Represents the possible states of a file write operation as determined by the Generator
     */
    enum State {
        Write("w", "Write"),
        WriteIfNewer("n", "Write if New/Updated"),
        Ignored("i", "Ignored"),
        SkippedOverwrite("s", "Skipped Overwrite"),
        Skipped("k", "Skipped by user option(s)"),
        Error("e", "Error evaluating file write state");

        private final String shortDisplay;
        private final String description;

        /**
         * Constructs a new {@link State} with required short value and human-readable description
         *
         * @param shortDisplay The short value used for display
         * @param description  A description of the state which is more human-readable than the enum's name
         */
        State(String shortDisplay, String description) {

            this.shortDisplay = shortDisplay;
            this.description = description;
        }

        /**
         * Gets a description of the state which is more human-readable than the enum's name
         *
         * @return A human-readable description
         */
        public String getDescription() {
            return description;
        }

        /**
         * Gets the short value used for display
         *
         * @return A character representing this state
         */
        public String getShortDisplay() {
            return shortDisplay;
        }
    }
}