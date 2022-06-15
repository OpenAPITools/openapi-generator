package org.openapitools.codegen;

import java.util.Optional;
import java.util.Set;

public class SpecValidationException extends RuntimeException {

    private Set<String> errors;
    private Set<String> warnings;

    /**
     * Constructs a new runtime exception with {@code null} as its
     * detail message.  The cause is not initialized, and may subsequently be
     * initialized by a call to {@link #initCause}.
     */
    public SpecValidationException() {
    }

    /**
     * Constructs a new runtime exception with the specified detail message.
     * The cause is not initialized, and may subsequently be initialized by a
     * call to {@link #initCause}.
     *
     * @param message the detail message. The detail message is saved for
     *                later retrieval by the {@link #getMessage()} method.
     */
    public SpecValidationException(String message) {
        super(message);
    }

    /**
     * Constructs a new runtime exception with the specified detail message and
     * cause.  <p>Note that the detail message associated with
     * {@code cause} is <i>not</i> automatically incorporated in
     * this runtime exception's detail message.
     *
     * @param message the detail message (which is saved for later retrieval
     *                by the {@link #getMessage()} method).
     * @param cause   the cause (which is saved for later retrieval by the
     *                {@link #getCause()} method).  (A <code>null</code> value is
     *                permitted, and indicates that the cause is nonexistent or
     *                unknown.)
     * @since 1.4
     */
    public SpecValidationException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * Constructs a new runtime exception with the specified cause and a
     * detail message of <code>(cause==null ? null : cause.toString())</code>
     * (which typically contains the class and detail message of
     * <code>cause</code>).  This constructor is useful for runtime exceptions
     * that are little more than wrappers for other throwables.
     *
     * @param cause the cause (which is saved for later retrieval by the
     *              {@link #getCause()} method).  (A <code>null</code> value is
     *              permitted, and indicates that the cause is nonexistent or
     *              unknown.)
     * @since 1.4
     */
    public SpecValidationException(Throwable cause) {
        super(cause);
    }

    /**
     * Constructs a new runtime exception with the specified detail
     * message, cause, suppression enabled or disabled, and writable
     * stack trace enabled or disabled.
     *
     * @param message            the detail message.
     * @param cause              the cause.  (A {@code null} value is permitted,
     *                           and indicates that the cause is nonexistent or unknown.)
     * @param enableSuppression  whether or not suppression is enabled
     *                           or disabled
     * @param writableStackTrace whether or not the stack trace should
     *                           be writable
     * @since 1.7
     */
    public SpecValidationException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

    public Set<String> getErrors() {
        return errors;
    }

    public Set<String> getWarnings() {
        return warnings;
    }

    public void setErrors(Set<String> errors) {
        this.errors = errors;
    }

    public void setWarnings(Set<String> warnings) {
        this.warnings = warnings;
    }

    /**
     * Returns the detail message string of this throwable.
     *
     * @return the detail message string of this {@code Throwable} instance
     * (which may be {@code null}).
     */
    @Override
    public String getMessage() {
        int errorCount = 0;
        if (errors != null) errorCount = errors.size();
        int warningCount = 0;
        if (warnings != null) warningCount = warnings.size();

        StringBuilder sb = new StringBuilder();
        sb.append(System.lineSeparator())
            .append("Errors: ")
            .append(System.lineSeparator());

        Optional.ofNullable(errors).ifPresent(_errors -> {
            for (String msg : errors) {
                sb.append("\t-").append(msg).append(System.lineSeparator());
            }
        });

        Optional.ofNullable(warnings).filter(warnings -> !warnings.isEmpty()).ifPresent(_errors -> {
            sb.append("Warnings: ").append(System.lineSeparator());
            for (String msg : errors) {
                sb.append("\t-").append(msg).append(System.lineSeparator());
            }
        });

        return super.getMessage() + " | " +
                "Error count: " + errorCount + ", Warning count: " + warningCount + sb;
    }
}
