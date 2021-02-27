package org.openapitools.codegen.api;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;

/**
 * Interface for abstractions around writing templated data to a file.
 */
public interface TemplateProcessor {
    /**
     * Writes data to a compiled template
     *
     * @param data Input data
     * @param template Input template location
     * @param target The targeted file output location
     *
     * @return The actual file
     * @throws IOException If file cannot be written.
     */
    File write(Map<String, Object> data, String template, File target) throws IOException;

    /**
     * Write bytes to a file
     *
     * @param filename The name of file to write
     * @param contents The contents bytes.  Typically this is a UTF-8 formatted string.
     * @return File representing the written file.
     * @throws IOException If file cannot be written.
     */
    File writeToFile(String filename, byte[] contents) throws IOException;

    /**
     * Allow a caller to mark a path as ignored with accompanying reason
     *
     * @param path The ignored path
     * @param context The reason for ignoring this path
     */
    void ignore(Path path, String context);

    /**
     * Allow a caller to mark a path as skipped with accompanying reason
     *
     * @param path The skipped path
     * @param context The reason for skipping this path
     */
    void skip(Path path, String context);

    /**
     * Allow a caller to mark a path having errored during processing with accompanying reason
     *
     * @param path The path which has caused an error
     * @param context The reason for the error
     */
    default void error(Path path, String context) { }
}
