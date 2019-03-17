package org.openapitools.codegen.api;

/**
 * Provides abstractions around the template engine adapter interface, for reuse by implementers.
 */
public abstract class AbstractTemplatingEngineAdapter implements TemplatingEngineAdapter {

    /**
     * Gets all possible template paths for a given location.
     *
     * @param location The full location of the template.
     *
     * @return A new array of locations, modified according to the extensions or other adapter rules.
     */
    protected String[] getModifiedFileLocation(String location) {
        String[] extensions = getFileExtensions();
        String[] result = new String[extensions.length];
        for (int i = 0; i < extensions.length; i++) {
            String extension = extensions[i];
            result[i] = String.format("%s.%s", getPathWithoutExtension(location), extension);
        }
        return result;
    }

    /**
     * Returns the path without an extension for an input location.
     *
     * @param location The location of the file, with original file extension intact.
     *
     * @return The full path, without extension (e.g. /path/to/file.txt => /path/to/file)
     */
    private String getPathWithoutExtension(String location) {
        if (location == null) return null;
        int idx = location.lastIndexOf('.');
        if (idx == -1) return location;
        return location.substring(0, idx);
    }
}
