package org.openapitools.codegen.utils.loaders;

import java.io.File;
import java.io.FileNotFoundException;

import com.github.jknack.handlebars.Helper;

/**
 * A class used to load helpers from an external JAR into the Handlebars engine
 */
public class HandlebarsHelperLoader extends AbstractHelperLoader<Helper> {
    public HandlebarsHelperLoader(File jar) throws IllegalAccessError, FileNotFoundException {
        super(Helper.class, jar);
    }
}

