package org.openapitools.codegen.utils.loaders;

import java.io.File;
import java.io.FileNotFoundException;

import com.samskivert.mustache.Mustache;

/**
 * A class used to load helpers from an external JAR into the Mustache engine
 */
public class MustacheHelperLoader extends AbstractHelperLoader<Mustache.Lambda> {
    public MustacheHelperLoader(File jar) throws IllegalAccessError, FileNotFoundException {
        super(Mustache.Lambda.class, jar);
    }
}

