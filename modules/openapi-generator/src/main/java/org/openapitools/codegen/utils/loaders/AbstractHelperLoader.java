package org.openapitools.codegen.utils.loaders;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.*;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * An abstract class used to load helpers included in JARs into the template engine
 *
 * @param <THelperType> The type of helper class this loader should output
 */
public abstract class AbstractHelperLoader<THelperType> {
    protected final Logger LOGGER = LoggerFactory.getLogger(AbstractHelperLoader.class);

    private final File _jar;

    private final Map<String, THelperType> _helpers = new HashMap<>();

    private final Class<THelperType> _helperClass;

    public Map<String, THelperType> getHelpers() {
        return this._helpers;
    }

    /**
     * An abstract class used to load helpers included in JARs into the template engine
     *
     * @param helperTypeClass The type of helper class to look for in the JAR. This could be Mustache.Lambda for example
     * @param jar             The JAR to look for helpers in
     * @throws IllegalArgumentException A helper class type or JAR path were not specified
     * @throws FileNotFoundException    The given JAR does not exist
     */
    protected AbstractHelperLoader(Class<THelperType> helperTypeClass, File jar) throws IllegalArgumentException, FileNotFoundException {
        if (helperTypeClass == null) {
            throw new IllegalArgumentException("helperTypeClass must be provided");
        }
        this._helperClass = helperTypeClass;

        if (jar == null) {
            throw new IllegalArgumentException("Given JAR cannot be null");
        }

        if (!jar.exists()) {
            throw new FileNotFoundException("Given JAR ('" + jar.getPath() + "') does not exist");
        }

        this._jar = jar;
    }

    /**
     * Loads the helpers defined in the JAR linked to this instance into the template engine
     *
     * @throws IOException Could not initialize an instance of URLClassLoader for the given JAR
     */
    public void load() throws IOException {
        List<JarEntry> classEntries = this.getClassJarEntries(this._jar);

        URL[] jarUrl = new URL[]{new URL("jar:file:" + this._jar.getAbsolutePath() + "!/")};
        try (URLClassLoader loader = URLClassLoader.newInstance(jarUrl)) {

            for (JarEntry classEntry : classEntries) {
                THelperType lambda = this.filterEntry(loader, classEntry);
                if (lambda != null) {
                    String className = lambda.getClass().getSimpleName();
                    LOGGER.debug("Storing helper {}", className);
                    this._helpers.put(className, lambda);
                }
            }
        }
    }

    /**
     * Get all classes from the given JAR
     *
     * @param jarPath The JAR to search for classes in
     * @return A list of JarEntry where each entry is a class
     * @throws IOException Could not read the given JAR
     */
    private List<JarEntry> getClassJarEntries(File jarPath) throws IOException {
        ArrayList<JarEntry> classes = new ArrayList<>();

        try (JarFile jarFile = new JarFile(jarPath)) {
            Enumeration<JarEntry> e = jarFile.entries();
            int entryCount = 0;
            while (e.hasMoreElements()) {
                JarEntry jarEntry = e.nextElement();
                entryCount++;
                if (jarEntry.getName().endsWith(".class")) {
                    LOGGER.debug("Entry {} is a class and will be tested", jarEntry.getName());
                    classes.add(jarEntry);
                }
            }
            LOGGER.debug("JAR {} contains {} entries, of which {} are classes", jarPath, entryCount, classes.size());
            return classes;
        }
    }

    @SuppressWarnings("unused")
    protected boolean isClassRelevant(Class<THelperType> classToTest) {
        // Parameter classToTest is for implementors to use if necessary
        return true;
    }

    /**
     * Tests whether the given class JarEntry is a helper and returns an instance of it if it is
     *
     * @param loader The class loader to use for instantiating the helper
     * @param entry  The class entry to test and construct
     * @return A new instance of THelperType if the entry is suitable, null otherwise
     */
    @SuppressWarnings("unchecked")
    protected THelperType filterEntry(URLClassLoader loader, JarEntry entry) {
        LOGGER.debug("Processing entry {}", entry.getName());

        if (!entry.getName().endsWith(".class")) {
            return null;
        }

        String fullClassName = entry.getName()
                .replace("/", ".")
                .replace(".class", "");
        try {
            Class<?> loadedClass = loader.loadClass(fullClassName);
            if (this._helperClass.isAssignableFrom(loadedClass)) {
                Class<THelperType> helper = (Class<THelperType>) loadedClass;
                if (this.isClassRelevant(helper)) {
                    LOGGER.debug("Entry {} is a helper class and will be returned", fullClassName);
                    return helper.getConstructor().newInstance();
                }
            }
        } catch (ClassNotFoundException e) {
            LOGGER.warn("Could not load class {} from JAR", fullClassName);
        } catch (NoSuchMethodException e) {
            LOGGER.warn("Could not load class {} - no default constructor was found", fullClassName);
        } catch (InvocationTargetException | InstantiationException | IllegalAccessException e) {
            LOGGER.warn("Could not instantiate class " + fullClassName, e);
        }
        return null;
    }
}
