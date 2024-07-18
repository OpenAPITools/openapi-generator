package org.openapitools.codegen.util;

import java.io.InputStream;
import java.net.URL;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Provide drop-in style classloading utils that allow the thread context classloader
 * to be used/preferred over other class sources. The intent of this class is to work
 * with both relative (ClassLoader) and absolute (Class) paths and allow flexibility
 * in class loading but give preference to the current thread's classloader.
 */
public class ClassLoadingUtils {

    /**
     * Attempts to load the resource stream from the Thread.currentThread().getContextClassloader() before
     * trying each of the provided sources. Sources are expected to be either an instance of Class or
     * of ClassLoader. The first instance found is the one that will be used.
     *
     * @param name of the resource to load
     * @param additionalSources provided to try after the current thread's classloader
     * @return the input stream representing the resource if found, null otherwise
     */
    public static InputStream getResourceAsStream(final String name, final Object... additionalSources) {
        return getFromSources(source -> {
            if (source instanceof Class) {
                return Optional
                        .ofNullable(((Class<?>) source).getResourceAsStream(name))
                        .or(() -> Optional.ofNullable(((Class<?>) source).getClassLoader().getResourceAsStream(name)))
                        .orElse(null);
            }
            if (source instanceof ClassLoader) {
                return ((ClassLoader)source).getResourceAsStream(name);
            }
            return null;
        }, additionalSources);
    }

    /**
     * Attempts to load the resource URL from the Thread.currentThread().getContextClassloader() before
     * trying each of the provided sources. Sources are expected to be either an instance of Class or
     * of ClassLoader. The first instance found is the one that will be used.
     *
     * @param name of the resource to load
     * @param additionalSources provided to try after the current thread's classloader
     * @return the URL representing the resource if found, null otherwise
     */
    public static URL getResource(final String name, final Object... additionalSources) {
        return getFromSources(source -> {
            if (source instanceof Class) {
                return Optional
                        .ofNullable(((Class<?>) source).getResource(name))
                        .or(() -> Optional.ofNullable(((Class<?>) source).getClassLoader().getResource(name)))
                        .orElse(null);
            }
            if (source instanceof ClassLoader) {
                return ((ClassLoader)source).getResource(name);
            }
            return null;
        }, additionalSources);
    }

    /**
     * Given a set of objects to act ass sources, this method takes a transform that will supply
     * the classloader and return the type expected from the classloader. Used as
     * a shortcut to stream through the thread classloader and then other sources
     * provided looking for whatever is needed from them.
     *
     * @param transform function that will be applied and used to turn the source into a response
     * @param additionalSources provided to try after the current thread's classloader
     * @return transformed value if available, null otherwise
     * @param <T> representing the returned type
     */
    private static <T> T getFromSources(Function<Object, T> transform, final Object... additionalSources) {
        final List<Object> loaderList = new ArrayList<>();
        loaderList.add(Thread.currentThread().getContextClassLoader());
        loaderList.addAll(Arrays.stream(additionalSources).collect(Collectors.toList()));

        return loaderList.stream()
                .filter(Objects::nonNull)
                .map(transform)
                .filter(Objects::nonNull)
                .findFirst()
                .orElse(null);
    }
}
