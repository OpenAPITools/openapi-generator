/*
 * Copyright 2019 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.openapitools.codegen.utils;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

import com.fasterxml.jackson.core.JsonPointer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.JsonNodeType;

/**
 * An interface for querying and mutating a JSON object graph. Clients indicate the location within the graph at which a
 * value is to be read or written using a <a href="https://tools.ietf.org/html/rfc6901">JSON Pointer</a> as either a
 * string or a <code>JsonPointer</code> instance. The latter is more efficient for repeated access to the same location.
 * For the <code>set(ptr|path, value)</code> and <code>getXxx(ptr|path, defaultValue)</code>methods, the cache
 * automatically creates missing container elements in the path as either object or array nodes according to whether the
 * corresponding path segment is a property name or an integer index respectively, and creates the leaf element as a
 * JSON value node of the appropriate type. Existing tree elements identified by a JSON pointer must match the pointer
 * segments by type; that is, an existing parent of an integer path segment <b>must</b> be an array node and existing
 * parent of a property name path segment <b>must</b> be an object node: if this condition is not met, the method throws
 * a {@linkplain CacheException}. Note that it is possible to store the same object at multiple locations
 * within the tree (thus converting the tree to a <i>graph</i>), in which case the same object may be retrieved using
 * any of the corresponding paths. However, this shared object becomes multiple independent objects when serialised to
 * JSON and if subsequently loaded into a cache instance. Such graphs <b>must</b> be acyclic: storing a cyclic graph in
 * the tree will cause a stack overflow when the cache is saved. Instances are not guaranteed threadsafe and require
 * external synchronisation if mutator methods may be called concurrently from multiple threads. Sparse arrays are not
 * supported - all array elements have a value, even if it is null.
 * <p>
 * <em>N.B. some <code>getXxx()</code> methods can return mutable objects, mutations to which will be unobserved by the
 * cache instance, thus compromising the reliability of the <code>flush*()</code> methods. Callers relying on these
 * methods should either not mutate such objects or they should call the appropriate <code>set()</code> method
 * afterwards to ensure that the cache's 'modified' flag is set.</em>
 * </p>
 *
 * @author Adrian Price, TIBCO Software Inc.
 * @since 4.0.0
 */
public interface JsonCache {
    /**
     * Exception thrown by cache operations. Not intended to be created by client code.
     */
    static class CacheException extends Exception {
        private static final long serialVersionUID = -1215367978375557620L;

        CacheException(String message) {
            super(message);
        }

        CacheException(Throwable cause) {
            super(cause);
        }
    }

    /**
     * A factory for creating JSON cache root instances.
     */
    interface Factory {
        /**
         * The singleton factory instance.
         */
        Factory instance = JsonCacheImpl.FactoryImpl.instance;

        /**
         * Returns a new cache root instance.
         *
         * @return A new instance.
         */
        Root create();

        /**
         * Returns the singleton cache root instance for the specified key. The same instance is returned every time the
         * method is called with the same <code>key</code> value, being lazily created on the first such call.
         *
         * @param key The instance key.
         * @return The singleton instance for <code>key</code>.
         */
        Root get(String key);
    }

    /**
     * Load/merge/save/flush functionality implemented by the root of a JSON cache hierarchy.
     */
    interface Root extends JsonCache {
        /**
         * Describes cache behaviour when a load method is called for a cache instance that is already loaded.
         */
        enum MergePolicy {
            /**
             * Calls to <code>load()</code> methods are ignored when the cache is already loaded.
             */
            NO_MERGE,
            /**
             * Recursively merges the incoming tree into the existing tree, retaining existing leaf properties.
             */
            MERGE_RECURSIVE,
            /**
             * Retains existing root properties, ignoring incoming duplicate properties.
             */
            KEEP_EXISTING,
            /**
             * Overwrites existing root properties with incoming duplicate properties.
             */
            OVERWRITE_EXISTING
        }

        /**
         * If the cache is dirty, saves the object graph in JSON format to the specified file.
         *
         * @param file The output file.
         * @return The receiver, to allow chaining.
         * @throws CacheException       if the root node does not exist or if unable to create or write the file.
         * @throws NullPointerException if <code>file</code> is <code>null</code>.
         * @see #flushOnShutdown(File)
         * @see #save(File)
         */
        Root flush(File file) throws CacheException;

        /**
         * If the cache is dirty, saves the object graph in JSON format to the specified stream.
         *
         * @param out The output stream, which is closed before the method returns.
         * @return The receiver, to allow chaining.
         * @throws CacheException       if the root node does not exist or if unable to create or write the file.
         * @throws NullPointerException if <code>out</code> is <code>null</code>.
         * @see #flushOnShutdown(OutputStream)
         * @see #save(OutputStream)
         */
        Root flush(OutputStream out) throws CacheException;

        /**
         * Makes a best-effort attempt to ensure that the cache gets flushed to a disk file if dirty on shutdown. The
         * call has no additional effect if the shutdown hook has already been registered.
         *
         * @param file The output file.
         * @return The receiver, to allow chaining.
         * @throws NullPointerException if <code>file</code> is <code>null</code>.
         * @see #save(File)
         */
        Root flushOnShutdown(File file);

        /**
         * Makes a best-effort attempt to ensure that the cache gets flushed to an output stream if dirty on shutdown.
         * The call has no additional effect if the shutdown hook has already been registered.
         *
         * @param out The output stream, which is closed after writing it on shutdown.
         * @return The receiver, to allow chaining.
         * @throws NullPointerException if <code>out</code> is <code>null</code>.
         * @see #save(OutputStream)
         */
        Root flushOnShutdown(OutputStream out);

        /**
         * Returns the mapper used for JSON-object marshalling and serialisation operations. Callers may configure the
         * mapper to achieve the desired JSON serialisation format.
         *
         * @return The mapper.
         * @see #mapper(ObjectMapper)
         */
        ObjectMapper getMapper();

        /**
         * Returns the merge policy that applies when <code>load()</code> is called on a cache that is already loaded.
         * The default is {@link MergePolicy#MERGE_RECURSIVE};
         *
         * @return The merge policy.
         * @see #mergePolicy(MergePolicy)
         */
        MergePolicy getMergePolicy();

        /**
         * Indicates whether the cached has unsaved changes.
         *
         * @return <code>true</code> if there are unsaved changes.
         */
        boolean isDirty();

        /**
         * Loads the cache from the specified file. If the cache is already loaded, merges the incoming tree according
         * to the current {@link #mergePolicy(MergePolicy) merge policy}. The call has no effect if the file does not
         * exist.
         *
         * @param file The JSON file to load.
         * @return The receiver, to allow chaining.
         * @throws CacheException       if the file exists but could not be read or its content is not valid JSON.
         * @throws NullPointerException if <code>file</code> is <code>null</code>.
         * @see #save(File)
         * @see #save(OutputStream)
         * @see #unload()
         */
        Root load(File file) throws CacheException;

        /**
         * Loads the cache from the specified stream. If the cache is already loaded, merges the incoming tree according
         * to the current {@link #mergePolicy(MergePolicy) merge policy}.
         *
         * @param in The input stream from which to load, which is closed before the method returns.
         * @return The receiver, to allow chaining.
         * @throws CacheException       if the stream content is not valid JSON.
         * @throws NullPointerException if <code>in</code> is <code>null</code>.
         * @see #save(File)
         * @see #save(OutputStream)
         * @see #unload()
         */
        Root load(InputStream in) throws CacheException;

        /**
         * Sets the mapper to use for JSON-object marshalling and serialisation operations.
         *
         * @param mapper The new mapper.
         * @return The receiver, to allow chaining.
         * @throws NullPointerException if <code>mapper</code> is <code>null</code>.
         * @see #getMapper()
         */
        Root mapper(ObjectMapper mapper);

        /**
         * Sets the merge policy that applies when <code>load()</code> is called for a cache that is already loaded.
         *
         * @param policy The merge policy.
         * @return The receiver, to allow chaining.
         * @throws NullPointerException if <code>policy</code> is null.
         * @see #getMergePolicy()
         */
        Root mergePolicy(MergePolicy policy);

        /**
         * Saves the object graph in JSON format to the specified file.
         *
         * @param file The output file.
         * @return The receiver, to allow chaining.
         * @throws NullPointerException if <code>file</code> is null.
         * @throws CacheException       if the root node does not exist or if unable to create or write the file.
         */
        Root save(File file) throws CacheException;

        /**
         * Saves the object graph in JSON format to the specified stream.
         *
         * @param out The output stream, which is closed before the method returns.
         * @return The receiver, to allow chaining.
         * @throws NullPointerException if <code>out</code> is null.
         * @throws CacheException       if the root node does not exist or if unable to create or write the file.
         */
        Root save(OutputStream out) throws CacheException;

        /**
         * Unloads the object graph, setting the root node to <code>null</code>.
         *
         * @return The receiver, to allow chaining.
         * @see #load(File)
         * @see #load(InputStream)
         */
        Root unload();
    }

    /**
     * Adds a <code>BigDecimal</code> to an array. The array is created if it does not already exist.
     *
     * @param ptr   A pointer to the property to set. If the last segment is a positive integer less than the current
     *              array size, <code>value</code> is inserted at the specified index; otherwise, it is appended to the
     *              end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    JsonCache add(JsonPointer ptr, BigDecimal value) throws CacheException;

    /**
     * Adds a <code>BigInteger</code> to an array. The array is created if it does not already exist.
     *
     * @param ptr   A pointer to the property to set. If the last segment is a positive integer less than the current
     *              array size, <code>value</code> is inserted at the specified index; otherwise, it is appended to the
     *              end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    JsonCache add(JsonPointer ptr, BigInteger value) throws CacheException;

    /**
     * Adds a <code>boolean</code> to an array. The array is created if it does not already exist.
     *
     * @param ptr   A pointer to the property to set. If the last segment is a positive integer less than the current
     *              array size, <code>value</code> is inserted at the specified index; otherwise, it is appended to the
     *              end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    JsonCache add(JsonPointer ptr, boolean value) throws CacheException;

    /**
     * Adds a <code>double</code> value to an array. The array is created if it does not already exist.
     *
     * @param ptr   A pointer to the property to set. If the last segment is a positive integer less than the current
     *              array size, <code>value</code> is inserted at the specified index; otherwise, it is appended to the
     *              end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    JsonCache add(JsonPointer ptr, double value) throws CacheException;

    /**
     * Adds a <code>float</code> value to an array. The array is created if it does not already exist.
     *
     * @param ptr   A pointer to the property to set. If the last segment is a positive integer less than the current
     *              array size, <code>value</code> is inserted at the specified index; otherwise, it is appended to the
     *              end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    JsonCache add(JsonPointer ptr, float value) throws CacheException;

    /**
     * Adds an <code>int</code> value to an array. The array is created if it does not already exist.
     *
     * @param ptr   A pointer to the property to set. If the last segment is a positive integer less than the current
     *              array size, <code>value</code> is inserted at the specified index; otherwise, it is appended to the
     *              end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    JsonCache add(JsonPointer ptr, int value) throws CacheException;

    /**
     * Adds a <code>long</code> value to an array. The array is created if it does not already exist.
     *
     * @param ptr   A pointer to the property to set. If the last segment is a positive integer less than the current
     *              array size, <code>value</code> is inserted at the specified index; otherwise, it is appended to the
     *              end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    JsonCache add(JsonPointer ptr, long value) throws CacheException;

    /**
     * Adds an object to an array. The array is created if it does not already exist.
     *
     * @param ptr   A pointer to the property to set. If the last segment is a positive integer less than the current
     *              array size, <code>value</code> is inserted at the specified index; otherwise, it is appended to the
     *              end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    JsonCache add(JsonPointer ptr, Object value) throws CacheException;

    /**
     * Adds a <code>short</code> value to an array. The array is created if it does not already exist.
     *
     * @param ptr   A pointer to the property to set. If the last segment is a positive integer less than the current
     *              array size, <code>value</code> is inserted at the specified index; otherwise, it is appended to the
     *              end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    JsonCache add(JsonPointer ptr, short value) throws CacheException;

    /**
     * Adds a <code>BigDecimal</code> to an array. The array is created if it does not already exist.
     *
     * @param path  A JSON Pointer expression for the property to set. If the last segment is a positive integer less
     *              than the current array size, <code>value</code> is inserted at the specified index; otherwise, it is
     *              appended to the end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     */
    JsonCache add(String path, BigDecimal value) throws CacheException;

    /**
     * Adds a <code>BigInteger</code> to an array. The array is created if it does not already exist.
     *
     * @param path  A JSON Pointer expression for the property to set. If the last segment is a positive integer less
     *              than the current array size, <code>value</code> is inserted at the specified index; otherwise, it is
     *              appended to the end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     */
    JsonCache add(String path, BigInteger value) throws CacheException;

    /**
     * Adds a <code>boolean</code> value to an array. The array is created if it does not already exist.
     *
     * @param path  A JSON Pointer expression for the property to set. If the last segment is a positive integer less
     *              than the current array size, <code>value</code> is inserted at the specified index; otherwise, it is
     *              appended to the end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     */
    JsonCache add(String path, boolean value) throws CacheException;

    /**
     * Adds a <code>double</code> value to an array. The array is created if it does not already exist.
     *
     * @param path  A JSON Pointer expression for the property to set. If the last segment is a positive integer less
     *              than the current array size, <code>value</code> is inserted at the specified index; otherwise, it is
     *              appended to the end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     */
    JsonCache add(String path, double value) throws CacheException;

    /**
     * Adds a <code>float</code> value to an array. The array is created if it does not already exist.
     *
     * @param path  A JSON Pointer expression for the property to set. If the last segment is a positive integer less
     *              than the current array size, <code>value</code> is inserted at the specified index; otherwise, it is
     *              appended to the end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     */
    JsonCache add(String path, float value) throws CacheException;

    /**
     * Adds an <code>int</code> value to an array. The array is created if it does not already exist.
     *
     * @param path  A JSON Pointer expression for the property to set. If the last segment is a positive integer less
     *              than the current array size, <code>value</code> is inserted at the specified index; otherwise, it is
     *              appended to the end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     */
    JsonCache add(String path, int value) throws CacheException;

    /**
     * Adds a <code>long</code> value to an array. The array is created if it does not already exist.
     *
     * @param path  A JSON Pointer expression for the property to set. If the last segment is a positive integer less
     *              than the current array size, <code>value</code> is inserted at the specified index; otherwise, it is
     *              appended to the end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     */
    JsonCache add(String path, long value) throws CacheException;

    /**
     * Adds an object to an array. The array is created if it does not already exist.
     *
     * @param path  A JSON Pointer expression for the property to set. If the last segment is a positive integer less
     *              than the current array size, <code>value</code> is inserted at the specified index; otherwise, it is
     *              appended to the end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     */
    JsonCache add(String path, Object value) throws CacheException;

    /**
     * Adds a <code>short</code> value to an array. The array is created if it does not already exist.
     *
     * @param path  A JSON Pointer expression for the property to set. If the last segment is a positive integer less
     *              than the current array size, <code>value</code> is inserted at the specified index; otherwise, it is
     *              appended to the end of the array.
     * @param value The value to add.
     * @return The receiver, to support chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     */
    JsonCache add(String path, short value) throws CacheException;

    /**
     * Returns a child cache rooted at the given location relative to the receiver's base pointer.
     *
     * @param ptr A pointer to the subtree managed by the new cache.
     * @return The child cache.
     * @throws NullPointerException if <code>ptr</code> is <code>null</code>.
     */
    JsonCache child(JsonPointer ptr);

    /**
     * Returns a child cache rooted at the given location relative to the receiver's base pointer.
     *
     * @param path A JSON Pointer expression for the subtree managed by the new cache. Note that the expression must
     *             start with a forward slash character.
     * @return The child cache.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    JsonCache child(String path);

    /**
     * Deletes a property within the object graph managed by the receiver.
     *
     * @param ptr A pointer to the property to set.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    void delete(JsonPointer ptr) throws CacheException;

    /**
     * Deletes a property within the object graph managed by the receiver.
     *
     * @param path A JSON Pointer expression for the property to set.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     * @throws CacheException           if <code>ptr</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     */
    void delete(String path) throws CacheException;

    /**
     * Tests for the existence of the specified path within the object graph managed by the receiver.
     *
     * @param ptr A pointer to the path to test.
     * @return <code>true</code> if a JSON node corresponding to <code>ptr</code> exists.
     */
    boolean exists(JsonPointer ptr);

    /**
     * Tests for the existence of the specified path within the object graph managed by the receiver.
     *
     * @param path A JSON Pointer expression for the path to test.
     * @return <code>true</code> if a JSON node corresponding to <code>ptr</code> exists.
     */
    boolean exists(String path);

    /**
     * Retrieves an <code>Object</code> value from within the graph.
     *
     * @param ptr A JSON Pointer expression for the value to return.
     * @return the property value or <code>null</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    Object get(JsonPointer ptr) throws CacheException;

    /**
     * Retrieves an <code>Object</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param ptr          A pointer to the value to return.
     * @param defaultValue The default value to return if <code>ptr</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    Object get(JsonPointer ptr, Object defaultValue) throws CacheException;

    /**
     * Retrieves an <code>Object</code> value from within the graph.
     *
     * @param path A pointer to the value to return.
     * @return the property value or <code>null</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    Object get(String path) throws CacheException;

    /**
     * Retrieves an <code>Object</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param path         A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>path</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    Object get(String path, Object defaultValue) throws CacheException;

    /**
     * Retrieves a <code>BigDecimal</code> value from within the graph.
     *
     * @param ptr A pointer to the value to retrieve.
     * @return the property value or <code>null</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    BigDecimal getBigDecimal(JsonPointer ptr) throws CacheException;

    /**
     * Retrieves a <code>BigDecimal</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param ptr          A pointer to the value to return.
     * @param defaultValue The default value to return if <code>ptr</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    BigDecimal getBigDecimal(JsonPointer ptr, BigDecimal defaultValue) throws CacheException;

    /**
     * Retrieves a <code>BigDecimal</code> value from within the graph.
     *
     * @param path A JSON Pointer expression for the value to retrieve.
     * @return the property value or <code>null</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    BigDecimal getBigDecimal(String path) throws CacheException;

    /**
     * Retrieves a <code>BigDecimal</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param path         A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>path</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    BigDecimal getBigDecimal(String path, BigDecimal defaultValue) throws CacheException;

    /**
     * Retrieves a <code>BigInteger</code> value from within the graph.
     *
     * @param ptr A pointer to the value to retrieve.
     * @return the property value or <code>null</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    BigInteger getBigInteger(JsonPointer ptr) throws CacheException;

    /**
     * Retrieves a <code>BigInteger</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param ptr          A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>ptr</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    BigInteger getBigInteger(JsonPointer ptr, BigInteger defaultValue) throws CacheException;

    /**
     * Retrieves a <code>BigInteger</code> value from within the graph.
     *
     * @param path A JSON Pointer expression for the value to retrieve.
     * @return the property value or <code>null</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    BigInteger getBigInteger(String path) throws CacheException;

    /**
     * Retrieves a <code>BigInteger</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param path         A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>path</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    BigInteger getBigInteger(String path, BigInteger defaultValue) throws CacheException;

    /**
     * Retrieves a <code>byte[]</code> value from within the graph.
     *
     * @param ptr A pointer to the value to retrieve.
     * @return the property value or <code>null</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    byte[] getBinary(JsonPointer ptr) throws CacheException;

    /**
     * Retrieves a <code>byte[]</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param ptr          A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>ptr</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    byte[] getBinary(JsonPointer ptr, byte[] defaultValue) throws CacheException;

    /**
     * Retrieves a <code>byte[]</code> value from within the graph.
     *
     * @param path A JSON Pointer expression for the value to retrieve.
     * @return the property value or <code>null</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    byte[] getBinary(String path) throws CacheException;

    /**
     * Retrieves a <code>byte[]</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param path         A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>path</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    byte[] getBinary(String path, byte[] defaultValue) throws CacheException;

    /**
     * Retrieves a <code>boolean</code> value from within the graph.
     *
     * @param ptr A pointer to the value to retrieve.
     * @return the property value or <code>false</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    boolean getBoolean(JsonPointer ptr) throws CacheException;

    /**
     * Retrieves a <code>boolean</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param ptr          A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>ptr</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    boolean getBoolean(JsonPointer ptr, boolean defaultValue) throws CacheException;

    /**
     * Retrieves a <code>boolean</code> value from within the graph.
     *
     * @param path A JSON Pointer expression for the value to retrieve.
     * @return the property value or <code>false</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    boolean getBoolean(String path) throws CacheException;

    /**
     * Retrieves a <code>boolean</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param path         A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>path</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    boolean getBoolean(String path, boolean defaultValue) throws CacheException;

    /**
     * Retrieves a <code>double</code> value from within the graph.
     *
     * @param ptr A pointer to the value to retrieve.
     * @return the property value or <code>0.0D</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    double getDouble(JsonPointer ptr) throws CacheException;

    /**
     * Retrieves a <code>double</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param ptr          A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>ptr</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    double getDouble(JsonPointer ptr, double defaultValue) throws CacheException;

    /**
     * Retrieves a <code>double</code> value from within the graph.
     *
     * @param path A JSON Pointer expression for the value to retrieve.
     * @return the property value or <code>0.0D</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    double getDouble(String path) throws CacheException;

    /**
     * Retrieves a <code>double</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param path         A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>path</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    double getDouble(String path, double defaultValue) throws CacheException;

    /**
     * Retrieves a <code>float</code> value from within the graph.
     *
     * @param ptr A pointer to the value to retrieve.
     * @return the property value or <code>0.0F</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    float getFloat(JsonPointer ptr) throws CacheException;

    /**
     * Retrieves a <code>float</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param ptr          A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>ptr</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    float getFloat(JsonPointer ptr, float defaultValue) throws CacheException;

    /**
     * Retrieves a <code>float</code> value from within the graph.
     *
     * @param path A JSON Pointer expression for the value to retrieve.
     * @return the property value or <code>0.0F</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    float getFloat(String path) throws CacheException;

    /**
     * Retrieves a <code>float</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param path         A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>path</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    float getFloat(String path, float defaultValue) throws CacheException;

    /**
     * Retrieves an <code>int</code> value from within the graph.
     *
     * @param ptr A pointer to the value to retrieve.
     * @return the property value or <code>0</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    int getInt(JsonPointer ptr) throws CacheException;

    /**
     * Retrieves an <code>int</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param ptr          A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>ptr</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    int getInt(JsonPointer ptr, int defaultValue) throws CacheException;

    /**
     * Retrieves an <code>int</code> value from within the graph.
     *
     * @param path A JSON Pointer expression for the value to retrieve.
     * @return the property value or <code>0</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    int getInt(String path) throws CacheException;

    /**
     * Retrieves an <code>int</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param path         A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>path</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    int getInt(String path, int defaultValue) throws CacheException;

    /**
     * Retrieves a <code>long</code> value from within the graph.
     *
     * @param ptr A pointer to the value to retrieve.
     * @return the property value or <code>0L</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    long getLong(JsonPointer ptr) throws CacheException;

    /**
     * Retrieves a <code>long</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param ptr          A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>ptr</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    long getLong(JsonPointer ptr, long defaultValue) throws CacheException;

    /**
     * Retrieves a <code>long</code> value from within the graph.
     *
     * @param path A JSON Pointer expression for the value to retrieve.
     * @return the property value or <code>0L</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    long getLong(String path) throws CacheException;

    /**
     * Retrieves a <code>long</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param path         A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>path</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    long getLong(String path, long defaultValue) throws CacheException;

    /**
     * Returns the node type at the specified location in the object graph.
     *
     * @param ptr A pointer to the node to test.
     * @return The node type.
     */
    JsonNodeType getNodeType(JsonPointer ptr);

    /**
     * Returns the node type at the specified location in the object graph.
     *
     * @param path A JSON Pointer expression for the node to test.
     * @return The node type.
     */
    JsonNodeType getNodeType(String path);

    /**
     * Retrieves a <code>Number</code> value from within the graph.
     *
     * @param ptr A pointer to the value to retrieve.
     * @return the property value or <code>null</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    Number getNumber(JsonPointer ptr) throws CacheException;

    /**
     * Retrieves a <code>Number</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param ptr          A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>ptr</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    Number getNumber(JsonPointer ptr, Number defaultValue) throws CacheException;

    /**
     * Retrieves a <code>Number</code> value from within the graph.
     *
     * @param path A JSON Pointer expression for the value to retrieve.
     * @return the property value or <code>null</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    Number getNumber(String path) throws CacheException;

    /**
     * Retrieves a <code>Number</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param path         A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>path</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    Number getNumber(String path, Number defaultValue) throws CacheException;

    /**
     * Retrieves a typed object value from within the graph.
     *
     * @param <T>  The type of object to return.
     * @param ptr  A pointer to the value to return.
     * @param type The type of object to return.
     * @return the property value or <code>null</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver or
     *                        if the node could not be converted to the requested type.
     */
    <T> T getObject(JsonPointer ptr, Class<T> type) throws CacheException;

    /**
     * Retrieves a typed object value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param <T>          The type of the object to return.
     * @param ptr          A pointer to the value to return.
     * @param defaultValue The default value to return if <code>ptr</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver or
     *                        if the node could not be converted to the requested type.
     */
    <T> T getObject(JsonPointer ptr, T defaultValue) throws CacheException;

    /**
     * Retrieves a typed object value from within the graph.
     *
     * @param <T>  The type of object to return.
     * @param path A JSON pointer expression for the value to return.
     * @param type The type of object to return.
     * @return the property value or <code>null</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver or if the node could not be converted to the requested type.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    <T> T getObject(String path, Class<T> type) throws CacheException;

    /**
     * Retrieves a typed object value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param <T>          The type of the object to return.
     * @param path         A JSON pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>path</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver or if the node could not be converted to the requested type.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    <T> T getObject(String path, T defaultValue) throws CacheException;

    /**
     * Retrieves a typed list of objects from within the graph.
     *
     * @param <T>  The list element type.
     * @param ptr  A pointer to the values to return.
     * @param type The list element type.
     * @return the property values.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver or
     *                        if the nodes could not be converted to the requested type.
     */
    <T> List<T> getObjects(JsonPointer ptr, Class<T> type) throws CacheException;

    /**
     * Retrieves a typed list of objects from within the graph. If the values are not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param <T>          The list element type.
     * @param ptr          A pointer to the values to return.
     * @param type         The list element type.
     * @param defaultValue The default values to return if <code>ptr</code> is not present in the graph.
     * @return the property values.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver or
     *                        if the nodes could not be converted to the requested type.
     */
    <T> List<T> getObjects(JsonPointer ptr, Class<T> type, List<T> defaultValue) throws CacheException;

    /**
     * Retrieves a typed list of objects from within the graph.
     *
     * @param <T>  The list element type.
     * @param path A JSON Pointer expression for the values to return.
     * @param type The list element type.
     * @return the property values.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver or if the nodes could not be converted to the requested type.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    <T> List<T> getObjects(String path, Class<T> type) throws CacheException;

    /**
     * Retrieves a typed list of objects from within the graph. If the values are not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param <T>          The list element type.
     * @param path         A JSON Pointer expression for the values to return.
     * @param type         The list element type.
     * @param defaultValue The default values to return if <code>path</code> is not present in the graph.
     * @return the property values.
     * @throws CacheException           if <code>ptr</code> is not a valid path within the object graph managed by the
     *                                  receiver or if the nodes could not be converted to the requested type.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    <T> List<T> getObjects(String path, Class<T> type, List<T> defaultValue) throws CacheException;

    /**
     * Retrieves a <code>short</code> value from within the graph.
     *
     * @param ptr A pointer to the value to retrieve.
     * @return the property value or <code>0</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    short getShort(JsonPointer ptr) throws CacheException;

    /**
     * Retrieves a <code>short</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param ptr          A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>ptr</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    short getShort(JsonPointer ptr, short defaultValue) throws CacheException;

    /**
     * Retrieves a <code>short</code> value from within the graph.
     *
     * @param path A JSON Pointer expression for the value to retrieve.
     * @return the property value or <code>0</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    short getShort(String path) throws CacheException;

    /**
     * Retrieves a <code>short</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param path         A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>path</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    short getShort(String path, short defaultValue) throws CacheException;

    /**
     * Retrieves a <code>String</code> value from within the graph.
     *
     * @param ptr A pointer to the value to retrieve.
     * @return the property value or <code>null</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    String getString(JsonPointer ptr) throws CacheException;

    /**
     * Retrieves a <code>String</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param ptr          A pointer to the value to return.
     * @param defaultValue The default value to return if <code>ptr</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     */
    String getString(JsonPointer ptr, String defaultValue) throws CacheException;

    /**
     * Retrieves a <code>String</code> value from within the graph.
     *
     * @param path A JSON Pointer expression for the value to retrieve.
     * @return the property value or <code>null</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    String getString(String path) throws CacheException;

    /**
     * Retrieves a <code>String</code> value from within the graph. If the value is not present, stores
     * <code>defaultValue</code> at the specified location and returns this value.
     *
     * @param path         A JSON Pointer expression for the value to return.
     * @param defaultValue The default value to return if <code>path</code> is not present in the graph.
     * @return the property value or <code>defaultValue</code> if not present.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON path expression.
     */
    String getString(String path, String defaultValue) throws CacheException;

    /**
     * Returns the parent cache of which this is a descendant.
     *
     * @return The parent cache, <code>null</code> if the receiver is the root cache.
     */
    JsonCache parent();

    /**
     * Returns the root cache of which this is a descendant.
     *
     * @return The root cache. The root cache returns itself.
     */
    Root root();

    /**
     * Sets a <code>BigDecimal</code> property within the object graph managed by the receiver.
     *
     * @param ptr   A pointer to the property to set.
     * @param value The value to set (can be null).
     * @return The receiver, to allow chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     * @see #delete(JsonPointer)
     */
    JsonCache set(JsonPointer ptr, BigDecimal value) throws CacheException;

    /**
     * Sets a <code>BigInteger</code> property within the object graph managed by the receiver.
     *
     * @param ptr   A pointer to the property to set.
     * @param value The value to set (can be null).
     * @return The receiver, to allow chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     * @see #delete(JsonPointer)
     */
    JsonCache set(JsonPointer ptr, BigInteger value) throws CacheException;

    /**
     * Sets a <code>boolean</code> property within the object graph managed by the receiver.
     *
     * @param ptr   A pointer to the property to set.
     * @param value The value to set.
     * @return The receiver, to allow chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     * @see #delete(JsonPointer)
     */
    JsonCache set(JsonPointer ptr, boolean value) throws CacheException;

    /**
     * Sets a <code>double</code> property within the object graph managed by the receiver.
     *
     * @param ptr   A pointer to the property to set.
     * @param value The value to set.
     * @return The receiver, to allow chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     * @see #delete(JsonPointer)
     */
    JsonCache set(JsonPointer ptr, double value) throws CacheException;

    /**
     * Sets a <code>float</code> property within the object graph managed by the receiver.
     *
     * @param ptr   A pointer to the property to set.
     * @param value The value to set.
     * @return The receiver, to allow chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     * @see #delete(JsonPointer)
     */
    JsonCache set(JsonPointer ptr, float value) throws CacheException;

    /**
     * Sets an <code>int</code> property within the object graph managed by the receiver.
     *
     * @param ptr   A pointer to the property to set.
     * @param value The value to set.
     * @return The receiver, to allow chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     * @see #delete(JsonPointer)
     */
    JsonCache set(JsonPointer ptr, int value) throws CacheException;

    /**
     * Sets a <code>List</code> property within the object graph managed by the receiver.
     *
     * @param ptr    A pointer to the property to set.
     * @param values The values to set.
     * @return The receiver, to allow chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     * @see #delete(JsonPointer)
     */
    JsonCache set(JsonPointer ptr, List<?> values) throws CacheException;

    /**
     * Sets a <code>long</code> property within the object graph managed by the receiver.
     *
     * @param ptr   A pointer to the property to set.
     * @param value The value to set.
     * @return The receiver, to allow chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     * @see #delete(JsonPointer)
     */
    JsonCache set(JsonPointer ptr, long value) throws CacheException;

    /**
     * Sets an <code>Object</code> property within the object graph managed by the receiver.
     *
     * @param ptr   A pointer to the property to set.
     * @param value The value to set (can be null).
     * @return The receiver, to allow chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     * @see #delete(JsonPointer)
     */
    JsonCache set(JsonPointer ptr, Object value) throws CacheException;

    /**
     * Sets a <code>short</code> property within the object graph managed by the receiver.
     *
     * @param ptr   A pointer to the property to set.
     * @param value The value to set.
     * @return The receiver, to allow chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     * @see #delete(JsonPointer)
     */
    JsonCache set(JsonPointer ptr, short value) throws CacheException;

    /**
     * Sets a <code>String</code> property within the object graph managed by the receiver.
     *
     * @param ptr   A pointer to the property to set.
     * @param value The value to set (can be null).
     * @return The receiver, to allow chaining.
     * @throws CacheException if <code>ptr</code> is not a valid path within the object graph managed by the receiver.
     * @see #delete(JsonPointer)
     */
    JsonCache set(JsonPointer ptr, String value) throws CacheException;

    /**
     * Sets a <code>BigDecimal</code> property within the object graph managed by the receiver.
     *
     * @param path  A JSON Pointer expression for the property to set.
     * @param value The value to set (can be null).
     * @return The receiver, to allow chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON Pointer expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @see #delete(String)
     */
    JsonCache set(String path, BigDecimal value) throws CacheException;

    /**
     * Sets a <code>BigInteger</code> property within the object graph managed by the receiver.
     *
     * @param path  A JSON Pointer expression for the property to set.
     * @param value The value to set (can be null).
     * @return The receiver, to allow chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON Pointer expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @see #delete(String)
     */
    JsonCache set(String path, BigInteger value) throws CacheException;

    /**
     * Sets a <code>boolean</code> property within the object graph managed by the receiver.
     *
     * @param path  A JSON Pointer expression for the property to set.
     * @param value The value to set.
     * @return The receiver, to allow chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON Pointer expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @see #delete(String)
     */
    JsonCache set(String path, boolean value) throws CacheException;

    /**
     * Sets a <code>double</code> property within the object graph managed by the receiver.
     *
     * @param path  A JSON Pointer expression for the property to set.
     * @param value The value to set.
     * @return The receiver, to allow chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON Pointer expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @see #delete(String)
     */
    JsonCache set(String path, double value) throws CacheException;

    /**
     * Sets a <code>float</code> property within the object graph managed by the receiver.
     *
     * @param path  A JSON Pointer expression for the property to set.
     * @param value The value to set.
     * @return The receiver, to allow chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON Pointer expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @see #delete(String)
     */
    JsonCache set(String path, float value) throws CacheException;

    /**
     * Sets an <code>int</code> property within the object graph managed by the receiver.
     *
     * @param path  A JSON Pointer expression for the property to set.
     * @param value The value to set.
     * @return The receiver, to allow chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON Pointer expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @see #delete(String)
     */
    JsonCache set(String path, int value) throws CacheException;

    /**
     * Sets a <code>List</code> property within the object graph managed by the receiver.
     *
     * @param path   A JSON Pointer expression for the property to set.
     * @param values The values to set.
     * @return The receiver, to allow chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON Pointer expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @see #delete(String)
     */
    JsonCache set(String path, List<?> values) throws CacheException;

    /**
     * Sets a <code>long</code> property within the object graph managed by the receiver.
     *
     * @param path  A JSON Pointer expression for the property to set.
     * @param value The value to set.
     * @return The receiver, to allow chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON Pointer expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @see #delete(String)
     */
    JsonCache set(String path, long value) throws CacheException;

    /**
     * Sets an <code>Object</code> property within the object graph managed by the receiver.
     *
     * @param path  A JSON Pointer expression for the property to set.
     * @param value The value to set (can be null).
     * @return The receiver, to allow chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON Pointer expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @see #delete(String)
     */
    JsonCache set(String path, Object value) throws CacheException;

    /**
     * Sets a <code>short</code> property within the object graph managed by the receiver.
     *
     * @param path  A JSON Pointer expression for the property to set.
     * @param value The value to set.
     * @return The receiver, to allow chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON Pointer expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @see #delete(String)
     */
    JsonCache set(String path, short value) throws CacheException;

    /**
     * Sets a <code>String</code> property within the object graph managed by the receiver.
     *
     * @param path  A JSON Pointer expression for the property to set.
     * @param value The value to set (can be null).
     * @return The receiver, to allow chaining.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON Pointer expression.
     * @throws CacheException           if <code>path</code> is not a valid path within the object graph managed by the
     *                                  receiver.
     * @see #delete(String)
     */
    JsonCache set(String path, String value) throws CacheException;

    /**
     * Returns the size of a node within the object graph managed by the receiver. For an array node, size is the number
     * of elements; for an object node, size is the number of properties; for other node types, size is <code>0</code>.
     *
     * @param ptr A pointer to the node.
     * @return The size of the node, or <code>0</code> if it does not exist.
     * @see #delete(String)
     */
    int size(JsonPointer ptr);

    /**
     * Returns the size of a node within the object graph managed by the receiver. For an array node, size is the number
     * of elements; for an object node, size is the number of properties; for other node types, size is <code>0</code>.
     *
     * @param path A JSON pointer expression for the node.
     * @return The size of the node, or <code>0</code> if it does not exist.
     * @throws IllegalArgumentException if <code>path</code> is not a valid JSON Pointer expression.
     * @see #delete(String)
     */
    int size(String path);
}