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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonPointer;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ContainerNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.JsonNodeType;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.POJONode;

/**
 * A cache implementation for loading, querying, mutating and saving a JSON object graph.
 *
 * @author Adrian Price, TIBCO Software Inc.
 * @since 4.0.0
 */
class JsonCacheImpl implements JsonCache.Root {
    /**
     * Manages a sub-tree of a parent cache, identified by a base pointer rooted.
     */
    private static class ChildCacheImpl implements JsonCache {
        private final JsonPointer basePtr;
        private final JsonCache parent;

        private ChildCacheImpl(JsonCache parent, JsonPointer basePtr) {
            this.parent = parent;
            this.basePtr = basePtr;
        }

        @Override
        public JsonCache add(JsonPointer ptr, BigDecimal value) throws CacheException {
            parent.add(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache add(JsonPointer ptr, BigInteger value) throws CacheException {
            parent.add(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache add(JsonPointer ptr, boolean value) throws CacheException {
            parent.add(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache add(JsonPointer ptr, double value) throws CacheException {
            parent.add(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache add(JsonPointer ptr, float value) throws CacheException {
            parent.add(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache add(JsonPointer ptr, int value) throws CacheException {
            parent.add(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache add(JsonPointer ptr, long value) throws CacheException {
            parent.add(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache add(JsonPointer ptr, Object value) throws CacheException {
            parent.add(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache add(JsonPointer ptr, short value) throws CacheException {
            parent.add(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache add(String path, BigDecimal value) throws CacheException {
            parent.add(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache add(String path, BigInteger value) throws CacheException {
            parent.add(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache add(String path, boolean value) throws CacheException {
            parent.add(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache add(String path, double value) throws CacheException {
            parent.add(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache add(String path, float value) throws CacheException {
            parent.add(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache add(String path, int value) throws CacheException {
            parent.add(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache add(String path, long value) throws CacheException {
            parent.add(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache add(String path, Object value) throws CacheException {
            parent.add(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache add(String path, short value) throws CacheException {
            parent.add(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache child(JsonPointer basePtr) {
            return new ChildCacheImpl(this, basePtr);
        }

        @Override
        public JsonCache child(String path) {
            return child(JsonPointer.compile(path));
        }

        @Override
        public void delete(JsonPointer ptr) throws CacheException {
            parent.delete(basePtr.append(ptr));
        }

        @Override
        public void delete(String path) throws CacheException {
            parent.delete(basePtr.append(JsonPointer.compile(path)));
        }

        @Override
        public boolean exists(JsonPointer ptr) {
            return parent.exists(basePtr.append(ptr));
        }

        @Override
        public boolean exists(String path) {
            return parent.exists(basePtr.append(JsonPointer.compile(path)));
        }

        @Override
        public Object get(JsonPointer ptr) throws CacheException {
            return parent.get(basePtr.append(ptr));
        }

        @Override
        public Object get(JsonPointer ptr, Object defaultValue) throws CacheException {
            return parent.get(basePtr.append(ptr), defaultValue);
        }

        @Override
        public Object get(String path) throws CacheException {
            return parent.get(basePtr.append(JsonPointer.compile(path)));
        }

        @Override
        public Object get(String path, Object defaultValue) throws CacheException {
            return parent.get(basePtr.append(JsonPointer.compile(path)), defaultValue);
        }

        @Override
        public BigDecimal getBigDecimal(JsonPointer ptr) throws CacheException {
            return parent.getBigDecimal(basePtr.append(ptr));
        }

        @Override
        public BigDecimal getBigDecimal(JsonPointer ptr, BigDecimal defaultValue) throws CacheException {
            return parent.getBigDecimal(basePtr.append(ptr), defaultValue);
        }

        @Override
        public BigDecimal getBigDecimal(String path) throws CacheException {
            return parent.getBigDecimal(basePtr.append(JsonPointer.compile(path)));
        }

        @Override
        public BigDecimal getBigDecimal(String path, BigDecimal defaultValue) throws CacheException {
            return parent.getBigDecimal(basePtr.append(JsonPointer.compile(path)), defaultValue);
        }

        @Override
        public BigInteger getBigInteger(JsonPointer ptr) throws CacheException {
            return parent.getBigInteger(basePtr.append(ptr));
        }

        @Override
        public BigInteger getBigInteger(JsonPointer ptr, BigInteger defaultValue) throws CacheException {
            return parent.getBigInteger(basePtr.append(ptr), defaultValue);
        }

        @Override
        public BigInteger getBigInteger(String path) throws CacheException {
            return parent.getBigInteger(basePtr.append(JsonPointer.compile(path)));
        }

        @Override
        public BigInteger getBigInteger(String path, BigInteger defaultValue) throws CacheException {
            return parent.getBigInteger(basePtr.append(JsonPointer.compile(path)), defaultValue);
        }

        @Override
        public byte[] getBinary(JsonPointer ptr) throws CacheException {
            return parent.getBinary(basePtr.append(ptr));
        }

        @Override
        public byte[] getBinary(JsonPointer ptr, byte[] defaultValue) throws CacheException {
            return parent.getBinary(basePtr.append(ptr), defaultValue);
        }

        @Override
        public byte[] getBinary(String path) throws CacheException {
            return parent.getBinary(basePtr.append(JsonPointer.compile(path)));
        }

        @Override
        public byte[] getBinary(String path, byte[] defaultValue) throws CacheException {
            return parent.getBinary(basePtr.append(JsonPointer.compile(path)), defaultValue);
        }

        @Override
        public boolean getBoolean(JsonPointer ptr) throws CacheException {
            return parent.getBoolean(basePtr.append(ptr));
        }

        @Override
        public boolean getBoolean(JsonPointer ptr, boolean defaultValue) throws CacheException {
            return parent.getBoolean(basePtr.append(ptr), defaultValue);
        }

        @Override
        public boolean getBoolean(String path) throws CacheException {
            return parent.getBoolean(basePtr.append(JsonPointer.compile(path)));
        }

        @Override
        public boolean getBoolean(String path, boolean defaultValue) throws CacheException {
            return parent.getBoolean(basePtr.append(JsonPointer.compile(path)), defaultValue);
        }

        @Override
        public double getDouble(JsonPointer ptr) throws CacheException {
            return parent.getDouble(basePtr.append(ptr));
        }

        @Override
        public double getDouble(JsonPointer ptr, double defaultValue) throws CacheException {
            return parent.getDouble(basePtr.append(ptr), defaultValue);
        }

        @Override
        public double getDouble(String path) throws CacheException {
            return parent.getDouble(basePtr.append(JsonPointer.compile(path)));
        }

        @Override
        public double getDouble(String path, double defaultValue) throws CacheException {
            return parent.getDouble(basePtr.append(JsonPointer.compile(path)), defaultValue);
        }

        @Override
        public float getFloat(JsonPointer ptr) throws CacheException {
            return parent.getFloat(basePtr.append(ptr));
        }

        @Override
        public float getFloat(JsonPointer ptr, float defaultValue) throws CacheException {
            return parent.getFloat(basePtr.append(ptr), defaultValue);
        }

        @Override
        public float getFloat(String path) throws CacheException {
            return parent.getFloat(basePtr.append(JsonPointer.compile(path)));
        }

        @Override
        public float getFloat(String path, float defaultValue) throws CacheException {
            return parent.getFloat(basePtr.append(JsonPointer.compile(path)), defaultValue);
        }

        @Override
        public int getInt(JsonPointer ptr) throws CacheException {
            return parent.getInt(basePtr.append(ptr));
        }

        @Override
        public int getInt(JsonPointer ptr, int defaultValue) throws CacheException {
            return parent.getInt(basePtr.append(ptr), defaultValue);
        }

        @Override
        public int getInt(String path) throws CacheException {
            return parent.getInt(basePtr.append(JsonPointer.compile(path)));
        }

        @Override
        public int getInt(String path, int defaultValue) throws CacheException {
            return parent.getInt(basePtr.append(JsonPointer.compile(path)), defaultValue);
        }

        @Override
        public long getLong(JsonPointer ptr) throws CacheException {
            return parent.getLong(basePtr.append(ptr));
        }

        @Override
        public long getLong(JsonPointer ptr, long defaultValue) throws CacheException {
            return parent.getLong(basePtr.append(ptr), defaultValue);
        }

        @Override
        public long getLong(String path) throws CacheException {
            return parent.getLong(basePtr.append(JsonPointer.compile(path)));
        }

        @Override
        public long getLong(String path, long defaultValue) throws CacheException {
            return parent.getLong(basePtr.append(JsonPointer.compile(path)), defaultValue);
        }

        @Override
        public JsonNodeType getNodeType(JsonPointer ptr) {
            return parent.getNodeType(basePtr.append(ptr));
        }

        @Override
        public JsonNodeType getNodeType(String path) {
            return parent.getNodeType(basePtr.append(JsonPointer.compile(path)));
        }

        @Override
        public Number getNumber(JsonPointer ptr) throws CacheException {
            return parent.getNumber(basePtr.append(ptr));
        }

        @Override
        public Number getNumber(JsonPointer ptr, Number defaultValue) throws CacheException {
            return parent.getNumber(basePtr.append(ptr), defaultValue);
        }

        @Override
        public Number getNumber(String path) throws CacheException {
            return parent.getNumber(basePtr.append(JsonPointer.compile(path)));
        }

        @Override
        public Number getNumber(String path, Number defaultValue) throws CacheException {
            return parent.getNumber(basePtr.append(JsonPointer.compile(path)), defaultValue);
        }

        @Override
        public <T> T getObject(JsonPointer ptr, Class<T> type) throws CacheException {
            return parent.getObject(basePtr.append(ptr), type);
        }

        @Override
        public <T> T getObject(JsonPointer ptr, T defaultValue) throws CacheException {
            return parent.getObject(basePtr.append(ptr), defaultValue);
        }

        @Override
        public <T> T getObject(String path, Class<T> type) throws CacheException {
            return parent.getObject(basePtr.append(JsonPointer.compile(path)), type);
        }

        @Override
        public <T> T getObject(String path, T defaultValue) throws CacheException {
            return parent.getObject(basePtr.append(JsonPointer.compile(path)), defaultValue);
        }

        @Override
        public <T> List<T> getObjects(JsonPointer ptr, Class<T> type) throws CacheException {
            return parent.getObjects(basePtr.append(ptr), type);
        }

        @Override
        public <T> List<T> getObjects(JsonPointer ptr, Class<T> type, List<T> defaultValue) throws CacheException {
            return parent.getObjects(basePtr.append(ptr), type, defaultValue);
        }

        @Override
        public <T> List<T> getObjects(String path, Class<T> type) throws CacheException {
            return parent.getObjects(basePtr.append(JsonPointer.compile(path)), type);
        }

        @Override
        public <T> List<T> getObjects(String path, Class<T> type, List<T> defaultValue) throws CacheException {
            return parent.getObjects(basePtr.append(JsonPointer.compile(path)), type, defaultValue);
        }

        @Override
        public short getShort(JsonPointer ptr) throws CacheException {
            return parent.getShort(basePtr.append(ptr));
        }

        @Override
        public short getShort(JsonPointer ptr, short defaultValue) throws CacheException {
            return parent.getShort(basePtr.append(ptr), defaultValue);
        }

        @Override
        public short getShort(String path) throws CacheException {
            return parent.getShort(basePtr.append(JsonPointer.compile(path)));
        }

        @Override
        public short getShort(String path, short defaultValue) throws CacheException {
            return parent.getShort(basePtr.append(JsonPointer.compile(path)), defaultValue);
        }

        @Override
        public String getString(JsonPointer ptr) throws CacheException {
            return parent.getString(basePtr.append(ptr));
        }

        @Override
        public String getString(JsonPointer ptr, String defaultValue) throws CacheException {
            return parent.getString(basePtr.append(ptr), defaultValue);
        }

        @Override
        public String getString(String path) throws CacheException {
            return parent.getString(basePtr.append(JsonPointer.compile(path)));
        }

        @Override
        public String getString(String path, String defaultValue) throws CacheException {
            return parent.getString(basePtr.append(JsonPointer.compile(path)), defaultValue);
        }

        @Override
        public JsonCache parent() {
            return parent;
        }

        @Override
        public Root root() {
            return parent.root();
        }

        @Override
        public JsonCache set(JsonPointer ptr, BigDecimal value) throws CacheException {
            parent.set(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache set(JsonPointer ptr, BigInteger value) throws CacheException {
            parent.set(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache set(JsonPointer ptr, boolean value) throws CacheException {
            parent.set(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache set(JsonPointer ptr, double value) throws CacheException {
            parent.set(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache set(JsonPointer ptr, float value) throws CacheException {
            parent.set(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache set(JsonPointer ptr, int value) throws CacheException {
            parent.set(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache set(JsonPointer ptr, List<?> values) throws CacheException {
            parent.set(basePtr.append(ptr), values);
            return this;
        }

        @Override
        public JsonCache set(JsonPointer ptr, long value) throws CacheException {
            parent.set(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache set(JsonPointer ptr, Object value) throws CacheException {
            parent.set(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache set(JsonPointer ptr, short value) throws CacheException {
            parent.set(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache set(JsonPointer ptr, String value) throws CacheException {
            parent.set(basePtr.append(ptr), value);
            return this;
        }

        @Override
        public JsonCache set(String path, BigDecimal value) throws CacheException {
            parent.set(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache set(String path, BigInteger value) throws CacheException {
            parent.set(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache set(String path, boolean value) throws CacheException {
            parent.set(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache set(String path, double value) throws CacheException {
            parent.set(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache set(String path, float value) throws CacheException {
            parent.set(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache set(String path, int value) throws CacheException {
            parent.set(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache set(String path, List<?> values) throws CacheException {
            parent.set(basePtr.append(JsonPointer.compile(path)), values);
            return this;
        }

        @Override
        public JsonCache set(String path, long value) throws CacheException {
            parent.set(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache set(String path, Object value) throws CacheException {
            parent.set(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache set(String path, short value) throws CacheException {
            parent.set(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public JsonCache set(String path, String value) throws CacheException {
            parent.set(basePtr.append(JsonPointer.compile(path)), value);
            return this;
        }

        @Override
        public int size(JsonPointer ptr) {
            return parent.size(basePtr.append(ptr));
        }

        @Override
        public int size(String path) {
            return parent.size(basePtr.append(JsonPointer.compile(path)));
        }

        @Override
        public String toString() {
            JsonNode node;
            try {
                node = getObject(EMPTY_PTR, JsonNode.class);
            } catch (CacheException e) {
                LOGGER.error("", e);
                node = null;
            }
            return "ChildCacheImpl [basePtr=" + basePtr + ", node=" + node + ']';
        }
    }

    static class FactoryImpl implements JsonCache.Factory {
        static final JsonCache.Factory instance = new FactoryImpl();
        private final Map<String, JsonCacheImpl> instances = new HashMap<>();

        @Override
        public Root create() {
            return new JsonCacheImpl();
        }

        @Override
        public Root get(String key) {
            synchronized (instances) {
                JsonCacheImpl instance = instances.get(key);
                if (instance == null) {
                    instance = new JsonCacheImpl();
                    instances.put(key, instance);
                }
                return instance;
            }
        }
    }

    protected static final JsonPointer EMPTY_PTR = JsonPointer.compile("/");

    private static final Pattern INTEGER = Pattern.compile("^\\d+$");

    protected static final Logger LOGGER = LoggerFactory.getLogger(JsonCacheImpl.class);

    protected boolean isDirty;

    protected boolean isLoaded;

    protected ObjectMapper mapper;

    protected MergePolicy mergePolicy = MergePolicy.MERGE_RECURSIVE;

    protected ContainerNode<?> root;

    private boolean shutdownHookRegistered;

    protected JsonCacheImpl() {
        mapper = new ObjectMapper();
        mapper.enable(SerializationFeature.INDENT_OUTPUT);
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    }

    @Override
    public JsonCache add(JsonPointer ptr, BigDecimal value) {
        return add(ptr, nodeFor(value));
    }

    @Override
    public JsonCache add(JsonPointer ptr, BigInteger value) {
        return add(ptr, nodeFor(value));
    }

    @Override
    public JsonCache add(JsonPointer ptr, boolean value) {
        return add(ptr, nodeFor(value));
    }

    @Override
    public JsonCache add(JsonPointer ptr, double value) {
        return add(ptr, nodeFor(value));
    }

    @Override
    public JsonCache add(JsonPointer ptr, float value) {
        return add(ptr, nodeFor(value));
    }

    @Override
    public JsonCache add(JsonPointer ptr, int value) {
        return add(ptr, nodeFor(value));
    }

    protected JsonCache add(JsonPointer ptr, JsonNode node) {
        // If ptr ends with an array index, this implies inserting at the specified index.
        // If ptr does not end with an array index, this implies appending to the end of the array.
        // In both cases the array in question and its ancestors must be created if they do not already exist.
        String lastProperty = ptr.last().getMatchingProperty();
        boolean isIndexed = isInteger(lastProperty);
        ContainerNode<?> container = ensureContainerExists(ptr, !isIndexed);
        switch (container.getNodeType()) {
            case ARRAY:
                ArrayNode array = (ArrayNode) container;
                int index = isIndexed ? Integer.parseInt(lastProperty) : array.size();
                if (index < array.size()) {
                    array.insert(index, node);
                } else {
                    // Fill any gap between current size and index with nulls (Jackson doesn't support sparse arrays).
                    for (int i = array.size(); i < index; i++)
                        array.add(array.nullNode());
                    array.add(node);
                }
                break;
            default:
                throw new IllegalArgumentException(ptr + " does not identify an array");
        }
        setDirty();
        return this;
    }

    @Override
    public JsonCache add(JsonPointer ptr, long value) {
        return add(ptr, nodeFor(value));
    }

    @Override
    public JsonCache add(JsonPointer ptr, Object value) {
        return add(ptr, nodeFor(value));
    }

    @Override
    public JsonCache add(JsonPointer ptr, short value) {
        return add(ptr, nodeFor(value));
    }

    @Override
    public JsonCache add(String path, BigDecimal value) {
        return add(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache add(String path, BigInteger value) {
        return add(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache add(String path, boolean value) {
        return add(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache add(String path, double value) {
        return add(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache add(String path, float value) {
        return add(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache add(String path, int value) {
        return add(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache add(String path, long value) {
        return add(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache add(String path, Object value) {
        return add(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache add(String path, short value) {
        return add(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache child(JsonPointer basePtr) {
        return new ChildCacheImpl(this, basePtr);
    }

    @Override
    public JsonCache child(String path) {
        return child(JsonPointer.compile(path));
    }

    @Override
    public void delete(JsonPointer ptr) {
        JsonPointer head = ptr.head();
        if (head == null) {
            root = null;
        } else if (root != null) {
            JsonNode parent = root.at(head);
            if (parent.isArray()) {
                ((ArrayNode) parent).remove(Integer.parseInt(ptr.last().getMatchingProperty()));
            } else if (parent.isObject()) {
                ((ObjectNode) parent).remove(ptr.last().getMatchingProperty());
            } else {
                throw new IllegalArgumentException(ptr + " does not identify a deletable node");
            }
        }
        setDirty();
    }

    @Override
    public void delete(String path) {
        delete(JsonPointer.compile(path));
    }

    /**
     * Ensures that a suitable container exists for the specified JSON pointer.
     *
     * @param ptr A <a href="https://tools.ietf.org/html/rfc6901">JSON Pointer</a> to the property to set.
     * @return The container that owns the property identified by <code>path</code>.
     */
    protected ContainerNode<?> ensureContainerExists(JsonPointer ptr) {
        return ensureContainerExists(ptr, false);
    }

    /**
     * Ensures that all ancestor containers exist for the specified JSON pointer.
     *
     * @param ptr        A <a href="https://tools.ietf.org/html/rfc6901">JSON Pointer</a> to the property to set.
     * @param forceArray <code>true</code> to create an array for the last segment of the pointer if it is non-integral.
     * @return The container that owns the property identified by <code>path</code>.
     */
    protected ContainerNode<?> ensureContainerExists(JsonPointer ptr, boolean forceArray) {
        if (root == null) {
            root = isInteger(ptr.getMatchingProperty()) // split
                    ? JsonNodeFactory.instance.arrayNode()
                    : JsonNodeFactory.instance.objectNode();
        }
        String lastProperty = ptr.last().getMatchingProperty();
        Deque<String> stack = new ArrayDeque<>();
        JsonPointer ancestorPtr = forceArray && !isInteger(lastProperty) ? ptr : ptr.head();
        JsonNode ancestor = root.at(ancestorPtr);
        while (ancestor.isMissingNode()) {
            stack.push(ancestorPtr.last().getMatchingProperty());
            ancestorPtr = ancestorPtr.head();
            ancestor = root.at(ancestorPtr);
        }
        if (!ancestor.isContainerNode())
            throw new IllegalArgumentException(ancestorPtr + " does not identify a container node");

        while (!stack.isEmpty()) {
            String ancestorProperty = stack.pop();
            String childProperty = stack.isEmpty() // split
                    ? forceArray && !isInteger(lastProperty) // split
                    ? "0" // split
                    : lastProperty // split
                    : stack.peek();
            // Parent can be array or object; child can be array or object - that's four possible combinations.
            // Infer the child container type from the child property name: an integer pattern implies an array node.
            if (isInteger(childProperty)) {
                switch (ancestor.getNodeType()) {
                    case ARRAY:
                        // ARRAY/ARRAY
                        ancestor = ((ArrayNode) ancestor).insertArray(Integer.parseInt(ancestorProperty));
                        break;
                    case OBJECT:
                        // OBJECT/ARRAY
                        ancestor = ((ObjectNode) ancestor).putArray(ancestorProperty);
                        break;
                    default:
                        throw new IllegalArgumentException(ancestorProperty + " does not identify an array node");
                }
            } else {
                switch (ancestor.getNodeType()) {
                    case ARRAY:
                        // ARRAY/OBJECT
                        ancestor = ((ArrayNode) ancestor).insertObject(Integer.parseInt(ancestorProperty));
                        break;
                    case OBJECT:
                        // OBJECT/OBJECT
                        ancestor = ((ObjectNode) ancestor).putObject(ancestorProperty);
                        break;
                    default:
                        throw new IllegalArgumentException(ancestorProperty + " does not identify an array node");
                }
            }
            setDirty();
        }

        return (ContainerNode<?>) ancestor;
    }

    @Override
    public boolean exists(JsonPointer ptr) {
        return root != null && !root.at(ptr).isMissingNode();
    }

    @Override
    public boolean exists(String path) {
        return exists(JsonPointer.compile(path));
    }

    @Override
    public Root flush(File file) throws CacheException {
        if (isDirty)
            save(file);
        return this;
    }

    @Override
    public Root flush(OutputStream out) throws CacheException {
        if (isDirty)
            save(out);
        return this;
    }

    @Override
    public Root flushOnShutdown(final File file) {
        if (!shutdownHookRegistered) {
            Runtime.getRuntime().addShutdownHook(new Thread() {
                @Override
                public void run() {
                    try {
                        flush(file);
                    } catch (CacheException e) {
                        e.printStackTrace();
                    }
                }
            });
            shutdownHookRegistered = true;
        }
        return this;
    }

    @Override
    public Root flushOnShutdown(final OutputStream out) {
        if (!shutdownHookRegistered) {
            Runtime.getRuntime().addShutdownHook(new Thread() {
                @Override
                public void run() {
                    try {
                        flush(out);
                    } catch (CacheException e) {
                        e.printStackTrace();
                    }
                }
            });
            shutdownHookRegistered = true;
        }
        return this;
    }

    @Override
    public Object get(JsonPointer ptr) throws CacheException {
        Object result;
        if (root == null) {
            result = null;
        } else {
            try {
                JsonNode node = root.at(ptr);
                switch (node.getNodeType()) {
                    case ARRAY:
                    case OBJECT:
                        result = node;
                        break;
                    case BINARY:
                        result = node.binaryValue();
                        break;
                    case BOOLEAN:
                        result = node.booleanValue();
                        break;
                    case NUMBER:
                        result = node.numberValue();
                        break;
                    case POJO:
                        result = ((POJONode) node).getPojo();
                        break;
                    case STRING:
                        result = node.textValue();
                        break;
                    default:
                        result = null;
                        break;
                }
            } catch (IOException e) {
                throw new CacheException(e);
            }
        }
        return result;
    }

    @Override
    public Object get(JsonPointer ptr, Object defaultValue) throws CacheException {
        Objects.requireNonNull(defaultValue, "defaultValue is required");
        Object result;
        if (exists(ptr)) {
            result = get(ptr);
        } else {
            set(ptr, defaultValue);
            result = defaultValue;
        }
        return result;
    }

    @Override
    public Object get(String path) throws CacheException {
        return get(JsonPointer.compile(path));
    }

    @Override
    public Object get(String path, Object defaultValue) throws CacheException {
        return get(JsonPointer.compile(path), defaultValue);
    }

    @Override
    public BigDecimal getBigDecimal(JsonPointer ptr) {
        return root == null ? null : root.at(ptr).decimalValue();
    }

    @Override
    public BigDecimal getBigDecimal(JsonPointer ptr, BigDecimal defaultValue) throws CacheException {
        Objects.requireNonNull(defaultValue, "defaultValue is required");
        BigDecimal result;
        if (exists(ptr)) {
            result = getBigDecimal(ptr);
        } else {
            set(ptr, defaultValue);
            result = defaultValue;
        }
        return result;
    }

    @Override
    public BigDecimal getBigDecimal(String path) {
        return getBigDecimal(JsonPointer.compile(path));
    }

    @Override
    public BigDecimal getBigDecimal(String path, BigDecimal defaultValue) throws CacheException {

        return getBigDecimal(JsonPointer.compile(path), defaultValue);
    }

    @Override
    public BigInteger getBigInteger(JsonPointer ptr) {
        return root == null ? null : root.at(ptr).bigIntegerValue();
    }

    @Override
    public BigInteger getBigInteger(JsonPointer ptr, BigInteger defaultValue) throws CacheException {
        Objects.requireNonNull(defaultValue, "defaultValue is required");
        BigInteger result;
        if (exists(ptr)) {
            result = getBigInteger(ptr);
        } else {
            set(ptr, defaultValue);
            result = defaultValue;
        }
        return result;
    }

    @Override
    public BigInteger getBigInteger(String path) {
        return getBigInteger(JsonPointer.compile(path));
    }

    @Override
    public BigInteger getBigInteger(String path, BigInteger defaultValue) throws CacheException {
        return getBigInteger(JsonPointer.compile(path), defaultValue);
    }

    @Override
    public byte[] getBinary(JsonPointer ptr) throws CacheException {
        try {
            return root == null ? null : root.at(ptr).binaryValue();
        } catch (IOException e) {
            throw new CacheException(e);
        }
    }

    @Override
    public byte[] getBinary(JsonPointer ptr, byte[] defaultValue) throws CacheException {
        Objects.requireNonNull(defaultValue, "defaultValue is required");
        byte[] result;
        if (exists(ptr)) {
            result = getBinary(ptr);
        } else {
            set(ptr, defaultValue);
            result = defaultValue;
        }
        return result;
    }

    @Override
    public byte[] getBinary(String path) throws CacheException {
        return getBinary(JsonPointer.compile(path));
    }

    @Override
    public byte[] getBinary(String path, byte[] defaultValue) throws CacheException {
        return getBinary(JsonPointer.compile(path), defaultValue);
    }

    @Override
    public boolean getBoolean(JsonPointer ptr) {
        return root != null && root.at(ptr).booleanValue();
    }

    @Override
    public boolean getBoolean(JsonPointer ptr, boolean defaultValue) throws CacheException {
        boolean result;
        if (exists(ptr)) {
            result = getBoolean(ptr);
        } else {
            set(ptr, defaultValue);
            result = defaultValue;
        }
        return result;
    }

    @Override
    public boolean getBoolean(String path) {
        return getBoolean(JsonPointer.compile(path));
    }

    @Override
    public boolean getBoolean(String path, boolean defaultValue) throws CacheException {
        return getBoolean(JsonPointer.compile(path), defaultValue);
    }

    @Override
    public double getDouble(JsonPointer ptr) {
        return root == null ? 0.0D : root.at(ptr).doubleValue();
    }

    @Override
    public double getDouble(JsonPointer ptr, double defaultValue) throws CacheException {
        Objects.requireNonNull(defaultValue, "defaultValue is required");
        double result;
        if (exists(ptr)) {
            result = getDouble(ptr);
        } else {
            set(ptr, defaultValue);
            result = defaultValue;
        }
        return result;
    }

    @Override
    public double getDouble(String path) {
        return getDouble(JsonPointer.compile(path));
    }

    @Override
    public double getDouble(String path, double defaultValue) throws CacheException {
        return getDouble(JsonPointer.compile(path), defaultValue);
    }

    @Override
    public float getFloat(JsonPointer ptr) {
        return root == null ? 0.0F : root.at(ptr).floatValue();
    }

    @Override
    public float getFloat(JsonPointer ptr, float defaultValue) throws CacheException {
        float result;
        if (exists(ptr)) {
            result = getFloat(ptr);
        } else {
            set(ptr, defaultValue);
            result = defaultValue;
        }
        return result;
    }

    @Override
    public float getFloat(String path) {
        return getFloat(JsonPointer.compile(path));
    }

    @Override
    public float getFloat(String path, float defaultValue) throws CacheException {
        return getFloat(JsonPointer.compile(path), defaultValue);
    }

    @Override
    public int getInt(JsonPointer ptr) {
        return root == null ? 0 : root.at(ptr).intValue();
    }

    @Override
    public int getInt(JsonPointer ptr, int defaultValue) throws CacheException {
        int result;
        if (exists(ptr)) {
            result = getInt(ptr);
        } else {
            set(ptr, defaultValue);
            result = defaultValue;
        }
        return result;
    }

    @Override
    public int getInt(String path) {
        return getInt(JsonPointer.compile(path));
    }

    @Override
    public int getInt(String path, int defaultValue) throws CacheException {
        return getInt(JsonPointer.compile(path), defaultValue);
    }

    @Override
    public long getLong(JsonPointer ptr) {
        return root == null ? 0L : root.at(ptr).longValue();
    }

    @Override
    public long getLong(JsonPointer ptr, long defaultValue) throws CacheException {
        long result;
        if (exists(ptr)) {
            result = getLong(ptr);
        } else {
            set(ptr, defaultValue);
            result = defaultValue;
        }
        return result;
    }

    @Override
    public long getLong(String path) {
        return getLong(JsonPointer.compile(path));
    }

    @Override
    public long getLong(String path, long defaultValue) throws CacheException {
        return getLong(JsonPointer.compile(path), defaultValue);
    }

    @Override
    public ObjectMapper getMapper() {
        return mapper;
    }

    @Override
    public MergePolicy getMergePolicy() {
        return mergePolicy;
    }

    @Override
    public JsonNodeType getNodeType(JsonPointer ptr) {
        return root.at(ptr).getNodeType();
    }

    @Override
    public JsonNodeType getNodeType(String path) {
        return getNodeType(JsonPointer.compile(path));
    }

    @Override
    public int hashCode() {
        // TODO Auto-generated method stub
        return super.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        // TODO Auto-generated method stub
        return super.equals(obj);
    }

    @Override
    protected Object clone() throws CloneNotSupportedException {
        // TODO Auto-generated method stub
        return super.clone();
    }

    @Override
    protected void finalize() throws Throwable {
        // TODO Auto-generated method stub
        super.finalize();
    }

    @Override
    public Number getNumber(JsonPointer ptr) {
        return root == null ? null : root.at(ptr).numberValue();
    }

    @Override
    public Number getNumber(JsonPointer ptr, Number defaultValue) throws CacheException {
        Objects.requireNonNull(defaultValue, "defaultValue is required");
        Number result;
        if (exists(ptr)) {
            result = getNumber(ptr);
        } else {
            set(ptr, defaultValue);
            result = defaultValue;
        }
        return result;
    }

    @Override
    public Number getNumber(String path) {
        return getNumber(JsonPointer.compile(path));
    }

    @Override
    public Number getNumber(String path, Number defaultValue) throws CacheException {
        return getNumber(JsonPointer.compile(path), defaultValue);
    }

    @Override
    public <T> T getObject(JsonPointer ptr, Class<T> type) throws CacheException {
        T result;
        if (root == null) {
            result = null;
        } else {
            JsonNode node = root.at(ptr);
            Object value = node.isPojo() && !JsonNode.class.isAssignableFrom(type) ? ((POJONode) node).getPojo() : node;
            if ((value != null) && (value.getClass() == type)) {
                result = (T) value;
            } else {
                result = mapper.convertValue(value, type);
            }
        }
        return result;
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T getObject(JsonPointer ptr, T defaultValue) throws CacheException {
        Objects.requireNonNull(defaultValue, "defaultValue is required");
        T result;
        if (exists(ptr)) {
            result = (T) getObject(ptr, defaultValue.getClass());
        } else {
            set(ptr, defaultValue);
            result = defaultValue;
        }
        return result;
    }

    @Override
    public <T> T getObject(String path, Class<T> type) throws CacheException {
        return getObject(JsonPointer.compile(path), type);
    }

    @Override
    public <T> T getObject(String path, T defaultValue) throws CacheException {
        return getObject(JsonPointer.compile(path), defaultValue);
    }

    @Override
    public <T> List<T> getObjects(JsonPointer ptr, Class<T> type) throws CacheException {
        List<T> result;

        if (root == null) {
            result = null;
        } else {
            JsonNode node = root.at(ptr);
            switch (node.getNodeType()) {
                case ARRAY:
                case OBJECT:
                    result = new ArrayList<T>(node.size());
                    Iterator<JsonNode> elements = node.elements();
                    while (elements.hasNext())
                        result.add(mapper.convertValue(elements.next(), type));
                    break;
                default:
                    result = Collections.emptyList();
                    break;
            }
        }

        return result;
    }

    @Override
    public <T> List<T> getObjects(JsonPointer ptr, Class<T> type, List<T> defaultValue) throws CacheException {
        Objects.requireNonNull(defaultValue, "defaultValue is required");
        List<T> result;
        if (exists(ptr)) {
            result = getObjects(ptr, type);
        } else {
            set(ptr, defaultValue);
            result = defaultValue;
        }
        return result;
    }

    @Override
    public <T> List<T> getObjects(String path, Class<T> type) throws CacheException {
        return getObjects(JsonPointer.compile(path), type);
    }

    @Override
    public <T> List<T> getObjects(String path, Class<T> type, List<T> defaultValue) throws CacheException {
        return getObjects(JsonPointer.compile(path), type, defaultValue);
    }

    @Override
    public short getShort(JsonPointer ptr) {
        return root == null ? (short) 0 : root.at(ptr).shortValue();
    }

    @Override
    public short getShort(JsonPointer ptr, short defaultValue) {
        short result;
        if (exists(ptr)) {
            result = getShort(ptr);
        } else {
            set(ptr, defaultValue);
            result = defaultValue;
        }
        return result;
    }

    @Override
    public short getShort(String path) {
        return getShort(JsonPointer.compile(path));
    }

    @Override
    public short getShort(String path, short defaultValue) {
        return getShort(JsonPointer.compile(path), defaultValue);
    }

    @Override
    public String getString(JsonPointer ptr) {
        return root == null ? null : root.at(ptr).textValue();
    }

    @Override
    public String getString(JsonPointer ptr, String defaultValue) {
        Objects.requireNonNull(defaultValue, "defaultValue is required");
        String result;
        if (exists(ptr)) {
            result = getString(ptr);
        } else {
            set(ptr, defaultValue);
            result = defaultValue;
        }
        return result;
    }

    @Override
    public String getString(String path) {
        return getString(JsonPointer.compile(path));
    }

    @Override
    public String getString(String path, String defaultValue) {
        return getString(JsonPointer.compile(path), defaultValue);
    }

    protected void insertNumber(ArrayNode array, int index, Number value) {
        if (value instanceof Short) {
            array.insert(index, (Short) value);
        } else if (value instanceof Integer) {
            array.insert(index, (Integer) value);
        } else if (value instanceof Long) {
            array.insert(index, (Long) value);
        } else if (value instanceof Float) {
            array.insert(index, (Float) value);
        } else if (value instanceof Double) {
            array.insert(index, (Double) value);
        } else if (value instanceof BigInteger) {
            array.insert(index, (BigInteger) value);
        } else if (value instanceof BigDecimal) {
            array.insert(index, (BigDecimal) value);
        } else {
            throw new IllegalArgumentException(
                    "unsupported numeric value: " + value + " (" + value.getClass().getSimpleName() + ')');
        }
    }

    @Override
    public boolean isDirty() {
        return isDirty;
    }

    protected boolean isInteger(String s) {
        return INTEGER.matcher(s).matches();
    }

    @Override
    public Root load(File file) throws CacheException {
        Objects.requireNonNull(file, "file is required");
        if (file.exists()) {
            try (InputStream in = new FileInputStream(file)) {
                load(in);
            } catch (IOException e) {
                throw new CacheException(e);
            }
        }
        return this;
    }

    @Override
    public Root load(InputStream in) throws CacheException {
        Objects.requireNonNull(in, "in is required");
        try (InputStream is = in) {
            if (isLoaded) {
                if (mergePolicy != MergePolicy.NO_MERGE) {
                    ContainerNode<?> tree = (ContainerNode<?>) mapper.readTree(in);

                    // The cache is already loaded, so merge the incoming object tree into the existing root.
                    merge(root, tree);
                }
            } else {
                root = (ContainerNode<?>) mapper.readTree(in);
                isDirty = false;
                isLoaded = true;
            }
        } catch (IOException e) {
            throw new CacheException(e);
        }
        return this;
    }

    @Override
    public Root mapper(ObjectMapper mapper) {
        Objects.requireNonNull(mapper, "mapper is required");
        this.mapper = mapper;
        return this;
    }

    protected void merge(ContainerNode<?> dest, ContainerNode<?> src) {
        if (dest.getNodeType() == src.getNodeType()) {
            if (dest.isArray()) {
                ArrayNode destArray = (ArrayNode) dest;
                ArrayNode srcArray = (ArrayNode) src;
                outer:
                for (int i = 0; i < srcArray.size(); i++) {
                    // Only add a source element if it is not already present in the destination array.
                    JsonNode srcElem = srcArray.get(i);
                    for (int j = 0; j < destArray.size(); j++) {
                        if (destArray.get(j).equals(srcElem))
                            continue outer;
                    }
                    destArray.add(srcElem);
                }
            } else if (dest.isObject()) {
                ObjectNode destObject = (ObjectNode) dest;
                ObjectNode srcObject = (ObjectNode) src;
                Iterator<Entry<String, JsonNode>> fields = srcObject.fields();
                while (fields.hasNext()) {
                    Entry<String, JsonNode> field = fields.next();
                    String fieldName = field.getKey();
                    JsonNode srcChild = field.getValue();
                    if (destObject.has(fieldName)) {
                        JsonNode destChild = destObject.get(fieldName);
                        switch (mergePolicy) {
                            case OVERWRITE_EXISTING:
                                destObject.set(fieldName, srcChild);
                                // Mark the cache as dirty as we've added items from another file.
                                isDirty = true;
                                LOGGER.info("Existing root property '" + fieldName
                                        + "' has been overwritten by incoming data");
                                break;
                            case MERGE_RECURSIVE:
                                if (destChild.isContainerNode() && srcChild.isContainerNode())
                                    merge((ContainerNode<?>) destChild, (ContainerNode<?>) srcChild);
                                break;
                            case KEEP_EXISTING:
                                LOGGER.info("Existing root property '" + fieldName
                                        + "' will not be overwritten by incoming data");
                            default:
                                // Nothing to do.
                                break;
                        }
                    } else {
                        destObject.set(fieldName, srcChild);
                        LOGGER.info("New property '" + fieldName + "' has been added from incoming data");
                        // Mark the cache as dirty as we've added items from another file.
                        isDirty = true;
                    }
                }
            }
        } else {
            LOGGER.warn("Cannot merge containers of differing types");
        }
    }

    @Override
    public Root mergePolicy(MergePolicy policy) {
        Objects.requireNonNull(policy, "policy is required");
        this.mergePolicy = policy;
        return this;
    }

    // @Override
    @SuppressWarnings("unchecked")
    protected JsonNode nodeFor(Object value) {
        JsonNode node;
        if (value == null) {
            node = root.nullNode();
        } else if (value instanceof JsonNode) {
            node = (JsonNode) value;
        } else if (value instanceof Boolean) {
            node = root.booleanNode((Boolean) value);
        } else if (value instanceof List) {
            node = root.arrayNode();
            for (Object element : (List<?>) value)
                ((ArrayNode) node).add(nodeFor(element));
        } else if (value instanceof Map) {
            node = root.objectNode();
            for (Entry<String, ?> entry : ((Map<String, ?>) value).entrySet())
                ((ObjectNode) node).set(entry.getKey(), nodeFor(entry.getValue()));
        } else if (value instanceof Number) {
            if (value instanceof Byte)
                node = root.numberNode((Byte) value);
            else if (value instanceof Short)
                node = root.numberNode((Short) value);
            else if (value instanceof Integer)
                node = root.numberNode((Integer) value);
            else if (value instanceof Long)
                node = root.numberNode((Long) value);
            else if (value instanceof Float)
                node = root.numberNode((Float) value);
            else if (value instanceof Double)
                node = root.numberNode((Double) value);
            else if (value instanceof BigInteger)
                node = root.numberNode((BigInteger) value);
            else if (value instanceof BigDecimal)
                node = root.numberNode((BigDecimal) value);
            else
                throw new IllegalArgumentException("unsupported number type: " + value.getClass().getSimpleName());
        } else if (value instanceof String) {
            node = root.textNode((String) value);
        } else if (value instanceof byte[]) {
            node = root.binaryNode((byte[]) value);
        } else {
            node = root.pojoNode(value);
        }
        return node;
    }

    protected JsonNodeType nodeTypeFor(Object value) {
        JsonNodeType type;
        if (value == null) {
            type = JsonNodeType.NULL;
        } else if (value instanceof Boolean) {
            type = JsonNodeType.BOOLEAN;
        } else if (value instanceof Number) {
            type = JsonNodeType.NUMBER;
        } else if (value instanceof String) {
            type = JsonNodeType.STRING;
        } else if (value instanceof ArrayNode || value instanceof List) {
            type = JsonNodeType.ARRAY;
        } else if (value instanceof byte[]) {
            type = JsonNodeType.BINARY;
        } else if (value instanceof ObjectNode || value instanceof Map) {
            type = JsonNodeType.OBJECT;
        } else {
            type = JsonNodeType.POJO;
        }
        return type;
    }

    @Override
    public JsonCache parent() {
        return null;
    }

    protected void putNumber(ObjectNode object, String property, Number value) {
        if (value instanceof Short) {
            object.put(property, (Short) value);
        } else if (value instanceof Integer) {
            object.put(property, (Integer) value);
        } else if (value instanceof Long) {
            object.put(property, (Long) value);
        } else if (value instanceof Float) {
            object.put(property, (Float) value);
        } else if (value instanceof Double) {
            object.put(property, (Double) value);
        } else if (value instanceof BigInteger) {
            object.put(property, (BigInteger) value);
        } else if (value instanceof BigDecimal) {
            object.put(property, (BigDecimal) value);
        } else {
            throw new IllegalArgumentException(
                    "unsupported numeric value: " + value + " (" + value.getClass().getSimpleName() + ')');
        }
    }

    @Override
    public Root root() {
        return this;
    }

    @Override
    public Root save(File file) throws CacheException {
        Objects.requireNonNull(file, "file is required");
        file.getParentFile().mkdirs();
        try {
            save(new FileOutputStream(file));
        } catch (FileNotFoundException e) {
            throw new CacheException(e);
        }
        return this;
    }

    @Override
    public Root save(OutputStream out) throws CacheException {
        if (root == null || root.isMissingNode())
            throw new CacheException("null or missing root node");
        Objects.requireNonNull(out, "out is required");
        try (OutputStream o = out) {
            mapper.writeValue(o, root);
        } catch (IOException e) {
            throw new CacheException(e);
        }
        isDirty = false;
        return this;
    }

    @Override
    public JsonCache set(JsonPointer ptr, BigDecimal value) {
        return set(ptr, (Object) value);
    }

    @Override
    public JsonCache set(JsonPointer ptr, BigInteger value) {
        return set(ptr, (Object) value);
    }

    @Override
    public JsonCache set(JsonPointer ptr, boolean value) {
        return set(ptr, (Object) value);
    }

    @Override
    public JsonCache set(JsonPointer ptr, double value) {
        return set(ptr, (Object) value);
    }

    @Override
    public JsonCache set(JsonPointer ptr, float value) {
        return set(ptr, (Object) value);
    }

    @Override
    public JsonCache set(JsonPointer ptr, int value) {
        return set(ptr, (Object) value);
    }

    @Override
    public JsonCache set(JsonPointer ptr, List<?> values) throws CacheException {
        // Note: if the node identified by ptr is not an array, we must create one before populating it.
        ArrayNode array;
        ContainerNode<?> container = ensureContainerExists(ptr);
        JsonNode target = container.at(ptr.last());
        if (target.isArray()) {
            array = (ArrayNode) target;
        } else {
            String property = ptr.last().getMatchingProperty();
            array = container.arrayNode();
            switch (container.getNodeType()) {
                case ARRAY:
                    int index = Integer.parseInt(property);
                    ((ArrayNode) container).set(index, array);
                    break;
                case OBJECT:
                    ((ObjectNode) container).set(property, array);
                    break;
                default:
                    throw new CacheException(ptr + " does not identify an array");
            }
        }

        // Now that the target array exists, we can populate it.
        array.removeAll();
        for (Object value : values) {
            JsonNode node = nodeFor(value);
            array.add(node);
        }
        setDirty();
        return this;
    }

    @Override
    public JsonCache set(JsonPointer ptr, long value) {
        return set(ptr, (Object) value);
    }

    @Override
    public JsonCache set(JsonPointer ptr, Object value) {
        String property = ptr.last().getMatchingProperty();
        ContainerNode<?> container = ensureContainerExists(ptr);
        JsonNode node = nodeFor(value);
        switch (container.getNodeType()) {
            case ARRAY:
                ArrayNode array = (ArrayNode) container;
                int index = Integer.parseInt(property);
                if (index < array.size()) {
                    array.set(index, node);
                } else {
                    // Fill any gap between current size and index with nulls (Jackson doesn't support sparse arrays).
                    for (int i = array.size(); i < index; i++)
                        array.add(array.nullNode());
                    array.add(node);
                }
                break;
            case OBJECT:
                ((ObjectNode) container).set(property, node);
                break;
            default:
                throw new IllegalArgumentException(ptr + " does not identify a settable container");
        }
        setDirty();
        return this;
    }

    @Override
    public JsonCache set(JsonPointer ptr, short value) {
        return set(ptr, (Object) value);
    }

    @Override
    public JsonCache set(JsonPointer ptr, String value) {
        return set(ptr, (Object) value);
    }

    @Override
    public JsonCache set(String path, BigDecimal value) {
        return set(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache set(String path, BigInteger value) {
        return set(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache set(String path, boolean value) {
        return set(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache set(String path, double value) {
        return set(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache set(String path, float value) {
        return set(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache set(String path, int value) {
        return set(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache set(String path, List<?> values) throws CacheException {
        return set(JsonPointer.compile(path), values);
    }

    @Override
    public JsonCache set(String path, long value) {
        return set(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache set(String path, Object value) {
        return set(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache set(String path, short value) {
        return set(JsonPointer.compile(path), value);
    }

    @Override
    public JsonCache set(String path, String value) {
        return set(JsonPointer.compile(path), value);
    }

    protected void setDirty() {
        isDirty = true;
        isLoaded = true;
    }

    @Override
    public int size(JsonPointer ptr) {
        return root == null ? 0 : root.at(ptr).size();
    }

    @Override
    public int size(String path) {
        return size(JsonPointer.compile(path));
    }

    @Override
    public String toString() {
        return "JsonCacheImpl [root=" + root + ']';
    }

    @Override
    public Root unload() {
        isLoaded = false;
        isDirty = false;
        root = null;
        return this;
    }
}
