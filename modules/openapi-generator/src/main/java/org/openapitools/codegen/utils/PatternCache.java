/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
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

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;

import java.util.regex.Pattern;

/**
 * A shared, process-wide cache of compiled {@link Pattern} objects, keyed by regex string
 * (and optional flags).
 *
 * <h3>Motivation</h3>
 * {@link Pattern#compile(String)} is non-trivial: it parses and compiles the regex into an
 * internal automaton. When the same literal regex is compiled inside a hot loop (e.g. once
 * per property name, per operation, or per model), the overhead accumulates. This cache
 * ensures each distinct {@code (regex, flags)} pair is compiled at most once.
 *
 * <h3>Thread safety</h3>
 * Backed by a Caffeine cache; all operations are thread-safe. Caffeine's internal striped
 * locking means concurrent requests for the same key result in only one compilation.
 *
 * <h3>Memory</h3>
 * The cache is bounded at {@value #MAX_SIZE} entries via LRU eviction, which is effectively
 * unlimited for any single codegen run (realistic specs have at most dozens of distinct
 * patterns) while protecting long-lived server processes (e.g. openapi-generator-online)
 * from unbounded growth caused by user-supplied {@code pattern:} values.
 *
 * <h3>What goes here vs. {@code static final Pattern}</h3>
 * Use this cache only for <em>dynamic</em> pattern keys — i.e. patterns whose regex string
 * is not known at compile time (user config, OpenAPI {@code pattern:} field values, etc.).
 * Fixed literal patterns that appear in a single class should remain
 * {@code private static final Pattern} constants; they are resolved at class-load time and
 * accessed via a direct field read, which is faster than any map lookup.
 */
public final class PatternCache {

    /** Maximum number of cached entries before LRU eviction kicks in. */
    static final int MAX_SIZE = 10_000;

    private static final Cache<String, Pattern> CACHE = Caffeine.newBuilder()
            .maximumSize(MAX_SIZE)
            .build();

    private PatternCache() {
        // utility class – no instances
    }

    /**
     * Returns a compiled {@link Pattern} for {@code regex} with no flags, compiling it on the
     * first call and returning the cached instance on subsequent calls.
     *
     * @param regex the regular-expression string (must not be {@code null})
     * @return a compiled {@link Pattern}
     */
    public static Pattern get(String regex) {
        return CACHE.get(regex, Pattern::compile);
    }

    /**
     * Returns a compiled {@link Pattern} for {@code regex} with the given {@code flags},
     * compiling it on the first call and returning the cached instance on subsequent calls.
     *
     * <p>The cache key encodes both the regex string and the flags value so that the same
     * regex compiled with different flags (e.g. {@link Pattern#UNICODE_CHARACTER_CLASS})
     * is stored as a separate entry.
     *
     * @param regex the regular-expression string (must not be {@code null})
     * @param flags match flags, as accepted by {@link Pattern#compile(String, int)}
     * @return a compiled {@link Pattern}
     */
    public static Pattern get(String regex, int flags) {
        // NUL character as separator is safe because it cannot appear in a valid regex string.
        return CACHE.get(regex + '\0' + flags, k -> Pattern.compile(regex, flags));
    }
}

