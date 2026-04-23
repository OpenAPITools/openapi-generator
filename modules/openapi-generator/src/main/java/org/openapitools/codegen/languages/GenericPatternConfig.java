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

package org.openapitools.codegen.languages;

/**
 * Configuration for a single generic-class substitution pattern (Tier 2 detection).
 *
 * <p>A pattern matches schemas whose name ends with {@link #suffix} <em>or</em> starts with
 * {@link #prefix}. Exactly one of {@code suffix} or {@code prefix} must be non-null.</p>
 *
 * <p>The {@link #slot} / {@link #slotArray} field names identify which property of the matched
 * schema holds the type parameter {@code T}.</p>
 *
 * <h2>Example YAML config (in a generator config file)</h2>
 * <pre>{@code
 * additionalProperties:
 *   genericPatterns:
 *     - suffix: Response
 *       genericClass: com.example.ApiResponse   # Mode A: FQN — import only, no file generated
 *       slot: data                              # 'data' property is T
 *     - suffix: Page
 *       genericClass: ApiPage                  # Mode B: simple name — generate class in configPackage
 *       slotArray: content                     # 'content' array property is List<T>
 * }</pre>
 *
 * <h2>Mode A vs Mode B</h2>
 * <ul>
 *   <li><b>Mode A</b>: {@code genericClass} contains a dot ({@code .}) — treated as a
 *       fully-qualified class name. Only an import mapping entry is added; no file is
 *       generated. Use this when the generic class already exists (e.g. a Spring or library
 *       type).</li>
 *   <li><b>Mode B</b>: {@code genericClass} is a simple name (no dot). A new source file
 *       ({@code <genericClass>.java} or {@code .kt}) is generated in the
 *       {@code configPackage} folder. The generated class has one type parameter {@code T}
 *       and mirrors the non-slot properties of the matched schemas.</li>
 * </ul>
 */
public class GenericPatternConfig {

    /**
     * Schema name suffix to match (e.g. {@code "Response"} matches {@code UserResponse},
     * {@code PetResponse}, …). Mutually exclusive with {@link #prefix}.
     */
    public String suffix;

    /**
     * Schema name prefix to match (e.g. {@code "Api"} matches {@code ApiUser},
     * {@code ApiPet}, …). Mutually exclusive with {@link #suffix}.
     */
    public String prefix;

    /**
     * Target generic class name.
     *
     * <ul>
     *   <li>FQN (contains {@code .}): Mode A — add to importMapping, no file generated.</li>
     *   <li>Simple name (no {@code .}): Mode B — generate class in configPackage.</li>
     * </ul>
     *
     * May be {@code null} or empty to skip this entry.
     */
    public String genericClass;

    /**
     * Name of the property that serves as the single {@code $ref} type slot.
     * The property's referenced schema becomes type argument {@code T}.
     * Mutually exclusive with {@link #slotArray}.
     */
    public String slot;

    /**
     * Name of the array property whose items serve as type argument {@code T}.
     * Mutually exclusive with {@link #slot}.
     */
    public String slotArray;

    public GenericPatternConfig() {}

    /** Fluent convenience constructor for testing. */
    public GenericPatternConfig suffix(String s) { this.suffix = s; return this; }
    public GenericPatternConfig prefix(String p) { this.prefix = p; return this; }
    public GenericPatternConfig genericClass(String g) { this.genericClass = g; return this; }
    public GenericPatternConfig slot(String s) { this.slot = s; return this; }
    public GenericPatternConfig slotArray(String s) { this.slotArray = s; return this; }

    @Override
    public String toString() {
        return "GenericPatternConfig{suffix=" + suffix + ", prefix=" + prefix
                + ", genericClass=" + genericClass + ", slot=" + slot
                + ", slotArray=" + slotArray + "}";
    }
}
