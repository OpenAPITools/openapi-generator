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

import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.regex.Pattern;

public class PatternCacheTest {

    @Test
    public void testSameInstanceReturnedForSameRegex() {
        Pattern first  = PatternCache.get("\\W+");
        Pattern second = PatternCache.get("\\W+");
        Assert.assertSame(first, second,
                "PatternCache must return the same Pattern instance for the same regex string");
    }

    @Test
    public void testSameInstanceReturnedForSameRegexAndFlags() {
        Pattern first  = PatternCache.get("\\W+", Pattern.UNICODE_CHARACTER_CLASS);
        Pattern second = PatternCache.get("\\W+", Pattern.UNICODE_CHARACTER_CLASS);
        Assert.assertSame(first, second,
                "PatternCache must return the same Pattern instance for identical (regex, flags) pairs");
    }

    @Test
    public void testDifferentInstanceForDifferentFlags() {
        Pattern noFlags   = PatternCache.get("\\W+");
        Pattern withFlags = PatternCache.get("\\W+", Pattern.UNICODE_CHARACTER_CLASS);
        Assert.assertNotSame(noFlags, withFlags,
                "PatternCache must store separate entries for the same regex compiled with different flags");
        Assert.assertEquals(noFlags.flags() & Pattern.UNICODE_CHARACTER_CLASS, 0,
                "Pattern compiled without flags must not have UNICODE_CHARACTER_CLASS set");
        Assert.assertEquals(withFlags.flags() & Pattern.UNICODE_CHARACTER_CLASS, Pattern.UNICODE_CHARACTER_CLASS,
                "Pattern compiled with UNICODE_CHARACTER_CLASS must have that flag set " +
                "(note: the JVM may also implicitly set UNICODE_CASE=64, so exact equality is not checked)");
    }

    @Test
    public void testDifferentInstanceForDifferentRegex() {
        Pattern a = PatternCache.get("\\d+");
        Pattern b = PatternCache.get("\\w+");
        Assert.assertNotSame(a, b,
                "PatternCache must store separate entries for different regex strings");
    }

    @Test
    public void testPatternFunctionallyCorrect() {
        Pattern pattern = PatternCache.get("\\d+");
        Assert.assertTrue(pattern.matcher("123").matches());
        Assert.assertFalse(pattern.matcher("abc").matches());
    }

    @Test
    public void testConcurrentAccessReturnsSameInstance() throws Exception {
        final String regex = "concurrent-test-[a-z]+";
        final int threads = 20;

        ExecutorService executor = Executors.newFixedThreadPool(threads);
        List<Callable<Pattern>> tasks = new ArrayList<>(threads);
        for (int i = 0; i < threads; i++) {
            tasks.add(() -> PatternCache.get(regex));
        }

        List<Future<Pattern>> futures = executor.invokeAll(tasks);
        executor.shutdown();

        Pattern expected = futures.get(0).get();
        for (Future<Pattern> future : futures) {
            Assert.assertSame(future.get(), expected,
                    "All concurrent callers must receive the same cached Pattern instance");
        }
    }

    @Test
    public void testMaxSizeIsReasonable() {
        Assert.assertTrue(PatternCache.MAX_SIZE >= 1_000,
                "PatternCache.MAX_SIZE should be at least 1000 to cover realistic workloads");
    }
}


