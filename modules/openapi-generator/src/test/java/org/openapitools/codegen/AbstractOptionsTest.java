/*
 * Copyright 2018 OpenAPI-Generator Contributors (https://openapi-generator.tech)
 * Copyright 2018 SmartBear Software
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

package org.openapitools.codegen;

import com.google.common.base.Function;
import org.apache.commons.lang3.StringUtils;
import org.mockito.MockSettings;
import org.openapitools.codegen.options.OptionsProvider;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Set;
import java.util.stream.Collectors;

import static org.mockito.Answers.CALLS_REAL_METHODS;
import static org.mockito.Mockito.withSettings;

/**
 * Base class for applying and processing generator options, then invoking a helper method to verify those options.
 */
public abstract class AbstractOptionsTest {
    protected MockSettings mockSettings = withSettings().useConstructor().defaultAnswer(CALLS_REAL_METHODS);
    private final OptionsProvider optionsProvider;

    protected AbstractOptionsTest(OptionsProvider optionsProvider) {
        this.optionsProvider = optionsProvider;
    }

    @SuppressWarnings("unused")
    @Test
    public void checkOptionsProcessing() {
        getCodegenConfig().additionalProperties().putAll(optionsProvider.createOptions());
        getCodegenConfig().processOpts();
        verifyOptions();
    }

    @Test(description = "check if all options described in documentation are presented in test case")
    public void checkOptionsHelp() {
        final List<String> cliOptions = getCodegenConfig().cliOptions().stream().map(getCliOptionTransformer()).collect(Collectors.toList());
        final Set<String> testOptions = optionsProvider.createOptions().keySet();
        final Set<String> skipped = new HashSet<String>(cliOptions);
        skipped.removeAll(testOptions);
        if (!skipped.isEmpty()) {
            Assert.fail(String.format(Locale.ROOT, "These options weren't checked: %s.", StringUtils.join(skipped, ", ")));
        }
        final Set<String> undocumented = new HashSet<String>(testOptions);
        undocumented.removeAll(cliOptions);
        if (!undocumented.isEmpty()) {
            Assert.fail(String.format(Locale.ROOT,"These options weren't documented: %s. Are you expecting base options and calling cliOptions.clear()?", StringUtils.join(undocumented, ", ")));
        }
    }

    private static Function<CliOption, String> getCliOptionTransformer() {
        return new Function<CliOption, String>() {
            @Override
            public String apply(CliOption option) {
                return option.getOpt();
            }
        };
    }

    protected abstract CodegenConfig getCodegenConfig();

    protected abstract void verifyOptions();
}
