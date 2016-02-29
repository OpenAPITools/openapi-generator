package io.swagger.codegen;

import io.swagger.codegen.options.OptionsProvider;

import com.google.common.base.Function;
import com.google.common.collect.Lists;

import mockit.FullVerifications;

import org.apache.commons.lang3.StringUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public abstract class AbstractOptionsTest {
    private final OptionsProvider optionsProvider;

    protected AbstractOptionsTest(OptionsProvider optionsProvider) {
        this.optionsProvider = optionsProvider;
    }

    @SuppressWarnings("unused")
    @Test
    public void checkOptionsProcessing() {
        getCodegenConfig().additionalProperties().putAll(optionsProvider.createOptions());
        setExpectations();

        getCodegenConfig().processOpts();

        new FullVerifications() {{
        }};
    }

    @Test(description = "check if all options described in documentation are presented in test case")
    public void checkOptionsHelp() {
        final List<String> cliOptions = Lists.transform(getCodegenConfig().cliOptions(), getCliOptionTransformer());
        final Set<String> testOptions = optionsProvider.createOptions().keySet();
        final Set<String> skipped = new HashSet<String>(cliOptions);
        skipped.removeAll(testOptions);
        if (!skipped.isEmpty()) {
            Assert.fail(String.format("These options weren't checked: %s.", StringUtils.join(skipped, ", ")));
        }
        final Set<String> undocumented = new HashSet<String>(testOptions);
        undocumented.removeAll(cliOptions);
        if (!undocumented.isEmpty()) {
            Assert.fail(String.format("These options weren't documented: %s.", StringUtils.join(undocumented, ", ")));
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

    protected abstract void setExpectations();
}
