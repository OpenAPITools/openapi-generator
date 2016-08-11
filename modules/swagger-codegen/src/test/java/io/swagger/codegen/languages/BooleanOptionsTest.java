package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.models.properties.BooleanProperty;
import org.reflections.Reflections;
import org.testng.Assert;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

@SuppressWarnings("static-method")
public class BooleanOptionsTest {
    private static final String OPTIONS_PROVIDER = "Codegen";

    @DataProvider(name = OPTIONS_PROVIDER)
    private Iterator<Object[]> listOptions() throws IllegalAccessException, InstantiationException {
        final String packageName = "io.swagger.codegen.languages";
        final Reflections reflections = new Reflections(packageName);
        final List<Object[]> codegenList = new ArrayList<Object[]>();

        for (Class<? extends DefaultCodegen> codegen : reflections.getSubTypesOf(DefaultCodegen.class)) {
            if (!Modifier.isAbstract(codegen.getModifiers())) {
                codegenList.add((new Object[] {codegen.newInstance()}));
            }
        }
        if (codegenList.size() == 0) {
            Assert.fail(String.format("No classes for testing have been found in the package %s", packageName));
        }

        return codegenList.iterator();
    }

    @Test(dataProvider = OPTIONS_PROVIDER)
    public void booleanOptionsTest(DefaultCodegen codegen) {
        for (CliOption option : codegen.cliOptions()) {
            if (option.getType().equals(BooleanProperty.TYPE)) {
                Assert.assertNotNull(option.getDefault());
                Assert.assertTrue(option.getDefault().equals(Boolean.TRUE.toString()) ||
                        option.getDefault().equals(Boolean.FALSE.toString()));
            }
        }
    }
}
