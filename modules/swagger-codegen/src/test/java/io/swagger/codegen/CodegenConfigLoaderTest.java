package io.swagger.codegen;

import org.reflections.Reflections;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static org.testng.Assert.assertEquals;

@SuppressWarnings("static-method")
public class CodegenConfigLoaderTest {

    @DataProvider(name = "codegenConfig")
    public Object[][] createCodegenConfigDataSet() throws Exception {

        Reflections reflections = new Reflections("io.swagger.codegen.languages");
        final Set<Class<? extends DefaultCodegen>> subTypesOf = reflections.getSubTypesOf(DefaultCodegen.class);

        List<CodegenConfig> codegenConfigList = new ArrayList<CodegenConfig>();

        for (Class<? extends DefaultCodegen> aClass : subTypesOf) {
            if (!Modifier.isAbstract(aClass.getModifiers())) {
                final DefaultCodegen defaultCodegen = aClass.newInstance();
                codegenConfigList.add((CodegenConfig) defaultCodegen);
            }
        }

        Object[][] result = new Object[codegenConfigList.size()][1];

        for (int i = 0; i < codegenConfigList.size(); i++) {
            result[i]= new Object[]{codegenConfigList.get(i)};
        }

        return result;
    }

    @Test(dataProvider = "codegenConfig")
    public void testLoadByName(CodegenConfig codegenConfig) throws Exception {
        final CodegenConfig loadedConfig = CodegenConfigLoader.forName(codegenConfig.getName());

        assertEquals(loadedConfig.getClass(), codegenConfig.getClass());
        assertEquals(loadedConfig.getName(), codegenConfig.getName());
    }

    @Test(dataProvider = "codegenConfig")
    public void testLoadByFullQualifiedName(CodegenConfig codegenConfig) throws Exception {
        final CodegenConfig loadedConfig = CodegenConfigLoader.forName(codegenConfig.getClass().getName());

        assertEquals(loadedConfig.getClass(), codegenConfig.getClass());
        assertEquals(loadedConfig.getName(), codegenConfig.getName());


    }
}
