package io.swagger.codegen.java;

import com.google.common.collect.Sets;
import io.swagger.codegen.CodegenModel;
import io.swagger.codegen.DefaultCodegen;
import io.swagger.codegen.languages.JavaClientCodegen;
import io.swagger.oas.models.media.ComposedSchema;
import io.swagger.oas.models.media.Discriminator;
import io.swagger.oas.models.media.Schema;
import io.swagger.oas.models.media.StringSchema;
import org.apache.commons.lang3.StringUtils;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.HashMap;
import java.util.Map;

public class JavaInheritanceTest {

    @SuppressWarnings("static-method")
    @Test(description = "convert a composed model with parent")
    // TODO verify inheritance with new oas3 structure
    public void javaInheritanceTest() {
        final Schema schema = new ComposedSchema()
                .addAllOfItem(new Schema().$ref("Base"))
                .addAllOfItem(new Schema().additionalProperties(new StringSchema()));
        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.parent, "Base");
        Assert.assertEquals(cm.imports, Sets.newHashSet("Base"));
    }

    @SuppressWarnings("static-method")
    @Test(description = "convert a composed model with discriminator")
    // TODO verify inheritance with new oas3 structure
    public void javaInheritanceWithDiscriminatorTest() {
        Schema base = new Schema();
        base.setDiscriminator(new Discriminator().mapping("name", StringUtils.EMPTY));

        final Schema schema = new ComposedSchema()
                .addAllOfItem(new Schema().$ref("Base"))
                .addAllOfItem(new Schema().additionalProperties(new StringSchema()));

        final Map<String, Schema> allDefinitions = new HashMap<String, Schema>();
        allDefinitions.put("Base", base);

        final DefaultCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, allDefinitions);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.parent, "Base");
        Assert.assertEquals(cm.imports, Sets.newHashSet("Base"));
    }
}
