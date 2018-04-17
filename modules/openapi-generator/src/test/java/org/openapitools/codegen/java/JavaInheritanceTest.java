package org.openapitools.codegen.java;

import com.google.common.collect.Sets;
import io.swagger.v3.oas.models.media.ComposedSchema;
import io.swagger.v3.oas.models.media.Discriminator;
import io.swagger.v3.oas.models.media.Schema;
import io.swagger.v3.oas.models.media.StringSchema;
import org.apache.commons.lang3.StringUtils;
import org.openapitools.codegen.CodegenModel;
import org.openapitools.codegen.languages.JavaClientCodegen;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.util.HashMap;
import java.util.Map;

public class JavaInheritanceTest {

    @Test(description = "convert a composed model with parent")
    public void javaInheritanceTest() {
        final Schema parentModel = new Schema().name("Base");

        final Schema schema = new ComposedSchema()
                .addAllOfItem(new Schema().$ref("Base"))
                .name("composed");

        final Map<String, Schema> allSchemas = new HashMap<>();
        allSchemas.put(parentModel.getName(), parentModel);
        allSchemas.put(schema.getName(), schema);

        final JavaClientCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, allSchemas);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.parent, "Base");
        Assert.assertEquals(cm.imports, Sets.newHashSet("Base"));
    }

    @Test(description = "convert a composed model with discriminator")
    public void javaInheritanceWithDiscriminatorTest() {
        final Schema base = new Schema().name("Base");
        base.setDiscriminator(new Discriminator().mapping("name", StringUtils.EMPTY));

        final Schema schema = new ComposedSchema()
                .addAllOfItem(new Schema().$ref("Base"));

        final Map<String, Schema> allDefinitions = new HashMap<String, Schema>();
        allDefinitions.put("Base", base);

        final JavaClientCodegen codegen = new JavaClientCodegen();
        final CodegenModel cm = codegen.fromModel("sample", schema, allDefinitions);

        Assert.assertEquals(cm.name, "sample");
        Assert.assertEquals(cm.classname, "Sample");
        Assert.assertEquals(cm.parent, "Base");
        Assert.assertEquals(cm.imports, Sets.newHashSet("Base"));
    }
}
