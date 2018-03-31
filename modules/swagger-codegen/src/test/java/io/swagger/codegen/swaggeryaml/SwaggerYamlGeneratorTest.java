package io.swagger.codegen.swaggeryaml;

import io.swagger.codegen.ClientOptInput;
import io.swagger.codegen.DefaultGenerator;
import io.swagger.codegen.config.CodegenConfigurator;
import org.apache.commons.io.FileUtils;
import org.junit.rules.TemporaryFolder;
import org.testng.Assert;
import org.testng.annotations.Test;

import java.io.File;

public class SwaggerYamlGeneratorTest {

    @Test
    public void testLongText() throws Exception {
        final String LONG_DESCRIPTION_SAMPLE = "Are creatures of the cosmos Rig Veda? Trillion! Made in the interiors of collapsing stars Apollonius of Perga, globular star cluster emerged into consciousness bits of moving fluff brain is the seed of intelligence citizens of distant epochs another world courage of our questions a mote of dust suspended in a sunbeam ship of the imagination, paroxysm of global death intelligent beings? Two ghostly white figures in coveralls and helmets are soflty dancing hearts of the stars brain is the seed of intelligence quasar, Drake Equation billions upon billions and billions upon billions upon billions upon billions upon billions upon billions upon billions";
        final String LONG_DESCRIPTION_SAMPLE2 = "Light years, culture, dispassionate extraterrestrial observer citizens of distant epochs intelligent beings Jean-Francois Champollion encyclopaedia galactica Sea of Tranquility emerged into consciousness Cambrian explosion another world Cambrian explosion globular star cluster. Emerged into consciousness take root and flourish explorations a mote of dust suspended in a sunbeam encyclopaedia galactica, not a sunrise but a galaxyrise, Orion's sword encyclopaedia galactica vastness is bearable only through love cosmos. Birth and billions upon billions upon billions upon billions upon billions upon billions upon billions.";

        final TemporaryFolder folder = new TemporaryFolder();
        folder.create();
        final File output = folder.getRoot();
        final CodegenConfigurator configurator = new CodegenConfigurator()
                .setLang("swagger-yaml")
                .setInputSpec("src/test/resources/2_0/long_description_issue_7839.json")
                .setOutputDir(output.getAbsolutePath());

        final ClientOptInput clientOptInput = configurator.toClientOptInput();
        new DefaultGenerator().opts(clientOptInput).generate();

        File outputFile = new File(output, "swagger.yaml");
        Assert.assertTrue(outputFile.exists());

        String content = FileUtils.readFileToString(outputFile);

        Assert.assertTrue(content.contains(LONG_DESCRIPTION_SAMPLE));
        Assert.assertTrue(content.contains(LONG_DESCRIPTION_SAMPLE2));

        folder.delete();

    }
}
