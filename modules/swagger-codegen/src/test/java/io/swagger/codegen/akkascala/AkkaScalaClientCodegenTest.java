package io.swagger.codegen.akkascala;

import io.swagger.codegen.SupportingFile;
import io.swagger.codegen.languages.AkkaScalaClientCodegen;
import org.junit.Before;
import org.junit.Test;

import java.util.List;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsEqual.equalTo;
import static org.junit.Assert.assertThat;

public class AkkaScalaClientCodegenTest {

    private AkkaScalaClientCodegen akkaScalaClientCodegen;

    @Before
    public void setup() {
        this.akkaScalaClientCodegen = new AkkaScalaClientCodegen();
    }

    @Test
    public void shouldGenerateReadmeFile() {
        List<SupportingFile> supportingFiles = this.akkaScalaClientCodegen.supportingFiles();

        assertThat(supportingFiles.contains(new SupportingFile("README.mustache", "", "README.md")), is(equalTo(true)));
        assertThat(supportingFiles.contains(new SupportingFile("build.sbt.mustache", "", "build.sbt")), is(equalTo(true)));
    }
}
