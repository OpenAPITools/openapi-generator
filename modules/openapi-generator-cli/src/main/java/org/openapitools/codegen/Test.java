package org.openapitools.codegen;
import java.util.* ;
import java.nio.file.Path;

import org.apache.commons.lang3.ArrayUtils;

import io.airlift.airline.Cli;

import org.openapitools.codegen.Generator;
import org.openapitools.codegen.cmd.Generate;
import org.openapitools.codegen.config.CodegenConfigurator;

public class Test {


    public static void main(String[] args) {


        String specFlag = "-i";
        String spec = "C:\\deepmatrix\\codegen\\new-data-service.yaml";
         String langFlag = "-g";

         String lang = "jaxrs-spec";
         String outputDirFlag = "-o"; 
         String outputDir = "C:\\deepmatrix\\codegen\\codegeneg2";


        CodegenConfigurator configurator = new CodegenConfigurator();
        Generator generator = new DefaultGenerator();
         
        final String[] commonArgs =
        {"generate", langFlag, lang, outputDirFlag, outputDir, specFlag, spec, "--library", "quarkus","--additional-properties","interfaceOnly=true"};


        String[] argsToUse = ArrayUtils.addAll(commonArgs);


        Cli.CliBuilder<Runnable> builder =
        Cli.<Runnable>builder("openapi-generator-cli").withCommands(Generate.class);

        Generate generate = (Generate) builder.build().parse(argsToUse);

        

        generate.configurator = configurator;
        generate.generator = generator;

        try {
            generate.run();
    } finally {
            configurator.setInputSpec(spec);
            configurator.setGeneratorName(lang);
            configurator.setOutputDir(outputDir);
        }
    } 
}