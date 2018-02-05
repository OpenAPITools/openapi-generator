package io.swagger.codegen.languages;

import java.io.File;
import java.io.IOException;
import java.io.Writer;

import com.samskivert.mustache.Mustache;
import com.samskivert.mustache.Template;
import io.swagger.codegen.*;

public class AdaCodegen extends AbstractAdaCodegen implements CodegenConfig {

    public AdaCodegen() {
        super();
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "ada";
    }

    @Override
    public String getHelp() {
        return "Generates an Ada client implementation (beta).";
    }

    @Override
    public void processOpts() {
        super.processOpts();
        if (additionalProperties.containsKey(CodegenConstants.PACKAGE_NAME)) {
            packageName = (String) additionalProperties.get(CodegenConstants.PACKAGE_NAME);
        }
        if (packageName == "") {
            packageName = modelPackage;
        }
        String srcPrefix = "src" + File.separator;
        String modelPrefix = srcPrefix + "model" + File.separator + toFilename(modelPackage);
        String clientPrefix = srcPrefix + "client" + File.separator + toFilename(modelPackage);
        supportingFiles.add(new SupportingFile("model-spec.mustache", null, modelPrefix + "-models.ads"));
        supportingFiles.add(new SupportingFile("model-body.mustache", null, modelPrefix + "-models.adb"));
        supportingFiles.add(new SupportingFile("client-spec.mustache", null, clientPrefix + "-clients.ads"));
        supportingFiles.add(new SupportingFile("client-body.mustache", null, clientPrefix + "-clients.adb"));

        if (additionalProperties.containsKey(CodegenConstants.PROJECT_NAME)) {
            projectName = (String) additionalProperties.get(CodegenConstants.PROJECT_NAME);
        } else {
            // default: set project based on package name
            // e.g. petstore.api (package name) => petstore_api (project name)
            projectName = packageName.replaceAll("\\.", "_");
        }
        String configBaseName = modelPackage.toLowerCase();
        supportingFiles.add(new SupportingFile("gnat-project.mustache", "", toFilename(projectName) + ".gpr"));
        // supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));
        supportingFiles.add(new SupportingFile("config.gpr", "", "config.gpr"));

        /*
         * Additional Properties.  These values can be passed to the templates and
         * are available in models, apis, and supporting files
         */
        additionalProperties.put("package", this.modelPackage);
        additionalProperties.put("packageConfig", configBaseName);
        additionalProperties.put("packageDir", "client");
        additionalProperties.put("mainName", "client");
        additionalProperties.put("isServer", false);
        additionalProperties.put(CodegenConstants.PROJECT_NAME, projectName);

        String names[] = this.modelPackage.split("\\.");
        String pkgName = names[0];
        additionalProperties.put("packageLevel1", pkgName);
        supportingFiles.add(new SupportingFile("package-spec-level1.mustache", null,
                            "src" + File.separator + toFilename(names[0]) + ".ads"));
        if (names.length > 1) {
            String fileName = toFilename(names[0]) + "-" + toFilename(names[1]) + ".ads";
            pkgName = names[0] + "." + names[1];
            additionalProperties.put("packageLevel2", pkgName);
            supportingFiles.add(new SupportingFile("package-spec-level2.mustache", null,
                                "src" + File.separator + fileName));
        }
        pkgName = this.modelPackage;
        supportingFiles.add(new SupportingFile("client.mustache", null,
                            "src" + File.separator + toFilename(pkgName) + "-client.adb"));
        additionalProperties.put("packageName", toFilename(pkgName));

        // add lambda for mustache templates
        additionalProperties.put("lambdaAdaComment", new Mustache.Lambda() {
            @Override
            public void execute(Template.Fragment fragment, Writer writer) throws IOException {
                String content = fragment.execute();
                content = content.trim().replaceAll("\n$", "");
                writer.write(content.replaceAll("\n", "\n   --  "));
            }
        });
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + "/" + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + "/model/" + modelPackage().replace('.', File.separatorChar);
    }
}
