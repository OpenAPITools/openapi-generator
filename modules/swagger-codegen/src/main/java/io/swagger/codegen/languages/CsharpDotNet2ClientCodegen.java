package io.swagger.codegen.languages;

import io.swagger.codegen.CliOption;
import io.swagger.codegen.CodegenConstants;
import io.swagger.codegen.CodegenType;
import io.swagger.codegen.SupportingFile;

import java.io.File;

public class CsharpDotNet2ClientCodegen extends AbstractCSharpCodegen {
    public static final String CLIENT_PACKAGE = "clientPackage";
    protected String clientPackage = "IO.Swagger.Client";
    protected String apiDocPath = "docs/";
    protected String modelDocPath = "docs/";

    public CsharpDotNet2ClientCodegen() {
        super();

        // clear import mapping (from default generator) as C# (2.0) does not use it
        // at the moment
        importMapping.clear();

        modelTemplateFiles.put("model.mustache", ".cs");
        apiTemplateFiles.put("api.mustache", ".cs");

        setApiPackage(packageName + ".Api");
        setModelPackage(packageName + ".Model");
        setClientPackage(packageName + ".Client");
        setSourceFolder("src" + File.separator + "main" + File.separator + "CsharpDotNet2");

        modelDocTemplateFiles.put("model_doc.mustache", ".md");
        apiDocTemplateFiles.put("api_doc.mustache", ".md");

        cliOptions.clear();
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_NAME,
                "C# package name (convention: Camel.Case).")
                .defaultValue(packageName));
        cliOptions.add(new CliOption(CodegenConstants.PACKAGE_VERSION,
                "C# package version.")
                .defaultValue(packageVersion));
        cliOptions.add(new CliOption(CLIENT_PACKAGE,
                "C# client package name (convention: Camel.Case).")
                .defaultValue(clientPackage));
    }

    @Override
    public void processOpts() {
        super.processOpts();

        if (additionalProperties.containsKey(CLIENT_PACKAGE)) {
            setClientPackage((String) additionalProperties.get(CLIENT_PACKAGE));
        } else {
            additionalProperties.put(CLIENT_PACKAGE, getClientPackage());
        }

        final String clientPackage = getClientPackage();
        final String clientPackagePath = clientPackage.replace(".", java.io.File.separator);

        additionalProperties.put("apiDocPath", apiDocPath);
        additionalProperties.put("modelDocPath", modelDocPath);

        supportingFiles.add(new SupportingFile("Configuration.mustache",
                sourceFolder + File.separator + clientPackagePath, "Configuration.cs"));
        supportingFiles.add(new SupportingFile("ApiClient.mustache",
                sourceFolder + File.separator + clientPackagePath, "ApiClient.cs"));
        supportingFiles.add(new SupportingFile("ApiException.mustache",
                sourceFolder + File.separator + clientPackagePath, "ApiException.cs"));
        supportingFiles.add(new SupportingFile("packages.config.mustache", "vendor", "packages.config"));
        supportingFiles.add(new SupportingFile("compile-mono.sh.mustache", "", "compile-mono.sh"));
        supportingFiles.add(new SupportingFile("README.mustache", "", "README.md"));

    }

    @Override
    public String apiPackage() {
        return packageName + ".Api";
    }

    @Override
    public String modelPackage() {
        return packageName + ".Model";
    }

    public String getClientPackage() {
        return clientPackage;
    }

    public void setClientPackage(String clientPackage) {
        this.clientPackage = clientPackage;
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "csharp-dotnet2";
    }

    @Override
    public String getHelp() {
        return "Generates a C# .Net 2.0 client library.";
    }

    @Override
    public String apiFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + apiPackage().replace('.', File.separatorChar);
    }

    @Override
    public String modelFileFolder() {
        return outputFolder + File.separator + sourceFolder + File.separator + modelPackage().replace('.', File.separatorChar);
    }

    @Override
    public String apiDocFileFolder() {
        return (outputFolder + "/" + apiDocPath).replace('/', File.separatorChar);
    }

    @Override
    public String modelDocFileFolder() {
        return (outputFolder + "/" + modelDocPath).replace('/', File.separatorChar);
    }

}
