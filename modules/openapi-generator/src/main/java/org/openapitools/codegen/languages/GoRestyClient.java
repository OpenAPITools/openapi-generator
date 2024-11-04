package org.openapitools.codegen.languages;

import com.samskivert.mustache.Mustache;
import org.openapitools.codegen.*;
import org.openapitools.codegen.templating.mustache.GoHttpStatusLambda;
import org.openapitools.codegen.templating.mustache.HttpStatusNameLambda;
import org.openapitools.codegen.templating.mustache.SpringHttpStatusLambda;

import java.io.File;

public class GoRestyClient extends GoGinServer2Codegen {

    @Override
    public CodegenType getTag() {
        return CodegenType.CLIENT;
    }

    @Override
    public String getName() {
        return "go-resty-client";
    }

    @Override
    public String getHelp() {
        return "Generates a go resty client.";
    }

    public GoRestyClient() {
        super();

        outputFolder = "generated-code" + File.separator + "go-resty-client";
        modelTemplateFiles.put("model.mustache", ".go");
        apiTemplateFiles.put("api.mustache", ".go");
        embeddedTemplateDir = templateDir = "go-resty-client";

        replacePath = false;
    }

    @Override
    protected void addSupportingFiles() {
        supportingFiles.add(new SupportingFile("client.mustache", path, "client.go"));
        supportingFiles.add(new SupportingFile("constants.mustache", path + "/common", "constants.go"));
        supportingFiles.add(new SupportingFile("request_options.mustache", path + "/common", "request_options.go"));
        supportingFiles.add(new SupportingFile("utils.mustache", path + "/common", "utils.go"));
        supportingFiles.add(new SupportingFile("README.mustache", path, "README.md"));
    }

    @Override
    public void processOpts() {
        super.processOpts();
        additionalProperties.put("lambda.append-package", (Mustache.Lambda) (fragment, writer) -> writer.write(appendPackage(fragment.execute())));
        additionalProperties.put("goHttpStatus", new GoHttpStatusLambda());
        additionalProperties.put("goHttpStatusName", new HttpStatusNameLambda());
    }

    private String appendPackage(String content) {
        return content.trim().replace("[]", "[]models.");
    }
}
