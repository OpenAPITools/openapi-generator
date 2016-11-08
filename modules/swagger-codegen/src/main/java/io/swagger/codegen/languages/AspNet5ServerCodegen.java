package io.swagger.codegen.languages;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AspNet5ServerCodegen extends AspNetCoreServerCodegen {

    @SuppressWarnings("hiding")
    protected Logger LOGGER = LoggerFactory.getLogger(AspNet5ServerCodegen.class);

    public AspNet5ServerCodegen() {
        super();

        embeddedTemplateDir = templateDir = "aspnetcore";
    }

    @Override
    public String getName() {
        return "aspnet5";
    }

    @Override
    public void processOpts() {
        super.processOpts();

        LOGGER.warn("aspnet5 is deprecated. Please use aspnetcore.");
    }
}
