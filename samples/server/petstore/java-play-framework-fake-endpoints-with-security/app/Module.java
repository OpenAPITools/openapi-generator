import com.google.inject.AbstractModule;

import controllers.*;
import openapitools.SecurityAPIUtils;

public class Module extends AbstractModule {

    @Override
    protected void configure() {
        bind(PetApiControllerImpInterface.class).to(PetApiControllerImp.class);
        bind(SecurityAPIUtils.class);
    }
}