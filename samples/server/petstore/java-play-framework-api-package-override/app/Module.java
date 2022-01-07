import com.google.inject.AbstractModule;

import com.puppies.store.apis.*;
import openapitools.SecurityAPIUtils;

public class Module extends AbstractModule {

    @Override
    protected void configure() {
        bind(PetApiControllerImpInterface.class).to(PetApiControllerImp.class);
        bind(StoreApiControllerImpInterface.class).to(StoreApiControllerImp.class);
        bind(UserApiControllerImpInterface.class).to(UserApiControllerImp.class);
        bind(SecurityAPIUtils.class);
    }
}