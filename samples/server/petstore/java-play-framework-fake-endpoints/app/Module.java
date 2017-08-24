import com.google.inject.AbstractModule;

import controllers.*;

public class Module extends AbstractModule {

    @Override
    protected void configure() {
        bind(FakeApiControllerImpInterface.class).to(FakeApiControllerImp.class);
        bind(FakeClassnameTags123ApiControllerImpInterface.class).to(FakeClassnameTags123ApiControllerImp.class);
        bind(PetApiControllerImpInterface.class).to(PetApiControllerImp.class);
        bind(StoreApiControllerImpInterface.class).to(StoreApiControllerImp.class);
        bind(UserApiControllerImpInterface.class).to(UserApiControllerImp.class);
    }
}