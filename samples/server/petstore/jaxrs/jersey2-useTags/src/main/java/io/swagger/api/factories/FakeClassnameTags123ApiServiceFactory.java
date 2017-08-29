package io.swagger.api.factories;

import io.swagger.api.FakeClassnameTags123ApiService;
import io.swagger.api.impl.FakeClassnameTags123ApiServiceImpl;


public class FakeClassnameTags123ApiServiceFactory {
    private final static FakeClassnameTags123ApiService service = new FakeClassnameTags123ApiServiceImpl();

    public static FakeClassnameTags123ApiService getFakeClassnameTags123Api() {
        return service;
    }
}
