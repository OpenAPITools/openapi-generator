package org.openapitools.api.factories;

import org.openapitools.api.FakeClassnameTags123ApiService;
import org.openapitools.api.impl.FakeClassnameTags123ApiServiceImpl;


public class FakeClassnameTags123ApiServiceFactory {
    private final static FakeClassnameTags123ApiService service = new FakeClassnameTags123ApiServiceImpl();

    public static FakeClassnameTags123ApiService getFakeClassnameTags123Api() {
        return service;
    }
}
