package org.openapitools.api;

import org.springframework.web.bind.annotation.RestController;

import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.context.request.NativeWebRequest;
import java.util.Optional;

@RestController
@RequestMapping("${openapi.openAPIPetstore.base-path:/v2}")
public class AnotherFakeApiController implements AnotherFakeApi {

}
