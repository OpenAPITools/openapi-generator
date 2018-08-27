package org.openapitools.api;

import org.openapitools.model.Client;
import io.swagger.annotations.*;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.context.request.NativeWebRequest;
import org.springframework.web.multipart.MultipartFile;

import javax.validation.constraints.*;
import javax.validation.Valid;
import java.util.List;
import java.util.Map;

@Controller
@RequestMapping("${openapi.openAPIPetstore.base-path:/v2}")
public class FakeClassnameTestApiController implements FakeClassnameTestApi {

    private final NativeWebRequest request;

    @org.springframework.beans.factory.annotation.Autowired
    public FakeClassnameTestApiController(NativeWebRequest request) {
        this.request = request;
    }

    public ResponseEntity<Client> testClassname(@ApiParam(value = "client model" ,required=true )  @Valid @RequestBody Client client) {
        for (MediaType mediaType: MediaType.parseMediaTypes(request.getHeader("Accept"))) {
            if (mediaType.isCompatibleWith(MediaType.valueOf("application/json"))) {
                ApiUtil.setExampleResponse(request, "application/json", "{  \"client\" : \"client\"}");
                break;
            }
        }
        return new ResponseEntity<>(HttpStatus.NOT_IMPLEMENTED);

    }

}
