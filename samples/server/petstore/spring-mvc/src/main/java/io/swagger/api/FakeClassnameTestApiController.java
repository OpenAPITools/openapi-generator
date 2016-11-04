package io.swagger.api;

import io.swagger.model.Client;

import io.swagger.annotations.*;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;



@Controller
public class FakeClassnameTestApiController implements FakeClassnameTestApi {

    public ResponseEntity<Client> testClassname(@ApiParam(value = "client model" ,required=true ) @RequestBody Client body) {
        // do some magic!
        return new ResponseEntity<Client>(HttpStatus.OK);
    }

}
