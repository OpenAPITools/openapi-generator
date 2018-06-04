package org.openapitools.virtualan.api;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.context.request.NativeWebRequest;
import org.openapitools.virtualan.VirtualServiceUtil;
import java.util.Optional;

@Controller
@RequestMapping("${openapi.openAPIPetstore.base-path:/v2}")
public class PetApiController implements PetApi {

	@org.springframework.beans.factory.annotation.Autowired
	private VirtualServiceUtil virtualServiceUtil;
    private final NativeWebRequest request;

    @org.springframework.beans.factory.annotation.Autowired
    public PetApiController(NativeWebRequest request) {
        this.request = request;
    }

	
	@Override
    public Optional<VirtualServiceUtil> getVirtualServiceUtil() {
        return Optional.ofNullable(virtualServiceUtil);
    }
	
	public String addQueryParamValue(Object value){
		return String.join(",", (java.util.List)value);
	}

    @Override
    public Optional<NativeWebRequest> getRequest() {
        return Optional.ofNullable(request);
    }

}
