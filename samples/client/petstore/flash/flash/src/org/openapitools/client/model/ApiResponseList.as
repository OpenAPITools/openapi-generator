package org.openapitools.client.model {

import org.openapitools.common.ListWrapper;

    public class ApiResponseList implements ListWrapper {
        // This declaration below of _ApiResponse_obj_class is to force flash compiler to include this class
        private var _apiResponse_obj_class: org.openapitools.client.model.ApiResponse = null;
        [XmlElements(name="apiResponse", type="org.openapitools.client.model.ApiResponse")]
        public var apiResponse: Array = new Array();

        public function getList(): Array{
            return apiResponse;
        }

}

}
