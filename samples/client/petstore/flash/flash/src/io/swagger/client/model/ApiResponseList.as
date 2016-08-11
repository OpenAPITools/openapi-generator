package io.swagger.client.model {

import io.swagger.common.ListWrapper;

    public class ApiResponseList implements ListWrapper {
        // This declaration below of _ApiResponse_obj_class is to force flash compiler to include this class
        private var _apiResponse_obj_class: io.swagger.client.model.ApiResponse = null;
        [XmlElements(name="apiResponse", type="io.swagger.client.model.ApiResponse")]
        public var apiResponse: Array = new Array();

        public function getList(): Array{
            return apiResponse;
        }

}

}
