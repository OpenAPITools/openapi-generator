package io.swagger.client.model {

import io.swagger.common.ListWrapper;

    public class 200ResponseList implements ListWrapper {
        // This declaration below of _200_response_obj_class is to force flash compiler to include this class
        private var _200Response__obj_class: io.swagger.client.model.200Response = null;
        [XmlElements(name="200Response_", type="io.swagger.client.model.200Response")]
        public var 200Response_: Array = new Array();

        public function getList(): Array{
            return 200Response_;
        }

}

}
