package io.swagger.client.model {

import io.swagger.common.ListWrapper;
import io.swagger.client.model.Object;
import io.swagger.client.model.Tag;

    public class InlineResponse200List implements ListWrapper {
        // This declaration below of _inline_response_200_obj_class is to force flash compiler to include this class
        private var _inlineResponse200_obj_class: io.swagger.client.model.InlineResponse200 = null;
        [XmlElements(name="inlineResponse200", type="io.swagger.client.model.InlineResponse200")]
        public var inlineResponse200: Array = new Array();

        public function getList(): Array{
            return inlineResponse200;
        }

}

}
