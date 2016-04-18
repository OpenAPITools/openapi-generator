package io.swagger.client.model {

import io.swagger.common.ListWrapper;

    public class ModelReturnList implements ListWrapper {
        // This declaration below of _Return_obj_class is to force flash compiler to include this class
        private var _return__obj_class: io.swagger.client.model.ModelReturn = null;
        [XmlElements(name="return_", type="io.swagger.client.model.ModelReturn")]
        public var return_: Array = new Array();

        public function getList(): Array{
            return return_;
        }

}

}
