package io.swagger.client.model {

import io.swagger.common.ListWrapper;

    public class NameList implements ListWrapper {
        // This declaration below of _Name_obj_class is to force flash compiler to include this class
        private var _name_obj_class: io.swagger.client.model.Name = null;
        [XmlElements(name="name", type="io.swagger.client.model.Name")]
        public var name: Array = new Array();

        public function getList(): Array{
            return name;
        }

}

}
