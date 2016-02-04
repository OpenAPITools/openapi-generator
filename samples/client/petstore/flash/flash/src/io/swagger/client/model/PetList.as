package io.swagger.client.model {

import io.swagger.common.ListWrapper;
import io.swagger.client.model.Category;
import io.swagger.client.model.Tag;

    public class PetList implements ListWrapper {
        // This declaration below of _Pet_obj_class is to force flash compiler to include this class
        private var _pet_obj_class: io.swagger.client.model.Pet = null;
        [XmlElements(name="pet", type="io.swagger.client.model.Pet")]
        public var pet: Array = new Array();

        public function getList(): Array{
            return pet;
        }

}
        

}
