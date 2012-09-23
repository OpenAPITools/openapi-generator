package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
import com.wordnik.client.model.Category;
import com.wordnik.client.model.Tag;
public class PetList implements ListWrapper {
        // This declaration below of _Pet_obj_class is to force flash compiler to include this class
        private var _pet_obj_class: com.wordnik.client.model.Pet = null;
        [XmlElements(name="pet", type="com.wordnik.client.model.Pet")]
        public var pet: Array = new Array();

        public function getList(): Array{
            return pet;
        }

}
}

