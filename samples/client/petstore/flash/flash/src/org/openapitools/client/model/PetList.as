package org.openapitools.client.model {

import org.openapitools.common.ListWrapper;
import org.openapitools.client.model.Category;
import org.openapitools.client.model.Tag;

    public class PetList implements ListWrapper {
        // This declaration below of _Pet_obj_class is to force flash compiler to include this class
        private var _pet_obj_class: org.openapitools.client.model.Pet = null;
        [XmlElements(name="pet", type="org.openapitools.client.model.Pet")]
        public var pet: Array = new Array();

        public function getList(): Array{
            return pet;
        }

}

}
