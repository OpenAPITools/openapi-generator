package io.swagger.client.model {

import io.swagger.common.ListWrapper;

    public class AnimalList implements ListWrapper {
        // This declaration below of _Animal_obj_class is to force flash compiler to include this class
        private var _animal_obj_class: io.swagger.client.model.Animal = null;
        [XmlElements(name="animal", type="io.swagger.client.model.Animal")]
        public var animal: Array = new Array();

        public function getList(): Array{
            return animal;
        }

}

}
