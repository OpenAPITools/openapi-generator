package io.swagger.client.model {

import io.swagger.common.ListWrapper;
import io.swagger.client.model.Animal;

    public class DogList implements ListWrapper {
        // This declaration below of _Dog_obj_class is to force flash compiler to include this class
        private var _dog_obj_class: io.swagger.client.model.Dog = null;
        [XmlElements(name="dog", type="io.swagger.client.model.Dog")]
        public var dog: Array = new Array();

        public function getList(): Array{
            return dog;
        }

}

}
