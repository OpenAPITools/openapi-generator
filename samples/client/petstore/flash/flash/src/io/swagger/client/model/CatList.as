package io.swagger.client.model {

import io.swagger.common.ListWrapper;
import io.swagger.client.model.Animal;

    public class CatList implements ListWrapper {
        // This declaration below of _Cat_obj_class is to force flash compiler to include this class
        private var _cat_obj_class: io.swagger.client.model.Cat = null;
        [XmlElements(name="cat", type="io.swagger.client.model.Cat")]
        public var cat: Array = new Array();

        public function getList(): Array{
            return cat;
        }

}

}
