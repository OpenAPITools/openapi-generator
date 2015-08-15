package io.swagger.client.model {

import io.swagger.common.ListWrapper;

    public class TagList implements ListWrapper {
        // This declaration below of _Tag_obj_class is to force flash compiler to include this class
        private var _tag_obj_class: io.swagger.client.model.Tag = null;
        [XmlElements(name="tag", type="io.swagger.client.model.Tag")]
        public var tag: Array = new Array();

        public function getList(): Array{
            return tag;
        }

}
        

}
