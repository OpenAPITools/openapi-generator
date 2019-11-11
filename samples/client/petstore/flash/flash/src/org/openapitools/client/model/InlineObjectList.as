package org.openapitools.client.model {

import org.openapitools.common.ListWrapper;

    public class InlineObjectList implements ListWrapper {
        // This declaration below of _inline_object_obj_class is to force flash compiler to include this class
        private var _inlineObject_obj_class: org.openapitools.client.model.InlineObject = null;
        [XmlElements(name="inlineObject", type="org.openapitools.client.model.InlineObject")]
        public var inlineObject: Array = new Array();

        public function getList(): Array{
            return inlineObject;
        }

}

}
