package org.openapitools.client.model {

import org.openapitools.common.ListWrapper;
import flash.filesystem.File;

    public class InlineObject1List implements ListWrapper {
        // This declaration below of _inline_object_1_obj_class is to force flash compiler to include this class
        private var _inlineObject1_obj_class: org.openapitools.client.model.InlineObject1 = null;
        [XmlElements(name="inlineObject1", type="org.openapitools.client.model.InlineObject1")]
        public var inlineObject1: Array = new Array();

        public function getList(): Array{
            return inlineObject1;
        }

}

}
