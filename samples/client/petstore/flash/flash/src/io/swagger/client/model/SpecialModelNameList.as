package io.swagger.client.model {

import io.swagger.common.ListWrapper;

    public class SpecialModelNameList implements ListWrapper {
        // This declaration below of _$special[model.name]_obj_class is to force flash compiler to include this class
        private var _$Special[modelName]_obj_class: io.swagger.client.model.SpecialModelName = null;
        [XmlElements(name="$Special[modelName]", type="io.swagger.client.model.SpecialModelName")]
        public var $Special[modelName]: Array = new Array();

        public function getList(): Array{
            return $Special[modelName];
        }

}

}
