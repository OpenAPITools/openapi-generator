package io.swagger.client.model {

import io.swagger.common.ListWrapper;
import io.swagger.client.model.ByteArray;

    public class FormatTestList implements ListWrapper {
        // This declaration below of _format_test_obj_class is to force flash compiler to include this class
        private var _formatTest_obj_class: io.swagger.client.model.FormatTest = null;
        [XmlElements(name="formatTest", type="io.swagger.client.model.FormatTest")]
        public var formatTest: Array = new Array();

        public function getList(): Array{
            return formatTest;
        }

}

}
