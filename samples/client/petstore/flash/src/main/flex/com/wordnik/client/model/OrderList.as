package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class OrderList implements ListWrapper {
        // This declaration below of _Order_obj_class is to force flash compiler to include this class
        private var _order_obj_class: com.wordnik.client.model.Order = null;
        [XmlElements(name="order", type="com.wordnik.client.model.Order")]
        public var order: Array = new Array();

        public function getList(): Array{
            return order;
        }

}
}

