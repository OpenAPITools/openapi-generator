package io.swagger.client.model {

import io.swagger.common.ListWrapper;

    public class CategoryList implements ListWrapper {
        // This declaration below of _Category_obj_class is to force flash compiler to include this class
        private var _category_obj_class: io.swagger.client.model.Category = null;
        [XmlElements(name="category", type="io.swagger.client.model.Category")]
        public var category: Array = new Array();

        public function getList(): Array{
            return category;
        }

}

}
