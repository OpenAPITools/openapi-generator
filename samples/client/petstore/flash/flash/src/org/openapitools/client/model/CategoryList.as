package org.openapitools.client.model {

import org.openapitools.common.ListWrapper;

    public class CategoryList implements ListWrapper {
        // This declaration below of _Category_obj_class is to force flash compiler to include this class
        private var _category_obj_class: org.openapitools.client.model.Category = null;
        [XmlElements(name="category", type="org.openapitools.client.model.Category")]
        public var category: Array = new Array();

        public function getList(): Array{
            return category;
        }

}

}
