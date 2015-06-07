package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;

public class CategoryList implements ListWrapper {
    // This declaration below of _Category_obj_class is to force flash compiler to include this class
    [XmlElements(name="category", type="com.wordnik.client.model.Category")]
    public var category:Array = new Array();
    private var _category_obj_class:com.wordnik.client.model.Category = null;

    public function getList():Array {
        return category;
    }

}
}

