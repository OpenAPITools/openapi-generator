package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;

public class TagList implements ListWrapper {
    // This declaration below of _Tag_obj_class is to force flash compiler to include this class
    [XmlElements(name="tag", type="com.wordnik.client.model.Tag")]
    public var tag:Array = new Array();
    private var _tag_obj_class:com.wordnik.client.model.Tag = null;

    public function getList():Array {
        return tag;
    }

}
}

