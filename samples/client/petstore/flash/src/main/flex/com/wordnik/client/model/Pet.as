package com.wordnik.client.model {

import com.wordnik.client.model.Category;
import com.wordnik.client.model.Tag;
[XmlRootNode(name="Pet")]
    public class Pet {
    // This declaration below of _tags_obj_class is to force flash compiler to include this class
        private var _tags_obj_class: com.wordnik.client.model.Tag = null;
        [XmlElementWrapper(name="tags")]
        [XmlElements(name="tag", type="com.wordnik.client.model.Tag")]
        public var tags: Array = new Array();

    [XmlElement(name="id")]
        public var id: Number = 0.0;

    [XmlElement(name="category")]
        public var category: Category = null;

    /* pet status in the store */
    [XmlElement(name="status")]
        public var status: String = null;

    [XmlElement(name="name")]
        public var name: String = null;

    // This declaration below of _photoUrls_obj_class is to force flash compiler to include this class
        private var _photoUrls_obj_class: com.wordnik.client.model.String = null;
        [XmlElementWrapper(name="photoUrls")]
        [XmlElements(name="photoUrl", type="com.wordnik.client.model.String")]
        public var photoUrls: Array = new Array();

    public function toString(): String {
            var str: String = "Pet: ";
            str += " (tags: " + tags + ")";
            str += " (id: " + id + ")";
            str += " (category: " + category + ")";
            str += " (status: " + status + ")";
            str += " (name: " + name + ")";
            str += " (photoUrls: " + photoUrls + ")";
            return str;
        }


}
}

