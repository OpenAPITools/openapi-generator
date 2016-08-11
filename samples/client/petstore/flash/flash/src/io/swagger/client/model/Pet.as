package io.swagger.client.model {

import io.swagger.client.model.Category;
import io.swagger.client.model.Tag;

    [XmlRootNode(name="Pet")]
    public class Pet {
                [XmlElement(name="id")]
        public var id: Number = 0;
                [XmlElement(name="category")]
        public var category: Category = NaN;
                [XmlElement(name="name")]
        public var name: String = null;
                // This declaration below of _photoUrls_obj_class is to force flash compiler to include this class
        private var _photoUrls_obj_class: Array = null;
        [XmlElementWrapper(name="photoUrls")]
        [XmlElements(name="photoUrls", type="Array")]
                public var photoUrls: Array = new Array();
                // This declaration below of _tags_obj_class is to force flash compiler to include this class
        private var _tags_obj_class: Array = null;
        [XmlElementWrapper(name="tags")]
        [XmlElements(name="tags", type="Array")]
                public var tags: Array = new Array();
        /* pet status in the store */
        [XmlElement(name="status")]
        public var status: String = null;

    public function toString(): String {
        var str: String = "Pet: ";
        str += " (id: " + id + ")";
        str += " (category: " + category + ")";
        str += " (name: " + name + ")";
        str += " (photoUrls: " + photoUrls + ")";
        str += " (tags: " + tags + ")";
        str += " (status: " + status + ")";
        return str;
    }

}

}
