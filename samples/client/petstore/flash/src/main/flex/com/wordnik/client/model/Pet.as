package com.wordnik.client.model {

import com.wordnik.client.model.Category;
import com.wordnik.client.model.Tag;
[XmlRootNode(name="Pet")]
    public class Pet {
    /* Unique identifier for the Pet */
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    /* Category the pet is in */
    [XmlElement(name="category")]
        public var category: Category = null;

    /* Friendly name of the pet */
    [XmlElement(name="name")]
        public var name: String = null;

    /* Image URLs */
    // This declaration below of _photoUrls_obj_class is to force flash compiler to include this class
        private var _photoUrls_obj_class: com.wordnik.client.model.String = null;
        [XmlElementWrapper(name="photoUrls")]
        [XmlElements(name="photoUrl", type="com.wordnik.client.model.String")]
        public var photoUrls: Array = new Array();

    /* Tags assigned to this pet */
    // This declaration below of _tags_obj_class is to force flash compiler to include this class
        private var _tags_obj_class: com.wordnik.client.model.Tag = null;
        [XmlElementWrapper(name="tags")]
        [XmlElements(name="tag", type="com.wordnik.client.model.Tag")]
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

