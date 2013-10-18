package com.wordnik.client.model {

[XmlRootNode(name="Category")]
    public class Category {
    /* Category unique identifier */
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    /* Name of the category */
    [XmlElement(name="name")]
        public var name: String = null;

    public function toString(): String {
            var str: String = "Category: ";
            str += " (id: " + id + ")";
            str += " (name: " + name + ")";
            return str;
        }


}
}

