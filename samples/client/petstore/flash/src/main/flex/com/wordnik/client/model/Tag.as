package com.wordnik.client.model {

[XmlRootNode(name="Tag")]
    public class Tag {
    /* Unique identifier for the tag */
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    /* Friendly name for the tag */
    [XmlElement(name="name")]
        public var name: String = null;

    public function toString(): String {
            var str: String = "Tag: ";
            str += " (id: " + id + ")";
            str += " (name: " + name + ")";
            return str;
        }


}
}

