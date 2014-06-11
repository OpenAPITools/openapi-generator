package com.wordnik.client.model {

[XmlRootNode(name="Tag")]
    public class Tag {
    [XmlElement(name="id")]
        public var id: Number = 0.0;

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

