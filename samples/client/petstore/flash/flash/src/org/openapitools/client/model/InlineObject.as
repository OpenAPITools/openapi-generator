package org.openapitools.client.model {


    [XmlRootNode(name="InlineObject")]
    public class InlineObject {
        /* Updated name of the pet */
        [XmlElement(name="name")]
        public var name: String = null;
        /* Updated status of the pet */
        [XmlElement(name="status")]
        public var status: String = null;

    public function toString(): String {
        var str: String = "InlineObject: ";
        str += " (name: " + name + ")";
        str += " (status: " + status + ")";
        return str;
    }

}

}
