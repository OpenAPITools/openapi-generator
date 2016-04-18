package io.swagger.client.model {


    [XmlRootNode(name="200Response")]
    public class 200Response {
                [XmlElement(name="name")]
        public var name: Number = 0;

    public function toString(): String {
        var str: String = "200Response: ";
        str += " (name: " + name + ")";
        return str;
    }

}

}
