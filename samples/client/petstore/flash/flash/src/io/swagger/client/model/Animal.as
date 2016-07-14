package io.swagger.client.model {


    [XmlRootNode(name="Animal")]
    public class Animal {
                [XmlElement(name="className")]
        public var className: String = null;

    public function toString(): String {
        var str: String = "Animal: ";
        str += " (className: " + className + ")";
        return str;
    }

}

}
