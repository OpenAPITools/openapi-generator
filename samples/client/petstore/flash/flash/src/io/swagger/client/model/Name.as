package io.swagger.client.model {


    [XmlRootNode(name="Name")]
    public class Name {
                [XmlElement(name="name")]
        public var name: Number = 0;
                [XmlElement(name="snake_case")]
        public var snakeCase: Number = 0;

    public function toString(): String {
        var str: String = "Name: ";
        str += " (name: " + name + ")";
        str += " (snakeCase: " + snakeCase + ")";
        return str;
    }

}

}
