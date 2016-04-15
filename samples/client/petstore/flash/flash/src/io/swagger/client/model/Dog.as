package io.swagger.client.model {

import io.swagger.client.model.Animal;

    [XmlRootNode(name="Dog")]
    public class Dog {
                [XmlElement(name="className")]
        public var className: String = null;
                [XmlElement(name="breed")]
        public var breed: String = null;

    public function toString(): String {
        var str: String = "Dog: ";
        str += " (className: " + className + ")";
        str += " (breed: " + breed + ")";
        return str;
    }

}

}
