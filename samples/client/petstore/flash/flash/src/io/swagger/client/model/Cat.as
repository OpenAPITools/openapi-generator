package io.swagger.client.model {

import io.swagger.client.model.Animal;

    [XmlRootNode(name="Cat")]
    public class Cat {
                [XmlElement(name="className")]
        public var className: String = null;
                [XmlElement(name="declawed")]
        public var declawed: Boolean = false;

    public function toString(): String {
        var str: String = "Cat: ";
        str += " (className: " + className + ")";
        str += " (declawed: " + declawed + ")";
        return str;
    }

}

}
