package io.swagger.client.model {


    [XmlRootNode(name="ModelReturn")]
    public class ModelReturn {
                [XmlElement(name="return")]
        public var return_: Number = 0;

    public function toString(): String {
        var str: String = "ModelReturn: ";
        str += " (return_: " + return_ + ")";
        return str;
    }

}

}
