package io.swagger.client.model {


    [XmlRootNode(name="ApiResponse")]
    public class ApiResponse {
                [XmlElement(name="code")]
        public var code: Number = 0;
                [XmlElement(name="type")]
        public var type: String = null;
                [XmlElement(name="message")]
        public var message: String = null;

    public function toString(): String {
        var str: String = "ApiResponse: ";
        str += " (code: " + code + ")";
        str += " (type: " + type + ")";
        str += " (message: " + message + ")";
        return str;
    }

}

}
