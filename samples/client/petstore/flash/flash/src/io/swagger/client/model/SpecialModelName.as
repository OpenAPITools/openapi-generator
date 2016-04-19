package io.swagger.client.model {


    [XmlRootNode(name="SpecialModelName")]
    public class SpecialModelName {
                [XmlElement(name="$special[property.name]")]
        public var $Special[propertyName]: Number = 0;

    public function toString(): String {
        var str: String = "SpecialModelName: ";
        str += " ($Special[propertyName]: " + $Special[propertyName] + ")";
        return str;
    }

}

}
