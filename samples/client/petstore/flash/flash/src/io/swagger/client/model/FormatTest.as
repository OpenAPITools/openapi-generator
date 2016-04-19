package io.swagger.client.model {

import io.swagger.client.model.ByteArray;

    [XmlRootNode(name="FormatTest")]
    public class FormatTest {
                [XmlElement(name="integer")]
        public var integer: Number = NaN;
                [XmlElement(name="int32")]
        public var int32: Number = 0;
                [XmlElement(name="int64")]
        public var int64: Number = 0;
                [XmlElement(name="number")]
        public var number: Number = NaN;
                [XmlElement(name="float")]
        public var float: Number = 0.0;
                [XmlElement(name="double")]
        public var double: Number = 0.0;
                [XmlElement(name="string")]
        public var string: String = null;
                [XmlElement(name="byte")]
        public var byte: ByteArray = NaN;
                [XmlElement(name="binary")]
        public var binary: String = NaN;
                [XmlElement(name="date")]
        public var date: Date = null;
                [XmlElement(name="dateTime")]
        public var dateTime: Date = null;
                [XmlElement(name="password")]
        public var password: String = null;

    public function toString(): String {
        var str: String = "FormatTest: ";
        str += " (integer: " + integer + ")";
        str += " (int32: " + int32 + ")";
        str += " (int64: " + int64 + ")";
        str += " (number: " + number + ")";
        str += " (float: " + float + ")";
        str += " (double: " + double + ")";
        str += " (string: " + string + ")";
        str += " (byte: " + byte + ")";
        str += " (binary: " + binary + ")";
        str += " (date: " + date + ")";
        str += " (dateTime: " + dateTime + ")";
        str += " (password: " + password + ")";
        return str;
    }

}

}
