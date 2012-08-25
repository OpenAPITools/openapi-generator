package com.wordnik.client.model {

[XmlRootNode(name="Order")]
    public class Order {
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    [XmlElement(name="petId")]
        public var petId: Number = 0.0;

    /* Order Status */
    [XmlElement(name="status")]
        public var status: String = null;

    [XmlElement(name="quantity")]
        public var quantity: Number = 0.0;

    [XmlElement(name="shipDate")]
        public var shipDate: Date = null;

    public function toString(): String {
            var str: String = "Order: ";
            str += " (id: " + id + ")";
            str += " (petId: " + petId + ")";
            str += " (status: " + status + ")";
            str += " (quantity: " + quantity + ")";
            str += " (shipDate: " + shipDate + ")";
            return str;
        }


}
}

