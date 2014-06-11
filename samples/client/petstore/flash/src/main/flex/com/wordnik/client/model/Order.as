package com.wordnik.client.model {

[XmlRootNode(name="Order")]
    public class Order {
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    [XmlElement(name="petId")]
        public var petId: Number = 0.0;

    [XmlElement(name="quantity")]
        public var quantity: Number = 0.0;

    /* Order Status */
    [XmlElement(name="status")]
        public var status: String = null;

    [XmlElement(name="shipDate")]
        public var shipDate: Date = null;

    public function toString(): String {
            var str: String = "Order: ";
            str += " (id: " + id + ")";
            str += " (petId: " + petId + ")";
            str += " (quantity: " + quantity + ")";
            str += " (status: " + status + ")";
            str += " (shipDate: " + shipDate + ")";
            return str;
        }


}
}

