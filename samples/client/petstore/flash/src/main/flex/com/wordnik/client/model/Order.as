package com.wordnik.client.model {

[XmlRootNode(name="Order")]
    public class Order {
    /* Unique identifier for the order */
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    /* ID of pet being ordered */
    [XmlElement(name="petId")]
        public var petId: Number = 0.0;

    /* Number of pets ordered */
    [XmlElement(name="quantity")]
        public var quantity: Number = 0.0;

    /* Status of the order */
    [XmlElement(name="status")]
        public var status: String = null;

    /* Date shipped, only if it has been */
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

