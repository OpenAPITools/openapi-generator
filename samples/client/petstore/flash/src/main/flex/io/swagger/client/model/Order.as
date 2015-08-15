package io.swagger.client.model {


    [XmlRootNode(name="Order")]
    public class Order {
        
        
        
        [XmlElement(name="id")]
        
        public var id: Number = null;
    
        
        
        [XmlElement(name="petId")]
        
        public var petId: Number = null;
    
        
        
        [XmlElement(name="quantity")]
        
        public var quantity: Number = null;
    
        
        
        [XmlElement(name="shipDate")]
        
        public var shipDate: Date = null;
    
        /* Order Status */
        
        
        [XmlElement(name="status")]
        
        public var status: String = null;
    
        
        
        [XmlElement(name="complete")]
        
        public var complete: Boolean = null;
    

    public function toString(): String {
        var str: String = "Order: ";
        
        str += " (id: " + id + ")";
        
        str += " (petId: " + petId + ")";
        
        str += " (quantity: " + quantity + ")";
        
        str += " (shipDate: " + shipDate + ")";
        
        str += " (status: " + status + ")";
        
        str += " (complete: " + complete + ")";
        
        return str;
    }

}

}
