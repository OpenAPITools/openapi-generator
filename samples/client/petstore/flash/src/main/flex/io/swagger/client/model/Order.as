package io.swagger/client/model {


    [XmlRootNode(name="Order")]
    public class Order {
    

    

        
        [XmlElement(name="id")]
        
        public var id: Number = None;

    

    

        
        [XmlElement(name="petId")]
        
        public var petId: Number = None;

    

    

        
        [XmlElement(name="quantity")]
        
        public var quantity: Number = None;

    

    

        
        [XmlElement(name="shipDate")]
        
        public var shipDate: Date = None;

    

    /* Order Status */
    

        
        [XmlElement(name="status")]
        
        public var status: String = None;

    

    

        
        [XmlElement(name="complete")]
        
        public var complete: Boolean = None;

    

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
