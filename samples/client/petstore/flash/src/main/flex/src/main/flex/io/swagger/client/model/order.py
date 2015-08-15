package src/main/flex/io.swagger/client/model {


    [XmlRootNode(name="Order")]
    public class Order {
    

    

        
        [XmlElement(name="id")]
        
        public var id: Number = None;

    

    

        
        [XmlElement(name="pet_id")]
        
        public var pet_id: Number = None;

    

    

        
        [XmlElement(name="quantity")]
        
        public var quantity: Number = None;

    

    

        
        [XmlElement(name="ship_date")]
        
        public var ship_date: Date = None;

    

    /* Order Status */
    

        
        [XmlElement(name="status")]
        
        public var status: String = None;

    

    

        
        [XmlElement(name="complete")]
        
        public var complete: Boolean = None;

    

        public function toString(): String {
            var str: String = "Order: ";
            
            str += " (id: " + id + ")";
            
            str += " (pet_id: " + pet_id + ")";
            
            str += " (quantity: " + quantity + ")";
            
            str += " (ship_date: " + ship_date + ")";
            
            str += " (status: " + status + ")";
            
            str += " (complete: " + complete + ")";
            
            return str;
        }


}
        

}
