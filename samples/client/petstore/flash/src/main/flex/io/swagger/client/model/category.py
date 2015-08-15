package io.swagger/client/model {


    [XmlRootNode(name="Category")]
    public class Category {
    

    

        
        [XmlElement(name="id")]
        
        public var id: Number = None;

    

    

        
        [XmlElement(name="name")]
        
        public var name: String = None;

    

        public function toString(): String {
            var str: String = "Category: ";
            
            str += " (id: " + id + ")";
            
            str += " (name: " + name + ")";
            
            return str;
        }


}
        

}
