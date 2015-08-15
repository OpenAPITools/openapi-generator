package swagger.client/models {


    [XmlRootNode(name="Tag")]
    public class Tag {
    

    

        
        [XmlElement(name="id")]
        
        public var id: Number = None;

    

    

        
        [XmlElement(name="name")]
        
        public var name: String = None;

    

        public function toString(): String {
            var str: String = "Tag: ";
            
            str += " (id: " + id + ")";
            
            str += " (name: " + name + ")";
            
            return str;
        }


}
        

}
