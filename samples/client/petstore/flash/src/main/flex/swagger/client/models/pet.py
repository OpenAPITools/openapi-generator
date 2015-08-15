package swagger.client/models {

import swagger.client/models.Category;
import swagger.client/models.Tag;
import java.util.List;

    [XmlRootNode(name="Pet")]
    public class Pet {
    

    

        
        [XmlElement(name="id")]
        
        public var id: Number = None;

    

    

        
        [XmlElement(name="category")]
        
        public var category: Category = None;

    

    

        
        [XmlElement(name="name")]
        
        public var name: String = None;

    

    

        
        
        public var photo_urls: Array[String] = None;

    

    

        
        
        public var tags: Array[Tag] = None;

    

    /* pet status in the store */
    

        
        [XmlElement(name="status")]
        
        public var status: String = None;

    

        public function toString(): String {
            var str: String = "Pet: ";
            
            str += " (id: " + id + ")";
            
            str += " (category: " + category + ")";
            
            str += " (name: " + name + ")";
            
            str += " (photo_urls: " + photo_urls + ")";
            
            str += " (tags: " + tags + ")";
            
            str += " (status: " + status + ")";
            
            return str;
        }


}
        

}
