package io.swagger/client/model {

import io.swagger/client/model.Category;
import io.swagger/client/model.Tag;
import java.util.List;

    [XmlRootNode(name="Pet")]
    public class Pet {
    

    

        
        [XmlElement(name="id")]
        
        public var id: Number = None;

    

    

        
        [XmlElement(name="category")]
        
        public var category: Category = None;

    

    

        
        [XmlElement(name="name")]
        
        public var name: String = None;

    

    

        
        
        public var photoUrls: Array[String] = None;

    

    

        
        
        public var tags: Array[Tag] = None;

    

    /* pet status in the store */
    

        
        [XmlElement(name="status")]
        
        public var status: String = None;

    

        public function toString(): String {
            var str: String = "Pet: ";
            
            str += " (id: " + id + ")";
            
            str += " (category: " + category + ")";
            
            str += " (name: " + name + ")";
            
            str += " (photoUrls: " + photoUrls + ")";
            
            str += " (tags: " + tags + ")";
            
            str += " (status: " + status + ")";
            
            return str;
        }


}
        

}
