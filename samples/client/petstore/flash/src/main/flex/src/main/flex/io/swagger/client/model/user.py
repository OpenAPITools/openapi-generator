package src/main/flex/io.swagger/client/model {


    [XmlRootNode(name="User")]
    public class User {
    

    

        
        [XmlElement(name="id")]
        
        public var id: Number = None;

    

    

        
        [XmlElement(name="username")]
        
        public var username: String = None;

    

    

        
        [XmlElement(name="first_name")]
        
        public var first_name: String = None;

    

    

        
        [XmlElement(name="last_name")]
        
        public var last_name: String = None;

    

    

        
        [XmlElement(name="email")]
        
        public var email: String = None;

    

    

        
        [XmlElement(name="password")]
        
        public var password: String = None;

    

    

        
        [XmlElement(name="phone")]
        
        public var phone: String = None;

    

    /* User Status */
    

        
        [XmlElement(name="user_status")]
        
        public var user_status: Number = None;

    

        public function toString(): String {
            var str: String = "User: ";
            
            str += " (id: " + id + ")";
            
            str += " (username: " + username + ")";
            
            str += " (first_name: " + first_name + ")";
            
            str += " (last_name: " + last_name + ")";
            
            str += " (email: " + email + ")";
            
            str += " (password: " + password + ")";
            
            str += " (phone: " + phone + ")";
            
            str += " (user_status: " + user_status + ")";
            
            return str;
        }


}
        

}
