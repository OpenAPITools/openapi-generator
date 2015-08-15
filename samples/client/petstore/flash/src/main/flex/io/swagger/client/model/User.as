package io.swagger/client/model {


    [XmlRootNode(name="User")]
    public class User {
    

    

        
        [XmlElement(name="id")]
        
        public var id: Number = None;

    

    

        
        [XmlElement(name="username")]
        
        public var username: String = None;

    

    

        
        [XmlElement(name="firstName")]
        
        public var firstName: String = None;

    

    

        
        [XmlElement(name="lastName")]
        
        public var lastName: String = None;

    

    

        
        [XmlElement(name="email")]
        
        public var email: String = None;

    

    

        
        [XmlElement(name="password")]
        
        public var password: String = None;

    

    

        
        [XmlElement(name="phone")]
        
        public var phone: String = None;

    

    /* User Status */
    

        
        [XmlElement(name="userStatus")]
        
        public var userStatus: Number = None;

    

        public function toString(): String {
            var str: String = "User: ";
            
            str += " (id: " + id + ")";
            
            str += " (username: " + username + ")";
            
            str += " (firstName: " + firstName + ")";
            
            str += " (lastName: " + lastName + ")";
            
            str += " (email: " + email + ")";
            
            str += " (password: " + password + ")";
            
            str += " (phone: " + phone + ")";
            
            str += " (userStatus: " + userStatus + ")";
            
            return str;
        }


}
        

}
