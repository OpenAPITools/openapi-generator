package com.wordnik.client.model {

[XmlRootNode(name="User")]
    public class User {
    /* Unique identifier for the user */
    [XmlElement(name="id")]
        public var id: Number = 0.0;

    /* Unique username */
    [XmlElement(name="username")]
        public var username: String = null;

    /* First name of the user */
    [XmlElement(name="firstName")]
        public var firstName: String = null;

    /* Last name of the user */
    [XmlElement(name="lastName")]
        public var lastName: String = null;

    /* Email address of the user */
    [XmlElement(name="email")]
        public var email: String = null;

    /* Password name of the user */
    [XmlElement(name="password")]
        public var password: String = null;

    /* Phone number of the user */
    [XmlElement(name="phone")]
        public var phone: String = null;

    /* User Status */
    [XmlElement(name="userStatus")]
        public var userStatus: Number = 0.0;

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

