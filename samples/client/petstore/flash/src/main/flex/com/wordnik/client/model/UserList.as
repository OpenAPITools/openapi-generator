package com.wordnik.client.model {

import com.wordnik.swagger.common.ListWrapper;
public class UserList implements ListWrapper {
        // This declaration below of _User_obj_class is to force flash compiler to include this class
        private var _user_obj_class: com.wordnik.client.model.User = null;
        [XmlElements(name="user", type="com.wordnik.client.model.User")]
        public var user: Array = new Array();

        public function getList(): Array{
            return user;
        }

}
}

