package io.swagger.client.model {

import io.swagger.common.ListWrapper;

    public class UserList implements ListWrapper {
        // This declaration below of _User_obj_class is to force flash compiler to include this class
        private var _user_obj_class: io.swagger.client.model.User = null;
        [XmlElements(name="user", type="io.swagger.client.model.User")]
        public var user: Array = new Array();

        public function getList(): Array{
            return user;
        }

}

}
