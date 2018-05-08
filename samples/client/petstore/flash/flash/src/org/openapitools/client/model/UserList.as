package org.openapitools.client.model {

import org.openapitools.common.ListWrapper;

    public class UserList implements ListWrapper {
        // This declaration below of _User_obj_class is to force flash compiler to include this class
        private var _user_obj_class: org.openapitools.client.model.User = null;
        [XmlElements(name="user", type="org.openapitools.client.model.User")]
        public var user: Array = new Array();

        public function getList(): Array{
            return user;
        }

}

}
