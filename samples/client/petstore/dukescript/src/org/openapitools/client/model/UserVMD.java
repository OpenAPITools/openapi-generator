package org.openapitools.client.model;

import java.util.List;
import net.java.html.json.ComputedProperty;
import net.java.html.json.Function;
import net.java.html.json.Model;
import net.java.html.json.ModelOperation;
import net.java.html.json.OnPropertyChange;
import net.java.html.json.OnReceive;
import net.java.html.json.Property;

@Model(className = "User", targetId = "", properties = {
    @Property(name = "id", type = long.class)
    , @Property(name = "username", type = String.class)
    , @Property(name = "firstName", type = String.class)
    , @Property(name = "lastName", type = String.class)
    , @Property(name = "email", type = String.class)
    , @Property(name = "password", type = String.class)
    , @Property(name = "phone", type = String.class)
    , @Property(name = "userStatus", type = int.class)
  }
)
public class UserVMD{

}
