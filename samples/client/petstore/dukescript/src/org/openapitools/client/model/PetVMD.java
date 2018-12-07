package org.openapitools.client.model;

import java.util.List;
import net.java.html.json.ComputedProperty;
import net.java.html.json.Function;
import net.java.html.json.Model;
import net.java.html.json.ModelOperation;
import net.java.html.json.OnPropertyChange;
import net.java.html.json.OnReceive;
import net.java.html.json.Property;

@Model(className = "Pet", targetId = "", properties = {
    @Property(name = "id", type = long.class)
    , @Property(name = "category", type = Category.class)
    , @Property(name = "name", type = String.class)
    , @Property(name = "photoUrls", type = String.class, array=true)
    , @Property(name = "tags", type = Tag.class, array=true)
    , @Property(name = "status", type = String.class)
  }
)
public class PetVMD{

}
