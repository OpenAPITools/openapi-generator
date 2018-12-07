package org.openapitools.client.model;

import java.util.List;
import net.java.html.json.ComputedProperty;
import net.java.html.json.Function;
import net.java.html.json.Model;
import net.java.html.json.ModelOperation;
import net.java.html.json.OnPropertyChange;
import net.java.html.json.OnReceive;
import net.java.html.json.Property;

@Model(className = "Order", targetId = "", properties = {
    @Property(name = "id", type = long.class)
    , @Property(name = "petId", type = long.class)
    , @Property(name = "quantity", type = int.class)
    , @Property(name = "shipDate", type = long.class)
    , @Property(name = "status", type = String.class)
    , @Property(name = "complete", type = boolean.class)
  }
)
public class OrderVMD{

}
