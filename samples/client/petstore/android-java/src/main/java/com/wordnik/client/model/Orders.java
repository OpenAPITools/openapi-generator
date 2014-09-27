package com.wordnik.client.model;

import java.util.Map;
import java.util.HashMap;
import com.wordnik.client.model.Orders;

import com.wordnik.swagger.annotations.*;


@ApiModel(description = "")
public class Orders extends HashMap<String, Orders> { 
  
  

  @Override
  public String toString()  {
    StringBuilder sb = new StringBuilder();
    sb.append("class Orders {\n");
    sb.append("  " + super.toString()).append("\n");
    sb.append("}\n");
    return sb.toString();
  }
}
