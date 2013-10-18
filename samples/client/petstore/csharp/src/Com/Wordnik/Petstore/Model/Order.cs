using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace Com.Wordnik.Petstore.Model {
  public class Order {
    /* Unique identifier for the order */
    public long id { get; set; }

    /* ID of pet being ordered */
    public long petId { get; set; }

    /* Number of pets ordered */
    public int quantity { get; set; }

    /* Status of the order */
    public string status { get; set; }

    /* Date shipped, only if it has been */
    public DateTime shipDate { get; set; }

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class Order {\n");
      sb.Append("  id: ").Append(id).Append("\n");
      sb.Append("  petId: ").Append(petId).Append("\n");
      sb.Append("  quantity: ").Append(quantity).Append("\n");
      sb.Append("  status: ").Append(status).Append("\n");
      sb.Append("  shipDate: ").Append(shipDate).Append("\n");
      sb.Append("}\n");
      return sb.ToString();
    }
  }
  }
