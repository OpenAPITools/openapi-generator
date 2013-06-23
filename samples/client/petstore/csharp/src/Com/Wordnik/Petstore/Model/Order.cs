using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace Com.Wordnik.Petstore.Model {
  public class Order {
    public long Id { get; set; }

    public long Petid { get; set; }

    /* Order Status */
    public string Status { get; set; }

    public int Quantity { get; set; }

    public DateTime Shipdate { get; set; }

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class Order {\n");
      sb.Append("  Id: ").Append(Id).Append("\n");
      sb.Append("  Petid: ").Append(Petid).Append("\n");
      sb.Append("  Status: ").Append(Status).Append("\n");
      sb.Append("  Quantity: ").Append(Quantity).Append("\n");
      sb.Append("  Shipdate: ").Append(Shipdate).Append("\n");
      sb.Append("}\n");
      return sb.ToString();
    }
  }
  }
