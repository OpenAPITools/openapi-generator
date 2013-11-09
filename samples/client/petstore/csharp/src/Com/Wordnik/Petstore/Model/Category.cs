using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace Com.Wordnik.Petstore.Model {
  public class Category {
    /* Category unique identifier */
    public long id { get; set; }

    /* Name of the category */
    public string name { get; set; }

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class Category {\n");
      sb.Append("  id: ").Append(id).Append("\n");
      sb.Append("  name: ").Append(name).Append("\n");
      sb.Append("}\n");
      return sb.ToString();
    }
  }
  }
