using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace Com.Wordnik.Petstore.Model {
  public class Tag {
    public long id { get; set; }

    public string name { get; set; }

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class Tag {\n");
      sb.Append("  id: ").Append(id).Append("\n");
      sb.Append("  name: ").Append(name).Append("\n");
      sb.Append("}\n");
      return sb.ToString();
    }
  }
  }
