using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace Com.Wordnik.Petstore.Model {
  public class Pet {
    /* Unique identifier for the Pet */
    public long id { get; set; }

    /* Category the pet is in */
    public Category category { get; set; }

    /* Friendly name of the pet */
    public string name { get; set; }

    /* Image URLs */
    public List<string> photoUrls { get; set; }

    /* Tags assigned to this pet */
    public List<Tag> tags { get; set; }

    /* pet status in the store */
    public string status { get; set; }

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class Pet {\n");
      sb.Append("  id: ").Append(id).Append("\n");
      sb.Append("  category: ").Append(category).Append("\n");
      sb.Append("  name: ").Append(name).Append("\n");
      sb.Append("  photoUrls: ").Append(photoUrls).Append("\n");
      sb.Append("  tags: ").Append(tags).Append("\n");
      sb.Append("  status: ").Append(status).Append("\n");
      sb.Append("}\n");
      return sb.ToString();
    }
  }
  }
