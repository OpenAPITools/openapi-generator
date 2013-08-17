using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace Com.Wordnik.Petstore.Model {
  public class User {
    public long id { get; set; }

    public string firstName { get; set; }

    public string username { get; set; }

    public string lastName { get; set; }

    public string email { get; set; }

    public string password { get; set; }

    public string phone { get; set; }

    /* User Status */
    public int userStatus { get; set; }

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class User {\n");
      sb.Append("  id: ").Append(id).Append("\n");
      sb.Append("  firstName: ").Append(firstName).Append("\n");
      sb.Append("  username: ").Append(username).Append("\n");
      sb.Append("  lastName: ").Append(lastName).Append("\n");
      sb.Append("  email: ").Append(email).Append("\n");
      sb.Append("  password: ").Append(password).Append("\n");
      sb.Append("  phone: ").Append(phone).Append("\n");
      sb.Append("  userStatus: ").Append(userStatus).Append("\n");
      sb.Append("}\n");
      return sb.ToString();
    }
  }
  }
