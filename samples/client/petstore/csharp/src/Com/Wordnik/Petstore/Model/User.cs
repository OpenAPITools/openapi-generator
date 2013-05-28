using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace Com.Wordnik.Petstore.Model {
  public class User {
    public long Id { get; set; }

    public string Lastname { get; set; }

    public string Phone { get; set; }

    public string Username { get; set; }

    public string Email { get; set; }

    /* User Status */
    public int Userstatus { get; set; }

    public string Firstname { get; set; }

    public string Password { get; set; }

    public override string ToString()  {
      var sb = new StringBuilder();
      sb.Append("class User {\n");
      sb.Append("  Id: ").Append(Id).Append("\n");
      sb.Append("  Lastname: ").Append(Lastname).Append("\n");
      sb.Append("  Phone: ").Append(Phone).Append("\n");
      sb.Append("  Username: ").Append(Username).Append("\n");
      sb.Append("  Email: ").Append(Email).Append("\n");
      sb.Append("  Userstatus: ").Append(Userstatus).Append("\n");
      sb.Append("  Firstname: ").Append(Firstname).Append("\n");
      sb.Append("  Password: ").Append(Password).Append("\n");
      sb.Append("}\n");
      return sb.ToString();
    }
  }
  }
