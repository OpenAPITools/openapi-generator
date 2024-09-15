namespace Org.OpenAPITools.Models;


/// <summary>
/// A User who is purchasing from the pet store
/// </summary>
public class User 
{
    public long Id { get; set; }
    public string Username { get; set; }
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public string Email { get; set; }
    public string Password { get; set; }
    public string Phone { get; set; }
    public int UserStatus { get; set; }
}


