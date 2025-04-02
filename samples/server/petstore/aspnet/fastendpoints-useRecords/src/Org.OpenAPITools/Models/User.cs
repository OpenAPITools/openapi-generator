namespace Org.OpenAPITools.Models;

/// <summary>
/// A User who is purchasing from the pet store
/// </summary>
public record User() 
{
    public long Id {get; init; }
    public string Username {get; init; }
    public string FirstName {get; init; }
    public string LastName {get; init; }
    public string Email {get; init; }
    public string Password {get; init; }
    public string Phone {get; init; }
    public int UserStatus {get; init; }
                                }



