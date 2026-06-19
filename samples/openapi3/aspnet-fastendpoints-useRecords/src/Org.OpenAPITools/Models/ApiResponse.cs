namespace Org.OpenAPITools.Models;

/// <summary>
/// Describes the result of uploading an image resource
/// </summary>
public record ApiResponse() 
{
    public int Code {get; init; }
    public string Type {get; init; }
    public string Message {get; init; }
            }



