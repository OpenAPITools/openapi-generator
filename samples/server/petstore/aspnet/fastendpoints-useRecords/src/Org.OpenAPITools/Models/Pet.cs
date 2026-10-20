namespace Org.OpenAPITools.Models;

/// <summary>
/// A pet for sale in the pet store
/// </summary>
public record Pet(string Name,List<string> PhotoUrls) 
{
    public long Id {get; init; }
    public Category Category {get; init; }
    public List<Tag> Tags {get; init; }
    public StatusEnum Status {get; init; }
                        
    /// <summary>
    /// pet status in the store
    /// </summary>
    /// <value>pet status in the store</value>
    public enum StatusEnum
    {
        
        /// <summary>
        /// Enum AvailableEnum for available
        /// </summary>
        AvailableEnum = 1,
        
        /// <summary>
        /// Enum PendingEnum for pending
        /// </summary>
        PendingEnum = 2,
        
        /// <summary>
        /// Enum SoldEnum for sold
        /// </summary>
        SoldEnum = 3
    }

}



