namespace Org.OpenAPITools.Models;


/// <summary>
/// A pet for sale in the pet store
/// </summary>
public class Pet 
{
    public long Id { get; set; }
    public Category Category { get; set; }
    public string Name { get; set; }
    public List<string> PhotoUrls { get; set; }
    public List<Tag> Tags { get; set; }
    
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

    public StatusEnum Status { get; set; }
}


