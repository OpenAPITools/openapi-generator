namespace Org.OpenAPITools.Models;

/// <summary>
/// An order for a pets from the pet store
/// </summary>
public record Order() 
{
    public long Id {get; init; }
    public long PetId {get; init; }
    public int Quantity {get; init; }
    public DateTime ShipDate {get; init; }
    public StatusEnum Status {get; init; }
    public bool Complete {get; init; } = false;
                    
    /// <summary>
    /// Order Status
    /// </summary>
    /// <value>Order Status</value>
    public enum StatusEnum
    {
        
        /// <summary>
        /// Enum PlacedEnum for placed
        /// </summary>
        PlacedEnum = 1,
        
        /// <summary>
        /// Enum ApprovedEnum for approved
        /// </summary>
        ApprovedEnum = 2,
        
        /// <summary>
        /// Enum DeliveredEnum for delivered
        /// </summary>
        DeliveredEnum = 3
    }

    }



