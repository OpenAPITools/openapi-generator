namespace Org.OpenAPITools.Models;


/// <summary>
/// An order for a pets from the pet store
/// </summary>
public class Order 
{
    public long Id { get; set; }
    public long PetId { get; set; }
    public int Quantity { get; set; }
    public DateTime ShipDate { get; set; }
    
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

    public StatusEnum Status { get; set; }
    public bool Complete { get; set; } = false;
}


