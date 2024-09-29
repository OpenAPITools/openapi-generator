using FastEndpoints;

using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Features;


public class DeleteOrderRequest
{
    /// <summary>
    /// ID of the order that needs to be deleted
    /// </summary>
    [BindFrom("orderId")]
    public string OrderId { get; set; }
}
public class GetInventoryRequest
{
}
public class GetOrderByIdRequest
{
    /// <summary>
    /// ID of pet that needs to be fetched
    /// </summary>
    [BindFrom("orderId")]
    public long OrderId { get; set; }
}
public class PlaceOrderRequest
{
    /// <summary>
    /// order placed for purchasing the pet
    /// </summary>
    [FromBody]
    public Order order { get; set; }
}


