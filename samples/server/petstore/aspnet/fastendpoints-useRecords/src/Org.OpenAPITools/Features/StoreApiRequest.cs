
using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Features;

public record DeleteOrderRequest([property: FastEndpoints.BindFrom("orderId")] string OrderId)
{
}

public record GetInventoryRequest()
{
}

public record GetOrderByIdRequest([property: FastEndpoints.BindFrom("orderId")] long OrderId)
{
}

public record PlaceOrderRequest([property: FastEndpoints.FromBody] Order order)
{
}




