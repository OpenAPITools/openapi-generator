using FastEndpoints;

using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Features;

public record DeleteOrderRequest([property: BindFrom("orderId")] string OrderId)
{
}

public record GetInventoryRequest()
{
}

public record GetOrderByIdRequest([property: BindFrom("orderId")] long OrderId)
{
}

public record PlaceOrderRequest([property: FromBody] Order order)
{
}




