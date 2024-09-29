using FastEndpoints;

using Org.OpenAPITools.Models;
using Order = Org.OpenAPITools.Models.Order;

namespace Org.OpenAPITools.Features;

public record DeleteOrderRequest(string OrderId)
{
}

public record GetInventoryRequest()
{
}

public record GetOrderByIdRequest(long OrderId)
{
}

public record PlaceOrderRequest([property: FromBody] Order order)
{
}




