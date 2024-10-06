using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Features;



/// <summary>
/// Delete purchase order by ID
/// </summary>

public class DeleteOrderEndpoint : FastEndpoints.Endpoint<DeleteOrderRequest>
{
    public override void Configure()
    {
        Delete("/v2/store/order/{orderId}");
        
        ResponseCache(60);
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("store");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemFE(x, 400);
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemFE(x, 404);
        });

        Summary(s => {
            s.Summary = "Delete purchase order by ID";
            s.RequestParam(r => r.OrderId, "ID of the order that needs to be deleted");
            s.Responses[400] = "Invalid ID supplied";
            s.Responses[404] = "Order not found";
        });
    }

    public override async Task HandleAsync(DeleteOrderRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// Returns pet inventories by status
/// </summary>

public class GetInventoryEndpoint : FastEndpoints.EndpointWithoutRequest<Dictionary<string, int>>
{
    public override void Configure()
    {
        Get("/v2/store/inventory");
        
        ResponseCache(60);
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("store");
        });

        Summary(s => {
            s.Summary = "Returns pet inventories by status";
            s.Responses[200] = "successful operation";
        });
    }

    public override async Task HandleAsync(CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// Find purchase order by ID
/// </summary>

public class GetOrderByIdEndpoint : FastEndpoints.Endpoint<GetOrderByIdRequest, Order>
{
    public override void Configure()
    {
        Get("/v2/store/order/{orderId}");
        
        ResponseCache(60);
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("store");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemFE(x, 400);
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemFE(x, 404);
        });

        Summary(s => {
            s.Summary = "Find purchase order by ID";
            s.RequestParam(r => r.OrderId, "ID of pet that needs to be fetched");
            s.Responses[200] = "successful operation";
            s.Responses[400] = "Invalid ID supplied";
            s.Responses[404] = "Order not found";
        });
    }

    public override async Task HandleAsync(GetOrderByIdRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// Place an order for a pet
/// </summary>

public class PlaceOrderEndpoint : FastEndpoints.Endpoint<PlaceOrderRequest, Order>
{
    public override void Configure()
    {
        Post("/v2/store/order");
        
        ResponseCache(60);
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("store");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemFE(x, 400);
        });

        Summary(s => {
            s.Summary = "Place an order for a pet";
            s.RequestParam(r => r.order, "order placed for purchasing the pet");
            s.Responses[200] = "successful operation";
            s.Responses[400] = "Invalid Order";
        });
    }

    public override async Task HandleAsync(PlaceOrderRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}

