using System;
using Nancy;
using Nancy.ModelBinding;
using System.Collections.Generic;
using Sharpility.Base;
using IO.Swagger.v2.Models;
using IO.Swagger.v2.Utils;
using NodaTime;
using System.Threading.Tasks;

namespace IO.Swagger.v2.Modules
{ 

    /// <summary>
    /// Module processing requests of Store domain.
    /// </summary>
    public sealed class StoreModule : NancyModule
    {
        /// <summary>
        /// Sets up HTTP methods mappings.
        /// </summary>
        /// <param name="service">Service handling requests</param>
        public StoreModule(StoreService service) : base("/v2")
        { 
            Delete["/store/order/{orderId}", true] = async (parameters, ct) =>
            {
                var orderId = Parameters.ValueOf<string>(parameters, Context.Request, "orderId", ParameterType.Path);
                Preconditions.IsNotNull(orderId, "Required parameter: 'orderId' is missing at 'DeleteOrder'");
                
                await service.DeleteOrder(Context, orderId);
                return new Response { ContentType = "application/xml"};
            };

            Get["/store/inventory", true] = async (parameters, ct) =>
            {
                
                return await service.GetInventory(Context);
            };

            Get["/store/order/{orderId}", true] = async (parameters, ct) =>
            {
                var orderId = Parameters.ValueOf<long?>(parameters, Context.Request, "orderId", ParameterType.Path);
                Preconditions.IsNotNull(orderId, "Required parameter: 'orderId' is missing at 'GetOrderById'");
                
                return await service.GetOrderById(Context, orderId);
            };

            Post["/store/order", true] = async (parameters, ct) =>
            {
                var body = this.Bind<Order>();
                Preconditions.IsNotNull(body, "Required parameter: 'body' is missing at 'PlaceOrder'");
                
                return await service.PlaceOrder(Context, body);
            };
        }
    }

    /// <summary>
    /// Service handling Store requests.
    /// </summary>
    public interface StoreService
    {
        /// <summary>
        /// For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="orderId">ID of the order that needs to be deleted</param>
        /// <returns></returns>
        Task DeleteOrder(NancyContext context, string orderId);

        /// <summary>
        /// Returns a map of status codes to quantities
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <returns>Dictionary&lt;string, int?&gt;</returns>
        Task<Dictionary<string, int?>> GetInventory(NancyContext context);

        /// <summary>
        /// For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="orderId">ID of pet that needs to be fetched</param>
        /// <returns>Order</returns>
        Task<Order> GetOrderById(NancyContext context, long? orderId);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="context">Context of request</param>
        /// <param name="body">order placed for purchasing the pet</param>
        /// <returns>Order</returns>
        Task<Order> PlaceOrder(NancyContext context, Order body);
    }

    /// <summary>
    /// Abstraction of StoreService.
    /// </summary>
    public abstract class AbstractStoreService: StoreService
    {
        public virtual Task DeleteOrder(NancyContext context, string orderId)
        {
            return DeleteOrder(orderId);
        }

        public virtual Task<Dictionary<string, int?>> GetInventory(NancyContext context)
        {
            return GetInventory();
        }

        public virtual Task<Order> GetOrderById(NancyContext context, long? orderId)
        {
            return GetOrderById(orderId);
        }

        public virtual Task<Order> PlaceOrder(NancyContext context, Order body)
        {
            return PlaceOrder(body);
        }

        protected abstract Task DeleteOrder(string orderId);

        protected abstract Task<Dictionary<string, int?>> GetInventory();

        protected abstract Task<Order> GetOrderById(long? orderId);

        protected abstract Task<Order> PlaceOrder(Order body);
    }

}
