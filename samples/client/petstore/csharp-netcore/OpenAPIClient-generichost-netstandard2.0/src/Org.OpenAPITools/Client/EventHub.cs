using Microsoft.Extensions.Logging;
using System;

namespace Org.OpenAPITools.Client
{
    public interface IEventHub 
    {
        void OnApiResponded(object sender, ApiResponseEventArgs args);
    }

    public class EventHub : IEventHub
    {
        /// <summary>
        /// An event to track the health of the server.
        /// If you store these event args, be sure to purge old event args to prevent a memory leak.
        /// </summary>
        public event ClientUtils.EventHandler<ApiResponseEventArgs> ApiResponded;

        public ILogger<EventHub> Logger { get; }

        public EventHub(ILogger<EventHub> logger)
        {
            Logger = logger;
        }

        public void OnApiResponded(object sender, ApiResponseEventArgs args)
        {
            if (ApiResponded != null)
            {
                try
                {
                    ApiResponded.Invoke(sender, args);
                }
                catch (Exception e)
                {
                    Logger.LogError(e, "An error occured while invoking ApiResponded.");
                }
            }
        }
    }
}
