using Microsoft.Extensions.Logging;
using System;

namespace Org.OpenAPITools.Client
{
    /// <summary>
    /// The IEventHub
    /// </summary>
    public interface IEventHub 
    {
        /// <summary>
        /// An event that fires when the server has responded successfully.
        /// </summary>
        void OnApiResponded(object sender, ApiResponseEventArgs args);
    }

    /// <summary>
    /// The EventHub class
    /// </summary>
    public class EventHub : IEventHub
    {
        /// <summary>
        /// An event to track the health of the server.
        /// If you store these event args, be sure to purge old event args to prevent a memory leak.
        /// This class is registered as a singleton.
        /// </summary>
        public event ClientUtils.EventHandler<ApiResponseEventArgs>? ApiResponded;

        /// <summary>
        /// The logger
        /// </summary>
        public ILogger<EventHub> Logger { get; }

        /// <summary>
        /// The EventHub class
        /// </summary>
        public EventHub(ILogger<EventHub> logger)
        {
            Logger = logger;
        }

        /// <summary>
        /// An event that fires when the server has responded successfully.
        /// </summary>
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
