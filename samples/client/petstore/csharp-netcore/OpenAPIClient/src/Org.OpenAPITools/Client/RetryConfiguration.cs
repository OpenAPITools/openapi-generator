using Polly.Retry;
using RestSharp;

namespace Org.OpenAPITools.Client
{
    public class RetryConfiguration
    {
        public static RetryPolicy<IRestResponse> RetryPolicy { get; set; }
        public static AsyncRetryPolicy<IRestResponse> AsyncRetryPolicy { get; set; }
    }
}