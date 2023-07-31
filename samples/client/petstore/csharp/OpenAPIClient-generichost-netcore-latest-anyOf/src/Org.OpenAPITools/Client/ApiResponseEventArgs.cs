using System;

namespace Org.OpenAPITools.Client
{
    /// <summary>
    /// Useful for tracking server health
    /// </summary>
    public class ApiResponseEventArgs<T> : EventArgs
    {
        /// <summary>
        /// The ApiResponse
        /// </summary>
        public ApiResponse<T> ApiResponse { get; }

        /// <summary>
        /// The ApiResponseEventArgs
        /// </summary>
        /// <param name="apiResponse"></param>
        public ApiResponseEventArgs(ApiResponse<T> apiResponse)
        {
            ApiResponse = apiResponse;
        }
    }
}
