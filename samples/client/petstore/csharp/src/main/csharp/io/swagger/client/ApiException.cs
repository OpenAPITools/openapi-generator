using System;

namespace IO.Swagger.Client {
  /// <summary>
  /// API Exception
  /// </summary>
  public class ApiException : Exception {
    /// <summary>
    /// Gets or sets the error code (HTTP status code)
    /// </summary>
    /// <value>The error code (HTTP status code).</value>
    public int ErrorCode { get; set; }

    /// <summary>
    /// Initializes a new instance of the <see cref="ApiException"/> class.
    /// </summary>
    /// <param name="basePath">The base path.</param>
    public ApiException() {}

    /// <summary>
    /// Initializes a new instance of the <see cref="ApiException"/> class.
    /// </summary>
    /// <param name="errorCode">HTTP status code.</param>
    /// <param name="message">Error message.</param>
    public ApiException(int errorCode, string message) : base(message) {
      this.ErrorCode = errorCode;
    }

  }

}
