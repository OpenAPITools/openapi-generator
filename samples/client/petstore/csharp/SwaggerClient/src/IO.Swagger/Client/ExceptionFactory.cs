using System;
using RestSharp;

namespace IO.Swagger.Client
{
    public delegate Exception ExceptionFactory(string methodName, IRestResponse response);
}
