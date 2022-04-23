using Microsoft.Extensions.DependencyInjection;
using System;

namespace CocApi.Rest.Client
{
    public interface IApiFactory
    {
        IResult Create<IResult>() where IResult : IApi;
    }

    public class ApiFactory : IApiFactory
    {
        public IServiceProvider Services { get; }


        public ApiFactory(IServiceProvider services)
        {
            Services = services;
        }

        public IResult Create<IResult>() where IResult : IApi
        {
            return Services.GetRequiredService<IResult>();
        }
    }
}
