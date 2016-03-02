using System;
using System.IO;
using System.Linq;
using Microsoft.AspNet.Builder;
using Microsoft.AspNet.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.PlatformAbstractions;
using Newtonsoft.Json.Serialization;
using Swashbuckle.SwaggerGen;
using Swashbuckle.SwaggerGen.XmlComments;

namespace IO.Swagger
{
    public class Startup
    {
        private readonly IHostingEnvironment _hostingEnv;
        private readonly IApplicationEnvironment _appEnv;

        public Startup(IHostingEnvironment env, IApplicationEnvironment appEnv)
        {
            _hostingEnv = env;
            _appEnv = appEnv;

            // Set up configuration sources.
            var builder = new ConfigurationBuilder()
                .AddJsonFile("appsettings.json")
                .AddEnvironmentVariables();
            Configuration = builder.Build();
        }

        public IConfigurationRoot Configuration { get; set; }

        // This method gets called by the runtime. Use this method to add services to the container.
        public void ConfigureServices(IServiceCollection services)
        {
            string xmlComments = string.Format(@"{0}{4}artifacts{4}{1}{4}{2}{3}{4}IO.Swagger.xml",
                   GetSolutionBasePath(),
                   _appEnv.Configuration,
                   _appEnv.RuntimeFramework.Identifier.ToLower(),
                   _appEnv.RuntimeFramework.Version.ToString().Replace(".", string.Empty),
                   Path.DirectorySeparatorChar);
                   
            // Add framework services.
            services.AddMvc()
                .AddJsonOptions(
                    opts => { opts.SerializerSettings.ContractResolver = new CamelCasePropertyNamesContractResolver(); });

            // Uncomment the following line to add Web API services which makes it easier to port Web API 2 controllers.
            // You will also need to add the Microsoft.AspNet.Mvc.WebApiCompatShim package to the 'dependencies' section of project.json.
            // services.AddWebApiConventions();

            services.AddSwaggerGen();
            services.ConfigureSwaggerDocument(options =>
            {
                options.SingleApiVersion(new Info
                {
                    Version = "v1",
                    Title = "IO.Swagger",
                    Description = "IO.Swagger (ASP.NET 5 Web API 2.x)"
                });
                
                options.OperationFilter(new ApplyXmlActionCommentsFixed(xmlComments));
            });

            services.ConfigureSwaggerSchema(options => { 
                options.DescribeAllEnumsAsStrings = true; 
                options.ModelFilter(new ApplyXmlTypeCommentsFixed(xmlComments));
            });
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IHostingEnvironment env, ILoggerFactory loggerFactory)
        {
            loggerFactory.MinimumLevel = LogLevel.Information;
            loggerFactory.AddConsole(Configuration.GetSection("Logging"));
            loggerFactory.AddDebug();

            app.UseIISPlatformHandler();

            app.UseDefaultFiles();
            app.UseStaticFiles();

            app.UseMvc();

            app.UseSwaggerGen();
            app.UseSwaggerUi();
        }

        // Taken from https://github.com/domaindrivendev/Ahoy/blob/master/test/WebSites/Basic/Startup.cs
        private string GetSolutionBasePath()
        {
            var dir = Directory.CreateDirectory(_appEnv.ApplicationBasePath);
            while (dir.Parent != null)
            {
                if (dir.GetDirectories("artifacts").Any())
                    return dir.FullName;

                dir = dir.Parent;
            }
            throw new InvalidOperationException("Failed to detect solution base path - artifacts not found. Did you run dnu pack --out artifacts?");
        }

        // Entry point for the application.
        public static void Main(string[] args) => WebApplication.Run<Startup>(args);
    }
    
        
    //  using Swashbuckle.SwaggerGen.XmlComments;
    public class ApplyXmlTypeCommentsFixed : ApplyXmlTypeComments
    {
        public ApplyXmlTypeCommentsFixed() : base("")
        {
            throw new NotImplementedException();
        }

        public ApplyXmlTypeCommentsFixed(string filePath): base(filePath)
        {

        }
    }

    public class ApplyXmlActionCommentsFixed : ApplyXmlActionComments
    {
        public ApplyXmlActionCommentsFixed() : base("")
        {
            throw new NotImplementedException();
        }

        public ApplyXmlActionCommentsFixed(string filePath): base(filePath)
        {

        }
    }
}