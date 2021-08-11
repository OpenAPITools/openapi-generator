using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Mvc.Filters;

namespace Org.OpenAPITools.Authentication
{
    /// <summary>
    /// A requirement that an ApiKey must be present.
    /// </summary>
    public class ApiKeyRequirement : IAuthorizationRequirement
    {
        /// <summary>
        /// Get the list of api keys
        /// </summary>
        public IReadOnlyList<string> ApiKeys { get; }

        /// <summary>
        /// Get the policy name,
        /// </summary>
        public string PolicyName { get; }

        /// <summary>
        /// Create a new instance of the <see cref="ApiKeyRequirement"/> class.
        /// </summary>
        /// <param name="apiKeys"></param>
        /// <param name="policyName"></param>
        public ApiKeyRequirement(IEnumerable<string> apiKeys, string policyName)
        {
            ApiKeys = apiKeys?.ToList() ?? new List<string>();
            PolicyName = policyName;
        }
    }

    /// <summary>
    /// Enforce that an api key is present.
    /// </summary>
    public class ApiKeyRequirementHandler : AuthorizationHandler<ApiKeyRequirement>
    {
        /// <copydoc cref="AuthorizationHandler{T}.HandleRequirementAsync" />
        protected override Task HandleRequirementAsync(AuthorizationHandlerContext context, ApiKeyRequirement requirement)
        {
            SucceedRequirementIfApiKeyPresentAndValid(context, requirement);
            return Task.CompletedTask;
        }

        private void SucceedRequirementIfApiKeyPresentAndValid(AuthorizationHandlerContext context, ApiKeyRequirement requirement)
        {

            if (context.Resource is AuthorizationFilterContext authorizationFilterContext)
            {
                var apiKey = authorizationFilterContext.HttpContext.Request.Headers["api_key"].FirstOrDefault();
                if (requirement.PolicyName == "api_key" && apiKey != null && requirement.ApiKeys.Any(requiredApiKey => apiKey == requiredApiKey))
                {
                    context.Succeed(requirement);
                }
            }

        }
    }
}

