using System.ComponentModel.DataAnnotations;
using System.Linq;
using Microsoft.AspNetCore.Mvc.Controllers;
using Microsoft.OpenApi.Models;
using Swashbuckle.AspNetCore.SwaggerGen;

namespace Org.OpenAPITools.Filters
{
    /// <summary>
    /// Path Parameter Validation Rules Filter
    /// </summary>
    public class GeneratePathParamsValidationFilter : IOperationFilter
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="operation">Operation</param>
        /// <param name="context">OperationFilterContext</param>
        public void Apply(OpenApiOperation operation, OperationFilterContext context)
        {
            var pars = context.ApiDescription.ParameterDescriptions;

            foreach (var par in pars)
            {
                var swaggerParam = operation.Parameters.SingleOrDefault(p => p.Name == par.Name);

                var attributes = ((ControllerParameterDescriptor)par.ParameterDescriptor).ParameterInfo.CustomAttributes.ToList();

                // See https://github.com/domaindrivendev/Swashbuckle.AspNetCore/issues/1147
                // and https://mikeralphson.github.io/openapi/2017/03/15/openapi3.0.0-rc0
                // Basically OpenAPI v3 body parameters are split out into RequestBody and the properties have moved to schema
                if (attributes.Any() && swaggerParam != null)
                {
                    // Required - [Required]
                    var requiredAttr = attributes.FirstOrDefault(p => p.AttributeType == typeof(RequiredAttribute));
                    if (requiredAttr != null)
                    {
                        swaggerParam.Required = true;
                    }

                    // Regex Pattern [RegularExpression]
                    var regexAttr = attributes.FirstOrDefault(p => p.AttributeType == typeof(RegularExpressionAttribute));
                    if (regexAttr != null)
                    {
                        var regex = (string)regexAttr.ConstructorArguments[0].Value;
                        swaggerParam.Schema.Pattern = regex;
                    }

                    // String Length [StringLength]
                    int? minLength = null, maxLength = null;
                    var stringLengthAttr = attributes.FirstOrDefault(p => p.AttributeType == typeof(StringLengthAttribute));
                    if (stringLengthAttr != null)
                    {
                        if (stringLengthAttr.NamedArguments.Count == 1)
                        {
                            minLength = (int)stringLengthAttr.NamedArguments.Single(p => p.MemberName == "MinimumLength").TypedValue.Value;
                        }
                        maxLength = (int)stringLengthAttr.ConstructorArguments[0].Value;
                    }

                    var minLengthAttr = attributes.FirstOrDefault(p => p.AttributeType == typeof(MinLengthAttribute));
                    if (minLengthAttr != null)
                    {
                        minLength = (int)minLengthAttr.ConstructorArguments[0].Value;
                    }

                    var maxLengthAttr = attributes.FirstOrDefault(p => p.AttributeType == typeof(MaxLengthAttribute));
                    if (maxLengthAttr != null)
                    {
                        maxLength = (int)maxLengthAttr.ConstructorArguments[0].Value;
                    }

                    if (minLength != null)
                    {
                        swaggerParam.Schema.MinLength = minLength;
                    }

                    if (maxLength != null)
                    {
                        swaggerParam.Schema.MaxLength = maxLength;
                    }

                    // Range [Range]
                    var rangeAttr = attributes.FirstOrDefault(p => p.AttributeType == typeof(RangeAttribute));
                    if (rangeAttr != null)
                    {
                        var rangeMin = (int)rangeAttr.ConstructorArguments[0].Value;
                        var rangeMax = (int)rangeAttr.ConstructorArguments[1].Value;

                        swaggerParam.Schema.MinLength = rangeMin;
                        swaggerParam.Schema.MaxLength = rangeMax;
                    }
                }
            }
        }
    }
}

