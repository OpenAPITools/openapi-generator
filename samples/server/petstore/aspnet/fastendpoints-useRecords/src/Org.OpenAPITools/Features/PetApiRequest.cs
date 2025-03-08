
using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Features;

public record AddPetRequest([property: FastEndpoints.FromBody] Pet pet)
{
}

public record DeletePetRequest([property: FastEndpoints.BindFrom("petId")] long PetId)
{
    [property: FastEndpoints.FromHeader] public string? ApiKey {get; init; }
}

public record FindPetsByStatusRequest([property: FastEndpoints.QueryParam, FastEndpoints.BindFrom("status")] List<string> Status)
{
}

public record FindPetsByTagsRequest([property: FastEndpoints.QueryParam, FastEndpoints.BindFrom("tags")] List<string> Tags)
{
}

public record GetPetByIdRequest([property: FastEndpoints.BindFrom("petId")] long PetId)
{
}

public record UpdatePetRequest([property: FastEndpoints.FromBody] Pet pet)
{
}

public record UpdatePetWithFormRequest([property: FastEndpoints.BindFrom("petId")] long PetId)
{
    [FastEndpoints.BindFrom("name")] public string? Name {get; init; }
    [FastEndpoints.BindFrom("status")] public string? Status {get; init; }
}

public record UploadFileRequest([property: FastEndpoints.BindFrom("petId")] long PetId)
{
    [FastEndpoints.BindFrom("additionalMetadata")] public string? AdditionalMetadata {get; init; }
    [FastEndpoints.BindFrom("file")] public System.IO.Stream? File {get; init; }
}




