using FastEndpoints;

using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Features;

public record AddPetRequest([property: FromBody] Pet pet)
{
}

public record DeletePetRequest([property: BindFrom("petId")] long PetId)
{
    [property: FromHeader] public string? ApiKey {get; init; }
}

public record FindPetsByStatusRequest([property: QueryParam, BindFrom("status")] List<string> Status)
{
}

public record FindPetsByTagsRequest([property: QueryParam, BindFrom("tags")] List<string> Tags)
{
}

public record GetPetByIdRequest([property: BindFrom("petId")] long PetId)
{
}

public record UpdatePetRequest([property: FromBody] Pet pet)
{
}

public record UpdatePetWithFormRequest([property: BindFrom("petId")] long PetId)
{
    [BindFrom("name")] public string? Name {get; init; }
    [BindFrom("status")] public string? Status {get; init; }
}

public record UploadFileRequest([property: BindFrom("petId")] long PetId)
{
    [BindFrom("additionalMetadata")] public string? AdditionalMetadata {get; init; }
    [BindFrom("file")] public System.IO.Stream? File {get; init; }
}




