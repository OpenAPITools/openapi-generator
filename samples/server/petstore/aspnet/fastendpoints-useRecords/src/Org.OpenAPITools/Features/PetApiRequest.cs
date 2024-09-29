using FastEndpoints;

using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Features;

public record AddPetRequest([property: FromBody] Pet pet)
{
}

public record DeletePetRequest(long PetId)
{
    [property: FromHeader] public string? ApiKey {get; init; }
}

public record FindPetsByStatusRequest([property: QueryParam] List<string> Status)
{
}

public record FindPetsByTagsRequest([property: QueryParam] List<string> Tags)
{
}

public record GetPetByIdRequest(long PetId)
{
}

public record UpdatePetRequest([property: FromBody] Pet pet)
{
}

public record UpdatePetWithFormRequest(long PetId)
{
    public string? Name {get; init; }
    public string? Status {get; init; }
}

public record UploadFileRequest(long PetId)
{
    public string? AdditionalMetadata {get; init; }
    public System.IO.Stream? File {get; init; }
}




