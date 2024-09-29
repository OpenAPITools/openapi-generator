using FastEndpoints;

using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Features;


public class AddPetRequest
{
    /// <summary>
    /// Pet object that needs to be added to the store
    /// </summary>
    [FromBody]
    public Pet pet { get; set; }
}
public class DeletePetRequest
{
    /// <summary>
    /// Pet id to delete
    /// </summary>
    [BindFrom("petId")]
    public long PetId { get; set; }
    /// <summary>
    /// 
    /// </summary>
    [FromHeader]
    public string? ApiKey { get; set; }
}
public class FindPetsByStatusRequest
{
    /// <summary>
    /// Status values that need to be considered for filter
    /// </summary>
    [QueryParam, BindFrom("status")]
    public List<string> Status { get; set; }
}
public class FindPetsByTagsRequest
{
    /// <summary>
    /// Tags to filter by
    /// </summary>
    [QueryParam, BindFrom("tags")]
    public List<string> Tags { get; set; }
}
public class GetPetByIdRequest
{
    /// <summary>
    /// ID of pet to return
    /// </summary>
    [BindFrom("petId")]
    public long PetId { get; set; }
}
public class UpdatePetRequest
{
    /// <summary>
    /// Pet object that needs to be added to the store
    /// </summary>
    [FromBody]
    public Pet pet { get; set; }
}
public class UpdatePetWithFormRequest
{
    /// <summary>
    /// ID of pet that needs to be updated
    /// </summary>
    [BindFrom("petId")]
    public long PetId { get; set; }
    /// <summary>
    /// Updated name of the pet
    /// </summary>
    [BindFrom("name")]
    public string? Name { get; set; }
    /// <summary>
    /// Updated status of the pet
    /// </summary>
    [BindFrom("status")]
    public string? Status { get; set; }
}
public class UploadFileRequest
{
    /// <summary>
    /// ID of pet to update
    /// </summary>
    [BindFrom("petId")]
    public long PetId { get; set; }
    /// <summary>
    /// Additional data to pass to server
    /// </summary>
    [BindFrom("additionalMetadata")]
    public string? AdditionalMetadata { get; set; }
    /// <summary>
    /// file to upload
    /// </summary>
    [BindFrom("file")]
    public System.IO.Stream? File { get; set; }
}


