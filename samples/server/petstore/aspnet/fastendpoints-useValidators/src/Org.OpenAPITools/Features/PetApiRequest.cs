using FastEndpoints;
using FluentValidation;
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
    [QueryParam]
    public List<string> Status { get; set; }
}
public class FindPetsByTagsRequest
{
    /// <summary>
    /// Tags to filter by
    /// </summary>
    [QueryParam]
    public List<string> Tags { get; set; }
}
public class GetPetByIdRequest
{
    /// <summary>
    /// ID of pet to return
    /// </summary>
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
    public long PetId { get; set; }
    /// <summary>
    /// Updated name of the pet
    /// </summary>
    public string? Name { get; set; }
    /// <summary>
    /// Updated status of the pet
    /// </summary>
    public string? Status { get; set; }
}
public class UploadFileRequest
{
    /// <summary>
    /// ID of pet to update
    /// </summary>
    public long PetId { get; set; }
    /// <summary>
    /// Additional data to pass to server
    /// </summary>
    public string? AdditionalMetadata { get; set; }
    /// <summary>
    /// file to upload
    /// </summary>
    public System.IO.Stream? File { get; set; }
}



public class AddPetRequestValidator : Validator<AddPetRequest>
{
    public AddPetRequestValidator()
    {
        RuleFor(x => x.pet).NotEmpty();
        //TODO Add any additional validation rules here
    }
}


public class DeletePetRequestValidator : Validator<DeletePetRequest>
{
    public DeletePetRequestValidator()
    {
        RuleFor(x => x.PetId).NotEmpty();
        RuleFor(x => x.ApiKey).NotEmpty();
        //TODO Add any additional validation rules here
    }
}


public class FindPetsByStatusRequestValidator : Validator<FindPetsByStatusRequest>
{
    public FindPetsByStatusRequestValidator()
    {
        RuleFor(x => x.Status).NotEmpty();
        //TODO Add any additional validation rules here
    }
}


public class FindPetsByTagsRequestValidator : Validator<FindPetsByTagsRequest>
{
    public FindPetsByTagsRequestValidator()
    {
        RuleFor(x => x.Tags).NotEmpty();
        //TODO Add any additional validation rules here
    }
}


public class GetPetByIdRequestValidator : Validator<GetPetByIdRequest>
{
    public GetPetByIdRequestValidator()
    {
        RuleFor(x => x.PetId).NotEmpty();
        //TODO Add any additional validation rules here
    }
}


public class UpdatePetRequestValidator : Validator<UpdatePetRequest>
{
    public UpdatePetRequestValidator()
    {
        RuleFor(x => x.pet).NotEmpty();
        //TODO Add any additional validation rules here
    }
}


public class UpdatePetWithFormRequestValidator : Validator<UpdatePetWithFormRequest>
{
    public UpdatePetWithFormRequestValidator()
    {
        RuleFor(x => x.PetId).NotEmpty();
        RuleFor(x => x.Name).NotEmpty();
        RuleFor(x => x.Status).NotEmpty();
        //TODO Add any additional validation rules here
    }
}


public class UploadFileRequestValidator : Validator<UploadFileRequest>
{
    public UploadFileRequestValidator()
    {
        RuleFor(x => x.PetId).NotEmpty();
        RuleFor(x => x.AdditionalMetadata).NotEmpty();
        RuleFor(x => x.File).NotEmpty();
        //TODO Add any additional validation rules here
    }
}


