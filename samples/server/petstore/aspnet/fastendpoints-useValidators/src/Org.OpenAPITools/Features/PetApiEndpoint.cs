using Org.OpenAPITools.Models;

namespace Org.OpenAPITools.Features;



/// <summary>
/// Add a new pet to the store
/// </summary>

public class AddPetEndpoint : FastEndpoints.Endpoint<AddPetRequest, Pet>
{
    public override void Configure()
    {
        Post("/v2/pet");
        
        
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("pet");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemFE(x, 405);
        });

        Summary(s => {
            s.Summary = "Add a new pet to the store";
            s.RequestParam(r => r.pet, "Pet object that needs to be added to the store");
            s.Responses[200] = "successful operation";
            s.Responses[405] = "Invalid input";
        });
    }

    public override async Task HandleAsync(AddPetRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// Deletes a pet
/// </summary>

public class DeletePetEndpoint : FastEndpoints.Endpoint<DeletePetRequest>
{
    public override void Configure()
    {
        Delete("/v2/pet/{petId}");
        
        
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("pet");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemFE(x, 400);
        });

        Summary(s => {
            s.Summary = "Deletes a pet";
            s.RequestParam(r => r.PetId, "Pet id to delete");
            s.RequestParam(r => r.ApiKey, "");
            s.Responses[400] = "Invalid pet value";
        });
    }

    public override async Task HandleAsync(DeletePetRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// Finds Pets by status
/// </summary>

public class FindPetsByStatusEndpoint : FastEndpoints.Endpoint<FindPetsByStatusRequest, List<Pet>>
{
    public override void Configure()
    {
        Get("/v2/pet/findByStatus");
        
        
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("pet");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemFE(x, 400);
        });

        Summary(s => {
            s.Summary = "Finds Pets by status";
            s.RequestParam(r => r.Status, "Status values that need to be considered for filter");
            s.Responses[200] = "successful operation";
            s.Responses[400] = "Invalid status value";
        });
    }

    public override async Task HandleAsync(FindPetsByStatusRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// Finds Pets by tags
/// </summary>
[Obsolete]
public class FindPetsByTagsEndpoint : FastEndpoints.Endpoint<FindPetsByTagsRequest, List<Pet>>
{
    public override void Configure()
    {
        Get("/v2/pet/findByTags");
        
        
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("pet");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemFE(x, 400);
        });

        Summary(s => {
            s.Summary = "Finds Pets by tags";
            s.RequestParam(r => r.Tags, "Tags to filter by");
            s.Responses[200] = "successful operation";
            s.Responses[400] = "Invalid tag value";
        });
    }

    public override async Task HandleAsync(FindPetsByTagsRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// Find pet by ID
/// </summary>

public class GetPetByIdEndpoint : FastEndpoints.Endpoint<GetPetByIdRequest, Pet>
{
    public override void Configure()
    {
        Get("/v2/pet/{petId}");
        
        
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("pet");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemFE(x, 400);
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemFE(x, 404);
        });

        Summary(s => {
            s.Summary = "Find pet by ID";
            s.RequestParam(r => r.PetId, "ID of pet to return");
            s.Responses[200] = "successful operation";
            s.Responses[400] = "Invalid ID supplied";
            s.Responses[404] = "Pet not found";
        });
    }

    public override async Task HandleAsync(GetPetByIdRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// Update an existing pet
/// </summary>

public class UpdatePetEndpoint : FastEndpoints.Endpoint<UpdatePetRequest, Pet>
{
    public override void Configure()
    {
        Put("/v2/pet");
        
        
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("pet");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemFE(x, 400);
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemFE(x, 404);
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemFE(x, 405);
        });

        Summary(s => {
            s.Summary = "Update an existing pet";
            s.RequestParam(r => r.pet, "Pet object that needs to be added to the store");
            s.Responses[200] = "successful operation";
            s.Responses[400] = "Invalid ID supplied";
            s.Responses[404] = "Pet not found";
            s.Responses[405] = "Validation exception";
        });
    }

    public override async Task HandleAsync(UpdatePetRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// Updates a pet in the store with form data
/// </summary>

public class UpdatePetWithFormEndpoint : FastEndpoints.Endpoint<UpdatePetWithFormRequest>
{
    public override void Configure()
    {
        Post("/v2/pet/{petId}");
        
        
        AllowAnonymous();
        
        Description(x =>
        {
            x.WithTags("pet");
            FastEndpoints.RouteHandlerBuilderExtensions.ProducesProblemFE(x, 405);
        });

        Summary(s => {
            s.Summary = "Updates a pet in the store with form data";
            s.RequestParam(r => r.PetId, "ID of pet that needs to be updated");
            s.RequestParam(r => r.Name, "Updated name of the pet");
            s.RequestParam(r => r.Status, "Updated status of the pet");
            s.Responses[405] = "Invalid input";
        });
    }

    public override async Task HandleAsync(UpdatePetWithFormRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}


/// <summary>
/// uploads an image
/// </summary>

public class UploadFileEndpoint : FastEndpoints.Endpoint<UploadFileRequest, ApiResponse>
{
    public override void Configure()
    {
        Post("/v2/pet/{petId}/uploadImage");
        
        
        AllowAnonymous();
        AllowFileUploads();
        Description(x =>
        {
            x.WithTags("pet");
        });

        Summary(s => {
            s.Summary = "uploads an image";
            s.RequestParam(r => r.PetId, "ID of pet to update");
            s.RequestParam(r => r.AdditionalMetadata, "Additional data to pass to server");
            s.RequestParam(r => r.File, "file to upload");
            s.Responses[200] = "successful operation";
        });
    }

    public override async Task HandleAsync(UploadFileRequest req, CancellationToken ct)
    {
        //Response = new()
        //{
            //...
        //};
        //return Task.CompletedTask;
    }
}

