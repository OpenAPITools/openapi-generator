import 'package:jaguar_retrofit/annotations/annotations.dart';
import 'package:jaguar_retrofit/jaguar_retrofit.dart';
import 'package:jaguar_serializer/jaguar_serializer.dart';
import 'package:jaguar_mimetype/jaguar_mimetype.dart';
import 'dart:async';

import 'package:openapi/model/pet.dart';
import 'package:openapi/model/api_response.dart';
import 'dart:typed_data';

part 'pet_api.jretro.dart';

@GenApiClient()
class PetApi extends ApiClient with _$PetApiClient {
    final Route base;
    final Map<String, CodecRepo> converters;
    final Duration timeout;

    PetApi({this.base, this.converters, this.timeout = const Duration(minutes: 2)});

    /// Add a new pet to the store
    ///
    /// 
    @PostReq(path: "/pet", metadata: {"auth": [ {"type": "oauth2", "name": "petstore_auth" }]})
    Future<void> addPet(
            
             @AsJson() Pet body
        ) {
        return super.addPet(

        
        body
        ).timeout(timeout);
    }

    /// Deletes a pet
    ///
    /// 
    @DeleteReq(path: "/pet/:petId", metadata: {"auth": [ {"type": "oauth2", "name": "petstore_auth" }]})
    Future<void> deletePet(
            @PathParam("petId") int petId
        ,
            @Header("api_key") String apiKey
        ) {
        return super.deletePet(
        petId
        ,
        apiKey

        ).timeout(timeout);
    }

    /// Finds Pets by status
    ///
    /// Multiple status values can be provided with comma separated strings
    @GetReq(path: "/pet/findByStatus", metadata: {"auth": [ {"type": "oauth2", "name": "petstore_auth" }]})
    Future<List<Pet>> findPetsByStatus(
        
            @QueryParam("status") List<String> status
        ) {
        return super.findPetsByStatus(
        
        status

        ).timeout(timeout);
    }

    /// Finds Pets by tags
    ///
    /// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    @GetReq(path: "/pet/findByTags", metadata: {"auth": [ {"type": "oauth2", "name": "petstore_auth" }]})
    Future<List<Pet>> findPetsByTags(
        
            @QueryParam("tags") List<String> tags
        ) {
        return super.findPetsByTags(
        
        tags

        ).timeout(timeout);
    }

    /// Find pet by ID
    ///
    /// Returns a single pet
    @GetReq(path: "/pet/:petId", metadata: {"auth": [ {"type": "apiKey", "name": "api_key", "keyName": "api_key", "where": "header" }]})
    Future<Pet> getPetById(
            @PathParam("petId") int petId
        ) {
        return super.getPetById(
        petId

        ).timeout(timeout);
    }

    /// Update an existing pet
    ///
    /// 
    @PutReq(path: "/pet", metadata: {"auth": [ {"type": "oauth2", "name": "petstore_auth" }]})
    Future<void> updatePet(
            
             @AsJson() Pet body
        ) {
        return super.updatePet(

        
        body
        ).timeout(timeout);
    }

    /// Updates a pet in the store with form data
    ///
    /// 
    @PostReq(path: "/pet/:petId", metadata: {"auth": [ {"type": "oauth2", "name": "petstore_auth" }]})
    Future<void> updatePetWithForm(
            @PathParam("petId") int petId
            ,
            @AsFormField() String name, 
            
            @AsFormField() String status
        ) {
        return super.updatePetWithForm(
        petId

        ,
        name, 
        
        status
        ).timeout(timeout);
    }

    /// uploads an image
    ///
    /// 
    @PostReq(path: "/pet/:petId/uploadImage", metadata: {"auth": [ {"type": "oauth2", "name": "petstore_auth" }]})
    Future<ApiResponse> uploadFile(
            @PathParam("petId") int petId
            ,
            @AsMultipartField() String additionalMetadata, 
            
            @AsMultipartField() MultipartFile file
        ) {
        return super.uploadFile(
        petId

        ,
        additionalMetadata, 
        
        file
        ).timeout(timeout);
    }


}
