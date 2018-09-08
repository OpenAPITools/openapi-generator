import 'package:jaguar_retrofit/annotations/annotations.dart';
import 'package:jaguar_retrofit/jaguar_retrofit.dart';
import 'package:jaguar_serializer/jaguar_serializer.dart';
import 'package:jaguar_serializer/src/repo/repo.dart';
import 'dart:async';

import 'package:openapi/model/pet.dart';
import 'package:openapi/model/api_response.dart';


part 'pet_api.jretro.dart';

@GenApiClient()
class PetApi extends _$PetApiClient implements ApiClient {
    final Route base;
    final SerializerRepo serializers;

    PetApi({this.base, this.serializers});

    /// Add a new pet to the store
    ///
    /// 
    @PostReq(path: '/pet')
    Future<void> addPet(
        
        @AsJson() Pet pet
    );

    /// Deletes a pet
    ///
    /// 
    @DeleteReq(path: '/pet/:petId')
    Future<void> deletePet(
        int petId
        ,
        @Header("api_key") String apiKey
    );

    /// Finds Pets by status
    ///
    /// Multiple status values can be provided with comma separated strings
    @GetReq(path: '/pet/findByStatus')
    Future<List<Pet>> findPetsByStatus(
        
        @QueryParam("status") List<String> status
    );

    /// Finds Pets by tags
    ///
    /// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
    @GetReq(path: '/pet/findByTags')
    Future<List<Pet>> findPetsByTags(
        
        @QueryParam("tags") List<String> tags
    );

    /// Find pet by ID
    ///
    /// Returns a single pet
    @GetReq(path: '/pet/:petId')
    Future<Pet> getPetById(
        int petId
    );

    /// Update an existing pet
    ///
    /// 
    @PutReq(path: '/pet')
    Future<void> updatePet(
        
        @AsJson() Pet pet
    );

    /// Updates a pet in the store with form data
    ///
    /// 
    @PostReq(path: '/pet/:petId')
    Future<void> updatePetWithForm(
        int petId
        ,
        @AsFormField() String name, 
        
        @AsFormField() String status
    );

    /// uploads an image
    ///
    /// 
    @PostReq(path: '/pet/:petId/uploadImage')
    Future<ApiResponse> uploadFile(
        int petId
        ,
        @AsMultipartField() String additionalMetadata, 
        
        @AsMultipartField() MultipartFile file
    );


}
