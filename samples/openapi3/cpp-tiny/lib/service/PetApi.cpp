#include "PetApi.h"

using namespace Tiny;



        Response<
            Pet
        >
        PetApi::
        addPet(
            
            Pet pet
            
        )
        {
            std::string url = basepath + "/pet"; //


            // Headers  | 

            // Query    | 

            // Form     | 
            addHeader("Content-Type", "application/json");





            std::string payload = "";
            // Send Request
            // METHOD | POST
            // Body     | pet



            payload = pet.toJson().dump();

            int httpCode = sendRequest(url, "POST", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = getResponseBody();
            std::string output_string = output.c_str();




            Pet obj(output_string);


            Response<Pet> response(obj, httpCode);
            return response;
        }

        Response<
            String
        >
        PetApi::
        deletePet(
            
            long petId
            , 
            
            std::string apiKey
            
        )
        {
            std::string url = basepath + "/pet/{petId}"; //petId 


            // Headers  |  apiKey 
            addHeader("api_key",apiKey);

            // Query    | 

            // Form     | 



                std::string s_petId("{");
                s_petId.append("petId");
                s_petId.append("}");

                int pos = url.find(s_petId);

                url.erase(pos, s_petId.length());
                url.insert(pos, stringify(petId));


            std::string payload = "";
            // Send Request
            // METHOD | DELETE
            // Body     | 
            int httpCode = sendRequest(url, "DELETE", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = getResponseBody();
            std::string output_string = output.c_str();


            Response<String> response(output, httpCode);
            return response;
        }

        Response<
            std::list<Pet>
        >
        PetApi::
        findPetsByStatus(
            std::list<std::string> status
            
            
        )
        {
            std::string url = basepath + "/pet/findByStatus"; //


            // Headers  | 

            // Query    | status 
            for (auto &x : status){
                addQueryParam("status", std::string(x));
            }

            // Form     | 





            std::string payload = "";
            // Send Request
            // METHOD | GET
            // Body     | 
            int httpCode = sendRequest(url, "GET", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = getResponseBody();
            std::string output_string = output.c_str();



            std::list<Pet> obj = std::list<Pet>();
            bourne::json jsonPayload(output_string);








            
            for(auto& var : jsonPayload.array_range())
            {
                Pet tmp(var.dump());
                obj.push_back(tmp);
            }
            







            Response<std::list<Pet>> response(obj, httpCode);
            return response;
        }

        Response<
            std::list<Pet>
        >
        PetApi::
        findPetsByTags(
            std::list<std::string> tags
            
            
        )
        {
            std::string url = basepath + "/pet/findByTags"; //


            // Headers  | 

            // Query    | tags 
            for (auto &x : tags){
                addQueryParam("tags", std::string(x));
            }

            // Form     | 





            std::string payload = "";
            // Send Request
            // METHOD | GET
            // Body     | 
            int httpCode = sendRequest(url, "GET", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = getResponseBody();
            std::string output_string = output.c_str();



            std::list<Pet> obj = std::list<Pet>();
            bourne::json jsonPayload(output_string);








            
            for(auto& var : jsonPayload.array_range())
            {
                Pet tmp(var.dump());
                obj.push_back(tmp);
            }
            







            Response<std::list<Pet>> response(obj, httpCode);
            return response;
        }

        Response<
            Pet
        >
        PetApi::
        getPetById(
            
            long petId
            
        )
        {
            std::string url = basepath + "/pet/{petId}"; //petId 


            // Headers  | 

            // Query    | 

            // Form     | 



                std::string s_petId("{");
                s_petId.append("petId");
                s_petId.append("}");

                int pos = url.find(s_petId);

                url.erase(pos, s_petId.length());
                url.insert(pos, stringify(petId));


            std::string payload = "";
            // Send Request
            // METHOD | GET
            // Body     | 
            int httpCode = sendRequest(url, "GET", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = getResponseBody();
            std::string output_string = output.c_str();




            Pet obj(output_string);


            Response<Pet> response(obj, httpCode);
            return response;
        }

        Response<
            Pet
        >
        PetApi::
        updatePet(
            
            Pet pet
            
        )
        {
            std::string url = basepath + "/pet"; //


            // Headers  | 

            // Query    | 

            // Form     | 
            addHeader("Content-Type", "application/json");





            std::string payload = "";
            // Send Request
            // METHOD | PUT
            // Body     | pet



            payload = pet.toJson().dump();

            int httpCode = sendRequest(url, "PUT", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = getResponseBody();
            std::string output_string = output.c_str();




            Pet obj(output_string);


            Response<Pet> response(obj, httpCode);
            return response;
        }

        Response<
            String
        >
        PetApi::
        updatePetWithForm(
            
            long petId
            , 
            
            std::string name
            , 
            
            std::string status
            
        )
        {
            std::string url = basepath + "/pet/{petId}"; //petId 


            // Headers  | 

            // Query    | 

            // Form     | name status 
            addHeader("Content-Type", "application/x-www-form-urlencoded");

            addFormParam("name",name);
            addFormParam("status",status);


                std::string s_petId("{");
                s_petId.append("petId");
                s_petId.append("}");

                int pos = url.find(s_petId);

                url.erase(pos, s_petId.length());
                url.insert(pos, stringify(petId));


            std::string payload = "";
            // Send Request
            // METHOD | POST
            // Body     | 
            int httpCode = sendRequest(url, "POST", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = getResponseBody();
            std::string output_string = output.c_str();


            Response<String> response(output, httpCode);
            return response;
        }

        Response<
            ApiResponse
        >
        PetApi::
        uploadFile(
            
            long petId
            , 
            
            std::string additionalMetadata
            , 
            
            std::string file
            
        )
        {
            std::string url = basepath + "/pet/{petId}/uploadImage"; //petId 


            // Headers  | 

            // Query    | 

            // Form     | additionalMetadata file 
            addHeader("Content-Type", "application/x-www-form-urlencoded");

            addFormParam("additionalMetadata",additionalMetadata);
            addFormParam("file",file);


                std::string s_petId("{");
                s_petId.append("petId");
                s_petId.append("}");

                int pos = url.find(s_petId);

                url.erase(pos, s_petId.length());
                url.insert(pos, stringify(petId));


            std::string payload = "";
            // Send Request
            // METHOD | POST
            // Body     | 
            int httpCode = sendRequest(url, "POST", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = getResponseBody();
            std::string output_string = output.c_str();




            ApiResponse obj(output_string);


            Response<ApiResponse> response(obj, httpCode);
            return response;
        }



