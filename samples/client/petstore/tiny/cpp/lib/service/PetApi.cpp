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
            // Query    | 
            // Headers  | 
            // Form     | 
            // Body     | pet


            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | POST
            http.addHeader("Content-Type", "application/json");



            payload = pet.toJson().dump();

            int httpCode = http.sendRequest("POST", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();




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
            // Query    | 
            // Headers  | apiKey 
            // Form     | 
            // Body     | 

                std::string s_petId("{");
                s_petId.append("petId");
                s_petId.append("}");

                int pos = url.find(s_petId);

                url.erase(pos, s_petId.length());
                url.insert(pos, stringify(petId));

            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | DELETE
            int httpCode = http.sendRequest("DELETE", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();


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
            // Query    | status 
            // Headers  | 
            // Form     | 
            // Body     | 


            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | GET
            int httpCode = http.sendRequest("GET", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();



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
            // Query    | tags 
            // Headers  | 
            // Form     | 
            // Body     | 


            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | GET
            int httpCode = http.sendRequest("GET", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();



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
            // Query    | 
            // Headers  | 
            // Form     | 
            // Body     | 

                std::string s_petId("{");
                s_petId.append("petId");
                s_petId.append("}");

                int pos = url.find(s_petId);

                url.erase(pos, s_petId.length());
                url.insert(pos, stringify(petId));

            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | GET
            int httpCode = http.sendRequest("GET", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();




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
            // Query    | 
            // Headers  | 
            // Form     | 
            // Body     | pet


            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | PUT
            http.addHeader("Content-Type", "application/json");



            payload = pet.toJson().dump();

            int httpCode = http.sendRequest("PUT", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();




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
            // Query    | 
            // Headers  | 
            // Form     | name status 
            // Body     | 

                std::string s_petId("{");
                s_petId.append("petId");
                s_petId.append("}");

                int pos = url.find(s_petId);

                url.erase(pos, s_petId.length());
                url.insert(pos, stringify(petId));

            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | POST
            int httpCode = http.sendRequest("POST", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();


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
            // Query    | 
            // Headers  | 
            // Form     | additionalMetadata file 
            // Body     | 

                std::string s_petId("{");
                s_petId.append("petId");
                s_petId.append("}");

                int pos = url.find(s_petId);

                url.erase(pos, s_petId.length());
                url.insert(pos, stringify(petId));

            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | POST
            int httpCode = http.sendRequest("POST", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();




            ApiResponse obj(output_string);


            Response<ApiResponse> response(obj, httpCode);
            return response;
        }




