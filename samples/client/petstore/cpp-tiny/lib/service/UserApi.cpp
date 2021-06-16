#include "UserApi.h"

using namespace Tiny;



        Response<
            String
        >
        UserApi::
        createUser(
            
            User user
            
        )
        {
            std::string url = basepath + "/user"; //


            // Headers  | 

            // Query    | 

            // Form     | 



            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | POST
            // Body     | user
            addHeader("Content-Type", "application/json");



            payload = user.toJson().dump();

            int httpCode = http.sendRequest("POST", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();


            Response<String> response(output, httpCode);
            return response;
        }

        Response<
            String
        >
        UserApi::
        createUsersWithArrayInput(
            std::list<User> user
            
            
        )
        {
            std::string url = basepath + "/user/createWithArray"; //


            // Headers  | 

            // Query    | 

            // Form     | 



            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | POST
            // Body     | user
            addHeader("Content-Type", "application/json");


            bourne::json tmp_arr = bourne::json::array();
            for(auto& var : user)
            {
                auto tmp = var.toJson();
                tmp_arr.append(tmp);

            }
            payload = tmp_arr.dump();


            int httpCode = http.sendRequest("POST", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();


            Response<String> response(output, httpCode);
            return response;
        }

        Response<
            String
        >
        UserApi::
        createUsersWithListInput(
            std::list<User> user
            
            
        )
        {
            std::string url = basepath + "/user/createWithList"; //


            // Headers  | 

            // Query    | 

            // Form     | 



            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | POST
            // Body     | user
            addHeader("Content-Type", "application/json");


            bourne::json tmp_arr = bourne::json::array();
            for(auto& var : user)
            {
                auto tmp = var.toJson();
                tmp_arr.append(tmp);

            }
            payload = tmp_arr.dump();


            int httpCode = http.sendRequest("POST", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();


            Response<String> response(output, httpCode);
            return response;
        }

        Response<
            String
        >
        UserApi::
        deleteUser(
            
            std::string username
            
        )
        {
            std::string url = basepath + "/user/{username}"; //username 


            // Headers  | 

            // Query    | 

            // Form     | 


                std::string s_username("{");
                s_username.append("username");
                s_username.append("}");

                int pos = url.find(s_username);

                url.erase(pos, s_username.length());
                url.insert(pos, stringify(username));

            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | DELETE
            // Body     | 
            int httpCode = http.sendRequest("DELETE", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();


            Response<String> response(output, httpCode);
            return response;
        }

        Response<
            User
        >
        UserApi::
        getUserByName(
            
            std::string username
            
        )
        {
            std::string url = basepath + "/user/{username}"; //username 


            // Headers  | 

            // Query    | 

            // Form     | 


                std::string s_username("{");
                s_username.append("username");
                s_username.append("}");

                int pos = url.find(s_username);

                url.erase(pos, s_username.length());
                url.insert(pos, stringify(username));

            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | GET
            // Body     | 
            int httpCode = http.sendRequest("GET", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();




            User obj(output_string);


            Response<User> response(obj, httpCode);
            return response;
        }

        Response<
            std::string
        >
        UserApi::
        loginUser(
            
            std::string username
            , 
            
            std::string password
            
        )
        {
            std::string url = basepath + "/user/login"; //


            // Headers  | 

            // Query    | username password 
            addQueryParam("username",username);
            addQueryParam("password",password);

            // Form     | 



            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | GET
            // Body     | 
            int httpCode = http.sendRequest("GET", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();



            bourne::json jsonPayload(output_string);
            std::string obj;
            jsonToValue(&obj, jsonPayload, "std::string");



            Response<std::string> response(obj, httpCode);
            return response;
        }

        Response<
            String
        >
        UserApi::
        logoutUser(
        )
        {
            std::string url = basepath + "/user/logout"; //


            // Headers  | 

            // Query    | 

            // Form     | 



            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | GET
            // Body     | 
            int httpCode = http.sendRequest("GET", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();


            Response<String> response(output, httpCode);
            return response;
        }

        Response<
            String
        >
        UserApi::
        updateUser(
            
            std::string username
            , 
            
            User user
            
        )
        {
            std::string url = basepath + "/user/{username}"; //username 


            // Headers  | 

            // Query    | 

            // Form     | 


                std::string s_username("{");
                s_username.append("username");
                s_username.append("}");

                int pos = url.find(s_username);

                url.erase(pos, s_username.length());
                url.insert(pos, stringify(username));

            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | PUT
            // Body     | user
            addHeader("Content-Type", "application/json");



            payload = user.toJson().dump();

            int httpCode = http.sendRequest("PUT", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();


            Response<String> response(output, httpCode);
            return response;
        }




