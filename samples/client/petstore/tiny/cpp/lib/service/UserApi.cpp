#include "UserApi.h"

using namespace std;
using namespace Tiny;


    UserApi::UserApi()
    {

    }

    UserApi::~UserApi()
    {

    }

        Response<
            String
        >
        UserApi::
        createUser(
            
            User user
            
        )
        {
            std::string url = basepath + "/user"; //
            // Query    | 
            // Headers  | 
            // Form     | 
            // Body     | user


            HTTPClient http;
                http.begin(String(url.c_str()), test_root_ca); //HTTPS connection


            std::string payload = "";
            // Send Request
            // METHOD | POST
            http.addHeader("Content-Type", "application/json");



            payload = user.toJson().dump();

            int httpCode = http.sendRequest("POST", (uint8_t *) payload.c_str(), payload.length());

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
            // Query    | 
            // Headers  | 
            // Form     | 
            // Body     | user


            HTTPClient http;
                http.begin(String(url.c_str()), test_root_ca); //HTTPS connection


            std::string payload = "";
            // Send Request
            // METHOD | POST
            http.addHeader("Content-Type", "application/json");


            bourne::json tmp_arr = bourne::json::array();
            for(auto& var : user)
            {
                auto tmp = var.toJson();
                tmp_arr.append(tmp);

            }
            payload = tmp_arr.dump();


            int httpCode = http.sendRequest("POST", (uint8_t *) payload.c_str(), payload.length());

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
            // Query    | 
            // Headers  | 
            // Form     | 
            // Body     | user


            HTTPClient http;
                http.begin(String(url.c_str()), test_root_ca); //HTTPS connection


            std::string payload = "";
            // Send Request
            // METHOD | POST
            http.addHeader("Content-Type", "application/json");


            bourne::json tmp_arr = bourne::json::array();
            for(auto& var : user)
            {
                auto tmp = var.toJson();
                tmp_arr.append(tmp);

            }
            payload = tmp_arr.dump();


            int httpCode = http.sendRequest("POST", (uint8_t *) payload.c_str(), payload.length());

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
            // Query    | 
            // Headers  | 
            // Form     | 
            // Body     | 

                string s_username("{");
                s_username.append("username");
                s_username.append("}");

                int pos = url.find(s_username);

                url.erase(pos, s_username.length());
                url.insert(pos, stringify(username));

            HTTPClient http;
                http.begin(String(url.c_str()), test_root_ca); //HTTPS connection


            std::string payload = "";
            // Send Request
            // METHOD | DELETE
            int httpCode = http.sendRequest("DELETE", (uint8_t *) payload.c_str(), payload.length());

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
            // Query    | 
            // Headers  | 
            // Form     | 
            // Body     | 

                string s_username("{");
                s_username.append("username");
                s_username.append("}");

                int pos = url.find(s_username);

                url.erase(pos, s_username.length());
                url.insert(pos, stringify(username));

            HTTPClient http;
                http.begin(String(url.c_str()), test_root_ca); //HTTPS connection


            std::string payload = "";
            // Send Request
            // METHOD | GET
            int httpCode = http.sendRequest("GET", (uint8_t *) payload.c_str(), payload.length());

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
            // Query    | username password 
            // Headers  | 
            // Form     | 
            // Body     | 


            HTTPClient http;
                http.begin(String(url.c_str()), test_root_ca); //HTTPS connection


            std::string payload = "";
            // Send Request
            // METHOD | GET
            int httpCode = http.sendRequest("GET", (uint8_t *) payload.c_str(), payload.length());

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
            // Query    | 
            // Headers  | 
            // Form     | 
            // Body     | 


            HTTPClient http;
                http.begin(String(url.c_str()), test_root_ca); //HTTPS connection


            std::string payload = "";
            // Send Request
            // METHOD | GET
            int httpCode = http.sendRequest("GET", (uint8_t *) payload.c_str(), payload.length());

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
            // Query    | 
            // Headers  | 
            // Form     | 
            // Body     | user

                string s_username("{");
                s_username.append("username");
                s_username.append("}");

                int pos = url.find(s_username);

                url.erase(pos, s_username.length());
                url.insert(pos, stringify(username));

            HTTPClient http;
                http.begin(String(url.c_str()), test_root_ca); //HTTPS connection


            std::string payload = "";
            // Send Request
            // METHOD | PUT
            http.addHeader("Content-Type", "application/json");



            payload = user.toJson().dump();

            int httpCode = http.sendRequest("PUT", (uint8_t *) payload.c_str(), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();


            Response<String> response(output, httpCode);
            return response;
        }




