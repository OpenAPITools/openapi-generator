#include "StoreApi.h"

using namespace Tiny;



        Response<
            String
        >
        StoreApi::
        deleteOrder(
            
            std::string orderId
            
        )
        {
            std::string url = basepath + "/store/order/{orderId}"; //orderId 
            // Query    | 
            // Headers  | 
            // Form     | 
            // Body     | 

                std::string s_orderId("{");
                s_orderId.append("orderId");
                s_orderId.append("}");

                int pos = url.find(s_orderId);

                url.erase(pos, s_orderId.length());
                url.insert(pos, stringify(orderId));

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
            String
        >
        StoreApi::
        getInventory(
        )
        {
            std::string url = basepath + "/store/inventory"; //
            // Query    | 
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



            //TODO: Implement map logic here




            //TODO: No support for maps.
            Response<String> response(output, httpCode);
            return response;
        }

        Response<
            Order
        >
        StoreApi::
        getOrderById(
            
            long orderId
            
        )
        {
            std::string url = basepath + "/store/order/{orderId}"; //orderId 
            // Query    | 
            // Headers  | 
            // Form     | 
            // Body     | 

                std::string s_orderId("{");
                s_orderId.append("orderId");
                s_orderId.append("}");

                int pos = url.find(s_orderId);

                url.erase(pos, s_orderId.length());
                url.insert(pos, stringify(orderId));

            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | GET
            int httpCode = http.sendRequest("GET", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();




            Order obj(output_string);


            Response<Order> response(obj, httpCode);
            return response;
        }

        Response<
            Order
        >
        StoreApi::
        placeOrder(
            
            Order order
            
        )
        {
            std::string url = basepath + "/store/order"; //
            // Query    | 
            // Headers  | 
            // Form     | 
            // Body     | order


            begin(url);

            std::string payload = "";
            // Send Request
            // METHOD | POST
            http.addHeader("Content-Type", "application/json");



            payload = order.toJson().dump();

            int httpCode = http.sendRequest("POST", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = http.getString();
            std::string output_string = output.c_str();

            http.end();




            Order obj(output_string);


            Response<Order> response(obj, httpCode);
            return response;
        }




