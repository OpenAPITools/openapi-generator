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


            // Headers  | 

            // Query    | 

            // Form     | 



                std::string s_orderId("{");
                s_orderId.append("orderId");
                s_orderId.append("}");

                int pos = url.find(s_orderId);

                url.erase(pos, s_orderId.length());
                url.insert(pos, stringify(orderId));


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
            String
        >
        StoreApi::
        getInventory(
        )
        {
            std::string url = basepath + "/store/inventory"; //


            // Headers  | 

            // Query    | 

            // Form     | 





            std::string payload = "";
            // Send Request
            // METHOD | GET
            // Body     | 
            int httpCode = sendRequest(url, "GET", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = getResponseBody();
            std::string output_string = output.c_str();



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


            // Headers  | 

            // Query    | 

            // Form     | 



                std::string s_orderId("{");
                s_orderId.append("orderId");
                s_orderId.append("}");

                int pos = url.find(s_orderId);

                url.erase(pos, s_orderId.length());
                url.insert(pos, stringify(orderId));


            std::string payload = "";
            // Send Request
            // METHOD | GET
            // Body     | 
            int httpCode = sendRequest(url, "GET", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = getResponseBody();
            std::string output_string = output.c_str();




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


            // Headers  | 

            // Query    | 

            // Form     | 
            addHeader("Content-Type", "application/json");





            std::string payload = "";
            // Send Request
            // METHOD | POST
            // Body     | order



            payload = order.toJson().dump();

            int httpCode = sendRequest(url, "POST", reinterpret_cast<uint8_t*>(&payload[0]), payload.length());

            // Handle Request
            String output = getResponseBody();
            std::string output_string = output.c_str();




            Order obj(output_string);


            Response<Order> response(obj, httpCode);
            return response;
        }



