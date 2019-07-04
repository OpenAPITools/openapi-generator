import 'package:jaguar_retrofit/annotations/annotations.dart';
import 'package:jaguar_retrofit/jaguar_retrofit.dart';
import 'package:jaguar_serializer/jaguar_serializer.dart';
import 'package:jaguar_mimetype/jaguar_mimetype.dart';
import 'dart:async';

import 'package:openapi/model/order.pb.dart';

part 'store_api.jretro.dart';

@GenApiClient()
class StoreApi extends ApiClient with _$StoreApiClient {
    final Route base;
    final Map<String, CodecRepo> converters;
    final Duration timeout;

    StoreApi({this.base, this.converters, this.timeout = const Duration(minutes: 2)});

    /// Delete purchase order by ID
    ///
    /// For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors
    @DeleteReq(path: "/store/order/:orderId")
    Future<void> deleteOrder(
            @PathParam("orderId") String orderId
        ) {
        return super.deleteOrder(
        orderId

        ).timeout(timeout);
    }

    /// Returns pet inventories by status
    ///
    /// Returns a map of status codes to quantities
    @GetReq(path: "/store/inventory", metadata: {"auth": [ {"type": "apiKey", "name": "api_key", "keyName": "api_key", "where": "header" }]})
    Future<Map<String, int>> getInventory(
        ) {
        return super.getInventory(

        ).timeout(timeout);
    }

    /// Find purchase order by ID
    ///
    /// For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions
    @GetReq(path: "/store/order/:orderId")
    Future<Order> getOrderById(
            @PathParam("orderId") int orderId
        ) {
        return super.getOrderById(
        orderId

        ).timeout(timeout);
    }

    /// Place an order for a pet
    ///
    /// 
    @PostReq(path: "/store/order")
    Future<Order> placeOrder(
            
             @Serialized(MimeTypes.binary) Order body
        ) {
        return super.placeOrder(

        
        body
        ).timeout(timeout);
    }


}
