//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, invalid_annotation_target
part of 'models.dart';

/// Order
    ///
    /// Properties:
        /// * [id] 
        /// * [petId] 
        /// * [quantity] 
        /// * [shipDate] 
        /// * [status] - Order Status
        /// * [complete] 

        @freezed
        class Order with _$Order {
        const Order._();
        
        const factory Order({
                    @JsonKey(name: r'id') 
    int?
 id,
                    @JsonKey(name: r'petId') 
    int?
 petId,
                    @JsonKey(name: r'quantity') 
    int?
 quantity,
                    @JsonKey(name: r'shipDate') 
    DateTime?
 shipDate,
                        /// Order Status
            @JsonKey(name: r'status') 
    OrderStatusEnum?
 status,
                    @JsonKey(name: r'complete') 
    bool?
 complete,
        }) = _Order;


        factory Order.fromJson(Map<String, dynamic> json) => _$OrderFromJson(json);






}


            /// Order Status
            @JsonEnum(valueField: 'value')
            enum OrderStatusEnum {
                                    placed(value: r'placed'),
                        approved(value: r'approved'),
                        delivered(value: r'delivered'),
                        unknownDefaultOpenApi(value: r'unknown_default_open_api');
                    const OrderStatusEnum({required this.value});
                    final String value;
            }
