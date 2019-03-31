// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'order.dart';

// **************************************************************************
// JaguarSerializerGenerator
// **************************************************************************

abstract class _$OrderSerializer implements Serializer<Order> {
  @override
  Map<String, dynamic> toMap(Order model) {
    if (model == null) return null;
    Map<String, dynamic> ret = <String, dynamic>{};
    setMapValue(ret, 'id', model.id);
    setMapValue(ret, 'petId', model.petId);
    setMapValue(ret, 'quantity', model.quantity);
    setMapValue(
        ret, 'shipDate', dateTimeUtcProcessor.serialize(model.shipDate));
    setMapValue(ret, 'status', model.status);
    setMapValue(ret, 'complete', model.complete);
    return ret;
  }

  @override
  Order fromMap(Map map) {
    if (map == null) return null;
    final obj = new Order(
        id: map['id'] as int ?? getJserDefault('id'),
        petId: map['petId'] as int ?? getJserDefault('petId'),
        quantity: map['quantity'] as int ?? getJserDefault('quantity'),
        shipDate: dateTimeUtcProcessor.deserialize(map['shipDate'] as String) ??
            getJserDefault('shipDate'),
        status: map['status'] as String ?? getJserDefault('status'),
        complete: map['complete'] as bool ?? getJserDefault('complete'));
    return obj;
  }
}
