// GENERATED CODE - DO NOT MODIFY BY HAND

part of 'order.dart';

// **************************************************************************
// BuiltValueGenerator
// **************************************************************************

Serializer<Order> _$orderSerializer = new _$OrderSerializer();

class _$OrderSerializer implements StructuredSerializer<Order> {
  @override
  final Iterable<Type> types = const [Order, _$Order];
  @override
  final String wireName = 'Order';

  @override
  Iterable<Object> serialize(Serializers serializers, Order object,
      {FullType specifiedType = FullType.unspecified}) {
    final result = <Object>[
      'id',
      serializers.serialize(object.id, specifiedType: const FullType(int)),
      'petId',
      serializers.serialize(object.petId, specifiedType: const FullType(int)),
      'quantity',
      serializers.serialize(object.quantity,
          specifiedType: const FullType(int)),
      'shipDate',
      serializers.serialize(object.shipDate,
          specifiedType: const FullType(DateTime)),
      'status',
      serializers.serialize(object.status,
          specifiedType: const FullType(String)),
      'complete',
      serializers.serialize(object.complete,
          specifiedType: const FullType(bool)),
    ];

    return result;
  }

  @override
  Order deserialize(Serializers serializers, Iterable<Object> serialized,
      {FullType specifiedType = FullType.unspecified}) {
    final result = new OrderBuilder();

    final iterator = serialized.iterator;
    while (iterator.moveNext()) {
      final key = iterator.current as String;
      iterator.moveNext();
      final dynamic value = iterator.current;
      switch (key) {
        case 'id':
          result.id = serializers.deserialize(value,
              specifiedType: const FullType(int)) as int;
          break;
        case 'petId':
          result.petId = serializers.deserialize(value,
              specifiedType: const FullType(int)) as int;
          break;
        case 'quantity':
          result.quantity = serializers.deserialize(value,
              specifiedType: const FullType(int)) as int;
          break;
        case 'shipDate':
          result.shipDate = serializers.deserialize(value,
              specifiedType: const FullType(DateTime)) as DateTime;
          break;
        case 'status':
          result.status = serializers.deserialize(value,
              specifiedType: const FullType(String)) as String;
          break;
        case 'complete':
          result.complete = serializers.deserialize(value,
              specifiedType: const FullType(bool)) as bool;
          break;
      }
    }

    return result.build();
  }
}

class _$Order extends Order {
  @override
  final int id;
  @override
  final int petId;
  @override
  final int quantity;
  @override
  final DateTime shipDate;
  @override
  final String status;
  @override
  final bool complete;

  factory _$Order([void Function(OrderBuilder) updates]) =>
      (new OrderBuilder()..update(updates)).build();

  _$Order._(
      {this.id,
      this.petId,
      this.quantity,
      this.shipDate,
      this.status,
      this.complete})
      : super._() {
    if (id == null) {
      throw new BuiltValueNullFieldError('Order', 'id');
    }
    if (petId == null) {
      throw new BuiltValueNullFieldError('Order', 'petId');
    }
    if (quantity == null) {
      throw new BuiltValueNullFieldError('Order', 'quantity');
    }
    if (shipDate == null) {
      throw new BuiltValueNullFieldError('Order', 'shipDate');
    }
    if (status == null) {
      throw new BuiltValueNullFieldError('Order', 'status');
    }
    if (complete == null) {
      throw new BuiltValueNullFieldError('Order', 'complete');
    }
  }

  @override
  Order rebuild(void Function(OrderBuilder) updates) =>
      (toBuilder()..update(updates)).build();

  @override
  OrderBuilder toBuilder() => new OrderBuilder()..replace(this);

  @override
  bool operator ==(Object other) {
    if (identical(other, this)) return true;
    return other is Order &&
        id == other.id &&
        petId == other.petId &&
        quantity == other.quantity &&
        shipDate == other.shipDate &&
        status == other.status &&
        complete == other.complete;
  }

  @override
  int get hashCode {
    return $jf($jc(
        $jc(
            $jc(
                $jc($jc($jc(0, id.hashCode), petId.hashCode),
                    quantity.hashCode),
                shipDate.hashCode),
            status.hashCode),
        complete.hashCode));
  }

  @override
  String toString() {
    return (newBuiltValueToStringHelper('Order')
          ..add('id', id)
          ..add('petId', petId)
          ..add('quantity', quantity)
          ..add('shipDate', shipDate)
          ..add('status', status)
          ..add('complete', complete))
        .toString();
  }
}

class OrderBuilder implements Builder<Order, OrderBuilder> {
  _$Order _$v;

  int _id;
  int get id => _$this._id;
  set id(int id) => _$this._id = id;

  int _petId;
  int get petId => _$this._petId;
  set petId(int petId) => _$this._petId = petId;

  int _quantity;
  int get quantity => _$this._quantity;
  set quantity(int quantity) => _$this._quantity = quantity;

  DateTime _shipDate;
  DateTime get shipDate => _$this._shipDate;
  set shipDate(DateTime shipDate) => _$this._shipDate = shipDate;

  String _status;
  String get status => _$this._status;
  set status(String status) => _$this._status = status;

  bool _complete;
  bool get complete => _$this._complete;
  set complete(bool complete) => _$this._complete = complete;

  OrderBuilder();

  OrderBuilder get _$this {
    if (_$v != null) {
      _id = _$v.id;
      _petId = _$v.petId;
      _quantity = _$v.quantity;
      _shipDate = _$v.shipDate;
      _status = _$v.status;
      _complete = _$v.complete;
      _$v = null;
    }
    return this;
  }

  @override
  void replace(Order other) {
    if (other == null) {
      throw new ArgumentError.notNull('other');
    }
    _$v = other as _$Order;
  }

  @override
  void update(void Function(OrderBuilder) updates) {
    if (updates != null) updates(this);
  }

  @override
  _$Order build() {
    final _$result = _$v ??
        new _$Order._(
            id: id,
            petId: petId,
            quantity: quantity,
            shipDate: shipDate,
            status: status,
            complete: complete);
    replace(_$result);
    return _$result;
  }
}

// ignore_for_file: always_put_control_body_on_new_line,always_specify_types,annotate_overrides,avoid_annotating_with_dynamic,avoid_as,avoid_catches_without_on_clauses,avoid_returning_this,lines_longer_than_80_chars,omit_local_variable_types,prefer_expression_function_bodies,sort_constructors_first,test_types_in_equals,unnecessary_const,unnecessary_new
