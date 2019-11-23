///
//  Generated code. Do not modify.
//  source: order.proto
///
// ignore_for_file: non_constant_identifier_names,library_prefixes,unused_import

// ignore: UNUSED_SHOWN_NAME
import 'dart:core' show int, bool, double, String, List, override;

import 'package:protobuf/protobuf.dart' as $pb;

class Order extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = new $pb.BuilderInfo('Order')
    ..a<int>(1, 'id', $pb.PbFieldType.O3)
    ..a<int>(2, 'petId', $pb.PbFieldType.O3)
    ..a<int>(3, 'quantity', $pb.PbFieldType.O3)
    ..aOS(4, 'shipDate')
    ..aOS(5, 'status')
    ..aOB(6, 'complete')
    ..hasRequiredFields = false
  ;

  Order() : super();
  Order.fromBuffer(List<int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) : super.fromBuffer(i, r);
  Order.fromJson(String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) : super.fromJson(i, r);
  Order clone() => new Order()..mergeFromMessage(this);
  Order copyWith(void Function(Order) updates) => super.copyWith((message) => updates(message as Order));
  $pb.BuilderInfo get info_ => _i;
  static Order create() => new Order();
  static $pb.PbList<Order> createRepeated() => new $pb.PbList<Order>();
  static Order getDefault() => _defaultInstance ??= create()..freeze();
  static Order _defaultInstance;
  static void $checkItem(Order v) {
    if (v is! Order) $pb.checkItemFailed(v, _i.qualifiedMessageName);
  }

  int get id => $_get(0, 0);
  set id(int v) { $_setSignedInt32(0, v); }
  bool hasId() => $_has(0);
  void clearId() => clearField(1);

  int get petId => $_get(1, 0);
  set petId(int v) { $_setSignedInt32(1, v); }
  bool hasPetId() => $_has(1);
  void clearPetId() => clearField(2);

  int get quantity => $_get(2, 0);
  set quantity(int v) { $_setSignedInt32(2, v); }
  bool hasQuantity() => $_has(2);
  void clearQuantity() => clearField(3);

  String get shipDate => $_getS(3, '');
  set shipDate(String v) { $_setString(3, v); }
  bool hasShipDate() => $_has(3);
  void clearShipDate() => clearField(4);

  String get status => $_getS(4, '');
  set status(String v) { $_setString(4, v); }
  bool hasStatus() => $_has(4);
  void clearStatus() => clearField(5);

  bool get complete => $_get(5, false);
  set complete(bool v) { $_setBool(5, v); }
  bool hasComplete() => $_has(5);
  void clearComplete() => clearField(6);
}

