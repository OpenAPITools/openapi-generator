///
//  Generated code. Do not modify.
//  source: pet.proto
///
// ignore_for_file: non_constant_identifier_names,library_prefixes,unused_import

// ignore: UNUSED_SHOWN_NAME
import 'dart:core' show int, bool, double, String, List, override;

import 'package:protobuf/protobuf.dart' as $pb;

import 'category.pb.dart' as $0;
import 'tag.pb.dart' as $1;

class Pet extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = new $pb.BuilderInfo('Pet')
    ..a<int>(1, 'id', $pb.PbFieldType.O3)
    ..a<$0.Category>(2, 'category', $pb.PbFieldType.OM, $0.Category.getDefault, $0.Category.create)
    ..aOS(3, 'name')
    ..pPS(4, 'photoUrls')
    ..pp<$1.Tag>(5, 'tags', $pb.PbFieldType.PM, $1.Tag.$checkItem, $1.Tag.create)
    ..aOS(6, 'status')
    ..hasRequiredFields = false
  ;

  Pet() : super();
  Pet.fromBuffer(List<int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) : super.fromBuffer(i, r);
  Pet.fromJson(String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) : super.fromJson(i, r);
  Pet clone() => new Pet()..mergeFromMessage(this);
  Pet copyWith(void Function(Pet) updates) => super.copyWith((message) => updates(message as Pet));
  $pb.BuilderInfo get info_ => _i;
  static Pet create() => new Pet();
  static $pb.PbList<Pet> createRepeated() => new $pb.PbList<Pet>();
  static Pet getDefault() => _defaultInstance ??= create()..freeze();
  static Pet _defaultInstance;
  static void $checkItem(Pet v) {
    if (v is! Pet) $pb.checkItemFailed(v, _i.qualifiedMessageName);
  }

  int get id => $_get(0, 0);
  set id(int v) { $_setSignedInt32(0, v); }
  bool hasId() => $_has(0);
  void clearId() => clearField(1);

  $0.Category get category => $_getN(1);
  set category($0.Category v) { setField(2, v); }
  bool hasCategory() => $_has(1);
  void clearCategory() => clearField(2);

  String get name => $_getS(2, '');
  set name(String v) { $_setString(2, v); }
  bool hasName() => $_has(2);
  void clearName() => clearField(3);

  List<String> get photoUrls => $_getList(3);

  List<$1.Tag> get tags => $_getList(4);

  String get status => $_getS(5, '');
  set status(String v) { $_setString(5, v); }
  bool hasStatus() => $_has(5);
  void clearStatus() => clearField(6);
}

