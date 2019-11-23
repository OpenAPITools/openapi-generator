///
//  Generated code. Do not modify.
//  source: category.proto
///
// ignore_for_file: non_constant_identifier_names,library_prefixes,unused_import

// ignore: UNUSED_SHOWN_NAME
import 'dart:core' show int, bool, double, String, List, override;

import 'package:protobuf/protobuf.dart' as $pb;

class Category extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = new $pb.BuilderInfo('Category')
    ..a<int>(1, 'id', $pb.PbFieldType.O3)
    ..aOS(2, 'name')
    ..hasRequiredFields = false
  ;

  Category() : super();
  Category.fromBuffer(List<int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) : super.fromBuffer(i, r);
  Category.fromJson(String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) : super.fromJson(i, r);
  Category clone() => new Category()..mergeFromMessage(this);
  Category copyWith(void Function(Category) updates) => super.copyWith((message) => updates(message as Category));
  $pb.BuilderInfo get info_ => _i;
  static Category create() => new Category();
  static $pb.PbList<Category> createRepeated() => new $pb.PbList<Category>();
  static Category getDefault() => _defaultInstance ??= create()..freeze();
  static Category _defaultInstance;
  static void $checkItem(Category v) {
    if (v is! Category) $pb.checkItemFailed(v, _i.qualifiedMessageName);
  }

  int get id => $_get(0, 0);
  set id(int v) { $_setSignedInt32(0, v); }
  bool hasId() => $_has(0);
  void clearId() => clearField(1);

  String get name => $_getS(1, '');
  set name(String v) { $_setString(1, v); }
  bool hasName() => $_has(1);
  void clearName() => clearField(2);
}

