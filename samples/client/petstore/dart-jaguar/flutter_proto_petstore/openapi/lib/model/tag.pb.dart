///
//  Generated code. Do not modify.
//  source: tag.proto
///
// ignore_for_file: non_constant_identifier_names,library_prefixes,unused_import

// ignore: UNUSED_SHOWN_NAME
import 'dart:core' show int, bool, double, String, List, override;

import 'package:protobuf/protobuf.dart' as $pb;

class Tag extends $pb.GeneratedMessage {
  static final $pb.BuilderInfo _i = new $pb.BuilderInfo('Tag')
    ..a<int>(1, 'id', $pb.PbFieldType.O3)
    ..aOS(2, 'name')
    ..hasRequiredFields = false
  ;

  Tag() : super();
  Tag.fromBuffer(List<int> i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) : super.fromBuffer(i, r);
  Tag.fromJson(String i, [$pb.ExtensionRegistry r = $pb.ExtensionRegistry.EMPTY]) : super.fromJson(i, r);
  Tag clone() => new Tag()..mergeFromMessage(this);
  Tag copyWith(void Function(Tag) updates) => super.copyWith((message) => updates(message as Tag));
  $pb.BuilderInfo get info_ => _i;
  static Tag create() => new Tag();
  static $pb.PbList<Tag> createRepeated() => new $pb.PbList<Tag>();
  static Tag getDefault() => _defaultInstance ??= create()..freeze();
  static Tag _defaultInstance;
  static void $checkItem(Tag v) {
    if (v is! Tag) $pb.checkItemFailed(v, _i.qualifiedMessageName);
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

