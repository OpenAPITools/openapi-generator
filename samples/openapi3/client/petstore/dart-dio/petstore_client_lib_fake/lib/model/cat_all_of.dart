//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'cat_all_of.g.dart';

abstract class CatAllOf implements Built<CatAllOf, CatAllOfBuilder> {

    @nullable
    @BuiltValueField(wireName: r'declawed')
    bool get declawed;

    // Boilerplate code needed to wire-up generated code
    CatAllOf._();

    static void _initializeBuilder(CatAllOfBuilder b) => b;

    factory CatAllOf([void updates(CatAllOfBuilder b)]) = _$CatAllOf;
    static Serializer<CatAllOf> get serializer => _$catAllOfSerializer;
}

