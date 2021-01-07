//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'tag.g.dart';

abstract class Tag implements Built<Tag, TagBuilder> {

    @nullable
    @BuiltValueField(wireName: r'id')
    int get id;

    @nullable
    @BuiltValueField(wireName: r'name')
    String get name;

    // Boilerplate code needed to wire-up generated code
    Tag._();

    static void _initializeBuilder(TagBuilder b) => b;

    factory Tag([void updates(TagBuilder b)]) = _$Tag;
    static Serializer<Tag> get serializer => _$tagSerializer;
}

