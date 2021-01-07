//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'has_only_read_only.g.dart';

abstract class HasOnlyReadOnly implements Built<HasOnlyReadOnly, HasOnlyReadOnlyBuilder> {

    @nullable
    @BuiltValueField(wireName: r'bar')
    String get bar;

    @nullable
    @BuiltValueField(wireName: r'foo')
    String get foo;

    // Boilerplate code needed to wire-up generated code
    HasOnlyReadOnly._();

    static void _initializeBuilder(HasOnlyReadOnlyBuilder b) => b;

    factory HasOnlyReadOnly([void updates(HasOnlyReadOnlyBuilder b)]) = _$HasOnlyReadOnly;
    static Serializer<HasOnlyReadOnly> get serializer => _$hasOnlyReadOnlySerializer;
}

