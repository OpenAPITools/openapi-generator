//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'foo.g.dart';

abstract class Foo implements Built<Foo, FooBuilder> {

    @nullable
    @BuiltValueField(wireName: r'bar')
    String get bar;

    // Boilerplate code needed to wire-up generated code
    Foo._();

    static void _initializeBuilder(FooBuilder b) => b
        ..bar = 'bar';

    factory Foo([void updates(FooBuilder b)]) = _$Foo;
    static Serializer<Foo> get serializer => _$fooSerializer;
}

