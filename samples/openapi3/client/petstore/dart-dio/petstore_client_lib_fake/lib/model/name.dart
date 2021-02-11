//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'name.g.dart';

abstract class Name implements Built<Name, NameBuilder> {

    @nullable
    @BuiltValueField(wireName: r'name')
    int get name;

    @nullable
    @BuiltValueField(wireName: r'snake_case')
    int get snakeCase;

    @nullable
    @BuiltValueField(wireName: r'property')
    String get property;

    @nullable
    @BuiltValueField(wireName: r'123Number')
    int get n123number;

    // Boilerplate code needed to wire-up generated code
    Name._();

    static void _initializeBuilder(NameBuilder b) => b;

    factory Name([void updates(NameBuilder b)]) = _$Name;
    static Serializer<Name> get serializer => _$nameSerializer;
}

