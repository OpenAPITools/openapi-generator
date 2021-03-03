//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'category.g.dart';

abstract class Category implements Built<Category, CategoryBuilder> {

    @nullable
    @BuiltValueField(wireName: r'id')
    int get id;

    @BuiltValueField(wireName: r'name')
    String get name;

    Category._();

    static void _initializeBuilder(CategoryBuilder b) => b
        ..name = 'default-name';

    factory Category([void updates(CategoryBuilder b)]) = _$Category;

    @BuiltValueSerializer(custom: true)
    static Serializer<Category> get serializer => _$CategorySerializer();
}

class _$CategorySerializer implements StructuredSerializer<Category> {

    @override
    final Iterable<Type> types = const [Category, _$Category];
    @override
    final String wireName = r'Category';

    @override
    Iterable<Object> serialize(Serializers serializers, Category object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.id != null) {
            result
                ..add(r'id')
                ..add(serializers.serialize(object.id,
                    specifiedType: const FullType(int)));
        }
        result
            ..add(r'name')
            ..add(serializers.serialize(object.name,
                specifiedType: const FullType(String)));
        return result;
    }

    @override
    Category deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = CategoryBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'id':
                    result.id = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    break;
                case r'name':
                    result.name = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    break;
            }
        }
        return result.build();
    }
}

