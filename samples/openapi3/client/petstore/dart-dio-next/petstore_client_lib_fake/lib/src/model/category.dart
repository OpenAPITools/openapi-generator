//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'category.g.dart';

/// Category
///
/// Properties:
/// * [id] 
/// * [name] 
abstract class Category implements Built<Category, CategoryBuilder> {
    @BuiltValueField(wireName: r'id')
    int? get id;

    @BuiltValueField(wireName: r'name')
    String get name;

    Category._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(CategoryBuilder b) => b
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
    Iterable<Object?> serialize(Serializers serializers, Category object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
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
    Category deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = CategoryBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'id':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(int)) as int;
                    result.id = valueDes;
                    break;
                case r'name':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.name = valueDes;
                    break;
            }
        }
        return result.build();
    }
}

