//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'class_model.g.dart';

/// Model for testing model with \"_class\" property
///
/// Properties:
/// * [classField] 
abstract class ClassModel implements Built<ClassModel, ClassModelBuilder> {
    @BuiltValueField(wireName: r'_class')
    String? get classField;

    ClassModel._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(ClassModelBuilder b) => b;

    factory ClassModel([void updates(ClassModelBuilder b)]) = _$ClassModel;

    @BuiltValueSerializer(custom: true)
    static Serializer<ClassModel> get serializer => _$ClassModelSerializer();
}

class _$ClassModelSerializer implements StructuredSerializer<ClassModel> {
    @override
    final Iterable<Type> types = const [ClassModel, _$ClassModel];

    @override
    final String wireName = r'ClassModel';

    @override
    Iterable<Object?> serialize(Serializers serializers, ClassModel object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.classField != null) {
            result
                ..add(r'_class')
                ..add(serializers.serialize(object.classField,
                    specifiedType: const FullType(String)));
        }
        return result;
    }

    @override
    ClassModel deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = ClassModelBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'_class':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(String)) as String;
                    result.classField = valueDes;
                    break;
            }
        }
        return result.build();
    }
}

