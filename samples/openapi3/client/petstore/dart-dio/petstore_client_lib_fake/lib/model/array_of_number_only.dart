import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'array_of_number_only.g.dart';

abstract class ArrayOfNumberOnly implements Built<ArrayOfNumberOnly, ArrayOfNumberOnlyBuilder> {

    @nullable
    @BuiltValueField(wireName: r'ArrayNumber')
    BuiltList<num> get arrayNumber;

    // Boilerplate code needed to wire-up generated code
    ArrayOfNumberOnly._();

    factory ArrayOfNumberOnly([updates(ArrayOfNumberOnlyBuilder b)]) = _$ArrayOfNumberOnly;
    static Serializer<ArrayOfNumberOnly> get serializer => _$arrayOfNumberOnlySerializer;
}

