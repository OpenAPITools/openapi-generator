import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'array_of_array_of_number_only.g.dart';

abstract class ArrayOfArrayOfNumberOnly implements Built<ArrayOfArrayOfNumberOnly, ArrayOfArrayOfNumberOnlyBuilder> {

    
    @nullable
    @BuiltValueField(wireName: r'ArrayArrayNumber')
    BuiltList<BuiltList<num>> get arrayArrayNumber;

    // Boilerplate code needed to wire-up generated code
    ArrayOfArrayOfNumberOnly._();

    factory ArrayOfArrayOfNumberOnly([updates(ArrayOfArrayOfNumberOnlyBuilder b)]) = _$ArrayOfArrayOfNumberOnly;
    static Serializer<ArrayOfArrayOfNumberOnly> get serializer => _$arrayOfArrayOfNumberOnlySerializer;
}

