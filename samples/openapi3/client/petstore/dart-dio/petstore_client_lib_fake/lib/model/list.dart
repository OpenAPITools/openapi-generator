import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'list.g.dart';

abstract class List implements Built<List, ListBuilder> {

    
    @nullable
    @BuiltValueField(wireName: r'123-list')
    String get n123list;

    // Boilerplate code needed to wire-up generated code
    List._();

    factory List([updates(ListBuilder b)]) = _$List;
    static Serializer<List> get serializer => _$listSerializer;
}

