        import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'tag.g.dart';

abstract class Tag implements Built<Tag, TagBuilder> {

    
        @nullable

    
    @BuiltValueField(wireName: 'id')
    int get id;
    
        @nullable

    
    @BuiltValueField(wireName: 'name')
    String get name;

    // Boilerplate code needed to wire-up generated code
    Tag._();

    factory Tag([updates(TagBuilder b)]) = _$Tag;
    static Serializer<Tag> get serializer => _$tagSerializer;

}

