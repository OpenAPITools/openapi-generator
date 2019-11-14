        import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'category.g.dart';

abstract class Category implements Built<Category, CategoryBuilder> {

    
        @nullable

    
    @BuiltValueField(wireName: 'id')
    int get id;
    
        @nullable

    
    @BuiltValueField(wireName: 'name')
    String get name;

    // Boilerplate code needed to wire-up generated code
    Category._();

    factory Category([updates(CategoryBuilder b)]) = _$Category;
    static Serializer<Category> get serializer => _$categorySerializer;

}

