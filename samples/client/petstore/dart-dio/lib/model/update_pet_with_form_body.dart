        import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'update_pet_with_form_body.g.dart';

abstract class UpdatePetWithFormBody implements Built<UpdatePetWithFormBody, UpdatePetWithFormBodyBuilder> {

    /* Updated name of the pet */
        @nullable
    @BuiltValueField(wireName: r'name')
    String get name;
    /* Updated status of the pet */
        @nullable
    @BuiltValueField(wireName: r'status')
    String get status;

    // Boilerplate code needed to wire-up generated code
    UpdatePetWithFormBody._();

    factory UpdatePetWithFormBody([updates(UpdatePetWithFormBodyBuilder b)]) = _$UpdatePetWithFormBody;
    static Serializer<UpdatePetWithFormBody> get serializer => _$updatePetWithFormBodySerializer;
}

