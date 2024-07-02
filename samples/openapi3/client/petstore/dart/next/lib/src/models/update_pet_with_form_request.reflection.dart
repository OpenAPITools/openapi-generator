// Model reflection

part of 'update_pet_with_form_request.dart';


//class reflection

class UpdatePetWithFormRequestReflection extends ClassReflection<UpdatePetWithFormRequest> {
  static const instance = UpdatePetWithFormRequestReflection._(
    name: PropertyReflection(
      dartName: r'name',
      nullable: false,
      required: false,
      oasName: r'name',
      oasType: r'string',
      pattern: null,
    ),
    status: PropertyReflection(
      dartName: r'status',
      nullable: false,
      required: false,
      oasName: r'status',
      oasType: r'string',
      pattern: null,
    ),
  );
  const UpdatePetWithFormRequestReflection._({
    required this.name,
  
    required this.status,
  });

  final PropertyReflection<UndefinedWrapper<
            String
>> name;
  final PropertyReflection<UndefinedWrapper<
            String
>> status;

  @override
  List<PropertyReflection> get members => [
    name,
status,
  ];

  @override
  bool Function(Object? src) get canDeserializeFunction =>
    (src) => UpdatePetWithFormRequest.canDeserialize(src);
  @override
  UpdatePetWithFormRequest Function(Object? src) get deserializeFunction =>
      (src) => UpdatePetWithFormRequest.deserialize(src);

  @override
  Object? Function(UpdatePetWithFormRequest src) get serializeFunction =>
      (src) => src.serialize();
}

class UpdatePetWithFormRequestXmlReflection {
    const UpdatePetWithFormRequestXmlReflection();
}

