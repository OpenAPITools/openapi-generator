import 'package:jaguar_serializer/jaguar_serializer.dart';


part 'update_pet_with_form_body.jser.dart';

class UpdatePetWithFormBody {
   /* Updated name of the pet */
  @Alias('name', isNullable: false,  )
  final String name;
   /* Updated status of the pet */
  @Alias('status', isNullable: false,  )
  final String status;
  

  UpdatePetWithFormBody(
      

{
     this.name = null,  
     this.status = null 
    
    }
  );

  @override
  String toString() {
    return 'UpdatePetWithFormBody[name=$name, status=$status, ]';
  }
}

@GenSerializer(nullableFields: true)
class UpdatePetWithFormBodySerializer extends Serializer<UpdatePetWithFormBody> with _$UpdatePetWithFormBodySerializer {

}

