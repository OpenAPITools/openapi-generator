import 'package:jaguar_serializer/jaguar_serializer.dart';

import 'package:swagger/model/currency.dart';
part 'amount.jser.dart';

class Amount {
   /* some description  */
  final double value;
   // range from 0.01 to 1000000000000000//
  final Currency currency;
  

  Amount(
    

{
    
     this.value = null,  
     this.currency = null 
    }
  );

  @override
  String toString() {
    return 'Amount[value=$value, currency=$currency, ]';
  }
}

@GenSerializer()
class AmountSerializer extends Serializer<Amount> with _$AmountSerializer {

}
