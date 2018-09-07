import 'package:jaguar_serializer/jaguar_serializer.dart';

part 'currency.jser.dart';

class Currency {
  

  Currency(
    


  );

  @override
  String toString() {
    return 'Currency[]';
  }
}

@GenSerializer()
class CurrencySerializer extends Serializer<Currency> with _$CurrencySerializer {

}
