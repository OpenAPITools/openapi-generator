import { HttpParameterCodec } from '@angular/common/http';

/**
* Custom HttpParameterCodec
* Workaround for https://github.com/angular/angular/issues/18261
*/
export class CustomHttpParameterCodec implements HttpParameterCodec {
    encodeKey(k: string): string {
        return encodeURIComponent(k);
    }
    encodeValue(v: string): string {
        return encodeURIComponent(k);
    }
}

