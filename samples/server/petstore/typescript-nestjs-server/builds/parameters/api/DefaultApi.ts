import { Injectable } from '@nestjs/common';
import { Observable } from 'rxjs';

export type FindPetsByStatusRequestParams = {
    pathDefault: string
    pathNullable: string
    queryDefault: string | undefined
    queryDefaultEnum: 'A' | 'B' | 'C' | undefined
    queryDefaultInt: number | undefined
    headerDefault: string | undefined
    headerDefaultEnum: 'A' | 'B' | 'C' | undefined
    headerDefaultInt: number | undefined
    cookieDefault: string | undefined
    cookieDefaultEnum: 'A' | 'B' | 'C' | undefined
    cookieDefaultInt: number | undefined
    queryNullable: string | null | undefined
    headerNullable: string | null | undefined
    cookieNullable: string | null | undefined
    $query$dollarSign: string | undefined
}

@Injectable()
export abstract class DefaultApi {
  abstract findPetsByStatus(findPetsByStatusRequestParams: FindPetsByStatusRequestParams, request: Request): void | Promise<void> | Observable<void>;


} 