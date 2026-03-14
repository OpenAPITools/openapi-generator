import { Injectable } from '@nestjs/common';

@Injectable()
export class TestService {
  hello(): string {
    return "Hello World";
  }
}