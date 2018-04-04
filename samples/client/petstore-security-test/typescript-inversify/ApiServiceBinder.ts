import {interfaces} from "inversify";

import { FakeService } from './api/fake.service';

export class ApiServiceBinder {
    public static with(container: interfaces.Container) {
        container.bind<FakeService>("FakeService").to(FakeService).inSingletonScope();
    }
}
