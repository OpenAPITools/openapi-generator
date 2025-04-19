import { Component } from '@angular/core';
import { RouterOutlet } from '@angular/router';
import { type Car, DefaultService } from '@swagger/api-client';


@Component({
  selector: 'app-root',
  imports: [RouterOutlet],
  templateUrl: './app.component.html',
  styleUrl: './app.component.css'
})
export class AppComponent {
  title = 'typescript-angular-deep-object-tests';
  result: any;

  constructor(defaultService: DefaultService) {
    defaultService.getCars({ make: 'bmw', model: '319' }).subscribe(result => this.result = result);
  }
}
