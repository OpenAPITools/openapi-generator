import * as FormData from 'form-data';
import { URLSearchParams } from 'url';

const globals = global as any;

globals.FormData = FormData;
globals.URLSearchParams = URLSearchParams;
