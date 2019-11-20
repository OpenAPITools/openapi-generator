import { Configuration } from "@swagger/typescript-fetch-petstore";
import fetch from 'node-fetch';

export const config = new Configuration({ fetchApi: fetch as any });
