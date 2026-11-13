# Unchecked indexed access regression test

Regression coverage for #25643. Type-check all checked-in TypeScript Fetch runtime samples with `strict` and `noUncheckedIndexedAccess` enabled. A consuming project compiles the runtime with its own compiler options, so the runtime must compile when this option is enabled.

From this directory, run:

```sh
npm ci
npm test
```
