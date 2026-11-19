# Isolated declaration regression test

Regression coverage for #25172. Compile all checked-in TypeScript Fetch runtime samples with strict declaration emission and `isolatedDeclarations` enabled. The consumer fixture also checks that the middleware methods preserve a subclass's type.

From this directory, run:

```sh
npm ci
npm test
```

Declaration output is written under `out/issue-25172-regression/` at the repository root. TypeScript is pinned to a version supporting `isolatedDeclarations`. Exact optional property types and unchecked indexed access are disabled to keep this test focused on declaration emission.
