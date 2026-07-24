import {
  HttpAuthorsService,
  HttpCatalogService,
  type FetchLike,
} from "../builds/default/http-client";

/**
 * Build a FetchLike that returns a canned wire payload, so the generated client
 * runs its real runtime path: response.json() -> Zod schema validation -> DTO
 * mapping, plus the error path when validation fails.
 */
function makeFetch(payload: unknown, status = 200): FetchLike {
  return function <T>(): Promise<{ json(): Promise<T>; status: number }> {
    return Promise.resolve({ status, json: async () => payload as T });
  };
}

const bookDto = {
  id: 1,
  title: "A Wizard of Earthsea",
  genre: "FICTION",
  author: { id: 1, name: "Ursula K. Le Guin" },
  pageCount: 183,
};

describe("typescript-express-zod-client (bookstore)", () => {
  it("parses and maps a valid response into a domain object", async () => {
    const catalog = new HttpCatalogService(makeFetch(bookDto), { root: "http://api.test" });
    const book = await catalog.getBook({ bookId: 1 });
    expect(book).toMatchObject({
      id: 1,
      title: "A Wizard of Earthsea",
      genre: "FICTION",
      author: { name: "Ursula K. Le Guin" },
    });
  });

  it("routes a schema-invalid response through the error mapper", async () => {
    // Missing the required `title` -> Zod validation fails -> { errors }.
    const catalog = new HttpCatalogService(makeFetch({ id: 1 }), {});
    const result = (await catalog.getBook({ bookId: 1 })) as unknown as {
      errors?: unknown[];
    };
    expect(Array.isArray(result.errors)).toBe(true);
    expect(result.errors!.length).toBeGreaterThan(0);
  });

  it("honors a custom mapZodError override on validation failure", async () => {
    const catalog = new HttpCatalogService(makeFetch({ id: 1 }), {
      mapZodError: () => [{ message: "custom-mapped" }],
    });
    const result = (await catalog.getBook({ bookId: 1 })) as unknown as {
      errors?: Array<{ message?: string }>;
    };
    expect(result.errors?.[0]?.message).toBe("custom-mapped");
  });

  it("parses a discriminated-union response", async () => {
    const catalog = new HttpCatalogService(
      makeFetch({ type: "BOOK_ADDED", bookId: 5 }),
      {},
    );
    const event = await catalog.getLatestBookEvent({ bookId: 5 });
    expect(event).toEqual({ type: "BOOK_ADDED", bookId: 5 });
  });

  it("works across generated service classes (authors)", async () => {
    const authors = new HttpAuthorsService(makeFetch({ id: 9, name: "Le Guin" }), {});
    const author = await authors.getAuthor({ authorId: 9 });
    expect(author).toMatchObject({ id: 9, name: "Le Guin" });
  });
});
