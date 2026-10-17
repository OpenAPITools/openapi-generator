import request from "supertest";

import { createApp } from "../src/app";

const app = createApp();

describe("typescript-express-zod-server (bookstore)", () => {
  it("GET /books/{bookId} returns the book; the path param is coerced to a number", async () => {
    const res = await request(app).get("/books/42");
    expect(res.status).toBe(200);
    expect(res.body).toMatchObject({ id: 42, title: "A Wizard of Earthsea", genre: "FICTION" });
    // nullable $ref property is mapped through the DTO layer
    expect(res.body.author).toMatchObject({ name: "Ursula K. Le Guin" });
  });

  it("POST /books validates the request body and returns the created book", async () => {
    const res = await request(app)
      .post("/books")
      .send({ title: "The Dispossessed", genre: "FICTION", authorId: 1 });
    expect(res.status).toBe(200);
    expect(res.body).toMatchObject({ title: "The Dispossessed" });
  });

  it("DELETE /books/{bookId} succeeds for a void operation", async () => {
    const res = await request(app).delete("/books/1");
    expect(res.status).toBe(200);
  });

  it("GET /books/{bookId}/latest-event returns a discriminated-union member", async () => {
    const res = await request(app).get("/books/5/latest-event");
    expect(res.status).toBe(200);
    expect(res.body).toEqual({ type: "BOOK_ADDED", bookId: 5 });
  });

  it("GET /authors/{authorId} returns the author", async () => {
    const res = await request(app).get("/authors/9");
    expect(res.status).toBe(200);
    expect(res.body).toMatchObject({ id: 9, name: "Ursula K. Le Guin" });
  });

  it("rejects a path param that fails schema validation with a 4xx", async () => {
    const res = await request(app).get("/books/not-a-number");
    expect(res.status).toBeGreaterThanOrEqual(400);
    expect(res.status).toBeLessThan(500);
  });

  it("returns 405 Method Not Allowed for an unsupported method on a known route", async () => {
    const res = await request(app).patch("/books/1");
    expect(res.status).toBe(405);
  });
});
