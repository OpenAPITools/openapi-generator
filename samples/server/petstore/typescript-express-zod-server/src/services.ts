/**
 * Hand-written implementations of the generated service interfaces.
 *
 * Each service is a plain object typed as the generated interface, so the
 * method parameter/return types are contextually checked against the generated
 * contract (in ../builds/default). The bodies use trivial in-memory data — the
 * point of the sample is to exercise the generated routing / validation /
 * DTO-mapping pipeline, not to be a real store.
 */
import type {
  AuthorsService,
  CatalogService,
  Author,
  Book,
} from "../builds/default/types";

const sampleAuthor: Author = { id: 1, name: "Ursula K. Le Guin" };

const sampleBook: Book = {
  id: 1,
  title: "A Wizard of Earthsea",
  genre: "FICTION",
  author: sampleAuthor,
  pageCount: 183,
};

export const catalogService: CatalogService = {
  async createBook(params) {
    return {
      ...sampleBook,
      title: params.createBookRequest.title,
      genre: params.createBookRequest.genre,
    };
  },
  async deleteBook() {},
  async getBook(params) {
    return { ...sampleBook, id: params.bookId };
  },
  async getLatestBookEvent(params) {
    return { type: "BOOK_ADDED", bookId: params.bookId };
  },
};

export const authorsService: AuthorsService = {
  async createAuthor(params) {
    return { ...params.author, id: params.author.id ?? 7 };
  },
  async getAuthor(params) {
    return { ...sampleAuthor, id: params.authorId };
  },
};
