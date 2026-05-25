export class HttpError extends Error {
  readonly status: number;
  readonly error: string;

  constructor(status: number, error: string, message: string) {
    super(message);
    this.name = "HttpError";
    this.status = status;
    this.error = error;
  }
}
