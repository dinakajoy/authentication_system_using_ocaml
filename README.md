# Auth System

A simple web application built with OCaml/Dream and PostgreSQL.

<video controls autoplay loop muted playsinline width="600">
  <source src="recording.mp4" type="video/mp4">
  Your browser does not support the video tag.
</video>

---

## Prerequisites

- [PostgreSQL](https://www.postgresql.org/) installed **or** Docker if you prefer running the database via Docker Compose.
- [OCaml](https://ocaml.org/) and [Dune](https://dune.build/) installed.
- `make` installed on your system.

---

## Setup

### 1. Run PostgreSQL

You can either:

- Use your local PostgreSQL installation, or  
- Run via Docker Compose:

```bash
docker-compose up -d
```

This will start a PostgreSQL container using the configuration in docker-compose.yml.

### 2. Configure environment variables

Create a `.env` file in the project root with the following variables:

```bash
DATABASE_URL=postgresql://user101:dev_password@localhost:5433/auth_db
SECRET=your_secret_here
```

> Replace `your_secret_here` with a secure secret key used by the application.

### 3. Install dependencies
```bash
make install
```

This will install OCaml dependencies and any JavaScript build dependencies.

### 4. Build the project
```bash
make build
```

This will build both the server and client assets.

### 5. Run the application

```bash
make serve
```

The server should start, and the application will be available at `http://localhost:8080`.

### Stopping PostgreSQL

If you ran PostgreSQL via Docker Compose, you can stop the container and remove volumes with:
```bash
docker-compose down -v
```

This will stop the database and remove all data volumes created by Docker Compose.

Notes:
- Ensure the client/main.js file exists in the client/ folder before starting the server to serve static assets correctly.
- For development, the Makefile commands handle building and running the project efficiently.
