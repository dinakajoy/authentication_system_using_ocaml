# Auth System

A secure authentication system built with **OCaml**, **Dream**, and **PostgreSQL**.

It demonstrates production-style authentication features including email verification, rate limiting, account lockout, and password reset via email.

![Demo](./recording.gif)

---

## Features

### Authentication
- User registration
- Secure login
- Logout
- Password hashing (Argon2)

### Email Verification
- Verification email sent after registration
- Token-based account activation
- Prevents login before verification

### Rate Limiting
- Login attempt tracking per email + IP
- Temporary blocking after too many failed attempts

### Account Lockout
- Automatic lockout after repeated failed logins
- `locked_until` timestamp enforcement
- Automatic unlock after timeout expires

### Password Reset (via Email)
- Secure reset token generation
- Email delivery with reset link
- Token expiration handling
- Safe password update flow

### Session Management
- Database-backed sessions using Dream

---

## Tech Stack

- **OCaml**
- **Dream** (Web framework)
- **Caqti** (Database access)
- **PostgreSQL**
- **Docker Compose**
- **Brevo** (Email delivery)

---

## Prerequisites

You need:

- [PostgreSQL](https://www.postgresql.org/)  
  **OR**
- [Docker](https://www.docker.com/) (recommended)

And:

- [OCaml](https://ocaml.org/)
- [Dune](https://dune.build/)
- `make`

---

## Setup

---

### 1️. Start PostgreSQL

You can either use a local PostgreSQL installation  
**or** run via Docker Compose:

```bash
docker-compose up -d
```

This starts PostgreSQL using the configuration in `docker-compose.yml`.

### 2. Configure Environment Variables

Create a `.env` file in the project root:
```bash
DATABASE_URL=postgresql://user101:dev_password@localhost:5433/auth_db
SECRET=your_secret_here
BREVO_KEY=your_brevo_api_key_here
```

### 3. Install Dependencies

```bash
make install
```

Installs OCaml dependencies and frontend assets.

### 4. Build the Project
```bash
make build
```

Builds:
- Dream server
- Client-side assets

### 5. Run the Application
```bash
make serve
```
The application will be available at:

[http://localhost:8080](http://localhost:8080)

### Stopping PostgreSQL
If you ran PostgreSQL via Docker Compose, you can stop the container and remove volumes with:
```bash
docker-compose down -v
```

---

## Authentication Flow Overview

### Registration
1. User registers
2. Verification email sent
3. Account marked unverified
4. User clicks verification link
5. Account becomes active

### Login
1. Check rate limiting
2. Check account lock
3. Verify password
4. Record failure or success
5. Lock account if threshold exceeded

### Password Reset
1. User requests reset
2. Email with secure token sent
3. Token validated
4. Password updated

## Development Notes
- Ensure client build output exists before starting the server.
- Sessions are stored in PostgreSQL.
- Email sending requires a valid Brevo API key.
- Rate limiting is enforced at the application layer using database tracking.

> This project demonstrates how to build a secure, real-world authentication system in `OCaml` using `Dream` and `PostgreSQL`.