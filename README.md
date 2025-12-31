# L Language

L is a very simple untyped functional programming language based on lambda calculus for evaluating simple math expressions.

This project puts a web interface to the interpreter.

![L Language Playground](docs/screenshot.png)

## 🚀 Project Setup & Run Guide

This project contains:

- ✅ A **Haskell Scotty server** (`l-lang`) with a **built-in Web Playground**
- ✅ An improved **CLI REPL** with interactive features
- ✅ A **Vite + React client** (`web-client`)
- ✅ Docker + Docker Compose configuration to run both together
- ✅ **Standalone mode** for deploying without the Haskell server

---

## 🎮 Built-in Haskell Playground & REPL

The Haskell application now includes a fully-featured CLI REPL and a separate Web Playground modeled after the Rust Playground.

### 🖥️ CLI REPL

Run the interactive Read-Eval-Print Loop:

```bash
stack run -- -r
# or simply
stack run
```

**Features:**

- **Interactive Commands**: `:help`, `:env`, `:quit`
- **Trace Mode**: Toggle step-by-step evaluation traces with `:trace`
- **Examples**: Load built-in examples with `:load <name>` (try `factorial`, `fibonacci`, `map`, `foldr`)

### 🌐 Web Playground

Start the built-in web server:

```bash
stack run -- -w
```

Visit **[http://localhost:3000](http://localhost:3000)** to access the new UI:

- **Modern Design**: Rust Playground-inspired aesthetic with split panes.
- **Visual Trace**: See how your code evaluates step-by-step.
- **Environment View**: Inspect the final state of your variables.
- **Tailwind CSS**: Clean, responsive styling.

---

---

## 📦 Project Structure

```
./            → Haskell Scotty app (with Dockerfile)
/web-client        → Vite + React app (with Dockerfile)
docker-compose.yml → top-level orchestration
```

---

## 🔧 Prerequisites

- Install **Docker** → [https://docs.docker.com/get-docker/](https://docs.docker.com/get-docker/)
- Install **Docker Compose** → included in recent Docker Desktop versions

For standalone mode:

- Install **Node.js** (v18 or higher) → [https://nodejs.org/](https://nodejs.org/)

---

## 🏗 How to Build & Run

### Option 1: Full Stack with Docker (Recommended for Development)

1️⃣ Clone the repository:

```bash
git clone https://github.com/claeusdev/l-lang
cd l-lang
```

2️⃣ Build and start everything:

```bash
docker compose up --build
```

✅ This will:

- Build the **server** from the Haskell Dockerfile.
- Build the **client** from the React Dockerfile.
- Expose:

  - **Server** → [http://localhost:3000](http://localhost:3000)
  - **Client (Vite dev server)** → [http://localhost:5173](http://localhost:5173)

### Option 2: Standalone React App (No Haskell Server Required)

🚀 **Perfect for production deployments or when you just want to try the L language!**

1️⃣ Navigate to the web client:

```bash
cd web-client
```

2️⃣ Install dependencies:

```bash
npm install
```

3️⃣ Build the standalone version:

```bash
# Using the build script (recommended)
./build-standalone.sh

# Or manually
npm run build:standalone
```

4️⃣ Serve the standalone app:

```bash
# Using the built-in server
npm run serve:standalone

# Or using the executable script directly
./standalone-server.js

# Or serve with any static file server
npx serve dist
```

✅ **Standalone Features:**

- 🔄 **Offline L language interpreter** - No backend required!
- 📝 **Sample code snippets** with valid L language syntax
- 💾 **Local storage** for saving your code snippets
- 🎨 **Full UI functionality** including Monaco editor
- 📱 **Responsive design** works on mobile and desktop

**Access the standalone app:** [http://localhost:8080](http://localhost:8080)

---

## 🌐 Access the Apps

| Service           | Local URL                                      | Description                    |
| ----------------- | ---------------------------------------------- | ------------------------------ |
| Full Stack Client | [http://localhost:5173](http://localhost:5173) | React app with Haskell backend |
| Haskell Server    | [http://localhost:3000](http://localhost:3000) | API server (development)       |
| Standalone App    | [http://localhost:8080](http://localhost:8080) | Self-contained React app       |

---

## 📚 L Language Examples

The standalone app comes with built-in sample snippets demonstrating L language syntax:

### Basic Arithmetic

```l
x = 10
y = 20
x + y
x * y
```

### Lambda Functions

```l
double = \x -> x * 2
triple = \x -> x * 3
square = \x -> x * x

double 5
triple 4
square 6
```

### Function Composition

```l
double = \x -> x * 2
triple = \x -> x * 3
compose = \f -> \g -> \x -> f (g x)

doubleTriple = compose double triple
doubleTriple 5
```

### Let Expressions

```l
double = \x -> x * 2
let x = 10 in double x
let y = 5 in let z = y + 3 in z * 2
```

### Complex Example from test.l

```l
double = \x -> x * 2
triple = \x -> x * x * x
compose = \f -> \g -> \x -> f (g x)

composedDoubleTriple = compose double
trippledDoubledComposed = composedDoubleTriple triple

trippledDoubledComposed 5

let x = 100 in trippledDoubledComposed (double 10)
```

---

## 🔄 Common Commands

### Full Stack Development

- **Stop the services:**

  ```bash
  docker-compose down
  ```

- **Rebuild only:**

  ```bash
  docker-compose build
  ```

- **View logs:**

  ```bash
  docker-compose logs -f
  ```

- **Restart with rebuild:**
  ```bash
  docker-compose up --build
  ```

### Standalone Development

- **Development mode:**

  ```bash
  cd web-client
  npm run dev
  ```

- **Build for production:**

  ```bash
  cd web-client
  npm run build:standalone
  ```

- **Serve built app:**
  ```bash
  cd web-client
  npm run serve:standalone
  ```

---

## 🚢 Deployment Options

### Traditional Static Hosting

The standalone build creates a `dist` folder that can be deployed to any static hosting service:

- Netlify
- Vercel
- GitHub Pages
- AWS S3 + CloudFront
- Any web server (Apache, Nginx)

### Self-Hosted

Use the included `standalone-server.js` script on any Node.js server:

```bash
# On your server
cd web-client
npm install --production
npm run build:standalone
node standalone-server.js
```

### Docker Standalone

Build a lightweight Docker image for the standalone app:

```dockerfile
FROM node:18-alpine
WORKDIR /app
COPY web-client/package*.json ./
RUN npm install --production
COPY web-client/dist ./dist
COPY web-client/standalone-server.js ./
EXPOSE 8080
CMD ["node", "standalone-server.js"]
```

---
