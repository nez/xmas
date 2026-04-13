#!/bin/sh
set -e

XMAS_DIR="$(cd "$(dirname "$0")" && pwd)"
MIN_JAVA=11

echo "=== xmas setup ==="

# --- Java ---
if command -v java >/dev/null 2>&1; then
  ver=$(java -version 2>&1 | head -1 | sed 's/.*"\([0-9]*\)\..*/\1/')
  if [ "$ver" -ge "$MIN_JAVA" ] 2>/dev/null; then
    echo "[ok] Java $ver"
  else
    echo "[!!] Java $ver found but need >= $MIN_JAVA"
    exit 1
  fi
else
  echo "Installing OpenJDK 21..."
  sudo apt-get update -qq && sudo apt-get install -y -qq openjdk-21-jdk
fi

# --- Clojure CLI ---
if command -v clojure >/dev/null 2>&1; then
  echo "[ok] $(clojure --version 2>&1)"
else
  echo "Installing Clojure CLI..."
  curl -sL -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh
  chmod +x linux-install.sh
  sudo ./linux-install.sh
  rm linux-install.sh
fi

# --- clj-kondo (linter) ---
if command -v clj-kondo >/dev/null 2>&1; then
  echo "[ok] clj-kondo $(clj-kondo --version 2>&1)"
else
  echo "Installing clj-kondo..."
  curl -sL https://raw.githubusercontent.com/clj-kondo/clj-kondo/master/script/install-clj-kondo | sudo bash
fi

# --- rlwrap (for clj REPL) ---
if command -v rlwrap >/dev/null 2>&1; then
  echo "[ok] rlwrap"
else
  echo "Installing rlwrap..."
  sudo apt-get install -y -qq rlwrap 2>/dev/null || echo "[skip] rlwrap — use 'clojure' instead of 'clj'"
fi

# --- stty (required) ---
if command -v stty >/dev/null 2>&1; then
  echo "[ok] stty"
else
  echo "[!!] stty not found — xmas needs it for terminal control"
  exit 1
fi

# --- GraalVM native-image (optional) ---
if command -v native-image >/dev/null 2>&1; then
  echo "[ok] native-image $(native-image --version 2>&1 | head -1)"
else
  echo "[--] native-image not found (optional — 'make native' for instant startup)"
fi

# --- Fetch deps ---
echo ""
echo "Fetching dependencies..."
cd "$XMAS_DIR"
clojure -P 2>/dev/null

# --- Create user dir ---
mkdir -p "$HOME/.xmas"

# --- Verify compilation ---
echo "Compiling..."
if clojure -M -e '(require (quote xmas.ed))' 2>/dev/null; then
  echo "[ok] Compilation"
else
  echo "[!!] Compilation failed — check src/ for errors"
  exit 1
fi

# --- Lint ---
echo "Linting..."
clj-kondo --lint src || true

echo ""
echo "=== Ready ==="
echo "  make dev     — run from source"
echo "  make run     — build + run uberjar"
echo "  make lint    — run clj-kondo"
echo "  make test    — run tests"
echo "  make native  — native binary (needs GraalVM)"
