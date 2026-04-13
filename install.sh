#!/bin/sh
set -e

echo "=== xmas setup ==="

# Java
if ! command -v java >/dev/null 2>&1; then
  echo "Installing OpenJDK 21..."
  sudo apt-get update -qq && sudo apt-get install -y -qq openjdk-21-jdk
else
  echo "Java: $(java -version 2>&1 | head -1)"
fi

# Clojure CLI
if ! command -v clojure >/dev/null 2>&1; then
  echo "Installing Clojure CLI..."
  curl -sL -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh
  chmod +x linux-install.sh
  sudo ./linux-install.sh
  rm linux-install.sh
else
  echo "Clojure: $(clojure --version 2>&1)"
fi

# GraalVM native-image (optional)
if ! command -v native-image >/dev/null 2>&1; then
  echo "native-image not found — skipping (optional, for fast startup)"
  echo "  Install: sdk install java 21.0.5-graal && gu install native-image"
else
  echo "native-image: $(native-image --version 2>&1 | head -1)"
fi

# Fetch deps
echo "Fetching Clojure deps..."
cd "$(dirname "$0")"
clojure -P

echo "=== Done. Run: make run ==="
