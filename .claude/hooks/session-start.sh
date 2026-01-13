#!/bin/bash
set -euo pipefail

# Only run in Claude Code on the web
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

echo "Setting up Racket environment..."

# Check if Racket is installed
if ! command -v racket &> /dev/null; then
  echo "Installing Racket..."

  # Install Racket using the installer script
  # Using full installation to include plot libraries and other common packages
  # Installing to user directory (no sudo required)
  RACKET_DIR="$HOME/.local/racket"

  wget -q https://mirror.racket-lang.org/installers/9.0/racket-9.0-x86_64-linux-cs.sh
  chmod +x racket-9.0-x86_64-linux-cs.sh
  ./racket-9.0-x86_64-linux-cs.sh --in-place --dest "$RACKET_DIR"
  rm racket-9.0-x86_64-linux-cs.sh

  # Add Racket to PATH for this session
  export PATH="$RACKET_DIR/bin:$PATH"

  # Persist PATH for future sessions
  if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
    echo "export PATH=\"$RACKET_DIR/bin:\$PATH\"" >> "$CLAUDE_ENV_FILE"
  fi

  echo "✅ Racket installed successfully"
else
  echo "✅ Racket already installed"
fi

echo "Installing package dependencies..."

# Install package dependencies
# Using --auto to automatically install dependencies
# Using --batch for non-interactive mode
# Using --no-docs to skip documentation (faster installation)
raco pkg install --auto --batch --no-docs

echo "✅ Package dependencies installed successfully"
