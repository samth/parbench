#!/bin/bash
set -euo pipefail

# Only run in Claude Code on the web
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

echo "Setting up Racket environment..."

RACKET_DIR="$HOME/.local/racket"
RACKET_VERSION="9.0"

# Check if Racket is installed
if ! command -v racket &> /dev/null && [ ! -x "$RACKET_DIR/bin/racket" ]; then

  echo "Installing Racket $RACKET_VERSION..."
  INSTALLER_URL="https://mirror.racket-lang.org/installers/$RACKET_VERSION/racket-$RACKET_VERSION-x86_64-linux-cs.sh"
  INSTALLER_FILE="/tmp/racket-installer.sh"

  curl -fsSL -o "$INSTALLER_FILE" "$INSTALLER_URL"
  chmod +x "$INSTALLER_FILE"
  "$INSTALLER_FILE" --in-place --dest "$RACKET_DIR"
  rm -f "$INSTALLER_FILE"

  echo "Racket $RACKET_VERSION installed successfully"
fi

# Set up environment for this session
if [ -x "$RACKET_DIR/bin/racket" ]; then
  export PATH="$RACKET_DIR/bin:$PATH"

  if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
    echo "export PATH=\"$RACKET_DIR/bin:\$PATH\"" >> "$CLAUDE_ENV_FILE"
  fi
fi

# Verify Racket is available
if ! command -v racket &> /dev/null; then
  echo "ERROR: Racket installation failed"
  exit 1
fi

echo "Racket $(racket --version) ready"

# Link the current project as a package
echo "Linking project as package..."
PROJECT_DIR="${CLAUDE_PROJECT_DIR:-$(pwd)}"
raco pkg install --auto --batch --no-docs --link "$PROJECT_DIR" 2>&1 || true

echo "Setup complete"
