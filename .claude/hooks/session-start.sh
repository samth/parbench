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

  # Persist PATH for future shell invocations
  if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
    echo "export PATH=\"$RACKET_DIR/bin:\$PATH\"" >> "$CLAUDE_ENV_FILE"
  elif ! grep -q "$RACKET_DIR/bin" ~/.bashrc 2>/dev/null; then
    # Fallback: add to .bashrc if CLAUDE_ENV_FILE not available
    echo "export PATH=\"$RACKET_DIR/bin:\$PATH\"" >> ~/.bashrc
  fi
fi

# Verify Racket is available
if ! command -v racket &> /dev/null; then
  echo "ERROR: Racket installation failed"
  exit 1
fi

echo "Racket $(racket --version) ready"

# Install dependencies via git clone (avoids proxy auth issues with package catalog)
PROJECT_DIR="${CLAUDE_PROJECT_DIR:-$(pwd)}"
DEPS_DIR="/tmp/racket-deps"

# Check if parbench is already installed
if raco pkg show parbench 2>/dev/null | grep -q "parbench"; then
  echo "parbench package already installed"
else
  echo "Installing dependencies via git clone..."
  mkdir -p "$DEPS_DIR"

  # Clone and install rackcheck-lib if needed
  if ! raco pkg show rackcheck-lib 2>/dev/null | grep -q "rackcheck-lib"; then
    if [ ! -d "$DEPS_DIR/rackcheck" ]; then
      echo "Cloning rackcheck..."
      git clone --depth 1 https://github.com/Bogdanp/rackcheck.git "$DEPS_DIR/rackcheck" 2>&1
    fi
    echo "Installing rackcheck-lib..."
    raco pkg install --batch --no-docs --link "$DEPS_DIR/rackcheck/rackcheck-lib" 2>&1
  fi

  # Clone and install recspecs if needed
  if ! raco pkg show recspecs 2>/dev/null | grep -q "recspecs"; then
    if [ ! -d "$DEPS_DIR/recspecs" ]; then
      echo "Cloning recspecs..."
      git clone --depth 1 https://github.com/samth/recspecs.git "$DEPS_DIR/recspecs" 2>&1
    fi
    echo "Installing recspecs packages..."
    raco pkg install --batch --no-docs --link "$DEPS_DIR/recspecs/recspecs-lib" "$DEPS_DIR/recspecs/recspecs" 2>&1
  fi

  # Link the current project as a package
  echo "Linking project as package..."
  raco pkg install --batch --no-docs --link "$PROJECT_DIR" 2>&1
fi

echo "Setup complete"
