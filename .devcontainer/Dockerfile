FROM racket/racket:snapshot-full

# Install additional development tools
RUN apt-get update && apt-get install -y \
    git \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /workspace

# Install the package from the checkout
COPY . /workspace
RUN raco pkg install --auto --name parbench
