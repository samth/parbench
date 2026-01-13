# Racket Proxy Authentication Fix

This patch adds HTTP proxy authentication support to Racket's `net/url` library.

## Problem

Racket's networking library (`net/url`, `net/http-client`) does not support authenticated HTTP proxies. When using a proxy that requires authentication (via `Proxy-Authorization` header), HTTPS CONNECT tunneling fails with:

```
make-ports: HTTP CONNECT failed: HTTP/1.1 401 Unauthorized
```

## Solution

Two files were modified:

### `http-client.rkt`

1. Added `require net/base64` for encoding credentials
2. Modified `http-conn-CONNECT-tunnel` to accept optional `#:proxy-auth` parameter
3. When credentials are provided, sends `Proxy-Authorization: Basic <base64-encoded-credentials>` header

### `url.rkt`

1. Added `proxy-credentials-table` hash to store credentials extracted from proxy URLs
2. Modified `env->c-p-s-entries` to extract username (which contains `user:password`) from proxy environment variables
3. Added `proxy-credentials-for` function to look up credentials by URL scheme
4. Modified `make-ports` to pass credentials to `http-conn-CONNECT-tunnel`

## Installation

Replace the files in your Racket installation:

```bash
cp http-client.rkt $(racket -e '(displayln (collection-file-path "http-client.rkt" "net"))')
cp url.rkt $(racket -e '(displayln (collection-file-path "url.rkt" "net"))')

# Recompile
raco setup --pkgs net-lib
```

## Usage

Set proxy environment variables with credentials in the URL:

```bash
export HTTPS_PROXY="http://user:password@proxy-host:port"
export HTTP_PROXY="http://user:password@proxy-host:port"
```

The credentials are automatically extracted and used for proxy authentication.

## Compatibility

- Tested with Racket 9.0
- Backward compatible (works without proxy auth when no credentials provided)
