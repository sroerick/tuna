# pp-slice T4: reverse-proxy fragment (operator step; no secrets here).

Tuna speaks plain HTTP on loopback (`TUNA_HTTP_PORT`). Terminate TLS at
the reverse proxy and forward the original host so the generated
`/agent.txt` and `/.well-known/agent.json` base URLs are correct
(`Agent.base_url_of_headers` prefers `X-Forwarded-Host` /
`X-Forwarded-Proto`, then `Host`).

## Caddy

```
tuna.example.com {
    encode gzip
    reverse_proxy 127.0.0.1:18092 {
        header_up X-Forwarded-Host {host}
        header_up X-Forwarded-Proto {scheme}
    }
}
```

Caddy sets `X-Forwarded-*` by default; the explicit `header_up` lines
are shown for clarity.

## nginx

```
server {
    listen 443 ssl http2;
    server_name tuna.example.com;
    ssl_certificate     /etc/letsencrypt/live/tuna.example.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/tuna.example.com/privkey.pem;

    location / {
        proxy_pass         http://127.0.0.1:18092;
        proxy_set_header   Host              $host;
        proxy_set_header   X-Forwarded-Host  $host;
        proxy_set_header   X-Forwarded-Proto $scheme;
        proxy_set_header   X-Real-IP         $remote_addr;
    }
}
```

## The flip (one operator command)

On the host that serves the slice:

```
scripts/serve.sh start          # PG + migrations + build + public + serve
scripts/serve.sh status         # :18092 health
```

Then point the proxy at `127.0.0.1:$TUNA_HTTP_PORT`. TLS, the public
DNS name, and the certificate stay with the proxy — tuna never sees a
private key. There is no OAuth/JWT in v1; browser sessions and agent
bearer tokens are the only credentials (see README "Honest limitations").
