# arrow-protocol

CLOS Apache Arrow / Parquet for [cl-stack](https://github.com/egao1980/cl-stack). Implements `serdes-protocol` `:arrow` (IPC) and `:parquet`.

Native Lisp codec — **not** a CFFI wrap of Arrow C++. Wave-1 covers the type/encoding subset below.

```lisp
(asdf:load-system "arrow-protocol")

(let* ((schema (stack-arrow:make-arrow-schema
                (list (stack-arrow:make-arrow-field :name "n" :type :int32)
                      (stack-arrow:make-arrow-field :name "s" :type :utf8))))
       (table (stack-arrow:table-from-rows
               (list (alexandria:alist-hash-table '(("n" . 1) ("s" . "a")) :test #'equal))
               :schema schema)))
  (stack-arrow:encode table :format :arrow)
  (stack-arrow:encode table :format :parquet)
  (serdes-protocol:encode table :format :arrow)
  (serdes-protocol:encode table :format :parquet))
```

| Call | `:arrow` | `:parquet` |
|------|----------|------------|
| `encode` / `decode` | IPC file (`ARROW1` + footer) | parquet file |
| `stream-encode-value` / `stream-decode-value` | IPC stream (schema, batches, EOS) | signals `arrow-error` |

Missing values are `:null` (boolean `nil` stays false). Timestamps are epoch integers in the type’s unit.

**Write defaults:** parquet uses dictionary + snappy when `cl-stack-snappy` is loaded, else uncompressed. Nested write is compliant 3-level LIST. Encryption is `AES_GCM_V1` via `crypto-protocol` (`:footer-key`).

**Soft natives:** `cl-stack-snappy` / `cl-stack-zstd` / `cl-stack-brotli` / chipz+salza2 (gzip). Missing decode codec → `arrow-unsupported-type`.

Deferred: Arrow dictionary/union/extension, Flight, C Data Interface, `AES_GCM_CTR_V1`, KMS.

CI: canned [`cl-repository`](https://github.com/egao1980/cl-repository) (`test-system.yml` / `publish-source.yml`). Deps from `ghcr.io/egao1980/cl-systems`.

## License

MIT
