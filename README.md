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

**Column storage** follows the Arrow layout. Fixed-width types (int8…uint64, float32/64, date, time, timestamp, duration) keep their values in a specialized vector, e.g. `(simple-array double-float (*))`; bool is a packed bitmap; utf8 / binary are int32 offsets plus one octet vector. Nulls are a separate validity bitmap (octets, LSB-first, 1 = valid, `nil` when there are none) with a cached null count. Decimal, fixed-size-binary, float16, list, struct and map stay boxed (a `simple-vector` with `:null`).

| | |
|--|--|
| `make-arrow-array type values` | Lisp values (`:null` for nulls) → typed storage; a value that does not fit signals `arrow-type-error` |
| `make-typed-arrow-array type data &key validity null-count offsets length` | wraps an existing specialized vector (or bitmap / octets + offsets), no copy |
| `array-ref`, `array-null-p`, `array-value-octets` | element access; `array-value-octets` → `(values octets start end)` of a utf8 / binary value, no copy |
| `arrow-array-data` / `-validity` / `-offsets` / `-null-count` / `-kind` / `-element-type` / `-typed-p` | the buffers themselves (do not modify) |
| `arrow-array-values` | boxed `simple-vector` view, built on first use and cached |
| `make-column-builder` / `column-builder-push` / `column-builder-finish` | build one column value by value |

IPC write copies each typed buffer into the message body with one `memcpy` ([`buffer-protocol`](https://github.com/egao1980/buffer-protocol)), and IPC read copies each body buffer out the same way. On a big-endian host, or on ABCL (whose CFFI copies a vector instead of pinning it, and whose arrays box floats and 64-bit integers), the same functions run a per-element little-endian loop with identical output.

**Write defaults:** parquet uses dictionary + snappy when `cl-stack-snappy` is loaded, else uncompressed. Nested write is compliant 3-level LIST. Encryption is `AES_GCM_V1` via `crypto-protocol` (`:footer-key`).

Parquet footer field 5 (`key_value_metadata`) stores **`ARROW:schema`** by default — base64 of the IPC schema message, same convention as pyarrow / Arrow C++ `store_schema()`. `:store-schema nil` omits it; decode then uses the SchemaElement tree. `parquet-schema` / `parquet-key-value-metadata` read the footer only.

`parquet-schema` → `arrow-schema` is faithful when field 5 is present. Compiling that to `defschema` is still lossy — go through [`schema-protocol-arrow`](https://github.com/egao1980/schema-protocol-arrow) `parse-schema` / `:format :arrow`.

**Soft natives:** `cl-stack-snappy` / `cl-stack-zstd` / `cl-stack-brotli` / chipz+salza2 (gzip). Missing decode codec → `arrow-unsupported-type`.

Deferred: Arrow dictionary/union/extension, Flight, C Data Interface, `AES_GCM_CTR_V1`, KMS.

CI: canned [`cl-repository`](https://github.com/egao1980/cl-repository) (`test-system.yml` / `publish-source.yml`). Deps from `ghcr.io/egao1980/cl-systems`.

## License

MIT
