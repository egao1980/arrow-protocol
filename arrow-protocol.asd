(defsystem "arrow-protocol"
  :version "0.1.2"
  :description "CLOS Apache Arrow / Parquet for cl-stack; implements serdes-protocol :arrow / :parquet"
  :author "egao1980"
  :license "MIT"
  :depends-on ("babel" "serdes-protocol" "encoding-protocol"
               (:version "compression-protocol" "0.3.0"))
  :properties (:cl-repo
               (:ci (:with ("cl-stack-snappy" "cl-stack-zstd" "cl-stack-brotli"
                            "compression-backend-chipz" "crypto-backend-ironclad"))))
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "types")
               (:file "array")
               (:file "table")
               (:file "flatbuffers")
               (:file "ipc")
               (:file "rle")
               (:file "delta")
               (:file "dremel")
               (:file "compress")
               (:file "crypto")
               (:file "parquet")
               (:file "protocol")
               (:file "serdes"))
  :in-order-to ((test-op (test-op "arrow-protocol/tests"))))

(defsystem "arrow-protocol/tests"
  :depends-on ("arrow-protocol" "serdes-protocol" "compression-backend-chipz" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "types-test")
               (:file "ipc-test")
               (:file "parquet-test")
               (:file "serdes-test")
               (:file "fixture-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
