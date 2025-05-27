;; On-Demand Cleanup Contract

;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-not-owner (err u100))
(define-constant err-account-not-found (err u101))
(define-constant err-not-marked-for-cleanup (err u102))
(define-constant err-already-marked-for-cleanup (err u103))
(define-constant err-not-authorized (err u104))
(define-constant err-confirmation-mismatch (err u105))

;; Define data maps
(define-map accounts-to-cleanup
  { account: principal }
  {
    marked-by: principal,
    marked-at: uint,
    confirmation-code: (string-ascii 16)
  }
)

(define-map account-data
  { account: principal }
  {
    data: (string-ascii 256),
    created-at: uint,
    last-modified: uint
  }
)

;; Define variables
(define-data-var cleanup-counter uint u0)

;; Mark an account for cleanup
(define-public (mark-for-cleanup (account principal) (confirmation-code (string-ascii 16)))
  (let
    (
      (existing-mark (map-get? accounts-to-cleanup { account: account }))
      (current-block stacks-block-height)
    )
    (asserts! (is-none existing-mark) (err err-already-marked-for-cleanup))
    (asserts! (is-some (map-get? account-data { account: account })) (err err-account-not-found))
    (asserts! (or (is-eq tx-sender account) (is-eq tx-sender contract-owner)) (err err-not-authorized))
    
    (map-set accounts-to-cleanup
      { account: account }
      {
        marked-by: tx-sender,
        marked-at: current-block,
        confirmation-code: confirmation-code
      }
    )
    (var-set cleanup-counter (+ (var-get cleanup-counter) u1))
    (ok true)
  )
)

;; Cancel cleanup request
(define-public (cancel-cleanup (account principal))
  (let
    (
      (existing-mark (unwrap! (map-get? accounts-to-cleanup { account: account }) (err err-not-marked-for-cleanup)))
    )
    (asserts! (or (is-eq tx-sender account) 
                 (is-eq tx-sender (get marked-by existing-mark)) 
                 (is-eq tx-sender contract-owner)) 
             (err err-not-authorized))
    
    (map-delete accounts-to-cleanup { account: account })
    (var-set cleanup-counter (- (var-get cleanup-counter) u1))
    (ok true)
  )
)

;; Confirm and execute cleanup
(define-public (confirm-cleanup (account principal) (confirmation-code (string-ascii 16)))
  (let
    (
      (cleanup-info (unwrap! (map-get? accounts-to-cleanup { account: account }) (err err-not-marked-for-cleanup)))
    )
    (asserts! (or (is-eq tx-sender account) 
                 (is-eq tx-sender (get marked-by cleanup-info)) 
                 (is-eq tx-sender contract-owner)) 
             (err err-not-authorized))
    (asserts! (is-eq (get confirmation-code cleanup-info) confirmation-code) (err err-confirmation-mismatch))
    
    ;; Execute the cleanup
    (map-delete account-data { account: account })
    (map-delete accounts-to-cleanup { account: account })
    (var-set cleanup-counter (- (var-get cleanup-counter) u1))
    
    ;; Here you would add any additional cleanup logic specific to your application
    
    (ok true)
  )
)

;; Add or update account data (for testing purposes)
(define-public (add-account-data (data (string-ascii 256)))
  (let
    (
      (current-block stacks-block-height)
      (existing-data (map-get? account-data { account: tx-sender }))
    )
    (if (is-some existing-data)
      (map-set account-data
        { account: tx-sender }
        {
          data: data,
          created-at: (get created-at (unwrap-panic existing-data)),
          last-modified: current-block
        }
      )
      (map-set account-data
        { account: tx-sender }
        {
          data: data,
          created-at: current-block,
          last-modified: current-block
        }
      )
    )
    (ok true)
  )
)

;; Check if an account is marked for cleanup
(define-read-only (is-marked-for-cleanup (account principal))
  (is-some (map-get? accounts-to-cleanup { account: account }))
)

;; Get cleanup information for an account
(define-read-only (get-cleanup-info (account principal))
  (map-get? accounts-to-cleanup { account: account })
)

;; Get account data
(define-read-only (get-account-data (account principal))
  (map-get? account-data { account: account })
)

;; Get total number of accounts marked for cleanup
(define-read-only (get-cleanup-count)
  (var-get cleanup-counter)
)

;; Admin function to force cleanup (only contract owner)
(define-public (admin-force-cleanup (account principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) (err err-not-owner))
    (asserts! (is-some (map-get? account-data { account: account })) (err err-account-not-found))
    
    ;; Execute the cleanup
    (map-delete account-data { account: account })
    (if (is-some (map-get? accounts-to-cleanup { account: account }))
      (begin
        (map-delete accounts-to-cleanup { account: account })
        (var-set cleanup-counter (- (var-get cleanup-counter) u1))
      )
      true
    )
    
    (ok true)
  )
)