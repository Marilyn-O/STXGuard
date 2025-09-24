;; STXGuard Security - Advanced Security Monitoring and Threat Detection
;; Multi-layered access control with proactive security monitoring

;; ===== CONSTANTS =====
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_ACCOUNT_FROZEN (err u402))
(define-constant ERR_TIMELOCK_ACTIVE (err u403))
(define-constant ERR_INVALID_THRESHOLD (err u404))
(define-constant ERR_THREAT_DETECTED (err u405))
(define-constant ERR_INSUFFICIENT_REPUTATION (err u406))
(define-constant ERR_RATE_LIMIT_EXCEEDED (err u407))
(define-constant ERR_INVALID_OPERATION (err u408))

;; Security thresholds
(define-constant MAX_DAILY_TRANSACTIONS u100)
(define-constant MIN_TIMELOCK_DURATION u144) ;; ~24 hours in blocks
(define-constant THREAT_SCORE_THRESHOLD u75)
(define-constant MIN_REPUTATION_SCORE u50)

;; ===== DATA STRUCTURES =====

;; Account security profile
(define-map account-profiles principal {
    reputation-score: uint,
    risk-level: (string-ascii 10), ;; "low", "medium", "high", "critical"
    is-frozen: bool,
    freeze-reason: (string-ascii 100),
    freeze-timestamp: uint,
    total-transactions: uint,
    failed-attempts: uint,
    last-activity: uint,
    security-flags: uint ;; Bitfield for various security flags
})

;; Time-locked operations
(define-map time-locks {account: principal, operation-id: uint} {
    operation-type: (string-ascii 50),
    target-amount: uint,
    target-address: (optional principal),
    unlock-height: uint,
    created-by: principal,
    is-executed: bool,
    security-level: uint
})

;; Behavioral analysis patterns
(define-map behavioral-patterns principal {
    avg-transaction-amount: uint,
    typical-hours: uint, ;; Bitfield representing typical active hours
    common-recipients: (list 10 principal),
    transaction-frequency: uint,
    last-pattern-update: uint,
    anomaly-score: uint
})

;; Known malicious addresses
(define-map malicious-addresses principal {
    threat-level: uint, ;; 1-100 scale
    reported-by: principal,
    report-timestamp: uint,
    threat-type: (string-ascii 50),
    is-verified: bool
})

;; Security events log
(define-map security-events uint {
    event-type: (string-ascii 50),
    account: principal,
    timestamp: uint,
    severity: uint, ;; 1-10 scale
    details: (string-ascii 200),
    auto-response: bool,
    resolved: bool
})

;; Access control roles
(define-map user-roles principal {
    role: (string-ascii 20), ;; "admin", "security", "analyst", "user"
    permissions: uint, ;; Bitfield for permissions
    granted-by: principal,
    granted-at: uint,
    expires-at: (optional uint)
})

;; Rate limiting
(define-map rate-limits principal {
    daily-count: uint,
    last-reset: uint,
    burst-count: uint,
    last-burst-reset: uint
})

;; ===== DATA VARIABLES =====
(define-data-var next-operation-id uint u1)
(define-data-var next-event-id uint u1)
(define-data-var security-enabled bool true)
(define-data-var global-threat-level uint u0)
(define-data-var emergency-mode bool false)

;; ===== PRIVATE FUNCTIONS =====

;; Check if user has specific permission
(define-private (has-permission (user principal) (permission uint))
    (let ((user-role (default-to {role: "user", permissions: u0, granted-by: CONTRACT_OWNER, granted-at: u0, expires-at: none}
                                (map-get? user-roles user))))
        (> (bit-and (get permissions user-role) permission) u0)))

;; Calculate threat score based on multiple factors
(define-private (calculate-threat-score (account principal) (amount uint) (recipient (optional principal)))
    (let ((profile (get-account-profile account))
          (patterns (default-to {avg-transaction-amount: u0, typical-hours: u0, common-recipients: (list), 
                                transaction-frequency: u0, last-pattern-update: u0, anomaly-score: u0}
                               (map-get? behavioral-patterns account)))
          (base-score u0))
        ;; Check for unusual transaction amount
        (let ((amount-score (if (> amount (* (get avg-transaction-amount patterns) u5)) u25 u0)))
            ;; Check if recipient is known malicious
            (let ((recipient-score (match recipient
                                          some-recipient (if (is-some (map-get? malicious-addresses some-recipient)) u30 u0)
                                          u0)))
                ;; Check account reputation
                (let ((reputation-score (if (< (get reputation-score profile) MIN_REPUTATION_SCORE) u20 u0)))
                    (+ base-score amount-score recipient-score reputation-score))))))

;; Get account profile with defaults
(define-private (get-account-profile (account principal))
    (default-to {reputation-score: u100, risk-level: "low", is-frozen: false, freeze-reason: "",
                freeze-timestamp: u0, total-transactions: u0, failed-attempts: u0, 
                last-activity: u0, security-flags: u0}
               (map-get? account-profiles account)))

;; Log security event
(define-private (log-security-event (event-type (string-ascii 50)) (account principal) 
                                   (severity uint) (details (string-ascii 200)) (auto-response bool))
    (let ((event-id (var-get next-event-id)))
        (map-set security-events event-id {
            event-type: event-type,
            account: account,
            timestamp: stacks-block-height,
            severity: severity,
            details: details,
            auto-response: auto-response,
            resolved: false
        })
        (var-set next-event-id (+ event-id u1))
        event-id))

;; Update behavioral patterns
(define-private (update-behavioral-pattern (account principal) (amount uint))
    (let ((current-patterns (default-to {avg-transaction-amount: amount, typical-hours: u0, 
                                        common-recipients: (list), transaction-frequency: u1, 
                                        last-pattern-update: stacks-block-height, anomaly-score: u0}
                                       (map-get? behavioral-patterns account))))
        (map-set behavioral-patterns account (merge current-patterns {
            avg-transaction-amount: (/ (+ (* (get avg-transaction-amount current-patterns) u9) amount) u10),
            transaction-frequency: (+ (get transaction-frequency current-patterns) u1),
            last-pattern-update: stacks-block-height
        }))
        true))

;; Check rate limits
(define-private (check-rate-limit (account principal))
    (let ((limits (default-to {daily-count: u0, last-reset: stacks-block-height, burst-count: u0, last-burst-reset: stacks-block-height}
                             (map-get? rate-limits account)))
          (current-day (/ stacks-block-height u144))) ;; Approximate blocks per day
        ;; Reset daily counter if new day
        (let ((updated-limits (if (> current-day (/ (get last-reset limits) u144))
                                 (merge limits {daily-count: u0, last-reset: stacks-block-height})
                                 limits)))
            ;; Check if under daily limit
            (if (< (get daily-count updated-limits) MAX_DAILY_TRANSACTIONS)
                (begin
                    (map-set rate-limits account (merge updated-limits {
                        daily-count: (+ (get daily-count updated-limits) u1)
                    }))
                    (ok true))
                (err ERR_RATE_LIMIT_EXCEEDED)))))

;; ===== PUBLIC FUNCTIONS =====

;; Initialize account security profile
(define-public (initialize-account)
    (let ((account tx-sender))
        (if (is-none (map-get? account-profiles account))
            (begin
                (map-set account-profiles account {
                    reputation-score: u100,
                    risk-level: "low",
                    is-frozen: false,
                    freeze-reason: "",
                    freeze-timestamp: u0,
                    total-transactions: u0,
                    failed-attempts: u0,
                    last-activity: stacks-block-height,
                    security-flags: u0
                })
                (log-security-event "ACCOUNT_INITIALIZED" account u1 "New account security profile created" false)
                (ok true))
            (err ERR_INVALID_OPERATION))))

;; Create time-locked operation
(define-public (create-timelock (operation-type (string-ascii 50)) (target-amount uint) 
                               (target-address (optional principal)) (delay-blocks uint))
    (let ((account tx-sender)
          (operation-id (var-get next-operation-id))
          (profile (get-account-profile account)))
        ;; Check if account is frozen
        (if (get is-frozen profile)
            (err ERR_ACCOUNT_FROZEN)
            ;; Check minimum delay
            (if (< delay-blocks MIN_TIMELOCK_DURATION)
                (err ERR_INVALID_THRESHOLD)
                (begin
                    (map-set time-locks {account: account, operation-id: operation-id} {
                        operation-type: operation-type,
                        target-amount: target-amount,
                        target-address: target-address,
                        unlock-height: (+ stacks-block-height delay-blocks),
                        created-by: account,
                        is-executed: false,
                        security-level: (if (> target-amount u1000000) u3 u1) ;; Higher security for large amounts
                    })
                    (var-set next-operation-id (+ operation-id u1))
                    (log-security-event "TIMELOCK_CREATED" account u2 operation-type false)
                    (ok operation-id))))))

;; Execute time-locked operation
(define-public (execute-timelock (operation-id uint))
    (let ((account tx-sender)
          (timelock-key {account: account, operation-id: operation-id}))
        (match (map-get? time-locks timelock-key)
            timelock-data (if (get is-executed timelock-data)
                             (err ERR_INVALID_OPERATION)
                             (if (< stacks-block-height (get unlock-height timelock-data))
                                 (err ERR_TIMELOCK_ACTIVE)
                                 (begin
                                     (map-set time-locks timelock-key (merge timelock-data {is-executed: true}))
                                     (log-security-event "TIMELOCK_EXECUTED" account u3 (get operation-type timelock-data) false)
                                     (ok true))))
            (err ERR_INVALID_OPERATION))))

;; Perform security check before transaction
(define-public (security-check (amount uint) (recipient (optional principal)))
    (let ((account tx-sender)
          (profile (get-account-profile account)))
        ;; Check if security is enabled
        (if (not (var-get security-enabled))
            (ok true)
            ;; Check if account is frozen
            (if (get is-frozen profile)
                (err ERR_ACCOUNT_FROZEN)
                ;; Check rate limits
                (match (check-rate-limit account)
                    success (let ((threat-score (calculate-threat-score account amount recipient)))
                                ;; Check threat score
                                (if (> threat-score THREAT_SCORE_THRESHOLD)
                                    (begin
                                        (log-security-event "THREAT_DETECTED" account u8 "High threat score detected" true)
                                        (err ERR_THREAT_DETECTED))
                                    (begin
                                        ;; Update behavioral patterns
                                        (update-behavioral-pattern account amount)
                                        ;; Update account activity
                                        (map-set account-profiles account (merge profile {
                                            total-transactions: (+ (get total-transactions profile) u1),
                                            last-activity: stacks-block-height
                                        }))
                                        (ok true))))
                    error (err error))))))

;; Report malicious address
(define-public (report-malicious-address (malicious-addr principal) (threat-level uint) (threat-type (string-ascii 50)))
    (let ((reporter tx-sender))
        ;; Check if reporter has permission
        (if (or (is-eq reporter CONTRACT_OWNER) (has-permission reporter u4)) ;; Permission bit 4 for reporting
            (if (and (>= threat-level u1) (<= threat-level u100))
                (begin
                    (map-set malicious-addresses malicious-addr {
                        threat-level: threat-level,
                        reported-by: reporter,
                        report-timestamp: stacks-block-height,
                        threat-type: threat-type,
                        is-verified: (is-eq reporter CONTRACT_OWNER)
                    })
                    (log-security-event "MALICIOUS_ADDRESS_REPORTED" malicious-addr u6 threat-type false)
                    (ok true))
                (err ERR_INVALID_THRESHOLD))
            (err ERR_UNAUTHORIZED))))

;; Emergency freeze account
(define-public (emergency-freeze (target-account principal) (reason (string-ascii 100)))
    (let ((admin tx-sender))
        ;; Check admin permissions
        (if (or (is-eq admin CONTRACT_OWNER) (has-permission admin u8)) ;; Permission bit 8 for emergency actions
            (let ((profile (get-account-profile target-account)))
                (map-set account-profiles target-account (merge profile {
                    is-frozen: true,
                    freeze-reason: reason,
                    freeze-timestamp: stacks-block-height
                }))
                (log-security-event "EMERGENCY_FREEZE" target-account u10 reason true)
                (ok true))
            (err ERR_UNAUTHORIZED))))

;; Unfreeze account
(define-public (unfreeze-account (target-account principal))
    (let ((admin tx-sender))
        ;; Check admin permissions
        (if (or (is-eq admin CONTRACT_OWNER) (has-permission admin u8))
            (let ((profile (get-account-profile target-account)))
                (if (get is-frozen profile)
                    (begin
                        (map-set account-profiles target-account (merge profile {
                            is-frozen: false,
                            freeze-reason: "",
                            freeze-timestamp: u0
                        }))
                        (log-security-event "ACCOUNT_UNFROZEN" target-account u5 "Account unfrozen by admin" false)
                        (ok true))
                    (err ERR_INVALID_OPERATION)))
            (err ERR_UNAUTHORIZED))))

;; Grant role to user
(define-public (grant-role (user principal) (role (string-ascii 20)) (permissions uint) (expires-at (optional uint)))
    (let ((admin tx-sender))
        ;; Only contract owner or admin can grant roles
        (if (or (is-eq admin CONTRACT_OWNER) (has-permission admin u16)) ;; Permission bit 16 for role management
            (begin
                (map-set user-roles user {
                    role: role,
                    permissions: permissions,
                    granted-by: admin,
                    granted-at: stacks-block-height,
                    expires-at: expires-at
                })
                (log-security-event "ROLE_GRANTED" user u3 role false)
                (ok true))
            (err ERR_UNAUTHORIZED))))

;; Update reputation score
(define-public (update-reputation (target-account principal) (new-score uint) (reason (string-ascii 100)))
    (let ((admin tx-sender))
        ;; Check admin permissions
        (if (or (is-eq admin CONTRACT_OWNER) (has-permission admin u2)) ;; Permission bit 2 for reputation management
            (if (<= new-score u100)
                (let ((profile (get-account-profile target-account)))
                    (map-set account-profiles target-account (merge profile {
                        reputation-score: new-score
                    }))
                    (log-security-event "REPUTATION_UPDATED" target-account u4 reason false)
                    (ok true))
                (err ERR_INVALID_THRESHOLD))
            (err ERR_UNAUTHORIZED))))

;; Toggle emergency mode
(define-public (toggle-emergency-mode)
    (let ((admin tx-sender))
        (if (is-eq admin CONTRACT_OWNER)
            (begin
                (var-set emergency-mode (not (var-get emergency-mode)))
                (log-security-event "EMERGENCY_MODE_TOGGLED" admin u9 
                                   (if (var-get emergency-mode) "Emergency mode activated" "Emergency mode deactivated") true)
                (ok (var-get emergency-mode)))
            (err ERR_UNAUTHORIZED))))

;; ===== READ-ONLY FUNCTIONS =====

;; Get account security profile
(define-read-only (get-security-profile (account principal))
    (map-get? account-profiles account))

;; Get behavioral patterns
(define-read-only (get-behavioral-patterns (account principal))
    (map-get? behavioral-patterns account))

;; Check if address is malicious
(define-read-only (is-malicious-address (address principal))
    (is-some (map-get? malicious-addresses address)))

;; Get security event
(define-read-only (get-security-event (event-id uint))
    (map-get? security-events event-id))

;; Get timelock details
(define-read-only (get-timelock (account principal) (operation-id uint))
    (map-get? time-locks {account: account, operation-id: operation-id}))

;; Check user permissions
(define-read-only (get-user-permissions (user principal))
    (map-get? user-roles user))

;; Get system status
(define-read-only (get-system-status)
    {
        security-enabled: (var-get security-enabled),
        global-threat-level: (var-get global-threat-level),
        emergency-mode: (var-get emergency-mode),
        next-operation-id: (var-get next-operation-id),
        next-event-id: (var-get next-event-id)
    })

;; Calculate current threat score for account
(define-read-only (get-threat-score (account principal) (amount uint) (recipient (optional principal)))
    (calculate-threat-score account amount recipient))
