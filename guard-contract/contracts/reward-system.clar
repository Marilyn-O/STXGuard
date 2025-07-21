;; Reward System Smart Contract
;; Handles automatic reward calculation, distribution, and tracking

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INSUFFICIENT_BALANCE (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_USER_NOT_FOUND (err u103))
(define-constant ERR_ALREADY_CLAIMED (err u104))
(define-constant ERR_INVALID_METRIC (err u105))

;; Reward rates (in micro-STX per unit)
(define-constant REWARD_PER_ACCOUNT_CLEANED u1000000) ;; 1 STX per account cleaned
(define-constant BONUS_THRESHOLD u10) ;; Bonus after 10 cleanups
(define-constant BONUS_MULTIPLIER u150) ;; 1.5x bonus (150/100)

;; Data Variables
(define-data-var total-rewards-distributed uint u0)
(define-data-var total-accounts-cleaned uint u0)
(define-data-var reward-rate uint REWARD_PER_ACCOUNT_CLEANED)
(define-data-var contract-balance uint u0)

;; Data Maps
;; Track user cleanup activities
(define-map user-cleanup-count principal uint)
(define-map user-total-rewards principal uint)
(define-map user-pending-rewards principal uint)

;; Track cleanup sessions
(define-map cleanup-sessions 
  { user: principal, session-id: uint }
  {
    accounts-cleaned: uint,
    timestamp: uint,
    reward-amount: uint,
    claimed: bool
  }
)

;; Track session counter for each user
(define-map user-session-counter principal uint)

;; Administrative Functions

;; Initialize contract with initial funding
(define-public (initialize-contract)
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set contract-balance (stx-get-balance tx-sender))
    (ok true)
  )
)

;; Fund the contract for reward distribution
(define-public (fund-contract (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set contract-balance (+ (var-get contract-balance) amount))
    (ok amount)
  )
)

;; Update reward rate (only owner)
(define-public (update-reward-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-rate u0) ERR_INVALID_AMOUNT)
    (var-set reward-rate new-rate)
    (ok new-rate)
  )
)

;; Core Reward Functions

;; Report cleanup activity and calculate rewards
(define-public (report-cleanup (accounts-cleaned uint))
  (let (
    (user tx-sender)
    (current-count (default-to u0 (map-get? user-cleanup-count user)))
    (session-id (+ (default-to u0 (map-get? user-session-counter user)) u1))
    (base-reward (* accounts-cleaned (var-get reward-rate)))
    (total-cleanups (+ current-count accounts-cleaned))
    (bonus-reward (if (>= total-cleanups BONUS_THRESHOLD)
                    (/ (* base-reward (- BONUS_MULTIPLIER u100)) u100)
                    u0))
    (total-reward (+ base-reward bonus-reward))
  )
    (asserts! (> accounts-cleaned u0) ERR_INVALID_METRIC)
    
    ;; Update user cleanup count
    (map-set user-cleanup-count user total-cleanups)
    
    ;; Update session counter
    (map-set user-session-counter user session-id)
    
    ;; Record cleanup session
    (map-set cleanup-sessions
      { user: user, session-id: session-id }
      {
        accounts-cleaned: accounts-cleaned,
        timestamp: block-height,
        reward-amount: total-reward,
        claimed: false
      }
    )
    
    ;; Add to pending rewards
    (let ((current-pending (default-to u0 (map-get? user-pending-rewards user))))
      (map-set user-pending-rewards user (+ current-pending total-reward))
    )
    
    ;; Update global counters
    (var-set total-accounts-cleaned (+ (var-get total-accounts-cleaned) accounts-cleaned))
    
    (ok {
      session-id: session-id,
      accounts-cleaned: accounts-cleaned,
      reward-amount: total-reward,
      bonus-applied: (> bonus-reward u0)
    })
  )
)

;; Claim pending rewards
(define-public (claim-rewards)
  (let (
    (user tx-sender)
    (pending-amount (default-to u0 (map-get? user-pending-rewards user)))
    (current-total (default-to u0 (map-get? user-total-rewards user)))
  )
    (asserts! (> pending-amount u0) ERR_INSUFFICIENT_BALANCE)
    (asserts! (>= (var-get contract-balance) pending-amount) ERR_INSUFFICIENT_BALANCE)
    
    ;; Transfer rewards to user
    (try! (as-contract (stx-transfer? pending-amount tx-sender user)))
    
    ;; Update balances
    (var-set contract-balance (- (var-get contract-balance) pending-amount))
    (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) pending-amount))
    
    ;; Update user records
    (map-set user-total-rewards user (+ current-total pending-amount))
    (map-delete user-pending-rewards user)
    
    (ok pending-amount)
  )
)

;; Claim rewards for specific session
(define-public (claim-session-reward (session-id uint))
  (let (
    (user tx-sender)
    (session-key { user: user, session-id: session-id })
    (session-data (unwrap! (map-get? cleanup-sessions session-key) ERR_USER_NOT_FOUND))
    (reward-amount (get reward-amount session-data))
  )
    (asserts! (not (get claimed session-data)) ERR_ALREADY_CLAIMED)
    (asserts! (>= (var-get contract-balance) reward-amount) ERR_INSUFFICIENT_BALANCE)
    
    ;; Transfer reward
    (try! (as-contract (stx-transfer? reward-amount tx-sender user)))
    
    ;; Mark session as claimed
    (map-set cleanup-sessions session-key (merge session-data { claimed: true }))
    
    ;; Update balances
    (var-set contract-balance (- (var-get contract-balance) reward-amount))
    (var-set total-rewards-distributed (+ (var-get total-rewards-distributed) reward-amount))
    
    ;; Update user total rewards
    (let ((current-total (default-to u0 (map-get? user-total-rewards user))))
      (map-set user-total-rewards user (+ current-total reward-amount))
    )
    
    ;; Update pending rewards
    (let ((current-pending (default-to u0 (map-get? user-pending-rewards user))))
      (if (>= current-pending reward-amount)
        (map-set user-pending-rewards user (- current-pending reward-amount))
        (map-delete user-pending-rewards user)
      )
    )
    
    (ok reward-amount)
  )
)

;; Calculate potential reward for cleanup amount
(define-read-only (calculate-reward (user principal) (accounts-to-clean uint))
  (let (
    (current-count (default-to u0 (map-get? user-cleanup-count user)))
    (base-reward (* accounts-to-clean (var-get reward-rate)))
    (total-after-cleanup (+ current-count accounts-to-clean))
    (bonus-reward (if (>= total-after-cleanup BONUS_THRESHOLD)
                    (/ (* base-reward (- BONUS_MULTIPLIER u100)) u100)
                    u0))
  )
    {
      base-reward: base-reward,
      bonus-reward: bonus-reward,
      total-reward: (+ base-reward bonus-reward),
      will-trigger-bonus: (>= total-after-cleanup BONUS_THRESHOLD)
    }
  )
)

;; Read-only functions for querying data

;; Get user's cleanup statistics
(define-read-only (get-user-stats (user principal))
  {
    cleanup-count: (default-to u0 (map-get? user-cleanup-count user)),
    total-rewards: (default-to u0 (map-get? user-total-rewards user)),
    pending-rewards: (default-to u0 (map-get? user-pending-rewards user)),
    session-count: (default-to u0 (map-get? user-session-counter user))
  }
)

;; Get cleanup session details
(define-read-only (get-cleanup-session (user principal) (session-id uint))
  (map-get? cleanup-sessions { user: user, session-id: session-id })
)

;; Get contract statistics
(define-read-only (get-contract-stats)
  {
    total-rewards-distributed: (var-get total-rewards-distributed),
    total-accounts-cleaned: (var-get total-accounts-cleaned),
    current-reward-rate: (var-get reward-rate),
    contract-balance: (var-get contract-balance),
    bonus-threshold: BONUS_THRESHOLD,
    bonus-multiplier: BONUS_MULTIPLIER
  }
)

;; Get current reward rate
(define-read-only (get-reward-rate)
  (var-get reward-rate)
)

;; Get user's pending rewards
(define-read-only (get-pending-rewards (user principal))
  (default-to u0 (map-get? user-pending-rewards user))
)

;; Get user's total cleanup count
(define-read-only (get-cleanup-count (user principal))
  (default-to u0 (map-get? user-cleanup-count user))
)

;; Check if user qualifies for bonus
(define-read-only (qualifies-for-bonus (user principal))
  (>= (default-to u0 (map-get? user-cleanup-count user)) BONUS_THRESHOLD)
)

;; Get contract balance
(define-read-only (get-contract-balance)
  (var-get contract-balance)
)

;; Emergency functions (owner only)

;; Emergency withdrawal (owner only)
(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= amount (var-get contract-balance)) ERR_INSUFFICIENT_BALANCE)
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
    (var-set contract-balance (- (var-get contract-balance) amount))
    (ok amount)
  )
)
