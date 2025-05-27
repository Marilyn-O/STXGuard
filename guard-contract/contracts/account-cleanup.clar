;; Account Cleanup Reward System Smart Contract
;; Manages rewards for users who clean up accounts based on defined metrics

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_USER_NOT_FOUND (err u103))
(define-constant ERR_INVALID_METRIC (err u104))
(define-constant ERR_ALREADY_REWARDED (err u105))

;; Data Variables
(define-data-var contract-balance uint u0)
(define-data-var base-reward-per-account uint u100) ;; Base reward in microSTX
(define-data-var bonus-multiplier uint u150) ;; 1.5x multiplier for bonus (150/100)
(define-data-var minimum-accounts-for-bonus uint u10)
(define-data-var reward-pool-balance uint u0)

;; Data Maps
;; Track user cleanup activities and rewards
(define-map user-cleanup-stats
  { user: principal }
  {
    total-accounts-cleaned: uint,
    total-rewards-earned: uint,
    last-cleanup-block: uint,
    cleanup-sessions: uint
  }
)

;; Track individual cleanup sessions for transparency
(define-map cleanup-sessions
  { session-id: uint }
  {
    user: principal,
    accounts-cleaned: uint,
    reward-amount: uint,
    block-height: uint,
    timestamp: uint,
    bonus-applied: bool
  }
)

;; Track reward distribution history
(define-map reward-history
  { user: principal, session-id: uint }
  {
    amount: uint,
    distributed: bool,
    block-height: uint
  }
)

;; Session counter
(define-data-var next-session-id uint u1)

;; Administrative Functions

;; Fund the reward pool
(define-public (fund-reward-pool (amount uint))
  (begin
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set reward-pool-balance (+ (var-get reward-pool-balance) amount))
    (ok amount)
  )
)

;; Update reward parameters (owner only)
(define-public (update-base-reward (new-reward uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (> new-reward u0) ERR_INVALID_AMOUNT)
    (var-set base-reward-per-account new-reward)
    (ok new-reward)
  )
)

(define-public (update-bonus-settings (multiplier uint) (min-accounts uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (>= multiplier u100) ERR_INVALID_AMOUNT) ;; At least 1x
    (asserts! (> min-accounts u0) ERR_INVALID_AMOUNT)
    (var-set bonus-multiplier multiplier)
    (var-set minimum-accounts-for-bonus min-accounts)
    (ok true)
  )
)

;; Core Reward Functions

;; Calculate reward based on accounts cleaned
(define-private (calculate-reward (accounts-cleaned uint))
  (let
    (
      (base-reward (* accounts-cleaned (var-get base-reward-per-account)))
      (bonus-eligible (>= accounts-cleaned (var-get minimum-accounts-for-bonus)))
    )
    (if bonus-eligible
      (/ (* base-reward (var-get bonus-multiplier)) u100)
      base-reward
    )
  )
)

;; Record cleanup activity and calculate rewards
(define-public (record-cleanup-session (user principal) (accounts-cleaned uint))
  (let
    (
      (session-id (var-get next-session-id))
      (reward-amount (calculate-reward accounts-cleaned))
      (bonus-applied (>= accounts-cleaned (var-get minimum-accounts-for-bonus)))
      (current-stats (default-to 
        { total-accounts-cleaned: u0, total-rewards-earned: u0, last-cleanup-block: u0, cleanup-sessions: u0 }
        (map-get? user-cleanup-stats { user: user })
      ))
    )
    (asserts! (> accounts-cleaned u0) ERR_INVALID_AMOUNT)
    (asserts! (>= (var-get reward-pool-balance) reward-amount) ERR_INSUFFICIENT_FUNDS)
    
    ;; Record the cleanup session
    (map-set cleanup-sessions
      { session-id: session-id }
      {
        user: user,
        accounts-cleaned: accounts-cleaned,
        reward-amount: reward-amount,
        block-height: stacks-block-height,
        timestamp: (unwrap-panic (get-stacks-block-info? time stacks-block-height)),
        bonus-applied: bonus-applied
      }
    )
    
    ;; Update user stats
    (map-set user-cleanup-stats
      { user: user }
      {
        total-accounts-cleaned: (+ (get total-accounts-cleaned current-stats) accounts-cleaned),
        total-rewards-earned: (+ (get total-rewards-earned current-stats) reward-amount),
        last-cleanup-block: stacks-block-height,
        cleanup-sessions: (+ (get cleanup-sessions current-stats) u1)
      }
    )
    
    ;; Record in reward history
    (map-set reward-history
      { user: user, session-id: session-id }
      {
        amount: reward-amount,
        distributed: false,
        block-height: stacks-block-height
      }
    )
    
    ;; Increment session counter
    (var-set next-session-id (+ session-id u1))
    
    (ok { session-id: session-id, reward-amount: reward-amount, bonus-applied: bonus-applied })
  )
)

;; Distribute rewards to user
(define-public (distribute-reward (user principal) (session-id uint))
  (let
    (
      (session-data (unwrap! (map-get? cleanup-sessions { session-id: session-id }) ERR_INVALID_METRIC))
      (reward-data (unwrap! (map-get? reward-history { user: user, session-id: session-id }) ERR_USER_NOT_FOUND))
      (reward-amount (get amount reward-data))
    )
    (asserts! (is-eq user (get user session-data)) ERR_UNAUTHORIZED)
    (asserts! (not (get distributed reward-data)) ERR_ALREADY_REWARDED)
    (asserts! (>= (var-get reward-pool-balance) reward-amount) ERR_INSUFFICIENT_FUNDS)
    
    ;; Transfer reward to user
    (try! (as-contract (stx-transfer? reward-amount tx-sender user)))
    
    ;; Update reward pool balance
    (var-set reward-pool-balance (- (var-get reward-pool-balance) reward-amount))
    
    ;; Mark as distributed
    (map-set reward-history
      { user: user, session-id: session-id }
      (merge reward-data { distributed: true })
    )
    
    (ok reward-amount)
  )
)


;; Read-only Functions

;; Get user cleanup statistics
(define-read-only (get-user-stats (user principal))
  (map-get? user-cleanup-stats { user: user })
)

;; Get cleanup session details
(define-read-only (get-session-details (session-id uint))
  (map-get? cleanup-sessions { session-id: session-id })
)

;; Get reward history for user and session
(define-read-only (get-reward-status (user principal) (session-id uint))
  (map-get? reward-history { user: user, session-id: session-id })
)

;; Calculate potential reward for accounts to be cleaned
(define-read-only (preview-reward (accounts-to-clean uint))
  (let
    (
      (base-reward (* accounts-to-clean (var-get base-reward-per-account)))
      (bonus-eligible (>= accounts-to-clean (var-get minimum-accounts-for-bonus)))
      (final-reward (if bonus-eligible
        (/ (* base-reward (var-get bonus-multiplier)) u100)
        base-reward))
    )
    {
      accounts: accounts-to-clean,
      base-reward: base-reward,
      bonus-applied: bonus-eligible,
      final-reward: final-reward
    }
  )
)

;; Get current reward pool balance
(define-read-only (get-reward-pool-balance)
  (var-get reward-pool-balance)
)

;; Get current reward settings
(define-read-only (get-reward-settings)
  {
    base-reward-per-account: (var-get base-reward-per-account),
    bonus-multiplier: (var-get bonus-multiplier),
    minimum-accounts-for-bonus: (var-get minimum-accounts-for-bonus)
  }
)

;; Get total rewards distributed to a user
(define-read-only (get-user-total-rewards (user principal))
  (match (map-get? user-cleanup-stats { user: user })
    stats (get total-rewards-earned stats)
    u0
  )
)

;; Emergency Functions

;; Emergency withdrawal (owner only)
(define-public (emergency-withdraw (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (asserts! (<= amount (var-get reward-pool-balance)) ERR_INSUFFICIENT_FUNDS)
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
    (var-set reward-pool-balance (- (var-get reward-pool-balance) amount))
    (ok amount)
  )
)

;; Initialize contract with initial funding
(define-public (initialize-contract (initial-funding uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (try! (fund-reward-pool initial-funding))
    (ok true)
  )
)