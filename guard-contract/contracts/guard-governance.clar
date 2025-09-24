;; STXGuard Governance Contract
;; Decentralized governance, voting mechanisms, and compliance automation

;; =============================================================================
;; CONSTANTS & ERROR CODES
;; =============================================================================

;; Error codes
(define-constant ERR_NOT_AUTHORIZED (err u1001))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u1002))
(define-constant ERR_PROPOSAL_EXPIRED (err u1003))
(define-constant ERR_PROPOSAL_NOT_ACTIVE (err u1004))
(define-constant ERR_ALREADY_VOTED (err u1005))
(define-constant ERR_INSUFFICIENT_TOKENS (err u1006))
(define-constant ERR_INVALID_PROPOSAL (err u1007))
(define-constant ERR_QUORUM_NOT_MET (err u1008))
(define-constant ERR_TREASURY_INSUFFICIENT (err u1009))
(define-constant ERR_COMPLIANCE_VIOLATION (err u1010))
(define-constant ERR_KYC_NOT_VERIFIED (err u1011))
(define-constant ERR_INVALID_JURISDICTION (err u1012))

;; Governance parameters
(define-constant PROPOSAL_DURATION u144) ;; ~24 hours in blocks
(define-constant MIN_PROPOSAL_THRESHOLD u1000000) ;; 1M tokens to create proposal
(define-constant QUORUM_THRESHOLD u5000000) ;; 5M tokens for quorum
(define-constant EXECUTION_DELAY u1008) ;; ~7 days delay for execution
(define-constant MAX_PROPOSAL_DESCRIPTION u500)

;; Proposal types
(define-constant PROPOSAL_TYPE_PARAMETER u1)
(define-constant PROPOSAL_TYPE_TREASURY u2)
(define-constant PROPOSAL_TYPE_UPGRADE u3)
(define-constant PROPOSAL_TYPE_COMPLIANCE u4)

;; Compliance jurisdictions
(define-constant JURISDICTION_US u1)
(define-constant JURISDICTION_EU u2)
(define-constant JURISDICTION_ASIA u3)
(define-constant JURISDICTION_OTHER u4)

;; =============================================================================
;; DATA VARIABLES
;; =============================================================================

(define-data-var contract-owner principal tx-sender)
(define-data-var governance-token principal .stxguard-token)
(define-data-var proposal-counter uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var total-governance-rewards uint u0)
(define-data-var compliance-enabled bool true)

;; =============================================================================
;; DATA MAPS
;; =============================================================================

;; Proposal storage
(define-map proposals
  { proposal-id: uint }
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    proposal-type: uint,
    target-contract: (optional principal),
    function-name: (optional (string-ascii 50)),
    parameters: (optional (buff 1024)),
    votes-for: uint,
    votes-against: uint,
    start-block: uint,
    end-block: uint,
    executed: bool,
    execution-block: (optional uint),
    quorum-met: bool
  }
)

;; User votes tracking
(define-map user-votes
  { user: principal, proposal-id: uint }
  {
    vote: bool, ;; true = for, false = against
    voting-power: uint,
    block-height: uint
  }
)

;; Governance participation rewards
(define-map governance-rewards
  { user: principal }
  {
    total-votes: uint,
    total-rewards: uint,
    last-claim-block: uint,
    participation-score: uint
  }
)

;; Treasury allocations
(define-map treasury-allocations
  { allocation-id: uint }
  {
    recipient: principal,
    amount: uint,
    purpose: (string-ascii 100),
    approved: bool,
    executed: bool,
    proposal-id: uint
  }
)

;; Compliance records
(define-map compliance-records
  { user: principal }
  {
    kyc-verified: bool,
    kyc-level: uint,
    jurisdiction: uint,
    last-updated: uint,
    compliance-score: uint,
    tax-reporting-enabled: bool
  }
)

;; Regulatory compliance rules
(define-map compliance-rules
  { jurisdiction: uint, rule-type: uint }
  {
    enabled: bool,
    parameters: (buff 512),
    last-updated: uint,
    updated-by: principal
  }
)

;; Authorized compliance officers
(define-map compliance-officers
  { officer: principal }
  {
    authorized: bool,
    jurisdiction: uint,
    permissions: uint,
    appointed-block: uint
  }
)

;; =============================================================================
;; GOVERNANCE FUNCTIONS
;; =============================================================================

;; Create a new governance proposal
(define-public (create-proposal 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (proposal-type uint)
  (target-contract (optional principal))
  (function-name (optional (string-ascii 50)))
  (parameters (optional (buff 1024))))
  (let (
    (proposer tx-sender)
    (current-block block-height)
    (proposal-id (+ (var-get proposal-counter) u1))
    (user-balance (get-voting-power proposer))
  )
    ;; Check if user has enough tokens to create proposal
    (asserts! (>= user-balance MIN_PROPOSAL_THRESHOLD) ERR_INSUFFICIENT_TOKENS)
    
    ;; Check compliance if enabled
    (asserts! (or (not (var-get compliance-enabled)) 
                  (is-compliant proposer)) ERR_COMPLIANCE_VIOLATION)
    
    ;; Validate proposal parameters
    (asserts! (and (> (len title) u0) 
                   (<= (len description) MAX_PROPOSAL_DESCRIPTION)
                   (<= proposal-type u4)
                   (> proposal-type u0)) ERR_INVALID_PROPOSAL)
    
    ;; Store proposal
    (map-set proposals
      { proposal-id: proposal-id }
      {
        proposer: proposer,
        title: title,
        description: description,
        proposal-type: proposal-type,
        target-contract: target-contract,
        function-name: function-name,
        parameters: parameters,
        votes-for: u0,
        votes-against: u0,
        start-block: current-block,
        end-block: (+ current-block PROPOSAL_DURATION),
        executed: false,
        execution-block: none,
        quorum-met: false
      }
    )
    
    ;; Update proposal counter
    (var-set proposal-counter proposal-id)
    
    ;; Emit event
    (print {
      event: "proposal-created",
      proposal-id: proposal-id,
      proposer: proposer,
      title: title,
      proposal-type: proposal-type
    })
    
    (ok proposal-id)
  )
)

;; Vote on a proposal
(define-public (vote-on-proposal (proposal-id uint) (vote bool))
  (let (
    (voter tx-sender)
    (current-block block-height)
    (voting-power (get-voting-power voter))
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
  )
    ;; Check if proposal is active
    (asserts! (and (>= current-block (get start-block proposal))
                   (<= current-block (get end-block proposal))) ERR_PROPOSAL_NOT_ACTIVE)
    
    ;; Check if user hasn't voted already
    (asserts! (is-none (map-get? user-votes { user: voter, proposal-id: proposal-id })) ERR_ALREADY_VOTED)
    
    ;; Check compliance
    (asserts! (or (not (var-get compliance-enabled)) 
                  (is-compliant voter)) ERR_COMPLIANCE_VIOLATION)
    
    ;; Check voting power
    (asserts! (> voting-power u0) ERR_INSUFFICIENT_TOKENS)
    
    ;; Record vote
    (map-set user-votes
      { user: voter, proposal-id: proposal-id }
      {
        vote: vote,
        voting-power: voting-power,
        block-height: current-block
      }
    )
    
    ;; Update proposal vote counts
    (let (
      (updated-proposal (merge proposal
        (if vote
          { votes-for: (+ (get votes-for proposal) voting-power), votes-against: (get votes-against proposal) }
          { votes-for: (get votes-for proposal), votes-against: (+ (get votes-against proposal) voting-power) }
        )
      ))
    )
      (map-set proposals { proposal-id: proposal-id } updated-proposal)
      
      ;; Update user governance participation
      (update-governance-participation voter voting-power)
      
      ;; Emit event
      (print {
        event: "vote-cast",
        proposal-id: proposal-id,
        voter: voter,
        vote: vote,
        voting-power: voting-power
      })
      
      (ok true)
    )
  )
)

;; Execute a passed proposal
(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
    (current-block block-height)
    (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
  )
    ;; Check if proposal has ended
    (asserts! (> current-block (get end-block proposal)) ERR_PROPOSAL_NOT_ACTIVE)
    
    ;; Check if not already executed
    (asserts! (not (get executed proposal)) ERR_INVALID_PROPOSAL)
    
    ;; Check if execution delay has passed
    (asserts! (> current-block (+ (get end-block proposal) EXECUTION_DELAY)) ERR_PROPOSAL_NOT_ACTIVE)
    
    ;; Check quorum and majority
    (asserts! (>= total-votes QUORUM_THRESHOLD) ERR_QUORUM_NOT_MET)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR_INVALID_PROPOSAL)
    
    ;; Mark as executed
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { 
        executed: true, 
        execution-block: (some current-block),
        quorum-met: true 
      })
    )
    
    ;; Execute based on proposal type using if statements instead of match
    (let ((proposal-type (get proposal-type proposal)))
      (if (is-eq proposal-type PROPOSAL_TYPE_TREASURY)
        (execute-treasury-proposal proposal-id)
        (if (is-eq proposal-type PROPOSAL_TYPE_PARAMETER)
          (execute-parameter-proposal proposal-id)
          (if (is-eq proposal-type PROPOSAL_TYPE_UPGRADE)
            (execute-upgrade-proposal proposal-id)
            (if (is-eq proposal-type PROPOSAL_TYPE_COMPLIANCE)
              (execute-compliance-proposal proposal-id)
              ERR_INVALID_PROPOSAL
            )
          )
        )
      )
    )
  )
)

;; =============================================================================
;; TREASURY MANAGEMENT
;; =============================================================================

;; Allocate treasury funds
(define-private (execute-treasury-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR_PROPOSAL_NOT_FOUND))
  )
    ;; Treasury allocation logic would be implemented here
    ;; This is a placeholder for the actual treasury management
    (print {
      event: "treasury-proposal-executed",
      proposal-id: proposal-id
    })
    (ok true)
  )
)

;; Deposit funds to treasury
(define-public (deposit-to-treasury (amount uint))
  (begin
    (var-set treasury-balance (+ (var-get treasury-balance) amount))
    (print {
      event: "treasury-deposit",
      amount: amount,
      new-balance: (var-get treasury-balance)
    })
    (ok true)
  )
)

;; =============================================================================
;; COMPLIANCE AUTOMATION
;; =============================================================================

;; Update user compliance status
(define-public (update-compliance-status 
  (user principal)
  (kyc-verified bool)
  (kyc-level uint)
  (jurisdiction uint))
  (let (
    (caller tx-sender)
  )
    ;; Check if caller is authorized compliance officer
    (asserts! (is-compliance-officer caller jurisdiction) ERR_NOT_AUTHORIZED)
    
    ;; Update compliance record
    (map-set compliance-records
      { user: user }
      {
        kyc-verified: kyc-verified,
        kyc-level: kyc-level,
        jurisdiction: jurisdiction,
        last-updated: block-height,
        compliance-score: (calculate-compliance-score kyc-verified kyc-level),
        tax-reporting-enabled: true
      }
    )
    
    (print {
      event: "compliance-updated",
      user: user,
      kyc-verified: kyc-verified,
      jurisdiction: jurisdiction
    })
    
    (ok true)
  )
)

;; Set compliance rules for jurisdiction
(define-public (set-compliance-rule 
  (jurisdiction uint)
  (rule-type uint)
  (enabled bool)
  (parameters (buff 512)))
  (begin
    ;; Check authorization
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    
    ;; Set compliance rule
    (map-set compliance-rules
      { jurisdiction: jurisdiction, rule-type: rule-type }
      {
        enabled: enabled,
        parameters: parameters,
        last-updated: block-height,
        updated-by: tx-sender
      }
    )
    
    (ok true)
  )
)

;; Authorize compliance officer
(define-public (authorize-compliance-officer 
  (officer principal)
  (jurisdiction uint)
  (permissions uint))
  (begin
    ;; Check authorization
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    
    ;; Authorize officer
    (map-set compliance-officers
      { officer: officer }
      {
        authorized: true,
        jurisdiction: jurisdiction,
        permissions: permissions,
        appointed-block: block-height
      }
    )
    
    (ok true)
  )
)

;; =============================================================================
;; GOVERNANCE REWARDS
;; =============================================================================

;; Claim governance participation rewards
(define-public (claim-governance-rewards)
  (let (
    (user tx-sender)
    (user-rewards (default-to 
      { total-votes: u0, total-rewards: u0, last-claim-block: u0, participation-score: u0 }
      (map-get? governance-rewards { user: user })))
    (blocks-since-claim (- block-height (get last-claim-block user-rewards)))
    (reward-amount (calculate-governance-reward user blocks-since-claim))
  )
    ;; Check if user has rewards to claim
    (asserts! (> reward-amount u0) ERR_INSUFFICIENT_TOKENS)
    
    ;; Update user rewards record
    (map-set governance-rewards
      { user: user }
      (merge user-rewards {
        total-rewards: (+ (get total-rewards user-rewards) reward-amount),
        last-claim-block: block-height
      })
    )
    
    ;; Update total governance rewards
    (var-set total-governance-rewards (+ (var-get total-governance-rewards) reward-amount))
    
    (print {
      event: "governance-rewards-claimed",
      user: user,
      amount: reward-amount
    })
    
    (ok reward-amount)
  )
)

;; =============================================================================
;; HELPER FUNCTIONS
;; =============================================================================

;; Get voting power of a user (placeholder - would integrate with token contract)
(define-private (get-voting-power (user principal))
  ;; This would call the governance token contract to get user balance
  ;; For now, returning a placeholder value
  u1000000
)

;; Check if user is compliant
(define-private (is-compliant (user principal))
  (let (
    (compliance (map-get? compliance-records { user: user }))
  )
    (match compliance
      record (and (get kyc-verified record) (> (get compliance-score record) u50))
      false ;; No compliance record means not compliant
    )
  )
)

;; Check if user is compliance officer
(define-private (is-compliance-officer (user principal) (jurisdiction uint))
  (let (
    (officer-record (map-get? compliance-officers { officer: user }))
  )
    (match officer-record
      record (and (get authorized record) (is-eq (get jurisdiction record) jurisdiction))
      false
    )
  )
)

;; Calculate compliance score
(define-private (calculate-compliance-score (kyc-verified bool) (kyc-level uint))
  (if kyc-verified
    (+ u50 (* kyc-level u10))
    u0
  )
)

;; Update governance participation
(define-private (update-governance-participation (user principal) (voting-power uint))
  (let (
    (current-rewards (default-to 
      { total-votes: u0, total-rewards: u0, last-claim-block: u0, participation-score: u0 }
      (map-get? governance-rewards { user: user })))
  )
    (map-set governance-rewards
      { user: user }
      (merge current-rewards {
        total-votes: (+ (get total-votes current-rewards) u1),
        participation-score: (+ (get participation-score current-rewards) (/ voting-power u1000))
      })
    )
  )
)

;; Calculate governance reward
(define-private (calculate-governance-reward (user principal) (blocks-elapsed uint))
  (let (
    (user-rewards (default-to 
      { total-votes: u0, total-rewards: u0, last-claim-block: u0, participation-score: u0 }
      (map-get? governance-rewards { user: user })))
    (participation-score (get participation-score user-rewards))
  )
    ;; Reward calculation based on participation score and time
    (/ (* participation-score blocks-elapsed) u1000)
  )
)

;; Execute parameter proposal (placeholder)
(define-private (execute-parameter-proposal (proposal-id uint))
  (begin
    (print { event: "parameter-proposal-executed", proposal-id: proposal-id })
    (ok true)
  )
)

;; Execute upgrade proposal (placeholder)
(define-private (execute-upgrade-proposal (proposal-id uint))
  (begin
    (print { event: "upgrade-proposal-executed", proposal-id: proposal-id })
    (ok true)
  )
)

;; Execute compliance proposal (placeholder)
(define-private (execute-compliance-proposal (proposal-id uint))
  (begin
    (print { event: "compliance-proposal-executed", proposal-id: proposal-id })
    (ok true)
  )
)

;; =============================================================================
;; READ-ONLY FUNCTIONS
;; =============================================================================

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

;; Get user vote on proposal
(define-read-only (get-user-vote (user principal) (proposal-id uint))
  (map-get? user-votes { user: user, proposal-id: proposal-id })
)

;; Get user compliance status
(define-read-only (get-compliance-status (user principal))
  (map-get? compliance-records { user: user })
)

;; Get governance rewards for user
(define-read-only (get-governance-rewards (user principal))
  (map-get? governance-rewards { user: user })
)

;; Get treasury balance
(define-read-only (get-treasury-balance)
  (var-get treasury-balance)
)

;; Get proposal count
(define-read-only (get-proposal-count)
  (var-get proposal-counter)
)

;; Check if proposal passed
(define-read-only (proposal-passed (proposal-id uint))
  (let (
    (proposal (map-get? proposals { proposal-id: proposal-id }))
  )
    (match proposal
      p (and 
          (> (get votes-for p) (get votes-against p))
          (>= (+ (get votes-for p) (get votes-against p)) QUORUM_THRESHOLD))
      false
    )
  )
)
