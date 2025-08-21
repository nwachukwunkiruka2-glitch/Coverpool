;; title: CoverPool - Parametric Micro-Insurance Platform
;; version: 1.0.0
;; summary: Automated insurance with oracle-triggered payouts and pooled underwriting
;; description: A decentralized insurance platform that enables users to purchase parametric policies
;;              with automated claim settlements based on oracle data. Underwriters can provide
;;              liquidity through LP shares and earn premiums from policy sales.

;; traits
(define-trait oracle-trait
  (
    (get-data (uint) (response uint uint))
  ))

;; token definitions
(define-fungible-token coverpool-lp-token)

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-FUNDS (err u101))
(define-constant ERR-POLICY-NOT-FOUND (err u102))
(define-constant ERR-POLICY-EXPIRED (err u103))
(define-constant ERR-CLAIM-ALREADY-SETTLED (err u104))
(define-constant ERR-INVALID-ORACLE (err u105))
(define-constant ERR-TRIGGER-NOT-MET (err u106))
(define-constant ERR-INVALID-RISK-LEVEL (err u107))
(define-constant ERR-POOL-INSUFFICIENT (err u108))
(define-constant ERR-INVALID-AMOUNT (err u109))
(define-constant ERR-POLICY-ACTIVE (err u110))

(define-constant MAX-RISK-LEVEL u5)
(define-constant MIN-POLICY-AMOUNT u1000000) ;; 1 STX minimum
(define-constant REINSURANCE-RATE u10) ;; 10% to reinsurance
(define-constant TREASURY-RATE u5) ;; 5% to treasury

;; data vars
(define-data-var contract-active bool true)
(define-data-var policy-counter uint u0)
(define-data-var total-pool-balance uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var reinsurance-balance uint u0)
(define-data-var authorized-oracle principal CONTRACT-OWNER)

;; data maps
(define-map policies
  { policy-id: uint }
  {
    holder: principal,
    amount: uint,
    premium: uint,
    risk-level: uint,
    trigger-value: uint,
    trigger-condition: (string-ascii 10), ;; ">=", "<=", "=="
    expiry-block: uint,
    settled: bool,
    payout-amount: uint
  }
)

(define-map underwriter-shares
  { underwriter: principal }
  { 
    shares: uint,
    total-contributed: uint,
    rewards-claimed: uint
  }
)

(define-map risk-premiums
  { risk-level: uint }
  { premium-rate: uint } ;; basis points (100 = 1%)
)

(define-map oracle-data
  { data-id: uint }
  {
    value: uint,
    timestamp: uint,
    verified: bool
  }
)

 public functions

;; Initialize risk premium rates
(define-public (initialize-risk-rates)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (map-set risk-premiums { risk-level: u1 } { premium-rate: u200 }) ;; 2%
    (map-set risk-premiums { risk-level: u2 } { premium-rate: u400 }) ;; 4%
    (map-set risk-premiums { risk-level: u3 } { premium-rate: u600 }) ;; 6%
    (map-set risk-premiums { risk-level: u4 } { premium-rate: u800 }) ;; 8%
    (map-set risk-premiums { risk-level: u5 } { premium-rate: u1000 }) ;; 10%
    (ok true)
  )
)

;; Add liquidity to the underwriting pool
(define-public (add-liquidity (amount uint))
  (let
    (
      (current-shares (get shares (default-to 
        { shares: u0, total-contributed: u0, rewards-claimed: u0 }
        (map-get? underwriter-shares { underwriter: tx-sender }))))
      (current-contributed (get total-contributed (default-to 
        { shares: u0, total-contributed: u0, rewards-claimed: u0 }
        (map-get? underwriter-shares { underwriter: tx-sender }))))
      (treasury-fee (/ (* amount TREASURY-RATE) u100))
      (net-amount (- amount treasury-fee))
      (shares-to-mint (if (is-eq (var-get total-pool-balance) u0)
        amount
        (/ (* amount (ft-get-supply coverpool-lp-token)) (var-get total-pool-balance))))
    )
    (asserts! (var-get contract-active) ERR-UNAUTHORIZED)
    (asserts! (>= amount MIN-POLICY-AMOUNT) ERR-INVALID-AMOUNT)
    
    ;; Transfer STX from user
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update treasury
    (var-set treasury-balance (+ (var-get treasury-balance) treasury-fee))
    
    ;; Update pool balance
    (var-set total-pool-balance (+ (var-get total-pool-balance) net-amount))
    
    ;; Mint LP tokens
    (try! (ft-mint? coverpool-lp-token shares-to-mint tx-sender))
    
    ;; Update underwriter data
    (map-set underwriter-shares 
      { underwriter: tx-sender }
      {
        shares: (+ current-shares shares-to-mint),
        total-contributed: (+ current-contributed amount),
        rewards-claimed: (get rewards-claimed (default-to 
          { shares: u0, total-contributed: u0, rewards-claimed: u0 }
          (map-get? underwriter-shares { underwriter: tx-sender })))
      }
    )
    
    (ok shares-to-mint)
  )
)

;; Remove liquidity from the underwriting pool
(define-public (remove-liquidity (shares-to-burn uint))
  (let
    (
      (user-shares (get shares (default-to 
        { shares: u0, total-contributed: u0, rewards-claimed: u0 }
        (map-get? underwriter-shares { underwriter: tx-sender }))))
      (total-supply (ft-get-supply coverpool-lp-token))
      (withdrawal-amount (/ (* shares-to-burn (var-get total-pool-balance)) total-supply))
    )
    (asserts! (>= user-shares shares-to-burn) ERR-INSUFFICIENT-FUNDS)
    (asserts! (>= (var-get total-pool-balance) withdrawal-amount) ERR-POOL-INSUFFICIENT)
    
    ;; Burn LP tokens
    (try! (ft-burn? coverpool-lp-token shares-to-burn tx-sender))
    
    ;; Update pool balance
    (var-set total-pool-balance (- (var-get total-pool-balance) withdrawal-amount))
    
    ;; Transfer STX to user
    (try! (as-contract (stx-transfer? withdrawal-amount tx-sender tx-sender)))
    
    ;; Update underwriter data
    (map-set underwriter-shares 
      { underwriter: tx-sender }
      {
        shares: (- user-shares shares-to-burn),
        total-contributed: (get total-contributed (default-to 
          { shares: u0, total-contributed: u0, rewards-claimed: u0 }
          (map-get? underwriter-shares { underwriter: tx-sender }))),
        rewards-claimed: (get rewards-claimed (default-to 
          { shares: u0, total-contributed: u0, rewards-claimed: u0 }
          (map-get? underwriter-shares { underwriter: tx-sender })))
      }
    )
    
    (ok withdrawal-amount)
  )
)

;; Purchase an insurance policy
(define-public (purchase-policy 
  (coverage-amount uint) 
  (risk-level uint) 
  (trigger-value uint)
  (trigger-condition (string-ascii 10))
  (duration-blocks uint))
  (let
    (
      (policy-id (+ (var-get policy-counter) u1))
      (premium-rate (get premium-rate (unwrap! (map-get? risk-premiums { risk-level: risk-level }) ERR-INVALID-RISK-LEVEL)))
      (premium-amount (/ (* coverage-amount premium-rate) u10000))
      (treasury-fee (/ (* premium-amount TREASURY-RATE) u100))
      (reinsurance-fee (/ (* premium-amount REINSURANCE-RATE) u100))
      (net-premium (- premium-amount (+ treasury-fee reinsurance-fee)))
      (expiry-block (+ block-height duration-blocks))
    )
    (asserts! (var-get contract-active) ERR-UNAUTHORIZED)
    (asserts! (<= risk-level MAX-RISK-LEVEL) ERR-INVALID-RISK-LEVEL)
    (asserts! (>= coverage-amount MIN-POLICY-AMOUNT) ERR-INVALID-AMOUNT)
    (asserts! (<= coverage-amount (var-get total-pool-balance)) ERR-POOL-INSUFFICIENT)
    
    ;; Transfer premium from user
    (try! (stx-transfer? premium-amount tx-sender (as-contract tx-sender)))
    
    ;; Distribute fees
    (var-set treasury-balance (+ (var-get treasury-balance) treasury-fee))
    (var-set reinsurance-balance (+ (var-get reinsurance-balance) reinsurance-fee))
    (var-set total-pool-balance (+ (var-get total-pool-balance) net-premium))
    
    ;; Create policy
    (map-set policies
      { policy-id: policy-id }
      {
        holder: tx-sender,
        amount: coverage-amount,
        premium: premium-amount,
        risk-level: risk-level,
        trigger-value: trigger-value,
        trigger-condition: trigger-condition,
        expiry-block: expiry-block,
        settled: false,
        payout-amount: u0
      }
    )
    
    ;; Update counter
    (var-set policy-counter policy-id)
    
    (ok policy-id)
  )
)

;; Submit oracle data (only authorized oracle)
(define-public (submit-oracle-data (data-id uint) (value uint))
  (begin
    (asserts! (is-eq tx-sender (var-get authorized-oracle)) ERR-UNAUTHORIZED)
    
    (map-set oracle-data
      { data-id: data-id }
      {
        value: value,
        timestamp: block-height,
        verified: true
      }
    )
    
    (ok true)
  )
)

;; Settle claim based on oracle data
(define-public (settle-claim (policy-id uint) (data-id uint))
  (let
    (
      (policy (unwrap! (map-get? policies { policy-id: policy-id }) ERR-POLICY-NOT-FOUND))
      (oracle-info (unwrap! (map-get? oracle-data { data-id: data-id }) ERR-INVALID-ORACLE))
    )
    (asserts! (not (get settled policy)) ERR-CLAIM-ALREADY-SETTLED)
    (asserts! (< block-height (get expiry-block policy)) ERR-POLICY-EXPIRED)
    (asserts! (get verified oracle-info) ERR-INVALID-ORACLE)
    
    (if (check-trigger-condition 
          (get trigger-condition policy) 
          (get value oracle-info) 
          (get trigger-value policy))
      ;; Trigger met - pay out claim
      (let
        (
          (payout-amount (get amount policy))
        )
        (asserts! (>= (var-get total-pool-balance) payout-amount) ERR-POOL-INSUFFICIENT)
        
        ;; Update pool balance
        (var-set total-pool-balance (- (var-get total-pool-balance) payout-amount))
        
        ;; Transfer payout to policy holder
        (try! (as-contract (stx-transfer? payout-amount tx-sender (get holder policy))))
        
        ;; Mark policy as settled
        (map-set policies
          { policy-id: policy-id }
          (merge policy { settled: true, payout-amount: payout-amount })
        )
        
        (ok { triggered: true, payout: payout-amount })
      )
      ;; Trigger not met - no payout
      (begin
        (map-set policies
          { policy-id: policy-id }
          (merge policy { settled: true, payout-amount: u0 })
        )
        
        (ok { triggered: false, payout: u0 })
      )
    )
  )
)

;; Emergency pause (only contract owner)
(define-public (toggle-contract-status)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set contract-active (not (var-get contract-active)))
    (ok (var-get contract-active))
  )
)

;; Update authorized oracle (only contract owner)
(define-public (set-authorized-oracle (new-oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set authorized-oracle new-oracle)
    (ok true)
  )
)

;; Withdraw treasury funds (only contract owner)
(define-public (withdraw-treasury (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (<= amount (var-get treasury-balance)) ERR-INSUFFICIENT-FUNDS)
    
    (var-set treasury-balance (- (var-get treasury-balance) amount))
    (try! (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER)))
    
    (ok amount)
  )
)

; read only functions

;; Get policy details
(define-read-only (get-policy (policy-id uint))
  (map-get? policies { policy-id: policy-id })
)

;; Get underwriter information
(define-read-only (get-underwriter-info (underwriter principal))
  (map-get? underwriter-shares { underwriter: underwriter })
)

;; Get pool statistics
(define-read-only (get-pool-stats)
  {
    total-pool-balance: (var-get total-pool-balance),
    treasury-balance: (var-get treasury-balance),
    reinsurance-balance: (var-get reinsurance-balance),
    total-lp-supply: (ft-get-supply coverpool-lp-token),
    active: (var-get contract-active)
  }
)

;; Get risk premium rate
(define-read-only (get-risk-premium-rate (risk-level uint))
  (map-get? risk-premiums { risk-level: risk-level })
)

;; Get oracle data
(define-read-only (get-oracle-data (data-id uint))
  (map-get? oracle-data { data-id: data-id })
)

;; Calculate premium for a policy
(define-read-only (calculate-premium (coverage-amount uint) (risk-level uint))
  (match (map-get? risk-premiums { risk-level: risk-level })
    premium-info 
      (let
        ((premium-rate (get premium-rate premium-info)))
        (ok (/ (* coverage-amount premium-rate) u10000))
      )
    ERR-INVALID-RISK-LEVEL
  )
)

;; Get user's LP token balance
(define-read-only (get-lp-balance (user principal))
  (ft-get-balance coverpool-lp-token user)
)

;; Check if policy is active
(define-read-only (is-policy-active (policy-id uint))
  (match (map-get? policies { policy-id: policy-id })
    policy
      (and 
        (not (get settled policy))
        (< block-height (get expiry-block policy))
      )
    false
  )
)

;; private functions