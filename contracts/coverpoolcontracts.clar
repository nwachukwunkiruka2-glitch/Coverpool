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