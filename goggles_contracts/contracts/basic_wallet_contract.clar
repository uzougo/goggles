;; Basic Wallet Contract in Clarity
;; A simple wallet that allows deposits and owner-only withdrawals

;; Define the contract owner (deployer)
(define-constant contract-owner tx-sender)

;; Error constants
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-transfer-failed (err u102))

;; Data variable to track the contract's STX balance
(define-data-var wallet-balance uint u0)

;; Public function: deposit STX into the wallet
;; This function accepts STX transfers and updates the balance
(define-public (deposit (amount uint))
  (begin
    ;; Transfer STX from caller to contract
    (match (stx-transfer? amount tx-sender (as-contract tx-sender))
      success (begin
        ;; Update the wallet balance
        (var-set wallet-balance (+ (var-get wallet-balance) amount))
        (ok amount))
      error (err err-transfer-failed))))

;; Public function: withdraw STX from the wallet (owner only)
;; Only the contract owner can withdraw funds
(define-public (withdraw (amount uint))
  (begin
    ;; Check if caller is the owner
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Check if contract has sufficient balance
    (asserts! (>= (var-get wallet-balance) amount) err-insufficient-balance)
    
    ;; Transfer STX from contract to owner
    (unwrap! (as-contract (stx-transfer? amount tx-sender contract-owner)) err-transfer-failed)
    
    ;; Update the wallet balance
    (var-set wallet-balance (- (var-get wallet-balance) amount))
    (ok amount)))

;; Read-only function: get current wallet balance
(define-read-only (get-balance)
  (var-get wallet-balance))

;; Read-only function: get contract owner
(define-read-only (get-owner)
  contract-owner)

;; Read-only function: check if caller is owner
(define-read-only (is-owner)
  (is-eq tx-sender contract-owner))