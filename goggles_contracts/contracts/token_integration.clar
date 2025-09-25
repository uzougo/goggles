;; TokenIntegration Smart Contract
;; Purpose: Use custom tokens to pay for storage and rewards

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-INVALID-TOKEN (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-TRANSFER-FAILED (err u103))
(define-constant ERR-INVALID-AMOUNT (err u104))
(define-constant ERR-UNAUTHORIZED (err u105))
(define-constant ERR-INVALID-RATE (err u106))
(define-constant ERR-TOKEN-NOT-REGISTERED (err u107))

;; Data Variables
(define-data-var contract-owner principal CONTRACT-OWNER)
(define-data-var stx-to-token-rate uint u1000) ;; 1 STX = 1000 tokens (adjustable)
(define-data-var treasury-wallet principal CONTRACT-OWNER)

;; Data Maps
(define-map registered-tokens principal 
  { 
    active: bool, 
    exchange-rate: uint,
    name: (string-ascii 32),
    symbol: (string-ascii 10)
  })
(define-map storage-payments 
  { user: principal, token: principal } 
  { amount: uint, block-height: uint })
(define-map node-rewards 
  { node: principal, token: principal } 
  { total-earned: uint, last-claim: uint })
(define-map authorized-operators principal bool)
(define-map pending-rewards
  { node: principal, token: principal }
  uint)

;; Token Registration Functions
(define-public (register-token 
  (token-contract principal) 
  (exchange-rate uint)
  (name (string-ascii 32))
  (symbol (string-ascii 10)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (asserts! (> exchange-rate u0) ERR-INVALID-RATE)
    (map-set registered-tokens token-contract 
      { 
        active: true, 
        exchange-rate: exchange-rate,
        name: name,
        symbol: symbol
      })
    (ok true)))

(define-public (deactivate-token (token-contract principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (asserts! (is-token-registered token-contract) ERR-TOKEN-NOT-REGISTERED)
    (map-set registered-tokens token-contract 
      (merge (unwrap-panic (map-get? registered-tokens token-contract))
             { active: false }))
    (ok true)))

(define-public (update-token-rate (token-contract principal) (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (asserts! (> new-rate u0) ERR-INVALID-RATE)
    (asserts! (is-token-registered token-contract) ERR-TOKEN-NOT-REGISTERED)
    (map-set registered-tokens token-contract 
      (merge (unwrap-panic (map-get? registered-tokens token-contract))
             { exchange-rate: new-rate }))
    (ok true)))

;; Internal function to safely transfer tokens
(define-private (safe-token-transfer 
  (token-contract principal)
  (amount uint)
  (sender principal)
  (recipient principal))
  (let (
    (token-info (unwrap! (map-get? registered-tokens token-contract) ERR-TOKEN-NOT-REGISTERED))
  )
    (asserts! (get active token-info) ERR-TOKEN-NOT-REGISTERED)
    ;; Use a simple transfer mechanism that relies on the token being pre-approved
    ;; In a real implementation, you would call the specific token's transfer function
    ;; For now, we'll track the transfer internally
    (ok true)))

;; Payment Functions
(define-public (pay-for-storage-with-token 
  (token-contract principal) 
  (amount uint) 
  (storage-size uint))
  (let (
    (token-info (unwrap! (map-get? registered-tokens token-contract) ERR-TOKEN-NOT-REGISTERED))
    (required-amount (* storage-size (get exchange-rate token-info)))
  )
    (asserts! (get active token-info) ERR-TOKEN-NOT-REGISTERED)
    (asserts! (>= amount required-amount) ERR-INSUFFICIENT-BALANCE)
    
    ;; In a real implementation, this would call the token's transfer function
    ;; For security, we only work with pre-registered, validated token contracts
    (try! (safe-token-transfer token-contract amount tx-sender (var-get treasury-wallet)))
    
    ;; Record payment
    (map-set storage-payments 
      { user: tx-sender, token: token-contract }
      { amount: amount, block-height: block-height })
    
    (ok amount)))

(define-public (pay-for-storage-with-stx (storage-size uint))
  (let (
    (stx-rate (var-get stx-to-token-rate))
    (required-amount (* storage-size stx-rate))
  )
    (asserts! (> required-amount u0) ERR-INVALID-AMOUNT)
    
    ;; Transfer STX to treasury
    (try! (stx-transfer? required-amount tx-sender (var-get treasury-wallet)))
    
    ;; Record payment (using contract principal as "token" for STX)
    (map-set storage-payments 
      { user: tx-sender, token: (as-contract tx-sender) }
      { amount: required-amount, block-height: block-height })
    
    (ok required-amount)))

;; STX to Token Conversion (conceptual - would need actual token integration)
(define-public (convert-stx-to-tokens 
  (stx-amount uint) 
  (target-token principal))
  (let (
    (token-info (unwrap! (map-get? registered-tokens target-token) ERR-TOKEN-NOT-REGISTERED))
    (token-amount (* stx-amount (get exchange-rate token-info)))
  )
    (asserts! (get active token-info) ERR-TOKEN-NOT-REGISTERED)
    (asserts! (> stx-amount u0) ERR-INVALID-AMOUNT)
    
    ;; Transfer STX from user to contract
    (try! (stx-transfer? stx-amount tx-sender (as-contract tx-sender)))
    
    ;; In a real implementation, this would mint or transfer tokens to the user
    ;; For now, we record the conversion
    (map-set storage-payments 
      { user: tx-sender, token: target-token }
      { amount: token-amount, block-height: block-height })
    
    (ok token-amount)))

;; Reward Distribution Functions
(define-public (distribute-token-rewards 
  (node principal) 
  (token-contract principal) 
  (reward-amount uint))
  (let (
    (is-authorized (default-to false (map-get? authorized-operators tx-sender)))
    (token-info (unwrap! (map-get? registered-tokens token-contract) ERR-TOKEN-NOT-REGISTERED))
    (current-rewards (default-to 
      { total-earned: u0, last-claim: u0 } 
      (map-get? node-rewards { node: node, token: token-contract })))
  )
    (asserts! (or is-authorized (is-eq tx-sender (var-get contract-owner))) ERR-UNAUTHORIZED)
    (asserts! (get active token-info) ERR-TOKEN-NOT-REGISTERED)
    (asserts! (> reward-amount u0) ERR-INVALID-AMOUNT)
    
    ;; Add to pending rewards instead of direct transfer
    (map-set pending-rewards 
      { node: node, token: token-contract }
      (+ (default-to u0 (map-get? pending-rewards { node: node, token: token-contract }))
         reward-amount))
    
    ;; Update reward records
    (map-set node-rewards 
      { node: node, token: token-contract }
      { 
        total-earned: (+ (get total-earned current-rewards) reward-amount),
        last-claim: block-height 
      })
    
    (ok reward-amount)))

(define-public (claim-node-rewards (token-contract principal))
  (let (
    (token-info (unwrap! (map-get? registered-tokens token-contract) ERR-TOKEN-NOT-REGISTERED))
    (pending-amount (default-to u0 (map-get? pending-rewards { node: tx-sender, token: token-contract })))
  )
    (asserts! (get active token-info) ERR-TOKEN-NOT-REGISTERED)
    (asserts! (> pending-amount u0) ERR-INSUFFICIENT-BALANCE)
    
    ;; Clear pending rewards
    (map-delete pending-rewards { node: tx-sender, token: token-contract })
    
    ;; In a real implementation, this would transfer actual tokens
    ;; For now, we just clear the pending amount
    (ok pending-amount)))

;; Administrative Functions
(define-public (add-authorized-operator (operator principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (map-set authorized-operators operator true)
    (ok true)))

(define-public (remove-authorized-operator (operator principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (map-delete authorized-operators operator)
    (ok true)))

(define-public (update-stx-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (asserts! (> new-rate u0) ERR-INVALID-RATE)
    (var-set stx-to-token-rate new-rate)
    (ok new-rate)))

(define-public (update-treasury-wallet (new-wallet principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (var-set treasury-wallet new-wallet)
    (ok new-wallet)))

(define-public (transfer-ownership (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (var-set contract-owner new-owner)
    (ok new-owner)))

;; Read-only Functions
(define-read-only (is-token-registered (token-contract principal))
  (match (map-get? registered-tokens token-contract)
    token-info (get active token-info)
    false))

(define-read-only (get-token-info (token-contract principal))
  (map-get? registered-tokens token-contract))

(define-read-only (get-token-rate (token-contract principal))
  (match (map-get? registered-tokens token-contract)
    token-info (some (get exchange-rate token-info))
    none))

(define-read-only (get-stx-rate)
  (var-get stx-to-token-rate))

(define-read-only (get-storage-payment (user principal) (token principal))
  (map-get? storage-payments { user: user, token: token }))

(define-read-only (get-node-rewards (node principal) (token principal))
  (map-get? node-rewards { node: node, token: token }))

(define-read-only (get-pending-rewards (node principal) (token principal))
  (map-get? pending-rewards { node: node, token: token }))

(define-read-only (is-authorized-operator (operator principal))
  (default-to false (map-get? authorized-operators operator)))

(define-read-only (get-contract-owner)
  (var-get contract-owner))

(define-read-only (get-treasury-wallet)
  (var-get treasury-wallet))

;; Emergency Functions
(define-public (emergency-withdraw-stx (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-OWNER-ONLY)
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    (ok amount)))