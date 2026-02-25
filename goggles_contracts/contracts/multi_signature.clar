;; Multi-Signature Wallet Smart Contract
;; Allows multiple owners to collectively manage funds with approval requirements

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_OWNER (err u101))
(define-constant ERR_PROPOSAL_NOT_FOUND (err u102))
(define-constant ERR_ALREADY_APPROVED (err u103))
(define-constant ERR_INSUFFICIENT_APPROVALS (err u104))
(define-constant ERR_PROPOSAL_EXECUTED (err u105))
(define-constant ERR_INVALID_AMOUNT (err u106))
(define-constant ERR_INSUFFICIENT_BALANCE (err u107))
(define-constant ERR_OWNER_ALREADY_EXISTS (err u108))
(define-constant ERR_INVALID_THRESHOLD (err u109))

;; Data Variables
(define-data-var required-approvals uint u2)
(define-data-var proposal-counter uint u0)

;; Data Maps
;; Store wallet owners
(define-map owners principal bool)

;; Store withdrawal proposals
(define-map proposals 
    uint 
    {
        proposer: principal,
        recipient: principal,
        amount: uint,
        approvals: uint,
        executed: bool,
        created-at: uint
    }
)

;; Track who approved which proposal
(define-map proposal-approvals {proposal-id: uint, approver: principal} bool)

;; Public Functions

;; Initialize the wallet with initial owners and required approvals
(define-public (initialize (initial-owners (list 10 principal)) (threshold uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (and (> threshold u0) (<= threshold (len initial-owners))) ERR_INVALID_THRESHOLD)
        (var-set required-approvals threshold)
        (fold add-owner-fold initial-owners true)
        (ok true)
    )
)

;; Add a new owner (requires existing owner)
(define-public (add-owner (new-owner principal))
    (begin
        (asserts! (is-owner tx-sender) ERR_NOT_OWNER)
        (asserts! (not (is-owner new-owner)) ERR_OWNER_ALREADY_EXISTS)
        (map-set owners new-owner true)
        (ok true)
    )
)

;; Remove an owner (requires existing owner)
(define-public (remove-owner (owner-to-remove principal))
    (begin
        (asserts! (is-owner tx-sender) ERR_NOT_OWNER)
        (asserts! (is-owner owner-to-remove) ERR_NOT_OWNER)
        (map-delete owners owner-to-remove)
        (ok true)
    )
)

;; Propose a withdrawal
(define-public (propose-withdrawal (recipient principal) (amount uint))
    (let 
        (
            (proposal-id (+ (var-get proposal-counter) u1))
        )
        (begin
            (asserts! (is-owner tx-sender) ERR_NOT_OWNER)
            (asserts! (> amount u0) ERR_INVALID_AMOUNT)
            (asserts! (>= (stx-get-balance (as-contract tx-sender)) amount) ERR_INSUFFICIENT_BALANCE)
            
            ;; Create the proposal
            (map-set proposals proposal-id
                {
                    proposer: tx-sender,
                    recipient: recipient,
                    amount: amount,
                    approvals: u1,
                    executed: false,
                    created-at: burn-block-height
                }
            )
            
            ;; Automatically approve by proposer
            (map-set proposal-approvals {proposal-id: proposal-id, approver: tx-sender} true)
            
            ;; Increment proposal counter
            (var-set proposal-counter proposal-id)
            
            (ok proposal-id)
        )
    )
)

;; Approve a withdrawal proposal
(define-public (approve-withdrawal (proposal-id uint))
    (let 
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
            (approval-key {proposal-id: proposal-id, approver: tx-sender})
        )
        (begin
            (asserts! (is-owner tx-sender) ERR_NOT_OWNER)
            (asserts! (not (get executed proposal)) ERR_PROPOSAL_EXECUTED)
            (asserts! (is-none (map-get? proposal-approvals approval-key)) ERR_ALREADY_APPROVED)
            
            ;; Add approval
            (map-set proposal-approvals approval-key true)
            
            ;; Update proposal with incremented approvals
            (map-set proposals proposal-id
                (merge proposal {approvals: (+ (get approvals proposal) u1)})
            )
            
            (ok true)
        )
    )
)

;; Execute withdrawal after sufficient approvals
(define-public (execute-withdrawal (proposal-id uint))
    (let 
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR_PROPOSAL_NOT_FOUND))
        )
        (begin
            (asserts! (is-owner tx-sender) ERR_NOT_OWNER)
            (asserts! (not (get executed proposal)) ERR_PROPOSAL_EXECUTED)
            (asserts! (>= (get approvals proposal) (var-get required-approvals)) ERR_INSUFFICIENT_APPROVALS)
            (asserts! (>= (stx-get-balance (as-contract tx-sender)) (get amount proposal)) ERR_INSUFFICIENT_BALANCE)
            
            ;; Mark as executed
            (map-set proposals proposal-id
                (merge proposal {executed: true})
            )
            
            ;; Transfer STX
            (as-contract (stx-transfer? (get amount proposal) tx-sender (get recipient proposal)))
        )
    )
)

;; Deposit STX to the wallet
(define-public (deposit (amount uint))
    (stx-transfer? amount tx-sender (as-contract tx-sender))
)

;; Update required approvals threshold
(define-public (set-required-approvals (new-threshold uint))
    (begin
        (asserts! (is-owner tx-sender) ERR_NOT_OWNER)
        (asserts! (> new-threshold u0) ERR_INVALID_THRESHOLD)
        (var-set required-approvals new-threshold)
        (ok true)
    )
)

;; Read-only Functions

;; Check if an address is an owner
(define-read-only (is-owner (address principal))
    (default-to false (map-get? owners address))
)

;; Get proposal details
(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

;; Check if someone approved a proposal
(define-read-only (has-approved (proposal-id uint) (approver principal))
    (default-to false (map-get? proposal-approvals {proposal-id: proposal-id, approver: approver}))
)

;; Get required approvals threshold
(define-read-only (get-required-approvals)
    (var-get required-approvals)
)

;; Get current proposal counter
(define-read-only (get-proposal-counter)
    (var-get proposal-counter)
)

;; Get wallet balance
(define-read-only (get-balance)
    (stx-get-balance (as-contract tx-sender))
)

;; Private Functions

;; Helper function for adding owners during initialization
(define-private (add-owner-fold (owner principal) (prev-result bool))
    (begin
        (map-set owners owner true)
        true
    )
)