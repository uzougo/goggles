;; Audit Trail Wallet Smart Contract
;; Maintains full history of all deposits and withdrawals with comprehensive audit trail

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-invalid-user (err u103))

;; Data Variables
(define-data-var next-transaction-id uint u1)

;; Data Maps
;; User balances
(define-map user-balances principal uint)

;; Transaction history - stores all deposit/withdrawal records
(define-map transaction-history 
    uint 
    {
        user: principal,
        transaction-type: (string-ascii 10), ;; "deposit" or "withdrawal"
        amount: uint,
        timestamp: uint, ;; burn-block-height when transaction occurred
        block-height: uint, ;; same as timestamp for consistency
        balance-after: uint
    }
)

;; User transaction indexes - maps user to list of their transaction IDs
(define-map user-transaction-ids principal (list 1000 uint))

;; Total transaction count per user
(define-map user-transaction-count principal uint)

;; Private Functions

;; Get current balance for a user
(define-private (get-balance-internal (user principal))
    (default-to u0 (map-get? user-balances user))
)

;; Update user balance
(define-private (set-balance (user principal) (new-balance uint))
    (map-set user-balances user new-balance)
)

;; Add transaction ID to user's transaction list
(define-private (add-transaction-to-user (user principal) (tx-id uint))
    (let ((current-list (default-to (list) (map-get? user-transaction-ids user))))
        (map-set user-transaction-ids user (unwrap-panic (as-max-len? (append current-list tx-id) u1000)))
    )
)

;; Increment user transaction count
(define-private (increment-user-tx-count (user principal))
    (let ((current-count (default-to u0 (map-get? user-transaction-count user))))
        (map-set user-transaction-count user (+ current-count u1))
    )
)

;; Record a transaction in the audit trail
(define-private (record-transaction (user principal) (tx-type (string-ascii 10)) (amount uint) (balance-after uint))
    (let ((tx-id (var-get next-transaction-id)))
        ;; Store transaction details
        (map-set transaction-history tx-id {
            user: user,
            transaction-type: tx-type,
            amount: amount,
            timestamp: burn-block-height,
            block-height: burn-block-height,
            balance-after: balance-after
        })
        ;; Add to user's transaction list
        (add-transaction-to-user user tx-id)
        ;; Increment counters
        (increment-user-tx-count user)
        (var-set next-transaction-id (+ tx-id u1))
        tx-id
    )
)

;; Public Functions

;; Deposit STX to user's wallet
(define-public (deposit (amount uint))
    (begin
        (asserts! (> amount u0) err-invalid-amount)
        (let ((user tx-sender)
              (current-balance (get-balance-internal tx-sender)))
            (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
            (let ((new-balance (+ current-balance amount)))
                (set-balance user new-balance)
                (record-transaction user "deposit" amount new-balance)
                (ok new-balance)
            )
        )
    )
)

;; Withdraw STX from user's wallet
(define-public (withdraw (amount uint))
    (begin
        (asserts! (> amount u0) err-invalid-amount)
        (let ((user tx-sender)
              (current-balance (get-balance-internal tx-sender)))
            (asserts! (>= current-balance amount) err-insufficient-balance)
            (try! (as-contract (stx-transfer? amount tx-sender user)))
            (let ((new-balance (- current-balance amount)))
                (set-balance user new-balance)
                (record-transaction user "withdrawal" amount new-balance)
                (ok new-balance)
            )
        )
    )
)

;; Read-only Functions

;; Get user's current balance
(define-read-only (get-balance (user principal))
    (get-balance-internal user)
)

;; Get specific transaction details by ID
(define-read-only (get-transaction (tx-id uint))
    (map-get? transaction-history tx-id)
)

;; Get user's transaction IDs list
(define-read-only (get-user-transaction-ids (user principal))
    (default-to (list) (map-get? user-transaction-ids user))
)

;; Get user's total transaction count
(define-read-only (get-user-transaction-count (user principal))
    (default-to u0 (map-get? user-transaction-count user))
)

;; Get user's recent transactions (last N transactions)
(define-read-only (get-recent-transactions (user principal) (limit uint))
    (let ((tx-ids (get-user-transaction-ids user))
          (tx-count (len tx-ids)))
        (if (> tx-count limit)
            ;; Return last 'limit' transactions
            (let ((start-index (- tx-count limit)))
                (slice? tx-ids start-index tx-count)
            )
            ;; Return all transactions if count <= limit
            (some tx-ids)
        )
    )
)

;; Get transaction details for a list of transaction IDs
(define-read-only (get-transactions-batch (tx-ids (list 100 uint)))
    (map get-transaction tx-ids)
)

;; Get user's transaction history with pagination
(define-read-only (get-user-transactions-paginated (user principal) (offset uint) (limit uint))
    (let ((tx-ids (get-user-transaction-ids user))
          (tx-count (len tx-ids)))
        (if (< offset tx-count)
            (let ((end-index (if (> (+ offset limit) tx-count) tx-count (+ offset limit))))
                (slice? tx-ids offset end-index)
            )
            none
        )
    )
)

;; Get user's deposit history only
(define-read-only (get-user-deposits (user principal))
    (filter is-deposit (map get-transaction (get-user-transaction-ids user)))
)

;; Get user's withdrawal history only
(define-read-only (get-user-withdrawals (user principal))
    (filter is-withdrawal (map get-transaction (get-user-transaction-ids user)))
)

;; Helper function to check if transaction is a deposit
(define-read-only (is-deposit (tx (optional {user: principal, transaction-type: (string-ascii 10), amount: uint, timestamp: uint, block-height: uint, balance-after: uint})))
    (match tx
        some-tx (is-eq (get transaction-type some-tx) "deposit")
        false
    )
)

;; Helper function to check if transaction is a withdrawal
(define-read-only (is-withdrawal (tx (optional {user: principal, transaction-type: (string-ascii 10), amount: uint, timestamp: uint, block-height: uint, balance-after: uint})))
    (match tx
        some-tx (is-eq (get transaction-type some-tx) "withdrawal")
        false
    )
)

;; Get total contract statistics
(define-read-only (get-contract-stats)
    {
        total-transactions: (- (var-get next-transaction-id) u1),
        contract-balance: (stx-get-balance (as-contract tx-sender))
    }
)

;; Admin function to get any user's balance (owner only)
(define-read-only (admin-get-balance (user principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (ok (get-balance-internal user))
    )
)