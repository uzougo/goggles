;; AccessLogs Smart Contract
;; Purpose: Log every access to a file for privacy tracking

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_FILE_NOT_FOUND (err u101))
(define-constant ERR_INVALID_PRIVACY_SETTING (err u102))
(define-constant ERR_LOGGING_DISABLED (err u103))
(define-constant ERR_ACCESS_LOG_NOT_FOUND (err u104))
(define-constant ERR_INVALID_INPUT (err u105))
(define-constant ERR_FILE_ID_TOO_LONG (err u106))
(define-constant ERR_ACCESS_TYPE_TOO_LONG (err u107))

;; Privacy setting constants
(define-constant PRIVACY_PUBLIC u0)
(define-constant PRIVACY_OWNER_ONLY u1)
(define-constant PRIVACY_DISABLED u2)

;; Data structures

;; File registry to track file ownership and privacy settings
(define-map files
  { file-id: (string-ascii 64) }
  {
    owner: principal,
    privacy-setting: uint,
    created-at: uint
  }
)

;; Access logs storage
(define-map access-logs
  { 
    file-id: (string-ascii 64),
    log-id: uint
  }
  {
    user-id: principal,
    timestamp: uint,
    access-type: (string-ascii 32)
  }
)

;; Counter for generating unique log IDs per file
(define-map log-counters
  { file-id: (string-ascii 64) }
  { count: uint }
)

;; Input validation functions
(define-private (is-valid-file-id (file-id (string-ascii 64)))
  (and (> (len file-id) u0)
       (<= (len file-id) u64)))

(define-private (is-valid-access-type (access-type (string-ascii 32)))
  (and (> (len access-type) u0)
       (<= (len access-type) u32)))

(define-private (is-valid-privacy-setting (setting uint))
  (or (is-eq setting PRIVACY_PUBLIC)
      (or (is-eq setting PRIVACY_OWNER_ONLY)
          (is-eq setting PRIVACY_DISABLED))))

;; Administrative functions

;; Register a new file with privacy settings
(define-public (register-file (file-id (string-ascii 64)) (privacy-setting uint))
  (begin
    ;; Validate inputs
    (asserts! (is-valid-file-id file-id) ERR_INVALID_INPUT)
    (asserts! (is-valid-privacy-setting privacy-setting) ERR_INVALID_PRIVACY_SETTING)
    
    ;; Check if file already exists
    (asserts! (is-none (map-get? files { file-id: file-id })) ERR_INVALID_INPUT)
    
    ;; Register the file
    (map-set files
      { file-id: file-id }
      {
        owner: tx-sender,
        privacy-setting: privacy-setting,
        created-at: block-height
      }
    )
    
    ;; Initialize log counter
    (map-set log-counters
      { file-id: file-id }
      { count: u0 }
    )
    
    (ok true)
  )
)

;; Update privacy settings for a file (owner only)
(define-public (update-privacy-setting (file-id (string-ascii 64)) (new-privacy-setting uint))
  (let ((file-info (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND)))
    ;; Validate inputs
    (asserts! (is-valid-file-id file-id) ERR_INVALID_INPUT)
    (asserts! (is-valid-privacy-setting new-privacy-setting) ERR_INVALID_PRIVACY_SETTING)
    
    ;; Check if caller is the owner
    (asserts! (is-eq tx-sender (get owner file-info)) ERR_UNAUTHORIZED)
    
    ;; Update privacy setting
    (map-set files
      { file-id: file-id }
      (merge file-info { privacy-setting: new-privacy-setting })
    )
    
    (ok true)
  )
)

;; Core logging function

;; Log file access
(define-public (log-access (file-id (string-ascii 64)) (access-type (string-ascii 32)))
  (let (
    (file-info (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
    (current-counter (default-to { count: u0 } (map-get? log-counters { file-id: file-id })))
    (log-id (+ (get count current-counter) u1))
  )
    ;; Validate inputs
    (asserts! (is-valid-file-id file-id) ERR_INVALID_INPUT)
    (asserts! (is-valid-access-type access-type) ERR_INVALID_INPUT)
    
    ;; Check if logging is enabled
    (asserts! (not (is-eq (get privacy-setting file-info) PRIVACY_DISABLED)) ERR_LOGGING_DISABLED)
    
    ;; Create access log entry
    (map-set access-logs
      { 
        file-id: file-id,
        log-id: log-id
      }
      {
        user-id: tx-sender,
        timestamp: block-height,
        access-type: access-type
      }
    )
    
    ;; Update log counter
    (map-set log-counters
      { file-id: file-id }
      { count: log-id }
    )
    
    (ok log-id)
  )
)

;; Query functions

;; Get file information
(define-read-only (get-file-info (file-id (string-ascii 64)))
  (map-get? files { file-id: file-id })
)

;; Get specific access log entry
(define-read-only (get-access-log (file-id (string-ascii 64)) (log-id uint))
  (let (
    (file-info (unwrap! (map-get? files { file-id: file-id }) ERR_FILE_NOT_FOUND))
    (log-entry (unwrap! (map-get? access-logs { file-id: file-id, log-id: log-id }) ERR_ACCESS_LOG_NOT_FOUND))
  )
    ;; Check privacy permissions
    (if (is-eq (get privacy-setting file-info) PRIVACY_OWNER_ONLY)
      (if (is-eq tx-sender (get owner file-info))
        (ok (some log-entry))
        ERR_UNAUTHORIZED)
      (ok (some log-entry))
    )
  )
)

;; Get access history for a file (with privacy controls)
(define-read-only (can-view-access-history (file-id (string-ascii 64)) (viewer principal))
  (match (map-get? files { file-id: file-id })
    file-info
      (let ((privacy-setting (get privacy-setting file-info)))
        (if (is-eq privacy-setting PRIVACY_DISABLED)
          false
          (if (is-eq privacy-setting PRIVACY_OWNER_ONLY)
            (is-eq viewer (get owner file-info))
            true ;; PRIVACY_PUBLIC
          )
        )
      )
    false ;; File not found
  )
)

;; Get total number of access logs for a file
(define-read-only (get-access-count (file-id (string-ascii 64)))
  (match (map-get? log-counters { file-id: file-id })
    counter (get count counter)
    u0
  )
)

;; Check if user is file owner
(define-read-only (is-file-owner (file-id (string-ascii 64)) (user principal))
  (match (map-get? files { file-id: file-id })
    file-info (is-eq user (get owner file-info))
    false
  )
)

;; Get privacy setting for a file
(define-read-only (get-privacy-setting (file-id (string-ascii 64)))
  (match (map-get? files { file-id: file-id })
    file-info (get privacy-setting file-info)
    PRIVACY_DISABLED ;; Default if file not found
  )
)

;; Utility functions for batch operations

;; Log multiple accesses (useful for bulk operations)
(define-public (log-multiple-accesses (file-accesses (list 10 { file-id: (string-ascii 64), access-type: (string-ascii 32) })))
  (fold log-single-access file-accesses (ok (list)))
)

;; Helper function for fold operation
(define-private (log-single-access 
  (access-info { file-id: (string-ascii 64), access-type: (string-ascii 32) })
  (prev-result (response (list 10 uint) uint))
)
  (match prev-result
    ok-list 
      (match (log-access (get file-id access-info) (get access-type access-info))
        log-id (ok (unwrap-panic (as-max-len? (append ok-list log-id) u10)))
        error-val (err error-val)
      )
    error-val (err error-val)
  )
)