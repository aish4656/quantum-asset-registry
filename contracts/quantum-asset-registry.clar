;; Mysticak-Token-Sanctuary
;; Decentralized registry for managing quantum assets and their metadata
;; This contract provides comprehensive asset management capabilities

;; ========================================
;; Authorization and Access Control Framework
;; =========================================

;; Primary contract deployer with administrative privileges
(define-constant CONTRACT-DEPLOYER tx-sender)

;; Error handling constants for system integrity
(define-constant ERR-UNAUTHORIZED-ACCESS (err u403))
(define-constant ERR-ASSET-NOT-FOUND (err u404))
(define-constant ERR-ASSET-EXISTS (err u409))
(define-constant ERR-INVALID-NAME-FORMAT (err u400))
(define-constant ERR-DIMENSION-OUT-OF-BOUNDS (err u422))
(define-constant ERR-INVALID-RECIPIENT (err u417))
(define-constant ERR-ADMIN-ONLY-FUNCTION (err u401))
(define-constant ERR-ACCESS-DENIED (err u405))

;; Sequential counter for asset identification system
(define-data-var total-registered-assets uint u0)

;; Core asset registry database structure
(define-map quantum-asset-registry
  { asset-id: uint }
  {
    asset-name: (string-ascii 64),
    creator: principal,
    size-metric: uint,
    creation-block: uint,
    description: (string-ascii 128),
    tag-list: (list 10 (string-ascii 32))
  }
)

;; Access control mechanism for asset visibility
(define-map asset-access-control
  { asset-id: uint, accessor: principal }
  { has-access: bool }
)

;; =========================================
;; Internal Validation and Helper Functions
;; =========================================

;; Determines if an asset exists in the registry
(define-private (asset-exists-check (asset-id uint))
  (is-some (map-get? quantum-asset-registry { asset-id: asset-id }))
)

;; Validates creator ownership of specific asset
(define-private (validate-creator-ownership (asset-id uint) (creator principal))
  (match (map-get? quantum-asset-registry { asset-id: asset-id })
    asset-data (is-eq (get creator asset-data) creator)
    false
  )
)

;; Extracts size metric from asset record
(define-private (get-asset-size-metric (asset-id uint))
  (default-to u0 
    (get size-metric 
      (map-get? quantum-asset-registry { asset-id: asset-id })
    )
  )
)

;; Validates tag collection integrity and constraints
(define-private (validate-tag-collection (tag-list (list 10 (string-ascii 32))))
  (and
    (> (len tag-list) u0)
    (<= (len tag-list) u10)
    (is-eq (len (filter validate-single-tag tag-list)) (len tag-list))
  )
)

;; Ensures individual tag meets system requirements
(define-private (validate-single-tag (tag (string-ascii 32)))
  (and 
    (> (len tag) u0)
    (< (len tag) u33)
  )
)

;; String validation for various text fields
(define-private (validate-string-constraints (text-input (string-ascii 64)) (min-len uint) (max-len uint))
  (and 
    (>= (len text-input) min-len)
    (<= (len text-input) max-len)
  )
)

;; Increments and returns previous asset counter value
(define-private (increment-asset-counter)
  (let ((previous-count (var-get total-registered-assets)))
    (var-set total-registered-assets (+ previous-count u1))
    (ok previous-count)
  )
)

;; =========================================
;; Public Interface Functions
;; =========================================

;; Creates new quantum asset entry in the registry
(define-public (create-quantum-asset (asset-name (string-ascii 64)) (size-metric uint) (description (string-ascii 128)) (tag-list (list 10 (string-ascii 32))))
  (let
    (
      (next-asset-id (+ (var-get total-registered-assets) u1))
    )
    ;; Comprehensive input validation sequence
    (asserts! (and (> (len asset-name) u0) (< (len asset-name) u65)) ERR-INVALID-NAME-FORMAT)
    (asserts! (and (> size-metric u0) (< size-metric u1000000000)) ERR-DIMENSION-OUT-OF-BOUNDS)
    (asserts! (and (> (len description) u0) (< (len description) u129)) ERR-INVALID-NAME-FORMAT)
    (asserts! (validate-tag-collection tag-list) ERR-INVALID-NAME-FORMAT)

    ;; Insert new asset into quantum registry
    (map-insert quantum-asset-registry
      { asset-id: next-asset-id }
      {
        asset-name: asset-name,
        creator: tx-sender,
        size-metric: size-metric,
        creation-block: block-height,
        description: description,
        tag-list: tag-list
      }
    )

    ;; Establish default access permissions for creator
    (map-insert asset-access-control
      { asset-id: next-asset-id, accessor: tx-sender }
      { has-access: true }
    )

    ;; Update global asset counter
    (var-set total-registered-assets next-asset-id)
    (ok next-asset-id)
  )
)

;; Fetches description field from specified asset
(define-public (fetch-asset-description (asset-id uint))
  (let
    (
      (asset-record (unwrap! (map-get? quantum-asset-registry { asset-id: asset-id }) ERR-ASSET-NOT-FOUND))
    )
    (ok (get description asset-record))
  )
)

;; Validates access permissions for specific user and asset
(define-public (check-access-permission (asset-id uint) (accessor principal))
  (let
    (
      (permission-record (map-get? asset-access-control { asset-id: asset-id, accessor: accessor }))
    )
    (ok (is-some permission-record))
  )
)

;; Calculates total number of tags associated with asset
(define-public (calculate-tag-count (asset-id uint))
  (let
    (
      (asset-record (unwrap! (map-get? quantum-asset-registry { asset-id: asset-id }) ERR-ASSET-NOT-FOUND))
    )
    (ok (len (get tag-list asset-record)))
  )
)

;; Verifies asset name compliance with system standards
(define-public (validate-name-format (asset-name (string-ascii 64)))
  (ok (and (> (len asset-name) u0) (<= (len asset-name) u64)))
)

;; Transfers asset ownership to different creator
(define-public (transfer-asset-ownership (asset-id uint) (new-creator principal))
  (let
    (
      (current-asset (unwrap! (map-get? quantum-asset-registry { asset-id: asset-id }) ERR-ASSET-NOT-FOUND))
    )
    (asserts! (asset-exists-check asset-id) ERR-ASSET-NOT-FOUND)
    (asserts! (is-eq (get creator current-asset) tx-sender) ERR-UNAUTHORIZED-ACCESS)

    ;; Execute ownership transfer operation
    (map-set quantum-asset-registry
      { asset-id: asset-id }
      (merge current-asset { creator: new-creator })
    )
    (ok true)
  )
)

;; Updates comprehensive asset metadata information
(define-public (update-asset-metadata (asset-id uint) (new-asset-name (string-ascii 64)) (new-size-metric uint) (new-description (string-ascii 128)) (new-tag-list (list 10 (string-ascii 32))))
  (let
    (
      (existing-asset (unwrap! (map-get? quantum-asset-registry { asset-id: asset-id }) ERR-ASSET-NOT-FOUND))
    )
    ;; Security and validation checks
    (asserts! (asset-exists-check asset-id) ERR-ASSET-NOT-FOUND)
    (asserts! (is-eq (get creator existing-asset) tx-sender) ERR-UNAUTHORIZED-ACCESS)
    (asserts! (and (> (len new-asset-name) u0) (< (len new-asset-name) u65)) ERR-INVALID-NAME-FORMAT)
    (asserts! (and (> new-size-metric u0) (< new-size-metric u1000000000)) ERR-DIMENSION-OUT-OF-BOUNDS)
    (asserts! (and (> (len new-description) u0) (< (len new-description) u129)) ERR-INVALID-NAME-FORMAT)
    (asserts! (validate-tag-collection new-tag-list) ERR-INVALID-NAME-FORMAT)

    ;; Execute comprehensive metadata update
    (map-set quantum-asset-registry
      { asset-id: asset-id }
      (merge existing-asset { 
        asset-name: new-asset-name, 
        size-metric: new-size-metric, 
        description: new-description, 
        tag-list: new-tag-list 
      })
    )
    (ok true)
  )
)

;; Permanently removes asset from quantum registry
(define-public (delete-quantum-asset (asset-id uint))
  (let
    (
      (target-asset (unwrap! (map-get? quantum-asset-registry { asset-id: asset-id }) ERR-ASSET-NOT-FOUND))
    )
    (asserts! (asset-exists-check asset-id) ERR-ASSET-NOT-FOUND)
    (asserts! (is-eq (get creator target-asset) tx-sender) ERR-UNAUTHORIZED-ACCESS)

    ;; Execute permanent asset removal
    (map-delete quantum-asset-registry { asset-id: asset-id })
    (ok true)
  )
)

