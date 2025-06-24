;; Interplanetary Value Transfer Registry
;;
;; A decentralized marketplace for virtual space commodities across star systems
;; featuring balanced distribution mechanisms and economic incentives to promote
;; fair trade throughout the galaxy.

;; ========== STORAGE STRUCTURES ==========
;; Maps associating stellar entities with their cosmic holdings
(define-map entity-resource-inventory principal uint)
(define-map entity-currency-reserves principal uint)
(define-map resource-marketplace-listings {entity: principal} {quantity: uint, valuation: uint})

;; ========== CONFIGURATION CONSTANTS ==========
(define-constant governance-authority tx-sender)
(define-constant error-unauthorized-operation (err u200))
(define-constant error-insufficient-resources (err u201))
(define-constant error-process-failure (err u202))
(define-constant error-invalid-valuation (err u203))
(define-constant error-invalid-quantity (err u204))
(define-constant error-compensation-insufficient (err u205))
(define-constant error-self-transaction (err u207))
(define-constant error-capacity-exceeded (err u208))
(define-constant error-invalid-parameters (err u209))

;; ========== SYSTEM PARAMETERS ==========
;; Operational parameters governing exchange mechanics
(define-data-var exchange-rate-percentage uint u90)
(define-data-var standard-resource-value uint u100)
(define-data-var entity-resource-maximum uint u10000)
(define-data-var universal-resource-tally uint u0)
(define-data-var transaction-fee-rate uint u5)
(define-data-var cosmos-capacity-limit uint u1000000)

;; ========== UTILITY FUNCTIONS ==========

;; Calculate operational fees for marketplace transactions
(define-private (compute-transaction-fee (value uint))
  (/ (* value (var-get transaction-fee-rate)) u100))

;; Calculate resource-to-currency conversion value
(define-private (determine-resource-value (quantity uint))
  (/ (* quantity (var-get standard-resource-value) (var-get exchange-rate-percentage)) u100))

;; Update the universal resource counter
(define-private (adjust-universal-resource-count (adjustment int))
  (let (
    (current-tally (var-get universal-resource-tally))
    (updated-tally (if (< adjustment 0)
                     (if (>= current-tally (to-uint (- 0 adjustment)))
                         (- current-tally (to-uint (- 0 adjustment)))
                         u0)
                     (+ current-tally (to-uint adjustment))))
  )
    (asserts! (<= updated-tally (var-get cosmos-capacity-limit)) error-capacity-exceeded)
    (var-set universal-resource-tally updated-tally)
    (ok true)))

;; ========== MARKETPLACE OPERATIONS ==========

;; Create a resource listing in the galactic marketplace
(define-public (list-resources-for-exchange (quantity uint) (valuation uint))
  (let (
    (available-resources (default-to u0 (map-get? entity-resource-inventory tx-sender)))
    (existing-listing (get quantity (default-to {quantity: u0, valuation: u0} 
                          (map-get? resource-marketplace-listings {entity: tx-sender}))))
    (total-listing-amount (+ quantity existing-listing))
  )
    ;; Input validation checks
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (> valuation u0) error-invalid-valuation)
    (asserts! (>= available-resources total-listing-amount) error-insufficient-resources)
    
    ;; Update universal tracking system
    (try! (adjust-universal-resource-count (to-int quantity)))
    
    ;; Register the marketplace listing
    (map-set resource-marketplace-listings {entity: tx-sender} 
             {quantity: total-listing-amount, valuation: valuation})
    
    (ok true)))

;; Withdraw resources from active marketplace listing
(define-public (withdraw-listed-resources (quantity uint))
  (let (
    (listing-data (default-to {quantity: u0, valuation: u0} 
                 (map-get? resource-marketplace-listings {entity: tx-sender})))
    (listed-quantity (get quantity listing-data))
    (listed-valuation (get valuation listing-data))
  )
    ;; Validation of request
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (>= listed-quantity quantity) error-insufficient-resources)

    ;; Update the marketplace registry
    (map-set resource-marketplace-listings 
             {entity: tx-sender} 
             {quantity: (- listed-quantity quantity), valuation: listed-valuation})

    (ok true)))

;; Cancel entire marketplace listing
(define-public (terminate-marketplace-listing)
  (let (
    (listing-data (default-to {quantity: u0, valuation: u0} 
                 (map-get? resource-marketplace-listings {entity: tx-sender})))
    (listed-amount (get quantity listing-data))
    (universal-total (var-get universal-resource-tally))
  )
    ;; Verify entity has an active listing
    (asserts! (> listed-amount u0) error-insufficient-resources)

    ;; Update universal resource tracking
    (var-set universal-resource-tally (- universal-total listed-amount))

    ;; Remove the listing completely
    (map-set resource-marketplace-listings {entity: tx-sender} {quantity: u0, valuation: u0})

    ;; Record the transaction
    (print {event: "listing-terminated", entity: tx-sender, amount: listed-amount})

    (ok true)))

;; Remove partial quantity from marketplace listing
(define-public (reduce-listing-quantity (quantity uint))
  (let (
    (current-listing (default-to {quantity: u0, valuation: u0} 
                    (map-get? resource-marketplace-listings {entity: tx-sender})))
    (listed-amount (get quantity current-listing))
    (listed-valuation (get valuation current-listing))
  )
    ;; Request validation
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (>= listed-amount quantity) error-insufficient-resources)

    ;; Update or delete the listing
    (if (is-eq listed-amount quantity)
        (map-delete resource-marketplace-listings {entity: tx-sender})
        (map-set resource-marketplace-listings {entity: tx-sender} 
                {quantity: (- listed-amount quantity), valuation: listed-valuation}))

    (ok true)))

;; ========== RESOURCE MANAGEMENT ==========

;; Register new resources into the protocol
(define-public (introduce-new-resources (quantity uint))
  (let (
    (current-inventory (default-to u0 (map-get? entity-resource-inventory tx-sender)))
    (updated-inventory (+ current-inventory quantity))
    (current-universal-count (var-get universal-resource-tally))
    (new-universal-count (+ current-universal-count quantity))
  )
    ;; Input validation
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (<= updated-inventory (var-get entity-resource-maximum)) error-capacity-exceeded)
    (asserts! (<= new-universal-count (var-get cosmos-capacity-limit)) error-capacity-exceeded)
    
    ;; Update entity's resource inventory
    (map-set entity-resource-inventory tx-sender updated-inventory)
    
    ;; Update universal resource count
    (var-set universal-resource-tally new-universal-count)
    
    ;; Return confirmation
    (ok true)))

;; Exchange resources for currency
(define-public (transform-resources-to-currency (quantity uint))
  (let (
    (entity-inventory (default-to u0 (map-get? entity-resource-inventory tx-sender)))
    (currency-amount (determine-resource-value quantity))
    (authority-currency-balance (default-to u0 (map-get? entity-currency-reserves governance-authority)))
  )
    ;; Input validations
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (>= entity-inventory quantity) error-insufficient-resources)
    (asserts! (>= authority-currency-balance currency-amount) error-compensation-insufficient)

    ;; Update entity's resource inventory
    (map-set entity-resource-inventory tx-sender (- entity-inventory quantity))

    ;; Process currency transfer
    (map-set entity-currency-reserves tx-sender 
             (+ (default-to u0 (map-get? entity-currency-reserves tx-sender)) currency-amount))
    (map-set entity-currency-reserves governance-authority (- authority-currency-balance currency-amount))

    (ok true)))

;; Protected resource exchange with validation
(define-public (secure-resource-transformation (quantity uint))
  (let (
        (entity-inventory (default-to u0 (map-get? entity-resource-inventory tx-sender)))
        (currency-amount (determine-resource-value quantity))
  )
    ;; Enhanced validation
    (asserts! (>= entity-inventory quantity) error-insufficient-resources)
    (asserts! (> currency-amount u0) error-compensation-insufficient)

    ;; Process the transformation
    (map-set entity-resource-inventory tx-sender (- entity-inventory quantity))
    (map-set entity-currency-reserves tx-sender 
             (+ (default-to u0 (map-get? entity-currency-reserves tx-sender)) currency-amount))
    (map-set entity-currency-reserves governance-authority 
             (- (default-to u0 (map-get? entity-currency-reserves governance-authority)) currency-amount))

    (ok true)))

;; Transfer resources directly to another entity
(define-public (dispatch-resources-to-entity (recipient principal) (quantity uint))
  (let (
    (sender-inventory (default-to u0 (map-get? entity-resource-inventory tx-sender)))
    (recipient-inventory (default-to u0 (map-get? entity-resource-inventory recipient)))
    (transfer-fee (compute-transaction-fee (var-get standard-resource-value)))
    (sender-currency-balance (default-to u0 (map-get? entity-currency-reserves tx-sender)))
  )
    ;; Validation checks
    (asserts! (not (is-eq tx-sender recipient)) error-self-transaction)
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (>= sender-inventory quantity) error-insufficient-resources)
    (asserts! (>= sender-currency-balance transfer-fee) error-insufficient-resources)
    (asserts! (<= (+ recipient-inventory quantity) (var-get entity-resource-maximum)) 
              error-capacity-exceeded)

    ;; Update resource inventories
    (map-set entity-resource-inventory tx-sender (- sender-inventory quantity))
    (map-set entity-resource-inventory recipient (+ recipient-inventory quantity))

    ;; Process fee payment
    (map-set entity-currency-reserves tx-sender (- sender-currency-balance transfer-fee))
    (map-set entity-currency-reserves governance-authority 
             (+ (default-to u0 (map-get? entity-currency-reserves governance-authority)) transfer-fee))

    (ok true)
  )
)

;; ========== EXCHANGE TRANSACTIONS ==========

;; Purchase resources from another entity
(define-public (acquire-entity-resources (provider principal) (quantity uint))
  (let (
    (listing-data (default-to {quantity: u0, valuation: u0} 
                 (map-get? resource-marketplace-listings {entity: provider})))
    (resource-cost (* quantity (get valuation listing-data)))
    (fee-amount (compute-transaction-fee resource-cost))
    (total-cost (+ resource-cost fee-amount))
    (provider-inventory (default-to u0 (map-get? entity-resource-inventory provider)))
    (buyer-currency (default-to u0 (map-get? entity-currency-reserves tx-sender)))
    (provider-currency (default-to u0 (map-get? entity-currency-reserves provider)))
    (authority-currency (default-to u0 (map-get? entity-currency-reserves governance-authority)))
  )
    ;; Transaction validations
    (asserts! (not (is-eq tx-sender provider)) error-self-transaction)
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (>= (get quantity listing-data) quantity) error-insufficient-resources)
    (asserts! (>= provider-inventory quantity) error-insufficient-resources)
    (asserts! (>= buyer-currency total-cost) error-insufficient-resources)

    ;; Update provider's inventory and listing
    (map-set entity-resource-inventory provider (- provider-inventory quantity))
    (map-set resource-marketplace-listings {entity: provider} 
             {quantity: (- (get quantity listing-data) quantity), 
              valuation: (get valuation listing-data)})

    ;; Update currency balances
    (map-set entity-currency-reserves tx-sender (- buyer-currency total-cost))
    (map-set entity-currency-reserves provider (+ provider-currency resource-cost))
    (map-set entity-currency-reserves governance-authority (+ authority-currency fee-amount))

    ;; Update buyer's inventory
    (map-set entity-resource-inventory tx-sender 
             (+ (default-to u0 (map-get? entity-resource-inventory tx-sender)) quantity))

    (ok true)))

;; Optimized resource acquisition for performance
(define-public (rapid-resource-acquisition (provider principal) (quantity uint))
  (let (
        (listing-data (default-to {quantity: u0, valuation: u0} 
                     (map-get? resource-marketplace-listings {entity: provider})))
        (resource-cost (* quantity (get valuation listing-data)))
        (buyer-currency (default-to u0 (map-get? entity-currency-reserves tx-sender)))
        (provider-inventory (default-to u0 (map-get? entity-resource-inventory provider)))
  )
    ;; Direct validation checks
    (asserts! (>= buyer-currency resource-cost) error-insufficient-resources)
    (asserts! (>= provider-inventory quantity) error-insufficient-resources)

    ;; Simplified balance updates
    (map-set entity-currency-reserves tx-sender (- buyer-currency resource-cost))
    (map-set entity-resource-inventory tx-sender 
             (+ (default-to u0 (map-get? entity-resource-inventory tx-sender)) quantity))
    (map-set entity-resource-inventory provider (- provider-inventory quantity))
    (map-set entity-currency-reserves provider 
             (+ (default-to u0 (map-get? entity-currency-reserves provider)) resource-cost))

    (ok true)))

;; ========== CURRENCY OPERATIONS ==========

;; Withdraw currency from the protocol
(define-public (extract-currency-reserves (quantity uint))
  (let (
    (current-balance (default-to u0 (map-get? entity-currency-reserves tx-sender)))
    (new-balance (if (>= current-balance quantity)
                    (- current-balance quantity)
                    u0))
  )
    ;; Input validations
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (>= current-balance quantity) error-insufficient-resources)

    ;; Update entity's currency balance
    (map-set entity-currency-reserves tx-sender new-balance)

    ;; Process currency transfer through contract
    (try! (as-contract (stx-transfer? quantity (as-contract tx-sender) tx-sender)))

    (ok new-balance)))

;; ========== GOVERNANCE FUNCTIONS ==========

;; Allocate resources to an entity (governance-only function)
(define-public (allocate-resources-to-entity (entity principal) (quantity uint))
  (let (
    (current-inventory (default-to u0 (map-get? entity-resource-inventory entity)))
    (new-inventory (+ current-inventory quantity))
    (universal-total (var-get universal-resource-tally))
    (updated-total (+ universal-total quantity))
  )
    ;; Authority-only validation
    (asserts! (is-eq tx-sender governance-authority) error-unauthorized-operation)
    (asserts! (> quantity u0) error-invalid-quantity)
    (asserts! (<= new-inventory (var-get entity-resource-maximum)) error-capacity-exceeded)
    (asserts! (<= updated-total (var-get cosmos-capacity-limit)) error-capacity-exceeded)

    ;; Update universal total
    (var-set universal-resource-tally updated-total)

    ;; Log the allocation for auditing
    (print {event: "resource-allocation", entity: entity, quantity: quantity, new-inventory: new-inventory})

    (ok new-inventory)))

