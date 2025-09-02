;; VectorSocial - Revolutionary Quadratic Voting Identity Reputation Platform

;; Error Constants
(define-constant ERR-UNAUTHORIZED (err u1001))
(define-constant ERR-INVALID-AMOUNT (err u1002))
(define-constant ERR-INVALID-DURATION (err u1003))
(define-constant ERR-IDENTITY-NOT-FOUND (err u1004))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u1005))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u1006))
(define-constant ERR-VOTING-CLOSED (err u1007))
(define-constant ERR-ALREADY-VOTED (err u1008))
(define-constant ERR-INVALID-PROPOSAL (err u1009))
(define-constant ERR-QUORUM-NOT-MET (err u1010))
(define-constant ERR-INVALID-TIMELOCK (err u1011))
(define-constant ERR-DELEGATION-FAILED (err u1012))
(define-constant ERR-TREASURY-INSUFFICIENT (err u1013))
(define-constant ERR-INVALID-CATEGORY (err u1014))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-COMMITMENT-DURATION u365) ;; 365 days
(define-constant MIN-COMMITMENT-DURATION u7)   ;; 7 days
(define-constant BASE-TRUST-WEIGHT u100)
(define-constant QUADRATIC-MULTIPLIER u150)
(define-constant SOCIAL-THRESHOLD u1000)

;; Data Variables
(define-data-var next-proposal-id uint u1)
(define-data-var treasury-balance uint u0)
(define-data-var base-quorum uint u10) ;; 10%
(define-data-var identity-token principal .identity-token)
(define-data-var protocol-active bool true)
(define-data-var total-committed uint u0)
(define-data-var trust-decay-rate uint u95) ;; 95% retention per period

;; Data Maps
(define-map identity-vectors 
  { user: principal } 
  { 
    amount: uint, 
    duration: uint, 
    start-block: uint, 
    trust-score: uint,
    interaction-count: uint,
    last-activity: uint
  })

(define-map social-proposals 
  { id: uint } 
  { 
    creator: principal, 
    title: (string-ascii 100), 
    description: (string-ascii 500),
    category: uint, ;; 1=treasury, 2=governance, 3=protocol
    votes-for: uint, 
    votes-against: uint, 
    start-block: uint, 
    end-block: uint,
    executed: bool,
    quorum-required: uint,
    treasury-amount: uint
  })

(define-map user-votes 
  { proposal-id: uint, user: principal } 
  { 
    weight: uint, 
    support: bool, 
    timestamp: uint 
  })

(define-map trust-delegation 
  { delegator: principal } 
  { 
    delegate: principal, 
    delegated-weight: uint, 
    active: bool 
  })

(define-map social-history 
  { user: principal, period: uint } 
  { 
    score: uint, 
    participation: uint, 
    consistency: uint 
  })

(define-map identity-configurations 
  { identity-id: principal } 
  { 
    voting-period: uint, 
    execution-delay: uint, 
    custom-quorum: uint, 
    features-enabled: uint 
  })

(define-map treasury-allocations 
  { proposal-id: uint } 
  { 
    recipient: principal, 
    amount: uint, 
    category: uint, 
    executed: bool 
  })

(define-map reputation-vectors 
  { user: principal } 
  { 
    base-score: uint, 
    community-rating: uint, 
    proposal-success-rate: uint, 
    governance-activity: uint 
  })

;; Helper Functions
(define-private (calculate-quadratic-trust-weight (amount uint) (trust uint))
  (let (
    (base-weight (/ (* amount BASE-TRUST-WEIGHT) u1000000)) ;; Normalize amount
    (trust-bonus (/ (* trust QUADRATIC-MULTIPLIER) u10000))
    (total-weight (+ base-weight trust-bonus))
  )
    ;; Apply quadratic formula: sqrt(weight) * multiplier
    (/ (* (sqrti total-weight) u100) u10)))

(define-private (calculate-dynamic-quorum (category uint))
  (let (
    (base (var-get base-quorum))
  )
    (if (is-eq category u1) ;; Treasury proposals need higher quorum
      (+ base u10)
      (if (is-eq category u2) ;; Governance proposals
        (+ base u5)
        base)))) ;; Protocol proposals use base quorum

(define-private (update-trust-score (user principal))
  (let (
    (current-identity (unwrap! (map-get? identity-vectors { user: user }) ERR-IDENTITY-NOT-FOUND))
    (blocks-passed (- block-height (get last-activity current-identity)))
    (decay-factor (if (> blocks-passed u1440) ;; ~10 days
      (var-get trust-decay-rate)
      u100))
    (new-trust (/ (* (get trust-score current-identity) decay-factor) u100))
  )
    (ok new-trust)))

;; Admin Functions
(define-public (set-identity-token (new-token principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set identity-token new-token)
    (ok true)))

(define-public (update-protocol-status (active bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set protocol-active active)
    (ok true)))

(define-public (adjust-base-quorum (new-quorum uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (asserts! (<= new-quorum u50) ERR-INVALID-AMOUNT) ;; Max 50%
    (var-set base-quorum new-quorum)
    (ok true)))

(define-public (fund-treasury (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
    (var-set treasury-balance (+ (var-get treasury-balance) amount))
    (ok true)))

;; Core Identity Functions
(define-public (commit-social-vector (amount uint) (duration uint))
  (let (
    (current-block block-height)
    (existing-vector (default-to 
      { amount: u0, duration: u0, start-block: u0, trust-score: u0, interaction-count: u0, last-activity: u0 }
      (map-get? identity-vectors { user: tx-sender })))
  )
    (asserts! (var-get protocol-active) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (and (>= duration MIN-COMMITMENT-DURATION) (<= duration MAX-COMMITMENT-DURATION)) ERR-INVALID-DURATION)
    
    ;; Calculate trust score based on amount, duration, and history
    (let (
      (base-trust (/ (* amount duration) u100))
      (time-multiplier (if (> duration u30) u120 u100))
      (new-trust (/ (* base-trust time-multiplier) u100))
      (updated-vector {
        amount: (+ (get amount existing-vector) amount),
        duration: duration,
        start-block: current-block,
        trust-score: (+ (get trust-score existing-vector) new-trust),
        interaction-count: (get interaction-count existing-vector),
        last-activity: current-block
      })
    )
      (map-set identity-vectors { user: tx-sender } updated-vector)
      (var-set total-committed (+ (var-get total-committed) amount))
      (ok new-trust))))

(define-public (withdraw-social-vector (amount uint))
  (let (
    (user-vector (unwrap! (map-get? identity-vectors { user: tx-sender }) ERR-IDENTITY-NOT-FOUND))
    (commitment-end (+ (get start-block user-vector) (get duration user-vector)))
  )
    (asserts! (>= block-height commitment-end) ERR-INVALID-DURATION)
    (asserts! (>= (get amount user-vector) amount) ERR-INSUFFICIENT-REPUTATION)
    
    (let (
      (updated-vector (merge user-vector { 
        amount: (- (get amount user-vector) amount),
        trust-score: (/ (* (get trust-score user-vector) (- (get amount user-vector) amount)) (get amount user-vector))
      }))
    )
      (map-set identity-vectors { user: tx-sender } updated-vector)
      (var-set total-committed (- (var-get total-committed) amount))
      (ok amount))))

(define-public (create-social-proposal (title (string-ascii 100)) (description (string-ascii 500)) (category uint) (treasury-amount uint))
  (let (
    (proposal-id (var-get next-proposal-id))
    (user-vector (unwrap! (map-get? identity-vectors { user: tx-sender }) ERR-IDENTITY-NOT-FOUND))
    (voting-period u1440) ;; ~10 days in blocks
    (dynamic-quorum (calculate-dynamic-quorum category))
  )
    (asserts! (var-get protocol-active) ERR-UNAUTHORIZED)
    (asserts! (> (get trust-score user-vector) SOCIAL-THRESHOLD) ERR-INSUFFICIENT-REPUTATION)
    (asserts! (and (>= category u1) (<= category u3)) ERR-INVALID-CATEGORY)
    (asserts! (or (is-eq treasury-amount u0) (<= treasury-amount (var-get treasury-balance))) ERR-TREASURY-INSUFFICIENT)
    
    (map-set social-proposals 
      { id: proposal-id }
      {
        creator: tx-sender,
        title: title,
        description: description,
        category: category,
        votes-for: u0,
        votes-against: u0,
        start-block: block-height,
        end-block: (+ block-height voting-period),
        executed: false,
        quorum-required: dynamic-quorum,
        treasury-amount: treasury-amount
      })
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)))

(define-public (vote-on-social-proposal (proposal-id uint) (support bool))
  (let (
    (proposal (unwrap! (map-get? social-proposals { id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (user-vector (unwrap! (map-get? identity-vectors { user: tx-sender }) ERR-IDENTITY-NOT-FOUND))
    (existing-vote (map-get? user-votes { proposal-id: proposal-id, user: tx-sender }))
    (trust-weight (calculate-quadratic-trust-weight (get amount user-vector) (get trust-score user-vector)))
  )
    (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
    (asserts! (<= block-height (get end-block proposal)) ERR-VOTING-CLOSED)
    (asserts! (not (get executed proposal)) ERR-VOTING-CLOSED)
    
    ;; Record vote
    (map-set user-votes 
      { proposal-id: proposal-id, user: tx-sender }
      { weight: trust-weight, support: support, timestamp: block-height })
    
    ;; Update proposal vote counts
    (let (
      (updated-proposal (merge proposal {
        votes-for: (if support (+ (get votes-for proposal) trust-weight) (get votes-for proposal)),
        votes-against: (if support (get votes-against proposal) (+ (get votes-against proposal) trust-weight))
      }))
    )
      (map-set social-proposals { id: proposal-id } updated-proposal)
      
      ;; Update user participation
      (map-set identity-vectors 
        { user: tx-sender } 
        (merge user-vector { 
          interaction-count: (+ (get interaction-count user-vector) u1),
          last-activity: block-height 
        }))
      
      (ok trust-weight))))

(define-public (execute-social-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? social-proposals { id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
    (total-supply (var-get total-committed))
    (quorum-met (>= (* total-votes u100) (* total-supply (get quorum-required proposal))))
  )
    (asserts! (> block-height (get end-block proposal)) ERR-VOTING-CLOSED)
    (asserts! (not (get executed proposal)) ERR-ALREADY-VOTED)
    (asserts! quorum-met ERR-QUORUM-NOT-MET)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR-INVALID-PROPOSAL)
    
    ;; Mark as executed
    (map-set social-proposals { id: proposal-id } (merge proposal { executed: true }))
    
    ;; Handle treasury allocation if needed
    (if (> (get treasury-amount proposal) u0)
      (begin
        (map-set treasury-allocations 
          { proposal-id: proposal-id }
          { 
            recipient: (get creator proposal), 
            amount: (get treasury-amount proposal), 
            category: (get category proposal), 
            executed: true 
          })
        (var-set treasury-balance (- (var-get treasury-balance) (get treasury-amount proposal))))
      true)
    
    (ok true)))

(define-public (delegate-trust-power (delegate principal))
  (let (
    (user-vector (unwrap! (map-get? identity-vectors { user: tx-sender }) ERR-IDENTITY-NOT-FOUND))
    (delegated-weight (get trust-score user-vector))
  )
    (asserts! (not (is-eq tx-sender delegate)) ERR-DELEGATION-FAILED)
    (asserts! (> delegated-weight u0) ERR-INSUFFICIENT-REPUTATION)
    
    (map-set trust-delegation 
      { delegator: tx-sender }
      { 
        delegate: delegate, 
        delegated-weight: delegated-weight, 
        active: true 
      })
    
    (ok delegated-weight)))

(define-public (revoke-trust-delegation)
  (begin
    (asserts! (is-some (map-get? trust-delegation { delegator: tx-sender })) ERR-DELEGATION-FAILED)
    
    (map-delete trust-delegation { delegator: tx-sender })
    (ok true)))

;; Read-only Functions
(define-read-only (get-identity-vector (user principal))
  (map-get? identity-vectors { user: user }))