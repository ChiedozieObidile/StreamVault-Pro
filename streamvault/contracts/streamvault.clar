;; StreamVault Pro: Tokenized Subscription Service
;; Description: Smart contract for managing subscription-based access to digital content with auto-renewal and trial periods

;; Constants
(define-constant platform-admin tx-sender)
(define-constant err-admin-required (err u100))
(define-constant err-setup-complete (err u101))
(define-constant err-setup-required (err u102))
(define-constant err-membership-lapsed (err u103))
(define-constant err-plan-invalid (err u104))
(define-constant err-access-denied (err u105))
(define-constant err-renewal-failed (err u106))
(define-constant err-trial-consumed (err u107))
(define-constant err-user-invalid (err u108))
(define-constant err-height-invalid (err u109))

;; Data Variables
(define-data-var platform-ready bool false)

;; Data Maps
(define-map user-memberships
    principal
    {plan: (string-ascii 6),
     expires-at: uint,
     is-active: bool,
     auto-renewal: bool,
     trial-consumed: bool,
     trial-expires: uint})

(define-map membership-plans
    (string-ascii 6)
    {cost: uint,
     period: uint,
     trial-period: uint,
     features: (string-ascii 50)})

;; Private Functions
(define-private (is-platform-admin)
    (is-eq tx-sender platform-admin))

(define-private (is-plan-valid (plan (string-ascii 6)))
    (is-some (map-get? membership-plans plan)))

(define-private (is-user-valid (user principal))
    (and 
        (not (is-eq user platform-admin))
        (not (is-eq user tx-sender))))

(define-private (is-membership-current (user principal))
    (begin 
        (asserts! (is-user-valid user) false)
        (let ((membership (unwrap! (map-get? user-memberships user) false)))
            (and 
                (get is-active membership)
                (> (get expires-at membership) block-height)))))

(define-private (process-auto-renewal (user principal))
    (begin 
        (asserts! (is-user-valid user) (err err-user-invalid))
        (let ((existing-membership (unwrap! (map-get? user-memberships user) (err err-renewal-failed)))
              (plan-details (unwrap! (map-get? membership-plans (get plan existing-membership)) (err err-plan-invalid))))
            
            (if (get auto-renewal existing-membership)
                (begin
                    (asserts! (> (get period plan-details) u0) (err err-plan-invalid))
                    (map-set user-memberships user
                        {plan: (get plan existing-membership),
                         expires-at: (+ block-height (get period plan-details)),
                         is-active: true,
                         auto-renewal: true,
                         trial-consumed: (get trial-consumed existing-membership),
                         trial-expires: (get trial-expires existing-membership)})
                    (ok true))
                (err err-renewal-failed)))))

;; Public Functions
(define-public (setup-platform)
    (begin
        (asserts! (is-platform-admin) err-admin-required)
        (asserts! (not (var-get platform-ready)) err-setup-complete)
        
        ;; Initialize membership plans with trial periods
        (map-set membership-plans "bronze"
            {cost: u100,
             period: u4320, ;; 30 days in blocks (assuming 10min block time)
             trial-period: u288, ;; 2 days trial
             features: "Basic access to content"})
        
        (map-set membership-plans "silver"
            {cost: u250,
             period: u4320,
             trial-period: u288,
             features: "Premium access + exclusive content"})
        
        (map-set membership-plans "gold"
            {cost: u500,
             period: u4320,
             trial-period: u288,
             features: "All access + early releases"})
        
        (var-set platform-ready true)
        (ok true)))

(define-public (create-membership (plan (string-ascii 6)) (enable-renewal (optional bool)))
    (begin 
        ;; Validate plan
        (asserts! (is-plan-valid plan) err-plan-invalid)
        
        (let ((plan-details (unwrap! (map-get? membership-plans plan) err-plan-invalid))
              (membership-cost (get cost plan-details))
              (membership-period (get period plan-details))
              (trial-length (get trial-period plan-details))
              (existing-membership (map-get? user-memberships tx-sender)))
            
            ;; Check if user has not used trial before
            (asserts! 
                (or 
                    (is-none existing-membership) 
                    (not (get trial-consumed (unwrap-panic existing-membership))))
                err-trial-consumed)
            
            ;; Validate block height calculations
            (asserts! (> membership-period u0) err-plan-invalid)
            (asserts! (> trial-length u0) err-plan-invalid)
            
            ;; Create or update membership
            (map-set user-memberships tx-sender
                {plan: plan,
                 expires-at: (+ block-height 
                                (if (is-none existing-membership) 
                                    trial-length 
                                    membership-period)),
                 is-active: true,
                 auto-renewal: (default-to false enable-renewal),
                 trial-consumed: (is-none existing-membership),
                 trial-expires: (+ block-height trial-length)})
            
            (ok true))))

(define-public (extend-membership)
    (let ((existing-membership (unwrap! (map-get? user-memberships tx-sender) err-access-denied))
          (plan-details (unwrap! (map-get? membership-plans (get plan existing-membership)) err-plan-invalid)))
        
        (map-set user-memberships tx-sender
            {plan: (get plan existing-membership),
             expires-at: (+ block-height (get period plan-details)),
             is-active: true,
             auto-renewal: (get auto-renewal existing-membership),
             trial-consumed: (get trial-consumed existing-membership),
             trial-expires: (get trial-expires existing-membership)})
        
        (ok true)))

(define-public (terminate-membership)
    (let ((existing-membership (unwrap! (map-get? user-memberships tx-sender) err-access-denied)))
        (map-set user-memberships tx-sender
            {plan: (get plan existing-membership),
             expires-at: block-height,
             is-active: false,
             auto-renewal: false,
             trial-consumed: (get trial-consumed existing-membership),
             trial-expires: (get trial-expires existing-membership)})
        
        (ok true)))

(define-public (update-auto-renewal (enable bool))
    (let ((existing-membership (unwrap! (map-get? user-memberships tx-sender) err-access-denied)))
        (map-set user-memberships tx-sender
            {plan: (get plan existing-membership),
             expires-at: (get expires-at existing-membership),
             is-active: (get is-active existing-membership),
             auto-renewal: enable,
             trial-consumed: (get trial-consumed existing-membership),
             trial-expires: (get trial-expires existing-membership)})
        
        (ok true)))

(define-public (batch-process-renewals (user-list (list 100 principal)))
    (begin
        (asserts! (is-platform-admin) err-admin-required)
        (let ((processing-results (map process-auto-renewal user-list)))
            (ok true))))

(define-public (verify-membership (user principal))
    (begin
        (asserts! (is-user-valid user) (err err-user-invalid))
        (ok (is-membership-current user))))

(define-read-only (fetch-membership-details (user principal))
    (map-get? user-memberships user))

(define-read-only (fetch-plan-details (plan (string-ascii 6)))
    (map-get? membership-plans plan))

;; Contract initialization check
(asserts! (is-platform-admin) err-access-denied)