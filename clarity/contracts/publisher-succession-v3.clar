;; publisher-succession-v3.clar
;;
;; Immutable sBTC-weighted publisher succession for AIBTC News.
;;
;; The publisher has full operational authority over the news network.
;; The ONLY on-chain governance actions are:
;;   1. Replacing the publisher (by vote or abdication)
;;   2. Staking sBTC for voting weight and rewards
;;
;; Design principles:
;;   - Monarchy with a kill switch (Yarvin: decisive leadership + accountability)
;;   - Capital-weighted governance (Hayes: rules enforced by math, not trust)
;;   - Skin in the game everywhere (no governance without ownership)
;;
;; Two paths to succession:
;;
;;   1. Abdication -- publisher announces a successor.
;;      - Successor must be a staker (owners govern, not outsiders).
;;      - 7-day delay before transfer completes.
;;      - Stakers can veto during the delay with >50% of staked weight.
;;      - Publisher can cancel their own abdication during the delay.
;;
;;   2. Supermajority vote -- stakers force a replacement.
;;      - 50% of staked sBTC must participate (quorum).
;;      - 95% of votes cast must be yes (threshold).
;;      - 7-day voting window. 7-day cooldown between proposals.
;;      - You cannot propose yourself or the current publisher.
;;      - Proposer can cancel their own proposal (releases stake locks).
;;
;; Staking:
;;   - Anyone can stake sBTC. 1 sat = 1 vote.
;;   - Stake/unstake locked during active proposals (not during abdication delays).
;;   - Publisher must maintain minimum stake to hold office.
;;   - Stakers earn pro-rata rewards from sBTC deposited into the reward pool.
;;
;; Rewards:
;;   - Anyone can deposit sBTC into the reward pool.
;;   - Rewards accrue to stakers proportional to their stake.
;;   - Claimable anytime, including during votes and abdication delays.
;;   - Reward pool tracked separately from stake pool (no accounting overlap).
;;
;; IMMUTABLE: No admin functions. No upgrade path. No owner key.
;; Deploy once, runs forever.

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(define-constant SBTC 'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token)
(define-constant SELF (as-contract tx-sender))

;; Governance parameters
(define-constant THRESHOLD u95)              ;; 95% of votes cast must be yes
(define-constant QUORUM u50)                 ;; 50% of total staked must participate
(define-constant VOTE_WINDOW u1008)          ;; ~7 days (10-min Stacks blocks)
(define-constant COOLDOWN u1008)             ;; ~7 days between proposals
(define-constant ABDICATION_DELAY u1008)     ;; ~7 days before abdication completes
(define-constant ABDICATION_VETO u50)        ;; 50% of staked can veto abdication
(define-constant MIN_TOTAL_STAKED u1000000)  ;; 0.01 BTC minimum to propose
(define-constant MIN_PUBLISHER_STAKE u100000) ;; 0.001 BTC publisher must maintain

;; Reward math precision (1e12)
(define-constant PRECISION u1000000000000)

;; ---------------------------------------------------------------------------
;; Errors
;; ---------------------------------------------------------------------------

(define-constant ERR_ZERO_AMOUNT (err u3000))
(define-constant ERR_INSUFFICIENT_STAKE (err u3001))
(define-constant ERR_STAKES_LOCKED (err u3002))
(define-constant ERR_PROPOSAL_ACTIVE (err u3003))
(define-constant ERR_NO_PROPOSAL (err u3004))
(define-constant ERR_VOTING_CLOSED (err u3005))
(define-constant ERR_VOTING_OPEN (err u3006))
(define-constant ERR_ALREADY_VOTED (err u3007))
(define-constant ERR_COOLDOWN_ACTIVE (err u3008))
(define-constant ERR_SELF_PROPOSAL (err u3009))
(define-constant ERR_THRESHOLD_NOT_MET (err u3010))
(define-constant ERR_QUORUM_NOT_MET (err u3011))
(define-constant ERR_USE_FINALIZE (err u3012))
(define-constant ERR_NOT_PUBLISHER (err u3013))
(define-constant ERR_NOT_STAKER (err u3014))
(define-constant ERR_INSUFFICIENT_PROPOSER_STAKE (err u3015))
(define-constant ERR_NO_REWARDS (err u3016))
(define-constant ERR_NO_STAKERS (err u3017))
(define-constant ERR_CANDIDATE_NOT_STAKER (err u3018))
(define-constant ERR_CANDIDATE_IS_PUBLISHER (err u3019))
(define-constant ERR_ABDICATION_ACTIVE (err u3020))
(define-constant ERR_NO_ABDICATION (err u3021))
(define-constant ERR_ABDICATION_PENDING (err u3022))
(define-constant ERR_ABDICATION_VETO_MET (err u3023))
(define-constant ERR_PUBLISHER_STAKE_TOO_LOW (err u3024))
(define-constant ERR_NOT_PROPOSER (err u3025))

;; ---------------------------------------------------------------------------
;; State -- Governance
;; ---------------------------------------------------------------------------

(define-data-var publisher principal tx-sender)
(define-data-var total-staked uint u0)

;; Proposal state (supermajority vote path)
(define-data-var proposal-count uint u0)
(define-data-var proposal-candidate (optional principal) none)
(define-data-var proposal-proposer (optional principal) none)
(define-data-var proposal-start uint u0)
(define-data-var proposal-yes uint u0)
(define-data-var proposal-no uint u0)
(define-data-var last-proposal-end uint u0)

;; Abdication state (voluntary handoff path)
(define-data-var abdication-successor (optional principal) none)
(define-data-var abdication-start uint u0)
(define-data-var abdication-veto-weight uint u0)

;; Per-staker state
(define-map stakes principal uint)
(define-map voted { id: uint, voter: principal } bool)
(define-map abdication-vetoed principal bool)

;; ---------------------------------------------------------------------------
;; State -- Rewards (MasterChef accumulator)
;; ---------------------------------------------------------------------------

;; Cumulative reward per staked sat, scaled by PRECISION. Monotonically increasing.
(define-data-var reward-per-token-stored uint u0)

;; Separate tracking of reward pool balance (not commingled with stakes)
(define-data-var reward-pool-balance uint u0)

;; Per-staker reward accounting
(define-map staker-reward-debt principal uint)
(define-map staker-pending-rewards principal uint)

;; ---------------------------------------------------------------------------
;; Read-only
;; ---------------------------------------------------------------------------

(define-read-only (get-publisher)
  (var-get publisher)
)

(define-read-only (get-stake (who principal))
  (default-to u0 (map-get? stakes who))
)

(define-read-only (get-total-staked)
  (var-get total-staked)
)

(define-read-only (get-reward-pool)
  (var-get reward-pool-balance)
)

(define-read-only (get-proposal)
  {
    id: (var-get proposal-count),
    candidate: (var-get proposal-candidate),
    proposer: (var-get proposal-proposer),
    start-block: (var-get proposal-start),
    end-block: (+ (var-get proposal-start) VOTE_WINDOW),
    yes-votes: (var-get proposal-yes),
    no-votes: (var-get proposal-no),
    total-staked: (var-get total-staked),
    active: (is-some (var-get proposal-candidate))
  }
)

(define-read-only (get-abdication)
  {
    successor: (var-get abdication-successor),
    start-block: (var-get abdication-start),
    end-block: (+ (var-get abdication-start) ABDICATION_DELAY),
    veto-weight: (var-get abdication-veto-weight),
    total-staked: (var-get total-staked),
    active: (is-some (var-get abdication-successor))
  }
)

(define-read-only (has-voted (voter principal))
  (default-to false (map-get? voted { id: (var-get proposal-count), voter: voter }))
)

(define-read-only (has-vetoed-abdication (who principal))
  (default-to false (map-get? abdication-vetoed who))
)

(define-read-only (get-pending-rewards (who principal))
  (let
    (
      (staker-stake (get-stake who))
      (rpt (var-get reward-per-token-stored))
      (debt (default-to u0 (map-get? staker-reward-debt who)))
      (settled (default-to u0 (map-get? staker-pending-rewards who)))
      (pending (/ (* staker-stake (- rpt debt)) PRECISION))
    )
    (+ settled pending)
  )
)

;; ---------------------------------------------------------------------------
;; Private
;; ---------------------------------------------------------------------------

(define-private (reset-proposal)
  (begin
    (var-set proposal-candidate none)
    (var-set proposal-proposer none)
    (var-set proposal-start u0)
    (var-set proposal-yes u0)
    (var-set proposal-no u0)
    (var-set last-proposal-end stacks-block-height)
    true
  )
)

(define-private (reset-abdication)
  (begin
    (var-set abdication-successor none)
    (var-set abdication-start u0)
    (var-set abdication-veto-weight u0)
    true
  )
)

;; Settle a staker's pending rewards before changing their stake.
(define-private (settle-rewards (account principal))
  (let
    (
      (staker-stake (get-stake account))
      (rpt (var-get reward-per-token-stored))
      (debt (default-to u0 (map-get? staker-reward-debt account)))
      (settled (default-to u0 (map-get? staker-pending-rewards account)))
      (pending (/ (* staker-stake (- rpt debt)) PRECISION))
    )
    (map-set staker-pending-rewards account (+ settled pending))
    (map-set staker-reward-debt account rpt)
    true
  )
)

;; ---------------------------------------------------------------------------
;; Public -- Staking
;; ---------------------------------------------------------------------------

;; Stake sBTC. Increases voting weight.
;; Blocked during active proposals (not during abdication delays).
(define-public (stake (amount uint))
  (let
    (
      (caller tx-sender)
      (current-stake (get-stake caller))
    )
    (asserts! (> amount u0) ERR_ZERO_AMOUNT)
    (asserts! (is-none (var-get proposal-candidate)) ERR_STAKES_LOCKED)
    (settle-rewards caller)
    (try! (contract-call? SBTC transfer amount caller SELF none))
    (map-set stakes caller (+ current-stake amount))
    (var-set total-staked (+ (var-get total-staked) amount))
    (print {
      event: "staked",
      staker: caller,
      amount: amount,
      new-balance: (+ current-stake amount),
      total-staked: (var-get total-staked)
    })
    (ok true)
  )
)

;; Withdraw staked sBTC. Blocked during active proposals.
;; Publisher cannot unstake below MIN_PUBLISHER_STAKE while in office.
(define-public (unstake (amount uint))
  (let
    (
      (caller tx-sender)
      (current-stake (get-stake caller))
      (new-stake (- current-stake amount))
    )
    (asserts! (> amount u0) ERR_ZERO_AMOUNT)
    (asserts! (<= amount current-stake) ERR_INSUFFICIENT_STAKE)
    (asserts! (is-none (var-get proposal-candidate)) ERR_STAKES_LOCKED)
    ;; Publisher must maintain minimum stake
    (asserts!
      (or
        (not (is-eq caller (var-get publisher)))
        (>= new-stake MIN_PUBLISHER_STAKE)
      )
      ERR_PUBLISHER_STAKE_TOO_LOW
    )
    (settle-rewards caller)
    (map-set stakes caller new-stake)
    (var-set total-staked (- (var-get total-staked) amount))
    (try! (as-contract (contract-call? SBTC transfer amount SELF caller none)))
    (print {
      event: "unstaked",
      staker: caller,
      amount: amount,
      new-balance: new-stake,
      total-staked: (var-get total-staked)
    })
    (ok true)
  )
)

;; ---------------------------------------------------------------------------
;; Public -- Rewards
;; ---------------------------------------------------------------------------

;; Deposit sBTC into the reward pool. Distributed pro-rata to all stakers.
(define-public (deposit-rewards (amount uint))
  (let
    (
      (caller tx-sender)
      (staked (var-get total-staked))
    )
    (asserts! (> amount u0) ERR_ZERO_AMOUNT)
    (asserts! (> staked u0) ERR_NO_STAKERS)
    (try! (contract-call? SBTC transfer amount caller SELF none))
    ;; Track reward pool separately from stake pool
    (var-set reward-pool-balance (+ (var-get reward-pool-balance) amount))
    ;; Increase cumulative reward-per-token
    (var-set reward-per-token-stored
      (+ (var-get reward-per-token-stored) (/ (* amount PRECISION) staked))
    )
    (print {
      event: "rewards-deposited",
      depositor: caller,
      amount: amount,
      reward-pool: (var-get reward-pool-balance),
      total-staked: staked
    })
    (ok true)
  )
)

;; Claim all pending rewards. Works anytime (including during votes/abdications).
(define-public (claim-rewards)
  (let
    (
      (caller tx-sender)
    )
    (settle-rewards caller)
    (let
      (
        (reward (default-to u0 (map-get? staker-pending-rewards caller)))
      )
      (asserts! (> reward u0) ERR_NO_REWARDS)
      ;; Clamp to available reward pool (prevents insolvency from rounding dust)
      (let
        (
          (payout (if (<= reward (var-get reward-pool-balance)) reward (var-get reward-pool-balance)))
        )
        (map-set staker-pending-rewards caller (- reward payout))
        (var-set reward-pool-balance (- (var-get reward-pool-balance) payout))
        (try! (as-contract (contract-call? SBTC transfer payout SELF caller none)))
        (print {
          event: "rewards-claimed",
          staker: caller,
          amount: payout
        })
        (ok payout)
      )
    )
  )
)

;; ---------------------------------------------------------------------------
;; Public -- Abdication (voluntary handoff with delay + veto)
;; ---------------------------------------------------------------------------

;; Publisher announces intent to abdicate to a successor.
;; Successor must be a staker (owners govern, not outsiders).
;; Begins a 7-day delay during which stakers can veto.
(define-public (abdicate (successor principal))
  (let
    (
      (caller tx-sender)
      (current-publisher (var-get publisher))
      (successor-stake (get-stake successor))
    )
    (asserts! (is-eq caller current-publisher) ERR_NOT_PUBLISHER)
    ;; No abdication during active proposal or existing abdication
    (asserts! (is-none (var-get proposal-candidate)) ERR_PROPOSAL_ACTIVE)
    (asserts! (is-none (var-get abdication-successor)) ERR_ABDICATION_ACTIVE)
    ;; Successor must be a staker with skin in the game
    (asserts! (>= successor-stake MIN_PUBLISHER_STAKE) ERR_CANDIDATE_NOT_STAKER)
    ;; Successor cannot be the current publisher (no-op prevention)
    (asserts! (not (is-eq successor current-publisher)) ERR_CANDIDATE_IS_PUBLISHER)

    (var-set abdication-successor (some successor))
    (var-set abdication-start stacks-block-height)
    (var-set abdication-veto-weight u0)

    (print {
      event: "abdication-announced",
      publisher: current-publisher,
      successor: successor,
      delay-ends: (+ stacks-block-height ABDICATION_DELAY)
    })
    (ok true)
  )
)

;; Stakers can veto the abdication during the delay window.
;; If >50% of staked weight vetoes, abdication is blocked.
(define-public (veto-abdication)
  (let
    (
      (caller tx-sender)
      (caller-stake (get-stake caller))
      (current-block stacks-block-height)
    )
    ;; Must have an active abdication
    (asserts! (is-some (var-get abdication-successor)) ERR_NO_ABDICATION)
    ;; Must be within delay window
    (asserts! (<= current-block (+ (var-get abdication-start) ABDICATION_DELAY)) ERR_VOTING_CLOSED)
    ;; Must be a staker
    (asserts! (> caller-stake u0) ERR_NOT_STAKER)
    ;; Must not have already vetoed
    (asserts! (not (has-vetoed-abdication caller)) ERR_ALREADY_VOTED)

    (map-set abdication-vetoed caller true)
    (var-set abdication-veto-weight (+ (var-get abdication-veto-weight) caller-stake))

    (print {
      event: "abdication-vetoed",
      voter: caller,
      weight: caller-stake,
      total-veto-weight: (var-get abdication-veto-weight)
    })

    ;; If veto threshold met, cancel immediately
    (if (>= (* (var-get abdication-veto-weight) u100) (* (var-get total-staked) ABDICATION_VETO))
      (begin
        (reset-abdication)
        (print { event: "abdication-cancelled", reason: "veto-threshold-met" })
        (ok true)
      )
      (ok true)
    )
  )
)

;; Complete the abdication after the delay window. Anyone can call.
;; Fails if veto threshold was met (abdication already cancelled).
(define-public (complete-abdication)
  (let
    (
      (current-block stacks-block-height)
      (successor (unwrap! (var-get abdication-successor) ERR_NO_ABDICATION))
      (previous-publisher (var-get publisher))
      (veto-weight (var-get abdication-veto-weight))
      (staked (var-get total-staked))
    )
    ;; Delay window must have passed
    (asserts! (> current-block (+ (var-get abdication-start) ABDICATION_DELAY)) ERR_ABDICATION_PENDING)
    ;; Veto threshold must NOT have been met
    (asserts! (< (* veto-weight u100) (* staked ABDICATION_VETO)) ERR_ABDICATION_VETO_MET)
    ;; Successor must still have sufficient stake (could have been slashed/unstaked)
    (asserts! (>= (get-stake successor) MIN_PUBLISHER_STAKE) ERR_CANDIDATE_NOT_STAKER)

    (var-set publisher successor)
    (reset-abdication)

    (print {
      event: "publisher-abdicated",
      previous-publisher: previous-publisher,
      new-publisher: successor,
      veto-weight: veto-weight,
      total-staked: staked,
      block: current-block
    })
    (ok successor)
  )
)

;; Publisher can cancel their own abdication during the delay.
(define-public (cancel-abdication)
  (let
    (
      (caller tx-sender)
    )
    (asserts! (is-eq caller (var-get publisher)) ERR_NOT_PUBLISHER)
    (asserts! (is-some (var-get abdication-successor)) ERR_NO_ABDICATION)
    (reset-abdication)
    (print { event: "abdication-cancelled", reason: "publisher-withdrew" })
    (ok true)
  )
)

;; ---------------------------------------------------------------------------
;; Public -- Proposals (supermajority vote path)
;; ---------------------------------------------------------------------------

;; Propose a new publisher. Caller must hold >= 1% of total staked.
(define-public (propose (candidate principal))
  (let
    (
      (caller tx-sender)
      (caller-stake (get-stake caller))
      (current-block stacks-block-height)
      (staked (var-get total-staked))
    )
    ;; FIX: 1% threshold (v2 bug required 100%)
    (asserts! (>= caller-stake (/ staked u100)) ERR_INSUFFICIENT_PROPOSER_STAKE)
    ;; Cannot propose yourself
    (asserts! (not (is-eq candidate caller)) ERR_SELF_PROPOSAL)
    ;; Cannot propose the current publisher (no-op prevention)
    (asserts! (not (is-eq candidate (var-get publisher))) ERR_CANDIDATE_IS_PUBLISHER)
    ;; Candidate must be a staker (owners govern)
    (asserts! (>= (get-stake candidate) MIN_PUBLISHER_STAKE) ERR_CANDIDATE_NOT_STAKER)
    ;; No active proposal or abdication
    (asserts! (is-none (var-get proposal-candidate)) ERR_PROPOSAL_ACTIVE)
    (asserts! (is-none (var-get abdication-successor)) ERR_ABDICATION_ACTIVE)
    ;; Cooldown since last proposal ended
    (asserts! (>= current-block (+ (var-get last-proposal-end) COOLDOWN)) ERR_COOLDOWN_ACTIVE)
    ;; Enough total stake for a meaningful vote
    (asserts! (>= staked MIN_TOTAL_STAKED) ERR_QUORUM_NOT_MET)

    (var-set proposal-count (+ (var-get proposal-count) u1))
    (var-set proposal-candidate (some candidate))
    (var-set proposal-proposer (some caller))
    (var-set proposal-start current-block)
    (var-set proposal-yes u0)
    (var-set proposal-no u0)

    (print {
      event: "proposal-created",
      id: (var-get proposal-count),
      candidate: candidate,
      proposer: caller,
      start-block: current-block,
      end-block: (+ current-block VOTE_WINDOW),
      total-staked: staked
    })
    (ok (var-get proposal-count))
  )
)

;; Cast a vote. Weight = your staked sBTC.
(define-public (vote (support bool))
  (let
    (
      (caller tx-sender)
      (caller-stake (get-stake caller))
      (current-block stacks-block-height)
      (pid (var-get proposal-count))
      (start (var-get proposal-start))
    )
    (asserts! (is-some (var-get proposal-candidate)) ERR_NO_PROPOSAL)
    (asserts! (<= current-block (+ start VOTE_WINDOW)) ERR_VOTING_CLOSED)
    (asserts! (> caller-stake u0) ERR_NOT_STAKER)
    (asserts! (not (has-voted caller)) ERR_ALREADY_VOTED)

    (map-set voted { id: pid, voter: caller } true)
    (if support
      (var-set proposal-yes (+ (var-get proposal-yes) caller-stake))
      (var-set proposal-no (+ (var-get proposal-no) caller-stake))
    )

    (print {
      event: "vote-cast",
      proposal-id: pid,
      voter: caller,
      support: support,
      weight: caller-stake
    })
    (ok support)
  )
)

;; Proposer can cancel their own proposal (releases stake locks).
(define-public (cancel-proposal)
  (let
    (
      (caller tx-sender)
      (proposer (unwrap! (var-get proposal-proposer) ERR_NO_PROPOSAL))
    )
    (asserts! (is-eq caller proposer) ERR_NOT_PROPOSER)
    (reset-proposal)
    (print { event: "proposal-cancelled", reason: "proposer-withdrew" })
    (ok true)
  )
)

;; ---------------------------------------------------------------------------
;; Public -- Resolution
;; ---------------------------------------------------------------------------

;; Finalize a successful proposal. Anyone can call after voting window closes.
(define-public (finalize)
  (let
    (
      (current-block stacks-block-height)
      (start (var-get proposal-start))
      (candidate (unwrap! (var-get proposal-candidate) ERR_NO_PROPOSAL))
      (yes (var-get proposal-yes))
      (no (var-get proposal-no))
      (votes-cast (+ yes no))
      (staked (var-get total-staked))
      (previous-publisher (var-get publisher))
    )
    ;; Voting window must be closed
    (asserts! (> current-block (+ start VOTE_WINDOW)) ERR_VOTING_OPEN)
    ;; Quorum: votes-cast * 100 >= total-staked * QUORUM
    (asserts! (>= (* votes-cast u100) (* staked QUORUM)) ERR_QUORUM_NOT_MET)
    ;; Threshold: yes * 100 >= votes-cast * THRESHOLD
    (asserts! (>= (* yes u100) (* votes-cast THRESHOLD)) ERR_THRESHOLD_NOT_MET)
    ;; Candidate must still have sufficient stake
    (asserts! (>= (get-stake candidate) MIN_PUBLISHER_STAKE) ERR_CANDIDATE_NOT_STAKER)

    (var-set publisher candidate)
    (reset-proposal)

    (print {
      event: "publisher-changed",
      previous-publisher: previous-publisher,
      new-publisher: candidate,
      proposal-id: (var-get proposal-count),
      yes-votes: yes,
      no-votes: no,
      votes-cast: votes-cast,
      total-staked: staked,
      block: current-block
    })
    (ok candidate)
  )
)

;; Cancel a failed proposal. Anyone can call after voting window closes.
(define-public (cancel-failed)
  (let
    (
      (current-block stacks-block-height)
      (start (var-get proposal-start))
      (yes (var-get proposal-yes))
      (no (var-get proposal-no))
      (votes-cast (+ yes no))
      (staked (var-get total-staked))
    )
    (asserts! (is-some (var-get proposal-candidate)) ERR_NO_PROPOSAL)
    (asserts! (> current-block (+ start VOTE_WINDOW)) ERR_VOTING_OPEN)
    ;; Either quorum not met OR threshold not met
    (asserts!
      (or
        (< (* votes-cast u100) (* staked QUORUM))
        (< (* yes u100) (* votes-cast THRESHOLD))
      )
      ERR_USE_FINALIZE
    )
    (reset-proposal)
    (print {
      event: "proposal-failed",
      proposal-id: (var-get proposal-count),
      yes-votes: yes,
      no-votes: no,
      votes-cast: votes-cast,
      total-staked: staked,
      block: current-block
    })
    (ok true)
  )
)
