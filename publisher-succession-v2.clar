;; publisher-succession-v2.clar
;;
;; Immutable sBTC-weighted publisher succession for AIBTC News.
;;
;; The publisher has full operational authority over the network.
;; The ONLY on-chain governance action is replacing the publisher.
;;
;; Two paths to succession:
;;   1. Abdication -- the publisher voluntarily hands off the role.
;;   2. Supermajority vote -- 95% of participating voters, with 50% quorum.
;;
;; Staking:
;;   - Anyone can stake sBTC into this contract.
;;   - Staked sBTC = voting weight. 1 sat = 1 vote.
;;   - All movement (stake AND unstake) locked during active proposals.
;;   - Stakers earn pro-rata rewards from sBTC deposited into the reward pool.
;;
;; Voting:
;;   - 50% of total staked must participate (quorum).
;;   - 95% of votes cast must be yes (threshold).
;;   - Abstaining does not count for or against -- only quorum matters.
;;   - 7-day voting window. 7-day cooldown between proposals.
;;   - You cannot propose yourself as publisher.
;;
;; Rewards:
;;   - Anyone can deposit sBTC into the reward pool (deposit-rewards).
;;   - Rewards accrue to stakers proportional to their stake.
;;   - Rewards can be claimed anytime, including during votes.
;;
;; IMMUTABLE: No admin functions. No upgrade path. No owner key.
;; Deploy once, runs forever.

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(define-constant SBTC 'SM3VDXK3WZZSA84XXFKAFAF15NNZX32CTSG82JFQ4.sbtc-token)
(define-constant SELF (as-contract tx-sender))

(define-constant THRESHOLD u95)            ;; 95% of votes cast must be yes
(define-constant QUORUM u50)               ;; 50% of total staked must participate
(define-constant VOTE_WINDOW u1008)        ;; ~7 days (10-min blocks)
(define-constant COOLDOWN u1008)           ;; ~7 days between proposals
(define-constant MIN_TOTAL_STAKED u1000000) ;; 0.01 BTC minimum to propose

;; Precision factor for reward-per-token accumulator (1e12)
(define-constant PRECISION u1000000000000)

;; ---------------------------------------------------------------------------
;; Errors
;; ---------------------------------------------------------------------------

(define-constant ERR_ZERO_AMOUNT (err u2000))
(define-constant ERR_INSUFFICIENT_STAKE (err u2001))
(define-constant ERR_STAKES_LOCKED (err u2002))
(define-constant ERR_PROPOSAL_ACTIVE (err u2003))
(define-constant ERR_NO_PROPOSAL (err u2004))
(define-constant ERR_VOTING_CLOSED (err u2005))
(define-constant ERR_VOTING_OPEN (err u2006))
(define-constant ERR_ALREADY_VOTED (err u2007))
(define-constant ERR_COOLDOWN_ACTIVE (err u2008))
(define-constant ERR_SELF_PROPOSAL (err u2009))
(define-constant ERR_THRESHOLD_NOT_MET (err u2010))
(define-constant ERR_QUORUM_NOT_MET (err u2011))
(define-constant ERR_USE_FINALIZE (err u2012))
(define-constant ERR_NOT_PUBLISHER (err u2013))
(define-constant ERR_NOT_STAKER (err u2014))
(define-constant ERR_INSUFFICIENT_PROPOSER_STAKE (err u2015))
(define-constant ERR_NO_REWARDS (err u2016))
(define-constant ERR_NO_STAKERS (err u2017))

;; ---------------------------------------------------------------------------
;; State -- Governance
;; ---------------------------------------------------------------------------

(define-data-var publisher principal tx-sender)
(define-data-var total-staked uint u0)
(define-data-var proposal-count uint u0)
(define-data-var proposal-candidate (optional principal) none)
(define-data-var proposal-proposer (optional principal) none)
(define-data-var proposal-start uint u0)
(define-data-var proposal-yes uint u0)
(define-data-var proposal-no uint u0)
(define-data-var last-proposal-end uint u0)

(define-map stakes principal uint)
(define-map voted { id: uint, voter: principal } bool)

;; ---------------------------------------------------------------------------
;; State -- Rewards (MasterChef accumulator pattern)
;; ---------------------------------------------------------------------------

;; Cumulative reward per staked sat, scaled by PRECISION.
;; Monotonically increasing. Never reset.
(define-data-var reward-per-token-stored uint u0)

;; Per-staker: reward-per-token at time of last interaction.
;; Used to calculate pending rewards since last settlement.
(define-map staker-reward-debt principal uint)

;; Per-staker: accumulated rewards not yet claimed.
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

(define-read-only (has-voted (voter principal))
  (default-to false (map-get? voted { id: (var-get proposal-count), voter: voter }))
)

;; Returns the staker's total claimable rewards (settled + pending).
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

;; Settle a staker's pending rewards before changing their stake.
;; Moves any unsettled rewards into staker-pending-rewards and updates debt.
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

;; Stake sBTC into this contract. Increases your voting weight.
;; Blocked during active proposals -- commit capital before the vote, not during.
(define-public (stake (amount uint))
  (let
    (
      (caller tx-sender)
      (current-stake (get-stake caller))
    )
    (asserts! (> amount u0) ERR_ZERO_AMOUNT)
    (asserts! (is-none (var-get proposal-candidate)) ERR_STAKES_LOCKED)
    ;; Settle rewards before changing stake
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
(define-public (unstake (amount uint))
  (let
    (
      (caller tx-sender)
      (current-stake (get-stake caller))
    )
    (asserts! (> amount u0) ERR_ZERO_AMOUNT)
    (asserts! (<= amount current-stake) ERR_INSUFFICIENT_STAKE)
    (asserts! (is-none (var-get proposal-candidate)) ERR_STAKES_LOCKED)
    ;; Settle rewards before changing stake
    (settle-rewards caller)
    (map-set stakes caller (- current-stake amount))
    (var-set total-staked (- (var-get total-staked) amount))
    (try! (as-contract (contract-call? SBTC transfer amount SELF caller none)))
    (print {
      event: "unstaked",
      staker: caller,
      amount: amount,
      new-balance: (- current-stake amount),
      total-staked: (var-get total-staked)
    })
    (ok true)
  )
)

;; ---------------------------------------------------------------------------
;; Public -- Rewards
;; ---------------------------------------------------------------------------

;; Deposit sBTC into the reward pool. Distributed pro-rata to all stakers.
;; Anyone can call -- typically the publisher sharing operational revenue.
(define-public (deposit-rewards (amount uint))
  (let
    (
      (caller tx-sender)
      (staked (var-get total-staked))
    )
    (asserts! (> amount u0) ERR_ZERO_AMOUNT)
    (asserts! (> staked u0) ERR_NO_STAKERS)
    (try! (contract-call? SBTC transfer amount caller SELF none))
    ;; Increase the cumulative reward-per-token
    (var-set reward-per-token-stored
      (+ (var-get reward-per-token-stored) (/ (* amount PRECISION) staked))
    )
    (print {
      event: "rewards-deposited",
      depositor: caller,
      amount: amount,
      total-staked: staked
    })
    (ok true)
  )
)

;; Claim all pending rewards. Works during active proposals.
(define-public (claim-rewards)
  (let
    (
      (caller tx-sender)
    )
    ;; Settle to move any pending into claimable
    (settle-rewards caller)
    (let
      (
        (reward (default-to u0 (map-get? staker-pending-rewards caller)))
      )
      (asserts! (> reward u0) ERR_NO_REWARDS)
      (map-set staker-pending-rewards caller u0)
      (try! (as-contract (contract-call? SBTC transfer reward SELF caller none)))
      (print {
        event: "rewards-claimed",
        staker: caller,
        amount: reward
      })
      (ok reward)
    )
  )
)

;; ---------------------------------------------------------------------------
;; Public -- Abdication (voluntary handoff)
;; ---------------------------------------------------------------------------

;; Publisher voluntarily transfers the role. Immediate. No vote needed.
;; Cannot abdicate during an active proposal -- would be unfair to voters.
(define-public (abdicate (successor principal))
  (let
    (
      (caller tx-sender)
      (current-publisher (var-get publisher))
    )
    (asserts! (is-eq caller current-publisher) ERR_NOT_PUBLISHER)
    (asserts! (is-none (var-get proposal-candidate)) ERR_PROPOSAL_ACTIVE)
    (var-set publisher successor)
    (print {
      event: "publisher-abdicated",
      previous-publisher: current-publisher,
      new-publisher: successor,
      block: stacks-block-height
    })
    (ok successor)
  )
)

;; ---------------------------------------------------------------------------
;; Public -- Proposals
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
    ;; Must hold >= 1% of total staked to propose (anti-grief)
    (asserts! (>= (* caller-stake u100) staked) ERR_INSUFFICIENT_PROPOSER_STAKE)
    ;; Cannot propose yourself
    (asserts! (not (is-eq candidate caller)) ERR_SELF_PROPOSAL)
    ;; No active proposal
    (asserts! (is-none (var-get proposal-candidate)) ERR_PROPOSAL_ACTIVE)
    ;; Cooldown since last proposal ended
    (asserts! (>= current-block (+ (var-get last-proposal-end) COOLDOWN)) ERR_COOLDOWN_ACTIVE)
    ;; Enough total stake for a meaningful vote
    (asserts! (>= staked MIN_TOTAL_STAKED) ERR_QUORUM_NOT_MET)

    ;; Create proposal
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
    ;; Must have an active proposal
    (asserts! (is-some (var-get proposal-candidate)) ERR_NO_PROPOSAL)
    ;; Must be within voting window
    (asserts! (<= current-block (+ start VOTE_WINDOW)) ERR_VOTING_CLOSED)
    ;; Must have stake
    (asserts! (> caller-stake u0) ERR_NOT_STAKER)
    ;; Must not have already voted
    (asserts! (not (has-voted caller)) ERR_ALREADY_VOTED)

    ;; Record vote
    (map-set voted { id: pid, voter: caller } true)

    ;; Both yes and no votes are tracked. Abstaining doesn't count
    ;; toward quorum or threshold -- only explicit votes do.
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

;; ---------------------------------------------------------------------------
;; Public -- Resolution
;; ---------------------------------------------------------------------------

;; Finalize a successful proposal. Anyone can call after voting window closes.
;; Requires: 50% quorum (of total staked), 95% threshold (of votes cast).
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
    ;; Quorum: votes-cast * 100 >= total-staked * 50
    (asserts! (>= (* votes-cast u100) (* staked QUORUM)) ERR_QUORUM_NOT_MET)
    ;; Threshold: yes * 100 >= votes-cast * 95
    (asserts! (>= (* yes u100) (* votes-cast THRESHOLD)) ERR_THRESHOLD_NOT_MET)

    ;; Transfer publisher role
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

;; Cancel a proposal that failed (quorum or threshold not met).
;; Anyone can call after voting window closes.
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
    ;; Must have an active proposal
    (asserts! (is-some (var-get proposal-candidate)) ERR_NO_PROPOSAL)
    ;; Voting window must be closed
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
